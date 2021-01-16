library(tidyverse)
library(lubridate)
library(janitor)
library(data.table)
library(ckanr)
library(mgcv)
library(foreach)
library(doParallel)
registerDoParallel(parallel::detectCores())

use_bam <- TRUE
if(exists("use_bam") == FALSE){use_bam <- FALSE}
print(use_bam)

#### SETUP ####
ckanr_setup(url = "https://www.opendata.nhs.scot/")
covid_trends_res <- resource_show(id = "427f9a25-db22-4014-a3bc-893b68243055")
covid_trends_df <- ckan_fetch(x=covid_trends_res$url)


#### ROLLING SUMS AND AVERAGES ####
# format and add in Scotland level data
df <- covid_trends_df %>%
  clean_names() %>%
  mutate(date = ymd(date)) %>%
  select(date, ca_name, daily_positive, cumulative_negative) %>%
  arrange(ca_name, date) %>%
  bind_rows(
    group_by(., date) %>%
      summarise(daily_positive = sum(daily_positive),
                cumulative_negative = sum(cumulative_negative)) %>%
      mutate(ca_name = "Scotland") %>%
      ungroup()
  )

# insert zeros for any missing rows so rolling sums are always over a full week
date_range <- min(df$date):max(df$date) %>% as_date()
zeros <- tibble(date = rep(date_range, length(unique(df$ca_name))),
                ca_name = rep(unique(df$ca_name), length(date_range)),
                daily_positive = 0,
                cumulative_negative = NA_real_)
missing <- anti_join(zeros, df, by = c("date", "ca_name"))

# calculate daily negative tests, and add in rolling weekly totals
df <- df %>%
  bind_rows(missing) %>%
  arrange(ca_name, date) %>%
  distinct() %>%
  group_by(ca_name) %>%
  fill(cumulative_negative, .direction = "down") %>%
  mutate(daily_negative = cumulative_negative - lag(cumulative_negative),
         daily_negative = if_else(row_number() == 1, cumulative_negative, daily_negative)) %>%
  select(-cumulative_negative) %>%
  mutate(roll_week_positive = frollsum(daily_positive, n = 7),
         roll_week_negative = frollsum(daily_negative, n = 7),
         roll_positivity_rate = roll_week_positive  / (roll_week_positive + roll_week_negative),
         positivity_rate = daily_positive / (daily_positive + daily_negative))


#### BINOMIAL SMOOTH MODEL ####
# create df with test results expanded out to one row per test, for binomial fit
areas_to_expand <- unique(df$ca_name)[!(unique(df$ca_name) %in% "Scotland")] # everything but Scotland to speed things up
df_expanded <- foreach(i = 1:length(areas_to_expand), .combine = "rbind") %:%
  foreach(j = 1:length(filter(df, ca_name == areas_to_expand[i])$date), .combine = "rbind") %dopar% {
    
    area <- areas_to_expand[i]
    date_to_expand <- filter(df, ca_name == area)$date[j]
    
    positives <- filter(df, ca_name == area, date == date_to_expand)$daily_positive
    negatives <- filter(df, ca_name == area, date == date_to_expand)$daily_negative
    result <- c(rep(1, positives), rep(0, negatives))
    
    tibble(ca_name = area,
           date = as_date(date_to_expand),
           result = result)
  }

df_expanded <- df_expanded %>% bind_rows(mutate(., ca_name = "Scotland")) # add in Scotland data (ie all test results!)

# fit model and extract fit values and 95% confidence intervals
fam <- binomial()
se_95 <- qnorm(0.025, lower.tail = FALSE)

# if use_bam == TRUE will use
model_data <- foreach(i = 1:length(unique(df_expanded$ca_name)), .combine = "rbind") %dopar% {
  area <- unique(df_expanded$ca_name)[i]
  
  if(use_bam == TRUE){
    a <- bam(result ~ s(x, k = 10),
             method = "REML", family = fam,
             data = df_expanded %>% filter(ca_name == area) %>% mutate(x = as.integer(date)))
  } else{
    a <- gam(result ~ s(x, k = 10),
             method = "REML", family = fam,
             data = df_expanded %>% filter(ca_name == area) %>% mutate(x = as.integer(date)))
  }

  b <- predict.gam(a, newdata = df %>% filter(ca_name == area) %>% mutate(x = as.integer(date)), 
                   type = "link", se.fit = TRUE)
  
  as_tibble(b) %>% mutate(model_fit = fam$linkinv(fit),
                          upper_95 = fam$linkinv(fit + (se_95*se.fit)),
                          lower_95 = fam$linkinv(fit - (se_95*se.fit)),
                          ca_name = area,
                          date = filter(df, ca_name == area)$date) %>% 
                          select(date, ca_name, model_fit, upper_95, lower_95)
}

# join model data onto test data, rolling averages etc, and save
df <- df %>% 
  left_join(model_data)
write_rds(df, "data/df.rds", compress = "gz")

# export date of peak positivity for first wave
first_wave_peaks <- df %>% 
  filter(date >= dmy("01/03/2020"),
         date < dmy("01/07/2020")) %>% 
  group_by(ca_name) %>% 
  filter(model_fit == max(model_fit, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(order = row_number()) %>% 
  mutate(max_date = date) %>% 
  select(ca_name, max_date, order)
write_rds(first_wave_peaks, "data/fwp.rds")

# clean up
rm(list = ls(all = TRUE))
gc()
# rstudioapi::restartSession()
