library(tidyverse)
library(janitor)
library(lubridate)
library(patchwork)

# https://informatics.sepa.org.uk/spotfire/wp/analysis?file=Public/SEPA/Projects/Sewage%20Monitoring/RNAMonitoring_Public&waid=p0CRcxcN3Eipo3SeA8J0z-1010075a00-FB3&wavid=0&options=1-0,2-0,3-0,4-0,5-0,6-0,7-0,8-1,9-0,10-0,11-0,12-0,13-0,14-0,15-0,16-0,17-0,18-1
# looks like resource url changes frequently so can't (easily) download programatically...

path <- "data/RNAMonitoring_Public - Result Description - N1 Gene, Reported Value - N1 Gene (gc-l), Days Since.csv"

df <- read.delim(path, sep="\t", fileEncoding = "UTF-16") %>% 
  as_tibble() %>% 
  clean_names() %>% 
  rename(health_board = health_area,
         result_value = reported_value_n1_gene_gc_l, 
         result_description = result_description_n1_gene,
         site = site_name) %>% 
  mutate(date = dmy(date),
         population = population %>% str_replace_all(",", "") %>% as.integer(),
         week = isoweek(date),
         week_commencing = floor_date(date, unit = "weeks", week_start = 1)) %>% 
  select(health_board, site, date, week, week_commencing, population_band, population, result_description, result_value) %>% 
  arrange(health_board, site, date, population_band) %>% 
  arrange(date)

df %>% 
  group_by(site, week_commencing) %>% 
  summarise(result_value = mean(result_value, na.rm=T)) %>% 
  ggplot(aes(x = week_commencing, y = result_value, colour = site)) +
  theme_bw() +
  geom_point() + geom_line() +
  facet_wrap(~site, scales = "free_y")

df %>% 
  group_by(site) %>% 
  mutate(result_value = as.integer(result_value)) %>%
  ggplot(aes(x = date, y = result_value, colour = site)) +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 5), method.args=list(family=poisson(), method = "REML"))   +
  theme_bw() +
  geom_point() + geom_line() +
  scale_y_continuous(breaks = NULL) +
  facet_wrap(~site, scales = "free_y")

# Stirling comparison
p1 <- df %>%
  filter(site == "Stirling") %>% 
  mutate(result_value = as.integer(result_value)) %>%
  ggplot(aes(x = date, y = result_value, colour = site)) +
  geom_vline(xintercept = dmy("26/10/2020"), alpha = .5, linetype = "dashed") +
  geom_vline(xintercept = dmy("13/09/2020"), alpha = .5, linetype = "dashed") +
  geom_point() +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 5), method.args=list(family=poisson(), method = "REML"), se = F, size = 1)   +
  theme_bw() 

days_to_peak <- dmy("13/09/2020") %--% dmy("26/10/2020") / days(1)
date_of_peak <- dmy("04/10/2020") + days(days_to_peak)

rna_lead_time <- dmy("04/10/2020") %--% dmy("13/09/2020") / days(-1)

p2 <- read_rds("data/df.rds") %>% 
  filter(ca_name == "Stirling") %>% 
  filter(date >= filter(df, site == "Stirling")$date %>% min(),
         date <= date_of_peak) %>% 
  ggplot(aes(x = date, y = model_fit, colour = ca_name)) +
  geom_vline(xintercept = dmy("04/10/2020"), alpha = .5, linetype = "dashed") +
  geom_vline(xintercept = dmy("16/11/2020"), alpha = .5, linetype = "dashed") +
  geom_line() + theme_bw()

p1 / p2

library(httr)
url <- "https://apps.sepa.org.uk/rainfall/api/Stations/115618"
a <- GET(url = url)
str(content(a))
b <- content(a, as = "parsed")

# all station info
response <- GET(url = "https://apps.sepa.org.uk/rainfall/api/Stations")
station_info <- bind_rows(content(response, as = "parsed")) %>% 
  select(starts_with("station"), ts_id, id) %>% 
  mutate(lat = as.numeric(station_latitude), long = as.numeric(station_longitude)) %>% 
  select(station_name, station_no, lat, long, station_id, ts_id, id)
write_rds(station_info, "data/sepa_station_info.rds", compress = "gz")
station_info <- read_rds("data/sepa_station_info.rds")

# # single station daily rainfall
# station_no <- 115618
# response <- GET(url = paste0("https://apps.sepa.org.uk/rainfall/api/Daily/", station_no, "?all=true"))
# json_data <- content(response, as = "parsed") 
# df_station <- bind_rows(json_data) %>% mutate(station_no = station_no)

# all station daily rainfall
df_stations <- tibble()
for(station in unique(station_info$station_no)){
  response <- GET(url = paste0("https://apps.sepa.org.uk/rainfall/api/Daily/", station, "?all=true"))
  json_data <- content(response, as = "parsed") 
  df_stations <- df_stations %>% 
    bind_rows(
    bind_rows(json_data) %>% 
    mutate(station_no = station))
}

write_rds(df_stations, "data/sepa_rain_raw.rds", compress = "gz")
df_stations <- read_rds("data/sepa_rain_raw.rds") %>% 
  mutate(rainfall = as.numeric(Value)) %>% 
  mutate(date = as_date(Timestamp, format = "%d/%m/%Y %H:%M:%S")) %>% 
  select(station_no, date, rainfall)

df_stations %>% 
  left_join(read_rds("data/sepa_station_info.rds")) %>% 
  select(station_name, station_no, date, rainfall, lat, long) %>% 
  write_rds("data/sepa_rain.rds", compress = "gz")

df <- read_rds("data/sepa_rain.rds")

df %>% 
  group_by(date) %>% 
  summarise(rainfall = sum(rainfall)) %>% 
  ggplot(aes(x = date, y = rainfall)) +
  geom_point() + geom_line()
