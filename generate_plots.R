use_bam <- F # use mgcv::bam rather than mgcv::gam for binomial fitting (faster)
source("get_data.R")

#### SETUP ####
library(tidyverse)
library(lubridate)
library(showtext)
font_add_google(name = "Lato") # https://fonts.google.com
showtext_auto()
background_colour <- viridis::plasma(1)

theme_set(theme_bw() + 
            theme(text = element_text(family = "Lato", colour = "grey90"),
                  panel.background = element_rect(fill = "black"),
                  panel.border = element_rect(colour = "grey90"),
                  panel.grid.major = element_line(colour = "grey30"),
                  panel.grid.minor = element_blank(),
                  plot.background = element_rect(fill = "black"),
                  plot.title = element_text(margin = margin(10, 0, 10, 0), size = 20),
                  plot.subtitle = element_text(margin = margin(10, 0, 30, 0), size = 14),
                  axis.text = element_text(face = "bold", size = 10, colour = "grey90"),
                  axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.position = "bottom",
                  legend.box.margin = margin(-10, 0, 0, 0),
                  legend.background = element_rect(fill = "black"),
                  legend.key = element_rect(fill = "black"),
                  legend.text = element_text(face = "bold", size = 9),
                  strip.background = element_rect(fill = "black", colour = "grey70"),
                  strip.text = element_text(colour = "grey90", face = "bold", size = 12, margin = margin(8, 0, 8, 0))))

df <- read_rds("data/df.rds")
first_wave_peaks <- read_rds("data/fwp.rds")


#### PLOTS ####
# Scotland level smoothed, with points
df %>%
  filter(ca_name == "Scotland") %>% 
  mutate(tests = daily_positive + daily_negative) %>% 
  ggplot(aes(x = date, y = model_fit)) +
  geom_line(aes(colour = ca_name), size = 1.1) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95, fill = ca_name), alpha = .2, colour = NA) +
  scale_colour_viridis_d(option = "plasma", begin = 1) +
  geom_point(aes(size = tests), colour = viridis::plasma(1, direction = -1), alpha = .08, stroke = .8) +
  scale_size_continuous(range = c(.5, 10), labels = scales::comma) +
  geom_hline(yintercept = .05, colour = "red", alpha = 1) +
  scale_fill_viridis_d(option = "plasma", begin = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
  labs(x = "", y = "", fill = "", colour = "", size = "",
       title = "Scotland COVID-19 Test Positivity",
       subtitle = "(point size indicates number of tests, line fit is smoothed binomial)") +
  theme(legend.position = "right")
ggsave("pics/plot_scot_smooth.png", dpi = 300, width = 200, height = 133, units = "mm")

# LAs, smoothed
df %>%
  filter(ca_name != "Scotland") %>% 
  ggplot(aes(x = date, y = model_fit, group = ca_name, colour = ca_name, fill = ca_name)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), alpha = .1, colour = NA) +
  geom_hline(yintercept = .05, colour = "red", alpha = 1) +
  scale_colour_viridis_d(option = "plasma") +
  scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
  labs(x = "", y = "", fill = "", colour = "",
       title = "Scotland COVID-19 Test Positivity",
       subtitle = "(local authority areas)") +
  coord_cartesian(ylim = c(0, .5)) 
ggsave("pics/plot_all_smooth.png", dpi = 300, width = 220, height = 200, units = "mm")


# Tile map, LAs alphabetically ordered
scot_first_wave_peak <- filter(first_wave_peaks, ca_name == "Scotland")$max_date

df %>%
  # filter(ca_name != "Scotland") %>%
  mutate(ca_name = factor(ca_name, levels = sort(unique(df$ca_name), decreasing = TRUE))) %>%
  select(date, ca_name, roll_positivity_rate, model_fit) %>% 
  pivot_longer(c(roll_positivity_rate, model_fit), names_to = "type") %>% 
  mutate(type = recode(type, "roll_positivity_rate" = "rolling avg",
                       "model_fit" = "binomial smooth")) %>% 
  mutate(type = factor(type, levels = c("rolling avg", "binomial smooth"))) %>% 
  ggplot(aes(x = date, y = ca_name)) +
  geom_tile(aes(fill = value)) +
  geom_vline(xintercept = scot_first_wave_peak, colour = "grey90") +
  facet_wrap(~type, ncol = 2) +
  scale_fill_viridis_c(option = "plasma", limits = c(0, .4), oob = scales::squish, 
                       labels = scales::percent_format(accuracy = 1), na.value = "black") +
  scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
  coord_cartesian(expand = FALSE) +
  labs(x = "", y = "", fill = "", colour = "",
       title = "Scotland COVID-19 Test Positivity",
       subtitle = "(first-wave peak highlighted at April 7th 2020)") +
  theme(legend.position = "right") +
  theme(axis.text.y = element_text(colour = ifelse(sort(unique(df$ca_name), decreasing = TRUE) == "Scotland", "white", "grey75")) )
ggsave("pics/plot_tile_alphabetical.png", dpi = 300, width = 320, height = 200, units = "mm")

# Tile map, ordered by peak date
df %>%
  # filter(ca_name != "Scotland") %>%
  mutate(ca_name = factor(ca_name, levels = first_wave_peaks$ca_name)) %>% 
  select(date, ca_name, roll_positivity_rate, model_fit) %>% 
  pivot_longer(c(roll_positivity_rate, model_fit), names_to = "type") %>% 
  mutate(type = recode(type, "roll_positivity_rate" = "rolling avg",
                       "model_fit" = "binomial smooth")) %>% 
  mutate(type = factor(type, levels = c("rolling avg", "binomial smooth"))) %>% 
  ggplot(aes(x = date, y = ca_name)) +
  geom_tile(aes(fill = value)) +
  geom_vline(xintercept = scot_first_wave_peak, colour = "grey90") +
  facet_wrap(~type, ncol = 2) +
  scale_fill_viridis_c(option = "plasma", limits = c(0, .4), oob = scales::squish, 
                       labels = scales::percent_format(accuracy = 1), na.value = "black") +
  scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
  coord_cartesian(expand = FALSE) +
  labs(x = "", y = "", fill = "", colour = "",
       title = "Scotland COVID-19 Test Positivity",
       subtitle = "(first-wave peak highlighted at April 7th 2020)") +
  theme(legend.position = "right") +
  theme(axis.text.y = element_text(colour = ifelse(sort(factor(unique(df$ca_name), levels = first_wave_peaks$ca_name), decreasing = F) == "Scotland", "white", "grey75")) )
ggsave("pics/plot_tile_fwpeakdate.png", dpi = 300, width = 320, height = 200, units = "mm")


# Gradient plots
first_wave_peaks <- first_wave_peaks %>% 
  mutate(peak_diff = max_date %--% scot_first_wave_peak / days(1))

# Scotland
df %>% 
  filter(ca_name == "Scotland") %>%
  arrange(ca_name, date) %>% 
  mutate(model_fit_diff = model_fit - lag(model_fit),
         upper_95_diff = upper_95 - lag(upper_95),
         lower_95_diff = lower_95 - lag(lower_95)) %>%
  ggplot(aes(x = date, y = model_fit_diff, colour = ca_name, fill = ca_name)) +
  geom_line() +
  geom_line(aes(colour = ca_name), size = 1.1) +
  geom_ribbon(aes(ymin = lower_95_diff, ymax = upper_95_diff, fill = ca_name), alpha = .2, colour = NA) +
  scale_colour_viridis_d(option = "plasma", begin = 1) +
  geom_vline(xintercept = scot_first_wave_peak, colour = "grey90") +
  scale_x_date(date_breaks = "months", date_labels = "%b %Y", expand = c(0,0)) +
  labs(x = "", y = "", fill = "", colour = "",
       title = "Scotland COVID-19 Test Positivity Gradient",
       subtitle = "(first-wave peak highlighted at April 7th 2020)") +
  coord_cartesian(xlim = c(dmy("01/03/2020"), max(df$date))) +
  theme(legend.position = "none")
ggsave("pics/plot_scot_gradient.png", dpi = 300, width = 200, height = 133, units = "mm")


# LAs
df %>% 
  # left_join(first_wave_peaks) %>% 
  # mutate(date = date + days(peak_diff)) %>%
  filter(ca_name != "Scotland") %>%
  filter(!(ca_name %in% c("Shetland Islands", "Na h-Eileanan Sia", "Orkney Islands"))) %>%
  group_by(ca_name) %>% 
  arrange(ca_name, date) %>% 
  mutate(model_fit_diff = model_fit - lag(model_fit),
         upper_95_diff = upper_95 - lag(upper_95),
         lower_95_diff = lower_95 - lag(lower_95)) %>%
  ggplot(aes(x = date, y = model_fit_diff, colour = ca_name, fill = ca_name)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_95_diff, ymax = upper_95_diff, fill = ca_name), alpha = .1, colour = NA) +
  scale_colour_viridis_d(option = "plasma") +
  scale_fill_viridis_d(option = "plasma") +
  geom_vline(xintercept = scot_first_wave_peak, colour = "grey90") +
  scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
  labs(x = "", y = "", fill = "", colour = "",
       title = "Scotland COVID-19 Test Positivity Gradient",
       subtitle = "(selected local authority areas)") +
  coord_cartesian(xlim = c(dmy("01/03/2020"), max(df$date)), expand = FALSE)
ggsave("pics/plot_all_gradient.png", dpi = 300, width = 220, height = 200, units = "mm")

# LAs, gradients shifted to match first wave peaks
df %>% 
  left_join(first_wave_peaks) %>% 
  mutate(date = date + days(peak_diff)) %>%
  filter(ca_name != "Scotland") %>%
  filter(!(ca_name %in% c("Shetland Islands", "Na h-Eileanan Sia", "Orkney Islands"))) %>%
  group_by(ca_name) %>% 
  arrange(ca_name, date) %>% 
  mutate(model_fit_diff = model_fit - lag(model_fit),
         upper_95_diff = upper_95 - lag(upper_95),
         lower_95_diff = lower_95 - lag(lower_95)) %>%
  ggplot(aes(x = date, y = model_fit_diff, colour = ca_name, fill = ca_name)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_95_diff, ymax = upper_95_diff, fill = ca_name), alpha = .1, colour = NA) +
  scale_colour_viridis_d(option = "plasma") +
  scale_fill_viridis_d(option = "plasma") +
  geom_vline(xintercept = scot_first_wave_peak, colour = "grey90") +
  scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
  labs(x = "", y = "", fill = "", colour = "",
       title = "Scotland COVID-19 Test Positivity Gradient",
       subtitle = "(selected local authority areas, coincident first-wave peaks)") +
  coord_cartesian(xlim = c(dmy("01/03/2020"), max(df$date)), expand = FALSE)
ggsave("pics/plot_all_gradient_shifted.png", dpi = 300, width = 220, height = 200, units = "mm")
