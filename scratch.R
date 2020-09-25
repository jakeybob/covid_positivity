
use_bam <- TRUE # use mgcv::bam rather than mgcv::gam for binomial fitting (faster)
source("get_data.R")

library(tidyverse)
library(lubridate)
library(showtext)
font_add_google(name = "Lato") # https://fonts.google.com
showtext_auto()

theme_set(theme_bw())

df <- read_rds("data/df.rds")
first_wave_peaks <- read_rds("data/fwp.rds")

# Local Authority smooth
df %>%
  ggplot(aes(x = date, y = model_fit, group = ca_name, colour = ca_name, fill = ca_name)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), alpha = .2) +
  # scale_colour_viridis_d(option = "magma") +
  # scale_fill_viridis_d(option = "magma") +
  theme_bw()

df %>%
  # filter(date >= dmy("01/03/2020")) %>%
  # filter(date < dmy("21/04/2020")) %>%
  filter(ca_name != "Scotland") %>% 
  mutate(ca_name = factor(ca_name, levels = first_wave_peaks$ca_name)) %>% 
  ggplot(aes(x = date, y = ca_name)) +
  geom_tile(aes(fill = model_fit)) +
  scale_fill_viridis_c(option = "viridis") +
  theme_bw()

first_wave_peaks %>% 
  ggplot(aes(x = max_date, y = order)) +
  geom_line() + geom_point()
