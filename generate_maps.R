# use_bam <- F # use mgcv::bam rather than mgcv::gam for binomial fitting (faster)
# source("get_data.R")

library(tidyverse)
library(lubridate)
library(sf)
library(rmapshaper)
library(gganimate)

background_colour <- "black"
title_text <- "COVID-19 Test Positivity"

theme_set(theme_bw() + 
            theme(
              axis.text.x = element_blank(), axis.text.y = element_blank(), 
              axis.ticks = element_blank(),
              panel.grid = element_blank(), panel.border = element_blank()) +
            theme(panel.background = element_rect(fill = background_colour, colour = background_colour),
                  plot.background = element_rect(fill = background_colour, colour = background_colour),
                  legend.background = element_rect(fill = background_colour),
                  plot.title = element_text(face = "bold", size = 16, colour = "grey90", margin=margin(10,0,0,0)),
                  plot.subtitle = element_text(face = "bold", size = 14, colour = "grey90", margin=margin(10,0,0,0)),
                  legend.text = element_text(face = "bold", size = 8, colour = "grey90"),
                  aspect.ratio = 1.1)) 

#### GET MAP ####
# https://spatialdata.gov.scot/geonetwork/srv/eng/catalog.search#/metadata/1cd57ea6-8d6e-412b-a9dd-d1c89a80ad62
scot_map_ca <- st_read("data/pub_las") %>% 
  # st_transform(crs="+proj=longlat +datum=WGS84") %>%
  ms_simplify(., drop_null_geometries = TRUE, keep = 8e-4) %>%
  ms_filter_islands(., min_area = 1e7) %>% # goodbye Millport
  select(-hectares) %>% 
  rename(ca_name = local_auth, 
         ca_code = code) %>% 
  mutate(ca_name = recode(ca_name,
                          "Eilean Siar" = "Na h-Eileanan Siar",
                          "Perth and Kinross" = "Perth & Kinross",
                          "Dumfries and Galloway" = "Dumfries & Galloway",
                          "Argyll and Bute" = "Argyll & Bute"))

box <- st_bbox(scot_map_ca)
box_expand <- ((box$ymax - box$ymin) - (box$xmax - box$xmin))/2

#### ANIMATION ####
# # example frame
# read_rds("data/df.rds") %>% 
#   filter(date <= dmy("21/04/2020")) %>%
#   select(ca_name, date, model_fit) %>% 
#   right_join(scot_map_ca) %>% 
#   ggplot() +
#   geom_sf(mapping = aes(fill = model_fit, geometry = geometry), colour = rgb(1, 1, 1, .5), size = .15) +
#   scale_fill_viridis_c(option = "plasma", labels = scales::percent_format(accuracy = 1), limits = c(0, .35), oob = scales::squish) +
#   labs(fill = "", title = title_text,
#        subtitle = "21 / 04 / 2020") +
#   coord_sf(xlim = c(box$xmin - box_expand, box$xmax + box_expand), expand = FALSE) 
# ggsave("pics/example_frame.png", dpi = 360, width = 3, height = 3, units = "in")

## data to be included in animation
# df <- read_rds("data/df.rds") # all data
# df <- read_rds("data/df.rds") %>% filter(date <= dmy("01/06/2020")) # first wave
df <- read_rds("data/df.rds") %>% filter(date >= dmy("01/08/2020")) # second wave

duration_days <- min(df$date) %--% max(df$date) / days(1) # days represented in animation
min_date <- df$date %>% min()

p <- df %>% 
  select(ca_name, date, model_fit) %>% 
  right_join(scot_map_ca) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = model_fit, geometry = geometry), colour = rgb(1, 1, 1, .5), size = .15) +
  scale_fill_viridis_c(option = "plasma", labels = scales::percent_format(accuracy = 1), limits = c(0, .35), oob = scales::squish) +
  labs(fill = "", title = title_text,
       subtitle = "{format((min_date + days(round(progress*duration_days))), format = \"%d / %m / %Y\")}") +
  coord_sf(xlim = c(box$xmin - box_expand, box$xmax + box_expand), expand = FALSE) +
  transition_states(date) +
  ease_aes("linear")

animate(p, width = 1080, height = 1080, type = "cairo", res = 300, fps = 30, duration = 15, 
        start_pause = 15,
        end_pause = 45)
anim_save("pics/second_pre.gif")

file.remove("pics/second.gif")
ffmpeg_command <- "ffmpeg -t 17 -i pics/second_pre.gif -vf \"fps=30,scale=1080:-1:flags=lanczos,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse\" -loop 0 pics/second.gif"
system(ffmpeg_command)
file.remove("pics/second_pre.gif")

rm(list = ls(all = TRUE))
gc()
# rstudioapi::restartSession()
