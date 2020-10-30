library(tidyverse)
library(lubridate)
library(gganimate)

background_colour <- "black"
title_text <- "COVID-19 Test Positivity"

theme_set(theme_bw() + 
            theme(text = element_text(colour = "grey90"),
                  panel.background = element_rect(fill = "black", colour = "black"),
                  panel.border = element_rect(colour = "grey90"),
                  panel.grid.major = element_line(colour = "grey30"),
                  panel.grid.minor = element_blank(),
                  plot.background = element_rect(fill = "black", colour = "black"),
                  plot.title = element_text(margin = margin(6, 0, 6, 0), size = 16),
                  plot.subtitle = element_text(margin = margin(6, 0, 10, 0), size = 14),
                  axis.text = element_text(face = "bold", size = 10, colour = "grey90"),
                  # axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.position = "right",
                  # legend.spacing = unit(14.0, "points"), 
                  legend.key.size = unit(10.0, "points"),
                  legend.box.margin = margin(-10, 0, 0, 0),
                  legend.background = element_rect(fill = "black"),
                  legend.key = element_rect(fill = "black"),
                  legend.text = element_text(face = "bold", size = 6, margin=margin(0,0,0,0)),
                  strip.background = element_rect(fill = "black", colour = "grey70"),
                  strip.text = element_text(colour = "grey90", face = "bold", size = 12, margin = margin(8, 0, 8, 0))))


df <- read_rds("data/df.rds") %>% 
  arrange(ca_name, date) %>% 
  mutate(model_fit_diff = model_fit - lag(model_fit),
         upper_95_diff = upper_95 - lag(lower_95),
         lower_95_diff = lower_95 - lag(upper_95)) %>% 
  select(date, model_fit, model_fit_diff)

duration_days <- min(df$date) %--% max(df$date) / days(1) # days represented in animation
min_date <- df$date %>% min()

# expand data for plottng purposes
for(plot_date in unique(df$date)){
  if(plot_date == unique(df$date)[1]){plot_dates <- tibble()}
  plot_date = as_date(plot_date)
  plot_dates <- plot_dates %>% 
    bind_rows(tibble(date = seq(min_date, plot_date, by = "days")) %>% 
                mutate(plot_date = plot_date))}

for(area in unique(df$ca_name)){
  if(area == unique(df$ca_name)[1]){df_plot <- tibble()}
  df_plot <- df_plot %>% bind_rows(mutate(plot_dates, ca_name = area))
}

df_plot <- arrange(df_plot, plot_date) %>% 
  left_join(df)
rm(plot_dates)

p <- df_plot %>% 
  filter(ca_name != "Scotland") %>% 
  # filter(plot_date <= dmy("01/04/2020")) %>%
  ggplot(aes(x = model_fit, y = model_fit_diff, colour = ca_name, fill = ca_name, group = plot_date)) +
  scale_colour_viridis_d(option = "plasma") +
  scale_fill_viridis_d(option = "plasma") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_hline(yintercept = 0, colour = "grey30") +
  geom_path(aes(alpha = date)) +
  guides(linetype = "none", alpha = "none") +
  labs(x = "positivity", y = "positivity change", fill = "", colour = "", fill = "", 
       title = "Scotland COVID-19 Test Positivity Phase Space",
       subtitle = "{format((min_date + days(round(progress*duration_days))), format = \"%d / %m / %Y\")}") +
  transition_states(plot_date) +
  ease_aes("linear")

animate(p, width = 1920, height = 1080, type = "cairo", res = 300, fps = 30, duration = 20,
        end_pause = 60)
anim_save("pics/phase.mp4")

# convert to reasonably sized h265
file.remove("pics/phase_output.mp4")
ffmpeg_command <- "ffmpeg -i pics/phase.mp4 -c:v libx265 pics/phase_output.mp4"
system(ffmpeg_command)
file.remove("pics/phase.mp4")


# Scotland example
pscot1 <- df_plot %>% 
  filter(ca_name == "Scotland") %>% 
  ggplot(aes(x = date, y = model_fit, colour = ca_name, fill = ca_name, group = plot_date)) +
  scale_colour_viridis_d(option = "plasma", begin = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "months", date_labels = "%b %Y", expand = c(0,0)) +
  geom_path() +
  guides(linetype = "none", alpha = "none", colour = "none", fill = "none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none") +
  labs(x = "", y = "positivity", fill = "", colour = "", fill = "", 
       title = "Scotland COVID-19 Test Positivity",
       subtitle = "(test positivity and phase space)") +
  transition_states(plot_date) +
  ease_aes("linear")

animate(pscot1, width = 1280, height = 1080, type = "cairo", res = 300, fps = 30, duration = 20,
        end_pause = 30)
anim_save("pics/phase_scot1.mp4")

pscot2 <- df_plot %>% 
  filter(ca_name == "Scotland") %>% 
  ggplot(aes(x = model_fit, y = model_fit_diff, colour = ca_name, fill = ca_name, group = plot_date)) +
  scale_colour_viridis_d(option = "plasma", begin = 1) +
  scale_y_continuous(position = "right") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_path() +
  guides(linetype = "none", alpha = "none", colour = "none", fill = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(x = "positivity", y = "positivity change", fill = "", colour = "", fill = "", 
       title = "Scotland COVID-19 Test Positivity",
       subtitle = "(test positivity and phase space)") +
  theme(plot.title = element_text(colour = "black"),
        plot.subtitle = element_text(colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  transition_states(plot_date) +
  ease_aes("linear")

animate(pscot2, width = 640, height = 1080, type = "cairo", res = 300, fps = 30, duration = 20,
        end_pause = 30)
anim_save("pics/phase_scot2.mp4")

# put two animations side-by-side and convert to h265
file.remove("pics/phase_scot_output.mp4")
ffmpeg_command <- "ffmpeg -i pics/phase_scot1.mp4 -i pics/phase_scot2.mp4 -filter_complex hstack pics/phase_scot_output.mp4"
system(ffmpeg_command)
file.remove(c("pics/phase_scot1.mp4", "pics/phase_scot2.mp4"))
