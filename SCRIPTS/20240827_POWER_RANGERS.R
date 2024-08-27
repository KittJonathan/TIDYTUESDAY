# TIDYTUESDAY CHALLENGE
# 2024
# WEEK 35
# THE POWER RANGERS FRANCHISE
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2024/2024-08-27

# 📦 PACKAGES -------------------------------------------------------------

library(tidyverse)
library(tidytuesdayR)
library(showtext)

# 🔠 IMPORT THE FONT ------------------------------------------------------

font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()

# 🔽 DOWNLOAD THE DATASETS ------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2024, week = 35)

power_rangers_episodes <- tuesdata$power_rangers_episodes
power_rangers_seasons <- tuesdata$power_rangers_seasons

# 🔎 EXPLORE THE DATA -----------------------------------------------------

glimpse(power_rangers_episodes)
glimpse(power_rangers_seasons)

# 🧹 CLEAN THE DATA -------------------------------------------------------

d <- power_rangers_episodes |> 
  filter(episode_num != 0) |> 
  select(season_title:IMDB_rating) |> 
  mutate(air_date_first_episode = min(air_date),
         air_date_last_episode = max(air_date),
         IMDB_rating_mean = round(mean(IMDB_rating), 1),
         .by = season_title) |> 
  left_join(select(power_rangers_seasons, season_title, season_num)) |> 
  select(season_num, everything())

seasons_ratings <- d |> 
  distinct(season_num, season_title, IMDB_rating_mean) |> 
  mutate(overall_mean = round(mean(IMDB_rating_mean), 1)) |> 
  mutate(y = 27:1, .before = season_num)


# 📊 CREATE THE PLOT ------------------------------------------------------

for (i in 1:27) {
  
  # i <- 1
  
  p <- ggplot() +
    geom_vline(xintercept = 7.5) +
    geom_segment(data = filter(seasons_ratings, season_num == i),
                 aes(x = overall_mean, xend = IMDB_rating_mean,
                     y = y, yend = y),
                 color = "#0000ff",
                 linewidth = 1.5) +
    geom_point(data = filter(seasons_ratings, season_num == i),
               aes(x = IMDB_rating_mean, y = y),
               shape = 21,
               size = 10,
               color = "#0000ff",
               fill = "#0000ff") +
    geom_text(data = filter(seasons_ratings, season_num == i),
              aes(x = IMDB_rating_mean, y = y, label = IMDB_rating_mean) ,
              color = "white",
              size = 10,
              hjust = 0.5,
              family = "Roboto Condensed") +
    geom_segment(data = filter(seasons_ratings, season_num != i),
                 aes(x = overall_mean, xend = IMDB_rating_mean,
                     y = y, yend = y),
                 color = "#0000ff",
                 alpha = 0.2,
                 linewidth = 0.5) +
    scale_x_continuous(limits = c(6, 9), breaks = seq(6, 9, 0.5)) +
    theme_bw() +
    theme(panel.grid.major.x = element_line(linetype = "dotted", linewidth = 1),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(family = "Roboto Condensed", size = 26),
          axis.text.y = element_blank())
  
  ggsave(filename = paste0("PLOTS/TT_WEEK35_P", i, ".png"),
         plot = p, width = 12, height = 6, dpi = 320)
  
  # seasons_ratings |> 
  #   mutate(highlight = case_when(season_num == i ~ TRUE,
  #                                .default = FALSE)) |> 
  #   ggplot() +
  #   geom_segment(aes(x = overall_mean, xend = IMDB_rating_mean,
  #                    y = y, yend = y,
  #                    alpha = highlight),
  #                color = "#0000ff",
  #                linewidth = 1.5,
  #                show.legend = FALSE) +
  #   geom_text(aes(x = IMDB_rating_mean, y = y,
  #                 colour = highlight,
  #                 label = IMDB_rating_mean)) +
  #   geom_vline(xintercept = 7.5) +
  #   scale_x_continuous(limits = c(6, 9), breaks = seq(6, 9, 0.5)) +
  #   # scale_color_manual(values = c("#ffc0cb", "#ff0000")) +
  #   theme_bw() +
  #   theme(panel.grid.major.x = element_line(linetype = "dotted", linewidth = 1),
  #         panel.grid.major.y = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         axis.title = element_blank(),
  #         axis.ticks = element_blank(),
  #         axis.text.y = element_blank())
  
}

seasons_ratings |> 
  mutate(y = 27:1, .before = season_num) |> 
  ggplot() +
  geom_segment(aes(x = overall_mean, xend = IMDB_rating_mean,
                   y = y, yend = y)) +
  geom_vline(xintercept = 7.5) +
  scale_x_continuous(limits = c(6, 9), breaks = seq(6, 9, 0.5)) +
  theme_bw() +
  theme(panel.grid.major.x = element_line(linetype = "dotted", linewidth = 1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank())

# air_date_min <- min(d$air_date)
# air_date_max <- max(d$air_date)

# IMDB_rating_min <- min(d$IMDB_rating)
# IMDB_rating_max <- max(d$IMDB_rating)

# IMDB_rating_mean_min <- min(d$IMDB_rating_mean)
# IMDB_rating_mean_max <- max(d$IMDB_rating_mean)

s1 <- d |> 
  filter(season_title == "Mighty Morphin (Season 1)")

s1_rating <- s1 |> 
  distinct(IMDB_rating_mean) |> 
  pull()

s2 <- d |> 
  filter(season_title == "Mighty Morphin (Season 2)")

s1 |> 
  ggplot() +
  geom_point(aes(x = episode_num, y = IMDB_rating)) +
  geom_line(aes(x = episode_num, y = IMDB_rating)) +
  geom_hline(aes(yintercept = s1_rating)) +
  scale_y_continuous(limits = c(4, 10))

s2 |> 
  ggplot() +
  geom_point(aes(x = episode_num, y = IMDB_rating)) +
  geom_line(aes(x = episode_num, y = IMDB_rating)) +
  scale_y_continuous(limits = c(4, 10))

s1 |> 
  ggplot() +
  geom_segment(aes(x = air_date_min, xend = air_date_max,
                   y = 0, yend = 0)) +
  geom_segment(aes(x = air_date_first_episode, xend = air_date_last_episode,
                   y = 0, yend = 0), linewidth = 1)

s1
