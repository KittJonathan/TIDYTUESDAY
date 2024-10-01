# TIDYTUESDAY CHALLENGE
# 2024 WEEK 40
# CHESS GAME DATA (LICHESS)
# 2024-10-01

# 📦 LOAD PACKAGES$ -------------------------------------------------------

library(tidyverse)
library(skimr)
library(summarytools)

# ⬇️ DOWNLOAD THE DATASET -------------------------------------------------

chess <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-01/chess.csv')

# 🔎 EXPLORE THE DATASET --------------------------------------------------

skim(chess)
view(dfSummary(chess))
glimpse(chess)

chess |> 
  filter((end_time - start_time) < 2e8) |> 
  ggplot(aes(x = rated, y = (end_time - start_time))) +
  geom_boxplot()
