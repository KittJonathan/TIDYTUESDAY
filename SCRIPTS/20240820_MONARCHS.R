# TIDYTUESDAY CHALLENGE
# 2024
# WEEK 34
# ENGLISH MONARCHS AND MARRIAGES
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2024/2024-08-20/readme.md

# 📦 PACKAGES -------------------------------------------------------------

library(tidyverse)

# ➡️ IMPORT DATA ----------------------------------------------------------

d <- 
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-20/english_monarchs_marriages_df.csv')

# 🔎 EXPLORE THE DATA -----------------------------------------------------

glimpse(d)
