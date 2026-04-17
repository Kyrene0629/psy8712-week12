# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(tm)
library(qdap)
library(textstem)
library(tidytext)
library(parallel)
library(doParallel)
library(tictoc)
library(stm)
library(jsonlite)

# Data Import and Cleaning
# reddit_posts <- fromJSON("https://www.reddit.com/r/IOPsychology/top.json?t=year&limit=100")$data$children$data %>%
#   as_tibble()

# week12_tbl <- reddit_posts %>%
#   select(upvotes = ups, title)

# save the downloaded data files to data subfolder
# saveRDS(reddit_posts, "../data/reddit_posts.csv")
# write_csv(week12_tbl, "../data/week12_tbl.csv")

# add code to import those files instead of downloading again
reddit_posts <- read_csv("../data/reddit_posts.csv")
week12_tbl <- read_csv("../data/week12_tbl.csv")

# Create corpus
corpus <- VCorpus(VectorSource(blogs$documents))

# Visualization

# Analysis

# Publication