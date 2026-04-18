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
library(RWeka)
library(rJava)

# Data Import and Cleaning
# reddit_posts <- fromJSON("https://www.reddit.com/r/IOPsychology/top.json?t=year&limit=100")$data$children$data %>%
#   as_tibble()

# week12_tbl <- reddit_posts %>%
#   select(upvotes = ups, title)

# save the downloaded data files to data subfolder
# saveRDS(reddit_posts, "../data/reddit_posts.rds")
# write_csv(week12_tbl, "../data/week12_tbl.csv")

# add code to import those files instead of downloading again
reddit_posts <- read_csv("../data/reddit_posts.csv")
week12_tbl <- read_csv("../data/week12_tbl.csv")

# Create corpus
io_corpus_original <- VCorpus(VectorSource(week12_tbl$title))

# Preprocessing using tm and qdap
remove_io_terms <- content_transformer(function(x) {
  x %>%
    str_replace_all("\\bi\\s*/\\s*o\\s+psychology\\b", " ") %>%
    str_replace_all("\\bi\\s*/\\s*o\\s+psych\\b", " ") %>%
    str_replace_all("\\bi\\s*-\\s*o\\s+psychology\\b", " ") %>%
    str_replace_all("\\bi\\s*-\\s*o\\s+psych\\b", " ") %>%
    str_replace_all("\\bio\\s+psychology\\b", " ") %>%
    str_replace_all("\\bio\\s+psych\\b", " ") %>%
    str_replace_all("\\biopsychology\\b", " ") %>%
    str_replace_all("\\bindustrial\\s+and\\s+organizational\\s+psychology\\b", " ") %>%
    str_replace_all("\\bindustrial\\s+organizational\\s+psychology\\b", " ") %>%
    str_replace_all("\\bindustrial[- ]organizational\\s+psychology\\b", " ")
})

io_corpus <- io_corpus_original %>%
  tm_map(content_transformer(replace_abbreviation)) %>%
  tm_map(content_transformer(replace_contraction)) %>%
  tm_map(content_transformer(str_to_lower)) %>%
  tm_map(remove_io_terms) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(lemmatize_strings)) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(stripWhitespace)

# Function to compare the same randomly selected row from two corpora
compare_them <- function(corpus_1, corpus_2) {
  row_id <- sample(seq_along(corpus_1), 1)
  
  cat("Row selected:", row_id, "\n\n")
  cat("Original:\n")
  cat(as.character(corpus_1[[row_id]]), "\n\n")
  cat("Processed:\n")
  cat(as.character(corpus_2[[row_id]]), "\n")
}

# Conversion into a DTM with ngram tokenization
myTokenizer <- function(x) { NGramTokenizer(x, Weka_control(min=1, max=2)) }
io_dtm <- DocumentTermMatrix(
  io_corpus, 
  control = list(tokenize = myTokenizer))
io_slim_dtm <- removeSparseTerms(io_dtm)
DTM_tbl <- io_slim_dtm %>% as.matrix %>% as_tibble

# Visualization

# Analysis

# Publication