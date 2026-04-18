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

# Create a document id so each title stays linked to its row
io_text_tbl <- tibble(
  document = 1:length(io_corpus),
  text = sapply(io_corpus, as.character)
)

# Unigrams
io_unigram_tbl <- io_text_tbl %>%
  unnest_tokens(term, text, token = "words") %>%
  count(document, term)

# Bigrams
io_bigram_tbl <- io_text_tbl %>%
  unnest_tokens(term, text, token = "ngrams", n = 2) %>%
  count(document, term)

# Combine into one DTM called io_dtm
io_dtm <- bind_rows(io_unigram_tbl, io_bigram_tbl) %>%
  group_by(document, term) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  cast_dtm(document, term, n)

# Sparsity trimming
io_slim_dtm <- removeSparseTerms(io_dtm, .97)
io_slim_dtm_tbl <- io_slim_dtm %>% as.matrix %>% as_tibble

# Visualization
# Word cloud
wordCounts <- colSums(io_slim_dtm_tbl)
wordNames <- names(io_slim_dtm_tbl)
wordcloud::wordcloud(wordNames, wordCounts, max.words = 50)

# Analysis
# Topic analysis 
dfm2stm <- readCorpus(io_slim_dtm, type="slam")
kresult <- searchK(
  dfm2stm$documents,
  dfm2stm$vocab,
  K = seq(2, 20, by = 2)
)

png("../figs/kresult_plot.png", width = 1200, height = 900, res = 150)
par(mar = c(4, 4, 1, 1))
plot(kresult)
dev.off()

topic_model <- stm(dfm2stm$documents, 
                   dfm2stm$vocab, 
                   7)


# Publication
# Interpretation of topic analysis
labelTopics(topic_model, n=10)
findThoughts(topic_model, texts=week12_tbl$documents, n=3)
png("../figs/topic_model_summary.png", width = 1200, height = 900, res = 150)
par(mar = c(4, 4, 1, 1))
plot(topic_model, type = "summary", n = 5)
dev.off()
topicCorr(topic_model)
png("../figs/topic_corr_plot.png", width = 1200, height = 900, res = 150)
par(mar = c(4, 4, 1, 1))
plot(topicCorr(topic_model))
dev.off()