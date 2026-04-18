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
library(caret)
library(Matrix)

# Data Import and Cleaning
# url <- "https://www.reddit.com/r/IOPsychology/new.json?limit=100"
# after <- NULL
# every_post <- list()
# cutoff_time <- Sys.time() - as.difftime(365, units = "days")
# done <- FALSE
# 
# while (!done) {
#   full_url <- paste0(url, if (!is.null(after)) paste0("&after=", after) else "")
#   res <- fromJSON(full_url)
#   posts <- res$data$children$data %>%
#     as_tibble()
#   
#   every_post[[length(every_post) + 1]] <- posts
#   after <- res$data$after
#   
#   oldest_post <- min(as.POSIXct(posts$created_utc, origin = "1970-01-01", tz = "UTC"))
#   if (is.null(after) || oldest_post <= cutoff_time) {
#     done <- TRUE
#   }
#   
#   Sys.sleep(1)
# }
# 
# reddit_posts <- bind_rows(every_post)
# saveRDS(reddit_posts, "../data/reddit_posts.rds")
# 
# week12_tbl <- reddit_posts %>%
#   select(title, upvotes = ups, created_utc) %>%
#   mutate(created_utc = as.POSIXct(created_utc, origin = "1970-01-01", tz = "UTC")) %>%
#   filter(created_utc > cutoff_time) %>%
#   select(upvotes, title)
# write_csv(week12_tbl, "../data/week12_tbl.csv")

# add code to import those files instead of downloading again
reddit_posts <- readRDS("../data/reddit_posts.rds")
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

compare_them(io_corpus_original, io_corpus)

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
  filter(!is.na(term), term != "") %>%
  group_by(document, term) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  cast_dtm(document, term, n)

# Sparsity trimming
io_slim_dtm <- removeSparseTerms(io_dtm, .995)

# Check N/k ratio and adjust sparsity threshold if needed
n_docs <- nrow(io_slim_dtm)
k_terms <- ncol(io_slim_dtm)
n_docs / k_terms
#  2.291209

# Visualization
# Word cloud
wordCounts <- slam::col_sums(io_dtm)
wordNames <- colnames(io_dtm)
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

topic_probs <- topic_model$theta
assigned_topic <- apply(topic_probs, 1, which.max)
assigned_prob <- apply(topic_probs, 1, max)
retained_doc_id <- which(slam::row_sums(io_slim_dtm) > 0)

topic_labels <- c(
  "1" = "AI & Career Advice",
  "2" = "Job Market & Application",
  "3" = "SIOP & Programs",
  "4" = "Organizations & Job Market",
  "5" = "Career Advice & Years & Application",
  "6" = "PhD & People Analytics",
  "7" = "Work & IO Field"
)

topics_tbl <- tibble(
  doc_id = retained_doc_id,
  original = week12_tbl$title[retained_doc_id],
  topic = assigned_topic,
  topic_label = topic_labels[as.character(assigned_topic)],
  probability = assigned_prob,
  upvotes = week12_tbl$upvotes[retained_doc_id]
)

# Explain in detail the specific reasoning process you used to determine the final number of topics. 

# Explain in detail the specific reasoning process for the topic labels you selected.

set.seed(1234)

ml_doc_id <- topics_tbl$doc_id
y <- topics_tbl$upvotes

x_tokens <- as.matrix(io_slim_dtm[ml_doc_id, ])

x_topics <- model.matrix(
  ~ factor(topic) + probability,
  data = topics_tbl)[, -1]

x_tokens_topics <- cbind(x_tokens, x_topics)

train_id <- createDataPartition(y, p = .80, list = FALSE)[, 1]
test_id <- setdiff(seq_along(y), train_id)

fit_tokens <- train(
  x = x_tokens[train_id, ],
  y = y[train_id],
  method = "glmnet",
  trControl = trainControl(method = "cv", 
                           number = 5),
  tuneGrid = expand.grid(alpha = c(0, 0.5, 1), 
                         lambda = 10 ^ seq(-3, 
                                           1, 
                                           length.out = 25)), 
                         metric = "RMSE")

fit_tokens_topics <- train(
  x = x_tokens_topics[train_id, ],
  y = y[train_id],
  method = "glmnet",
  trControl = trainControl(method = "cv", 
                           number = 5),
  tuneGrid = expand.grid(alpha = c(0, 0.5, 1), 
                         lambda = 10 ^ seq(-3, 
                                           1, 
                                           length.out = 25)), 
                         metric = "RMSE")

best_cv <- function(fit) {
  fit$results %>%
    filter(
      alpha == fit$bestTune$alpha,
      lambda == fit$bestTune$lambda
    ) %>%
    select(alpha, lambda, RMSE, Rsquared, MAE)
}

cv_results_tbl <- bind_rows(
  best_cv(fit_tokens) %>%
    mutate(model = "Tokens only", .before = 1),
  best_cv(fit_tokens_topics) %>%
    mutate(model = "Tokens + topics", .before = 1)
)

pred_tokens <- predict(fit_tokens, newdata = x_tokens[test_id, ])
pred_tokens_topics <- predict(fit_tokens_topics, newdata = x_tokens_topics[test_id, ])

holdout_metrics <- function(pred, obs) {
  out <- caret::postResample(pred = pred, obs = obs)
  tibble(
    RMSE = unname(out["RMSE"]),
    Rsquared = unname(out["Rsquared"]),
    MAE = mean(abs(pred - obs))
  )
}

holdout_results_tbl <- bind_rows(
  holdout_metrics(pred_tokens, y[test_id]) %>%
    mutate(model = "Tokens only", .before = 1),
  holdout_metrics(pred_tokens_topics, y[test_id]) %>%
    mutate(model = "Tokens + topics", .before = 1)
)

holdout_predictions_tbl <- tibble(
  doc_id = topics_tbl$doc_id[test_id],
  observed_upvotes = y[test_id],
  pred_tokens_only = pred_tokens,
  pred_tokens_topics = pred_tokens_topics,
  topic = topics_tbl$topic[test_id],
  topic_probability = topics_tbl$probability[test_id]
)

cv_results_tbl
holdout_results_tbl
head(holdout_predictions_tbl, 20)
