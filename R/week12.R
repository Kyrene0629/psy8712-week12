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
# I used this to download at least the last year of posts from r/IOPsychology.
# url <- "https://www.reddit.com/r/IOPsychology/new.json?limit=100"
# after <- NULL
# every_post <- list()
# cutoff_time <- Sys.time() - as.difftime(365, units = "days")
# done <- FALSE
# 
# repeat { # I use repeat() to create an infinite loop so it stops when I stop it with break. Keep requesitng pages and save each page until there are no more pages or reach to the point that it older than one year 
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
#     break
#   }
#   # this reduces the chance of rate limits
#   Sys.sleep(1)
# }
# 
# reddit_posts <- bind_rows(every_post)
# saveRDS(reddit_posts, "../data/reddit_posts.rds")
# 
# in the tibble, I only keep varibales that are required, comvert unix time to normal time, keep opsts from last year, and. ake sure it only has only upvotes and title
# week12_tbl <- reddit_posts %>%
#   select(title, upvotes = ups, created_utc) %>%
#   mutate(created_utc = as.POSIXct(created_utc, origin = "1970-01-01", tz = "UTC")) %>%
#   filter(created_utc > cutoff_time) %>%
#   select(upvotes, title)
# write_csv(week12_tbl, "../data/week12_tbl.csv")

# add code to import those files instead of downloading again
reddit_posts <- readRDS("../data/reddit_posts.rds")
week12_tbl <- read_csv("../data/week12_tbl.csv")

# Create corpus from post titles, so later the original and preproccessed version can be compared.
io_corpus_original <- VCorpus(VectorSource(week12_tbl$title))

# Preprocessing using tm and qdap by building the transformer that removes references (IO). These terms appear frequently but do not distinguish the posts meaningfully
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

# I then created the preproccessed corpus. First, I expand abbreviations and contractions. Then, lowercase text so they are not case-sensitive. 
# Then, I use the cutome transformer to remove IO terms. Then, I remove numbers and punctuation becuase they are not that meaningful for post titles.
# Then, I lemmatize after punctuation & number removal, so words are simplified. I then remove stopwords after lemmatization. finally, I strip the extra whitespace at the end
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

# Function to compare the same randomly selected row from original corpus and the processed corpus.
# I repeatedly check whether preprocessing is doing its work cleanly
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

# Unigrams are used to capture individual high-frequency concepts.
io_unigram_tbl <- io_text_tbl %>%
  unnest_tokens(term, text, token = "words") %>%
  count(document, term)

# Bigrams are used ton capture multiword concepts
io_bigram_tbl <- io_text_tbl %>%
  unnest_tokens(term, text, token = "ngrams", n = 2) %>%
  count(document, term)

# Combine into one DTM called io_dtm and filter out any blank or missing terms that might result from preprocessing.
io_dtm <- bind_rows(io_unigram_tbl, io_bigram_tbl) %>%
  filter(!is.na(term), term != "") %>%
  group_by(document, term) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  cast_dtm(document, term, n)

# Sparsity trimming
io_slim_dtm <- removeSparseTerms(io_dtm, .995)

# Check N/k ratio and adjust sparsity threshold to 0.995 to make it into the range 2:1 to 3:1
n_docs <- nrow(io_slim_dtm)
k_terms <- ncol(io_slim_dtm)
n_docs / k_terms
#  2.291209

# Visualization
# Word cloud
# I summrize total term frequencies across the full DTM and then produce a workdcloud. This can help me see what dominates discussion (only the words)
wordCounts <- slam::col_sums(io_dtm)
wordNames <- colnames(io_dtm)
wordcloud::wordcloud(wordNames, wordCounts, max.words = 50)


# Analysis
# Top Analysis
# I concert the dtm and then search for all possible topics. I then save the topic result plot into figs subfoolder, adjust the margins, draw the plot and close the graph so the file is actually written.
dfm2stm <- readCorpus(io_slim_dtm, type="slam")
kresult <- searchK(
  dfm2stm$documents,
  dfm2stm$vocab,
  K = 2:10
)
png("../figs/kresult_plot.png", width = 1200, height = 900, res = 150)
par(mar = c(4, 4, 1, 1))
plot(kresult)
dev.off()

# I fit the final topic model with 7 topics, so each document is a probability distribution of its own and each topic is a probability distribution of its own. 
# I then extract the topic probability and find tipics with their highest probability for each doc and give topic numbers. Also, I extract the highest probability and give that probability. 
topic_model <- stm(dfm2stm$documents, 
                   dfm2stm$vocab, 
                   7)
topic_probs <- topic_model$theta
assigned_topic <- apply(topic_probs, 1, which.max)
assigned_prob <- apply(topic_probs, 1, max)
# make sure the original document ids still exist after preprocessing
retained_doc_id <- which(slam::row_sums(io_slim_dtm) > 0)

# based on top words, I created labels and assign them into topic labels
topic_labels <- c(
  "1" = "AI & Career Advice",
  "2" = "Job Market & Application",
  "3" = "SIOP & Different Programs",
  "4" = "Organizations & Job Market",
  "5" = "Career Advice & Years & Application",
  "6" = "PhD & People Analytics",
  "7" = "Work & IO Field"
)

# Then, I created the tibble topics_tbl that has doc_id, original, topic, topic_label, probability, upvotes.
topics_tbl <- tibble(
  doc_id = retained_doc_id,
  original = week12_tbl$title[retained_doc_id],
  topic = assigned_topic,
  topic_label = topic_labels[as.character(assigned_topic)],
  probability = assigned_prob,
  upvotes = week12_tbl$upvotes[retained_doc_id]
)

# Explain in detail the specific reasoning process you used to determine the final number of topics. 
# I used searchK() to compare models from K = 2 to K = 10. The reason I started at 2 is because K = 1 would not meaninhfully separate the posts into different themes. 
# In the result, heldout is best at K = 7, heldout = -4.717701. Other diagnostics look better with either smaller or larger K values
# I then choose 7 because it gives the balance between model performance and interpretability because it separates the major themes well and not make the topics too broad or too narrow


# Explain in detail the specific reasoning process for the topic labels you selected
# I selected the topic labels by looking at the top words in each of the topics , so I can know what is the content
# I then make sure these words match with the actual titles in that topic
# I used the top words to figure out the main idea and then give each a short name to represent 
# I also make the label easy to understand, concise, and close to what the posts in each topic are really about (2 to 5 words) 


# set seed for reproducibility
set.seed(1234)

# for machine leanring part, I first define the id and outcomes
ml_doc_id <- topics_tbl$doc_id
y <- topics_tbl$upvotes

# I create predictors matrxi use the token counts & create topic
x_tokens <- as.matrix(io_slim_dtm[ml_doc_id, ])
x_topics <- model.matrix( # turn those into a numeric matrix
  ~ factor(topic) + probability, # turn it into dummy variables and include the ptobability
  data = topics_tbl)[, -1] # remove the intercept
x_tokens_topics <- cbind(x_tokens, x_topics) # combine them so I can compare token alone against tokens-plus-topics

# create training and holdout sets
train_id <- createDataPartition(y, p = .80, list = FALSE)[, 1]
test_id <- setdiff(seq_along(y), train_id)

# fit the token-alone elastic net model
fit_tokens <- train(
  x = x_tokens[train_id, ],
  y = y[train_id],
  method = "glmnet",
  trControl = trainControl(method = "cv", 
                           number = 5), # 5-fold cross-validation
  tuneGrid = expand.grid(alpha = c(0, 0.5, 1), 
                         lambda = 10 ^ seq(-3, 
                                           1, 
                                           length.out = 25)), 
                         metric = "RMSE")

# fit the tokens-plus-topics model, this can isolate the effect of adding topic information
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
# create a fucntion that can extract the cross-validated results, which can be used later in the CV comparison table for both models.
best_cv <- function(fit) {
  fit$results %>%
    filter(
      alpha == fit$bestTune$alpha,
      lambda == fit$bestTune$lambda
    ) %>%
    select(alpha, lambda, RMSE, Rsquared, MAE)
}

# I use this comparison table to report CV values for both the token-alone and tokens-plus-topics models
cv_results_tbl <- bind_rows(
  best_cv(fit_tokens) %>%
    mutate(model = "Tokens only", .before = 1),
  best_cv(fit_tokens_topics) %>%
    mutate(model = "Tokens + topics", .before = 1)
)

# for out-of-sample performance, I generate predictions on the holdout test set
pred_tokens <- predict(fit_tokens, newdata = x_tokens[test_id, ])
pred_tokens_topics <- predict(fit_tokens_topics, newdata = x_tokens_topics[test_id, ])

# create a fucntion that can extract the holdout prediction estimates, which can be used later in the holdout comparison table for both models.
holdout_metrics <- function(pred, obs) {
  out <- caret::postResample(pred = pred, obs = obs)
  tibble(
    RMSE = unname(out["RMSE"]),
    Rsquared = unname(out["Rsquared"]),
    MAE = mean(abs(pred - obs))
  )
}

# I use this comparison table to report holdout prediction for both the token-alone and tokens-plus-topics models
holdout_results_tbl <- bind_rows(
  holdout_metrics(pred_tokens, y[test_id]) %>%
    mutate(model = "Tokens only", .before = 1),
  holdout_metrics(pred_tokens_topics, y[test_id]) %>%
    mutate(model = "Tokens + topics", .before = 1)
)

# I then save the holdout prediction results for each doc into a tibble so that i can see whether the tokens-plus-topics model improve prediction for some posts, what topic and what probability the topic model has
holdout_predictions_tbl <- tibble(
  doc_id = topics_tbl$doc_id[test_id],
  observed_upvotes = y[test_id],
  pred_tokens_only = pred_tokens,
  pred_tokens_topics = pred_tokens_topics,
  topic = topics_tbl$topic[test_id],
  topic_probability = topics_tbl$probability[test_id]
)

# print output and show first 20 holdout predictions
cv_results_tbl
holdout_results_tbl
head(holdout_predictions_tbl, 20)

# Comment:
# cv_results_tbl
# model             alpha     lambda     RMSE    Rsquared      MAE
# Tokens only         1         10     21.27418 0.006764069 11.89745
# Tokens + topics     1         10.    21.83619 0.003940387 11.90436

# holdout_results_tbl
# A tibble: 2 × 4
# model            RMSE Rsquared   MAE
# Tokens only      11.8       NA  9.29
# Tokens + topics  11.8       NA  9.29

# The CV results show that both models perform poorly, both R squred are really close to 0. 
# The second model (tokens with topics) is a bit worse than the first model, which means adding topics do not improve predictions.
# The holdout predictions show that they have the same performance & same prediction. Therefore, adding topics do not improve holdout prediction at all.
# I used the elastic net tuning, but alpha = 1 & lambda = 10, so the tuning results show that the best model would be lasso with strong regularization not elastic net.
# lasso has strong penalty which can shrink most coefficients toward zero so predictions become almost constant for the out-of-sample performance.
