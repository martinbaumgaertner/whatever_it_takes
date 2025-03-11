library(tidyverse)
library(text2vec)
library(tidytext)

embeddings_path <- "data/embeddings/"

dataset <- read_rds("data/processed/text_data.Rds")

# LDA --------------------------------------------------------------------------

# make dtm for topicmodels package
dataset_dtm <- dataset %>%
  select(doc_id, text) %>%
  unnest_tokens("text", text, token = "ngrams", n = 1) %>%
  count(doc_id, text) %>%
  cast_dtm(doc_id, text, n)

# number of topics
k <- c(5, 10, 25, 50, 75, 100, 200, 300, 400, 500, 750, 1000)

# containers
lda_doc_embedding_collection <- c()
lda_word_embedding_collection <- c()

# loop through LDA
for (k in k) {
  print(k)
  # LDA hyperparameter
  burnin <- 1000 # set burn in
  iter <- 2000 # set iterations
  thin <- 500 # thin the spaces between samples
  nstart <- 5 # set random starts at 5
  best <- TRUE #  return the highest probability as the result


  # run LDA
  dataset_lda <- topicmodels::LDA(
    dataset_dtm,
    k = k,
    method = "Gibbs",
    control = list(
      alpha = 50 / k,
      delta = 0.01
    ),
    nstart = nstart, best = best, burnin = burnin,
    iter = iter, thin = thin
  )


  # transform into word and document embeddings:
  lda_doc_embedding <- dataset_lda %>%
    tidy(matrix = "gamma") %>%
    pivot_wider(names_from = topic, values_from = gamma, names_prefix = "doc_dim_")
  lda_word_embedding <- dataset_lda %>%
    tidy(matrix = "beta") %>%
    pivot_wider(names_from = topic, values_from = beta, names_prefix = "word_dim_")

  lda_doc_embedding_collection <- rbind(lda_doc_embedding_collection, lda_doc_embedding %>% mutate(topic_number = k))
  lda_word_embedding_collection <- rbind(lda_word_embedding_collection, lda_word_embedding %>% mutate(topic_number = k))
}

# Export LDA model -------------------------------------------------------------

saveRDS(lda_doc_embedding_collection, file = paste0(embeddings_path, "lda_doc_embedding.rds"))
saveRDS(lda_word_embedding_collection, file = paste0(embeddings_path, "lda_word_embedding.rds"))
