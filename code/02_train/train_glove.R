library(tidyverse)
library(text2vec)

dataset_path <- "data/processed/dataset.Rds"
embeddings_path <- "data/embeddings/"
google_vectors_path <- "data/aux/train/GoogleNews-vectors-negative300-SLIM.txt"

dataset <- read_rds("data/processed/text_data.Rds")

# fit glove --------------------------------------------------------------------
tokens <- space_tokenizer(dataset$text)

# Create vocabulary. Terms will be unigrams
it <- itoken(tokens, progressbar = TRUE)
vocab <- create_vocabulary(it)

vocab <- prune_vocabulary(vocab, term_count_min = 5L)

vectorizer <- vocab_vectorizer(vocab)
tcm <- create_tcm(it, vectorizer, skip_grams_window = 10L)

glove <- GlobalVectors$new(rank = 300, x_max = 10, learning_rate = .1)
glove$fit_transform(tcm, n_iter = 20, n_threads = 12)

word_vectors <- t(glove$components) %>%
  as_tibble(rownames = "word")

saveRDS(word_vectors, paste0(embeddings_path, "fullglove10w300.Rds"))
