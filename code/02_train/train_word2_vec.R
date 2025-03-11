library(tidyverse)
library(word2vec)

embeddings_path <- "data/embeddings/"

dataset <- read_rds("data/processed/text_data.Rds")

# fit word2vec -----------------------------------------------------------------
full_word2vec_cbow_300 <- word2vec(
  x = dataset$text, type = "cbow", window = 14, dim = 300, iter = 10, negative = 5, hs = FALSE, sample = 0.0001,
  min_count = 5, threads = 4
)
write.word2vec(full_word2vec_cbow_300, file = paste0(embeddings_path, "fullword2veccbow300.bin"))

full_word2vec_skipgram_300 <- word2vec(
  x = dataset$text, type = "skip-gram", window = 10, dim = 300, iter = 10, negative = 5, hs = FALSE, sample = 0.0001,
  min_count = 5, threads = 4
)
write.word2vec(full_word2vec_skipgram_300, file = paste0(embeddings_path, "fullword2vecskipgram300.bin"))
