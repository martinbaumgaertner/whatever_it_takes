library(tidyverse)
library(doc2vec)

embeddings_path <- "data/embeddings/"
google_vectors_path <- "data/helper/train/GoogleNews-vectors-negative300-SLIM.txt"

dataset <- read_rds("data/processed/text_data.Rds")

# fit doc2vec -----------------------------------------------------------------
speeches_doc2vec_pvdbow_300 <- paragraph2vec(
  x = dataset, type = "PV-DBOW", window = 15, dim = 300, iter = 20, negative = 5, hs = FALSE,
  sample = 0.0001, min_count = 5, lr = 0.05, threads = 12
)
write.paragraph2vec(speeches_doc2vec_pvdbow_300, file = paste0(embeddings_path, "fulldoc2vecpvdbow300.bin"))

speeches_doc2vec_pvdm_300 <- paragraph2vec(
  x = dataset, type = "PV-DM", window = 5, dim = 300, iter = 20, negative = 5,
  sample = 0.00001,
  min_count = 5, lr = 0.05, threads = 12
)
write.paragraph2vec(speeches_doc2vec_pvdm_300, file = paste0(embeddings_path, "fulldoc2vecpvdm300.bin"))


# fit doc2vec with pre-trained word embeddings ---------------------------------
# https://code.google.com/archive/p/word2vec/
source("code/train/helper_functions/load_google.R")

google_vectors <- load_google_vectors(
  file_path = google_vectors_path,
  remove_row = 234111
)

speeches_doc2vec_pvdbow_300 <- paragraph2vec(
  x = dataset, type = "PV-DBOW", window = 15, dim = 300, iter = 20, negative = 5, hs = FALSE,
  sample = 0.0001, min_count = 5, lr = 0.05, threads = 12, embeddings = google
)
write.paragraph2vec(speeches_doc2vec_pvdbow_300, file = paste0(embeddings_path, "fulldoc2vecpvdbowpre300.bin"))

speeches_doc2vec_pvdm_300 <- paragraph2vec(
  x = dataset, type = "pv-dm", window = 5, dim = 300, iter = 20, negative = 5,
  min_count = 5, sample = 0.00001, lr = 0.05, threads = 12
)
write.paragraph2vec(speeches_doc2vec_pvdm_300, file = paste0(embeddings_path, "fulldoc2vecpvdmpre300.bin"))
