# Description ------------------------------------------------------------------

# Title: Extrinsic Evaluation 1 - Word Prediction
#
# This R code replicates Table XXX from Chapter 4.1 of the paper "Whatever it takes to
# understand a central banker – Embedding their words using neural networks." To
# reproduce the results without modification, the following prerequisites are required:
#
#   1. Packages: All R packages listed in the project's setup file must be installed.
#   This ensures that all necessary functions and dependencies are available.
#
#   2. Text-Data: The text corpus itself is required. This corpus should be placed in
#   a subfolder named "corpus" within the same directory as this R script. This file
#   structure allows the code to locate and load the data correctly.
#
#   3. Access to all trained LLM's.
#

# Attention: Running the code takes a few hours on a modern computer.



# Speech Data ------------------------------------------------------------------

##### Load Speech-data

dataset <- readRDS("data/processed/text_data.Rds")

library(tidyverse)

# Shuffle the dataset rows and create 10 folds for cross-validation.
# The folds will be used later to train/test on subsets of the data.
set.seed(42)
dataset <- dataset %>%
  slice(sample(1:n())) %>% # randomize row order
  mutate(fold = cut(seq(1, n()), breaks = 10, labels = FALSE))



# Load libraries needed for tokenization, Keras deep learning, and word2vec models
library(tokenizers)
library(keras3)
# reticulate::install_python(version = "3.11:latest")
# keras3::install_keras()
library(word2vec)
library(doc2vec)



####### Create a unique list of words occurring in the text data
word_list <- tokenize_words(dataset$text) %>%
  unlist() %>%
  unique() %>%
  tibble(word = .)

# Save the word list to an RDS file for later re-use
saveRDS(word_list, "data/processed/wordlist.rds")

# Evaluation -------------------------------------------------------------------


# Load the word list from the RDS file
word_list <- readRDS("data/processed/wordlist.rds")

# Set the environment to CPU-only if needed
Sys.setenv("CUDA_VISIBLE_DEVICES" = -1)

# Helper function to create a quick report on how many words lack embeddings
embedding_report <- function(matrix, embedding_name) {
  total_words <- nrow(matrix)
  na_words <- matrix %>%
    summarise(sumNA = sum(is.na(.[, 2]))) %>%
    pull(sumNA)
  message(paste0(embedding_name, ": ", total_words, " words ", na_words, " (", round(na_words / total_words * 100), "%)", " missing"))
  return(list("total_words" = total_words, "na_words" = na_words))
}

# A generator function to produce skip-gram pairs and labels for training
skipgrams_generator <- function(text, tokenizer, window_size, negative_samples) {
  gen <- texts_to_sequences_generator(tokenizer, sample(text)) # Convert text to sequences of integer indices
  function() {
    skip <- generator_next(gen) %>%
      keras::skipgrams(
        vocabulary_size = tokenizer$num_words,
        window_size = window_size,
        negative_samples = negative_samples
      )
    # Convert skip-gram pairs into model input x and label y
    x <- transpose(skip$couples) %>% map(. %>% unlist() %>% as.matrix(ncol = 1))
    y <- skip$labels %>% as.matrix(ncol = 1)
    list(x, y)
  }
}

# Define a list of different embedding models that we want to evaluate
embedding_list <- c("glove", "lda_word", "fullword2veccbow300", "fullword2vecskipgram300", "fulldoc2vecPVDBOW300", "fulldoc2vecPVDM300", "fulldoc2vecPVDBOWpre300", "fulldoc2vecPVDMpre300", "word2vec_google")
# "glove6b",


# Define the path where embedding models are stored
embeddings_path <- "data/embeddings/"

# define an empty list to store all results
results <- list()

for (embedding_name in embedding_list) {
  # glove
  if (embedding_name == "glove6b") {
    embedding <- textdata::embedding_glove6b(dimensions = 300) %>%
      rename(word = token)
  } else if (embedding_name == "glove") {
    embedding <- readRDS(paste0(embeddings_path, "fullglove10w300.Rds"))

    # lda
  } else if (embedding_name == "lda_word") {
    embedding <- readRDS(paste0(embeddings_path, "lda_word_embedding.rds")) %>%
      rename(word = term)

    # word2vec
  } else if (embedding_name == "word2vec_google") {
    embedding <- read_table2(paste0(embeddings_path, "GoogleNews-vectors-negative300-SLIM.txt"),
      col_names = FALSE, skip = 1
    ) %>%
      .[-234111, ] %>% # remove word INVICTUS because of data error
      rename_at(vars(paste0("X", 2:301)), function(x) paste0("V", 1:300)) %>%
      rename(word = X1)
  } else if (embedding_name == "fullword2veccbow300") {
    embedding <- read.word2vec(paste0(embeddings_path, "fullword2veccbow300.bin")) %>%
      as.matrix() %>%
      as_tibble(rownames = "word", .name_repair = NULL)
  } else if (embedding_name == "fullword2vecskipgram300") {
    embedding <- read.word2vec(paste0(embeddings_path, "fullword2vecskipgram300.bin")) %>%
      as.matrix() %>%
      as_tibble(rownames = "word", .name_repair = NULL)

    # doc2vec
  } else if (embedding_name == "fulldoc2vecPVDBOW300") {
    embedding <- read.paragraph2vec(file = paste0(embeddings_path, "fulldoc2vecPVDBOW300.bin")) %>%
      as.matrix(which = "words") %>%
      as_tibble(rownames = "word", .name_repair = NULL)
  } else if (embedding_name == "fulldoc2vecPVDM300") {
    embedding <- read.paragraph2vec(file = paste0(embeddings_path, "fulldoc2vecPVDM300.bin")) %>%
      as.matrix(which = "words") %>%
      as_tibble(rownames = "word", .name_repair = NULL)
  } else if (embedding_name == "fulldoc2vecPVDBOWpre300") {
    embedding <- read.paragraph2vec(file = paste0(embeddings_path, "fulldoc2vecPVDBOWpre300.bin")) %>%
      as.matrix(which = "words") %>%
      as_tibble(rownames = "word", .name_repair = NULL)
  } else if (embedding_name == "fulldoc2vecPVDMpre300") {
    embedding <- read.paragraph2vec(file = paste0(embeddings_path, "fulldoc2vecPVDMpre300.bin")) %>%
      as.matrix(which = "words") %>%
      as_tibble(rownames = "word", .name_repair = NULL)
  }

  matrix <- word_list %>%
    left_join(embedding, by = "word")

  report <- embedding_report(matrix, embedding_name)

  matrix <- matrix %>%
    mutate_all(replace_na, 0) %>%
    select(-word) %>%
    as.matrix()

  embedding_size <- ncol(matrix)
  nrow(matrix)

  tokenizer <- keras::text_tokenizer(num_words = nrow(word_list)) %>%
    keras::fit_text_tokenizer(dataset$text)

  evaluation_results <- tibble("loss" = rep(NA, 10), "acc" = rep(NA, 10), "fold" = rep(NA, 10))
  train_results <- list()
  for (i in 1:10) {
    message(paste("Fold:", i))
    train_dat <- dataset %>%
      filter(fold != i)

    test_dat <- dataset %>%
      filter(fold == i)

    keras::k_clear_session()

    input_target <- layer_input(shape = 1)
    input_context <- layer_input(shape = 1)
    skipgram_model <- keras_model(
      # inputs
      list(input_target, input_context),
      # calculate dot product from 2 embedding matrices
      layer_dot(
        list(
          input_target %>%
            layer_embedding(
              input_dim = tokenizer$num_words + 1, #+1 because of mask token
              output_dim = embedding_size,
              name = "context_embedding",
              weights = list(matrix),
              trainable = FALSE,
            ) %>%
            layer_flatten(name = "context_flat"),
          input_context %>%
            layer_embedding(
              input_dim = tokenizer$num_words,
              output_dim = embedding_size,
              name = "target_embedding"
            ) %>%
            layer_flatten(name = "target_flat")
        ),
        axes = 1, name = "dot_product"
      ) %>%
        # sigmoid function
        layer_dense(units = 1, activation = "sigmoid", name = "sigmoid")
    )

    skipgram_model %>% compile(loss = "binary_crossentropy", optimizer = "adam", c("accuracy"))

    tryCatch(
      {
        skipgram_model %>%
          fit_generator(
            generator = skipgrams_generator(train_dat$text, tokenizer, 1, 10),
            steps_per_epoch = 100,
            epochs = 20,
            verbose = getOption("keras.fit_verbose", default = 1), callbacks = NULL
          )
      },
      error = function(e) {}
    )

    train_results[[i]] <- skipgram_model$history$history %>%
      purrr::map_df(., unlist)

    res <- skipgram_model %>%
      evaluate_generator(skipgrams_generator(test_dat$text_colloc, tokenizer, 1, 0), steps = 100)

    res[["fold"]] <- i

    evaluation_results[i, ] <- as.list(res)

    gc()
  }

  results[[embedding_name]] <- list("report" = report, "train_results" = train_results, "cv_results" = evaluation_results)
}

saveRDS(results, "evaluation_results.rds")


sessionInfo()
reticulate::py_exe()
reticulate::py_config()
reticulate::py_list_packages()
