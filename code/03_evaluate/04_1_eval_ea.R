# Description ------------------------------------------------------------------

# Title: Extrinsic Evaluation 2 - EA
#
# This R code replicates Table 2 from Chapter 4.1 of the paper "Whatever it takes to
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
#   3. Shadow Rate: The shadow Wu-Xia-Shadow Rate is nescesary to replicate the results.
#   The Rate can be obtained from the following website (last accessed: 01.10.2023):
#   https://sites.google.com/view/jingcynthiawu/shadow-rates
#
#   4. Access to all trained LLM's.
#
#   5. A personalized FRED API key (line 42). Otherwise, the datasets can be obtained
#   by hand from the FRED website. They key-identifier is supplied for every index.


# Attention: Running the code takes around 20 minutes on a modern computer.

# Preparation ------------------------------------------------------------------

library(fredr)
#https://fredaccount.stlouisfed.org/apikeys
fredr_set_key("your_key_here")
library(quanteda)
library(caret)

# Data -------------------------------------------------------------------------

## Speech Data ------------

##### Load Speech-data
dataset <- readRDS("data/processed/text_data.Rds") %>%
filter(cb == "European Central Bank", language == "en", date < "2021-12-31", date > "2000-01-01") %>%
mutate(quarter = floor_date(date, "month")) %>%
select(quarter, doc_id, text)

#### Document IDs with corresponding dates
docid_date <- dataset %>% select(date = quarter, doc_id)

##### Speech Train/Test Document-Frequency-Matrix (DFM)
dfm <- corpus(dataset, text_field = "text") %>%
  tokens() %>%
  dfm()

##### Split dfm into train and test set sets with fixed seed
set.seed(42)
split <- sample(1:nrow(dfm), size = nrow(dfm) * 0.8, replace = FALSE)
dfm_train <- dfm[split, ]
dfm_test <- dfm[-split, ]
rm(dfm)


# Macro Data ------------

##### Load Macroeconomic Indicators from FRED Database:
INTEREST_RATE <- fredr(c("IR3TIB01EZM156N")) %>% select(date, INTEREST_RATE = value) # 3-Month Interbank Interest Rate (IR3TIB01USM156N)
CPI <- fredr(c("CPHPTT01EZM659N")) %>% select(date, CPI = value) # CPI: Total for Euro Area (CPALTT01USM659N)
PRODUCTION <- fredr(c("PRMNTO01EZQ661N"), units = "chg") %>% select(date, PRODUCTION = value) # Production, Sales, Work Started and Orders: Production Volume: Economic Activity (PRMNTO01EZQ661N)
UNRATE <- fredr(c("LRHUTTTTEZM156S")) %>% select(date, UNRATE = value) # Unemployment Rate (LRHUTTTTEUM156S)
SHADOW_RATE <- read_csv("data/foreign_data/shadowrate_ecb.csv") %>% transmute(SHADOW_RATE = shadow_rate, date = ym(date)) # Wu-Xia Shadow Short Rate


#### Combine Macro-Data:
macro_df <- INTEREST_RATE %>%
  left_join(CPI) %>%
  left_join(PRODUCTION) %>%
  left_join(UNRATE) %>%
  left_join(SHADOW_RATE)
macro_df <- macro_df %>%
  group_by(date = floor_date(date, "month")) %>%
  summarise_all(.funs = mean) %>%
  filter(date > "1995-01-01", date < "2021-01-01")
macro_df <- macro_df %>% fill(PRODUCTION, .direction = "down")

#### Combine Interest Rate and Shadow Rate
macro_df <- macro_df %>% mutate(SHADOW_RATE = if_else(is.na(SHADOW_RATE), INTEREST_RATE, SHADOW_RATE))
macro_df <- macro_df %>% select(-INTEREST_RATE)
macro_df <- macro_df %>% na.omit()

### Table A8: Statistical Summary
stargazer::stargazer(data.frame(macro_df),
  type = "text", digits = 2,
  summary.stat = c("n", "mean", "sd", "p25", "median", "p75"),
  title = "Summary Statistics Evaluation"
)

#### Monetary policy shocks
ols <- lm(SHADOW_RATE ~ UNRATE + CPI + PRODUCTION, macro_df)
macro_df$d.int <- ols$residuals


# Functions --------------------------------------------------------------------

# The following two functions are used in the subsequent code to train and
# evaluate models predicting monetary policy shocks.


####### Elastic Net function with cross-validation
function_elastic_net <- function(train) {
  train_cont <- trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 1, # 5,
    search = "random",
    verboseIter = TRUE
  )
  set.seed(42)
  elastic_net <- train(d.int ~ .,
    data = train,
    method = "glmnet",
    preProcess = c("center", "scale"),
    tuneLength = 10, # 50
    trControl = train_cont
  )
  return(elastic_net)
}




##### Word-Embedding to Document Embedding function.

# This function prepares a document matrix for modeling. It identifies the
# shared vocabulary between the document-frequency matrix (dfm) and the
# embedding matrix, then multiplies the dfm with the transposed embedding
# matrix to get a document-embedding matrix. Finally, it joins this matrix
# with the macro data frame.

function_document_matrix <- function(dfm, embedding, macro_df) {
  # shared vocab
  dfm_vocab <- dfm %>%
    featnames() %>%
    tibble() %>%
    rename(token = ".")
  embedding_vocab <- embedding %>% select(token)
  vocab <- inner_join(dfm_vocab, embedding_vocab)
  # document matrix train
  date1 <- docvars(dfm)
  dfm <- dfm[, vocab$token] %>% convert(to = "matrix")
  embedding <- embedding %>%
    filter(token %in% vocab$token) %>%
    pivot_longer(-token, names_to = "dimension") %>%
    cast_sparse(token, dimension, value)
  df_train <- dfm %*% embedding %>% as("matrix")
  df_train <- df_train %>%
    as_tibble() %>%
    mutate(date = ymd(date1$quarter))
  df_train <- df_train %>%
    left_join(macro_df) %>%
    select(-starts_with("V"), starts_with("V"))
  return(df_train)
}







# No Model ---------------------------------------------------------------------


#### Initialize performance tables to store Mean Squared Error (MSE) for each model.
performance <- tibble(model = character(), mse = numeric())
performance_docs <- tibble(model = character(), mse = numeric())


# Record performance of the "No Model" baseline.
# This baseline simply uses the actual values of the monetary policy shock (d.int) as "predictions,"
# effectively representing a scenario with no predictive model.

###### Prepare the test set for the baseline calculation.
# Select a single arbitrary term from the test DFM. This is done because the "No Model" baseline
# doesn't use any textual information for prediction; we just need the dates to join with macro data.
test <- dfm_test[, 1] # Select the first term (arbitrary choice)

##### Extract dates corresponding to the test set documents.
date1 <- docvars(dfm_test)

#### Convert the selected term to a data frame, add the dates, and join with macroeconomic data.
test <- test %>%
  convert(to = "data.frame") %>%
  tibble() %>%
  mutate(date = as.Date(date1$quarter)) %>%
  left_join(macro_df, by = "date") %>%
  select(date, d.int) %>%
  na.omit()

##### Calculate the MSE for the "No Model" baseline.
# Since we're not using a model, the "predictions" are just the actual d.int values.
# Therefore, the MSE is calculated as the mean of the squared d.int values.
# Add the "No Model" performance to the performance tables:
performance <- performance %>% add_row(model = "No Model", mse = mean(test$d.int^2))
performance_docs <- performance_docs %>% add_row(model = "No Model", mse = mean(test$d.int^2))

# Dictionary approaches --------------------------------------------------------

#### Loughran and McDonald (2011) --------------

#### Train set
train <- dfm_train %>% dfm_lookup(dictionary = quanteda.sentiment::data_dictionary_LoughranMcDonald)
date1 <- docvars(dfm_train)
train <- train %>%
  convert(to = "data.frame") %>%
  mutate(SENTIMENT = (NEGATIVE - POSITIVE) / (NEGATIVE + POSITIVE)) %>%
  select(SENTIMENT, NEGATIVE, POSITIVE) %>%
  tibble() %>%
  mutate(date = as.Date(date1$quarter)) %>%
  left_join(macro_df, by = "date") %>%
  select(d.int, SENTIMENT, NEGATIVE, POSITIVE) %>%
  na.omit()

#### Test set
test <- dfm_test %>% dfm_lookup(dictionary = quanteda.sentiment::data_dictionary_LoughranMcDonald)
date1 <- docvars(dfm_test)
test <- test %>%
  convert(to = "data.frame") %>%
  mutate(SENTIMENT = (NEGATIVE - POSITIVE) / (NEGATIVE + POSITIVE)) %>%
  select(SENTIMENT, NEGATIVE, POSITIVE) %>%
  tibble() %>%
  mutate(date = as.Date(date1$quarter)) %>% 
  left_join(macro_df, by = "date") %>%
  select(date, d.int, SENTIMENT, NEGATIVE, POSITIVE) %>%
  na.omit()

### Evaluation
elastic_net <- function_elastic_net(train)
test$prediction <- predict(elastic_net, test)
mse <- mean((test$prediction - test$d.int)^2)
performance <- performance %>% add_row(model = "LM", mse = mse)





#### Hawk Dove by Bennani and Neuenkirch (2017) --------------

##### Hawk-Dove Dictionary by Bennani and Neuenkirch (2017)
# The keywords can be obtained in the paper in footnote 9.
hawk_dove_dic <- dictionary(list(
  HAWK = c("accelerat∗", "better", "boom∗", "emerg∗", "expansion", "fast∗", "favo(u)rabl∗", "firm∗", "great∗", "high∗", "improv∗", "increas∗", "larger", "positive", "rais∗", "ris∗", "stabili∗", "stable", "strengthen∗", "strong∗", "subdued", "unsustainable", "upside", "upswing", "upturn", "upward∗", "uncertain"),
  DOVE = c("collaps∗", "contraction", "dampen∗", "decelerat∗", "declin∗", "decreas∗", "delay∗", "depression", "destabili∗", "deteriorat∗", "difficul∗", "diminish∗", "disappear∗", "downside", "downswing", "downturn", "downward∗", "fall∗", "fragil∗", "low∗", "negative", "poor", "recession∗", "slow∗", "sluggish", "small∗", "struggling", "sustainable", "unfavo(u)rabl∗", "unstable", "weak∗", "worse")
))

#### Train set
train <- dfm_train %>% dfm_lookup(dictionary = hawk_dove_dic)
date1 <- docvars(dfm_train)
train <- train %>%
  convert(to = "data.frame") %>%
  mutate(HD = (HAWK - DOVE) / (HAWK + DOVE)) %>%
  select(HD, HAWK, DOVE) %>%
  tibble() %>%
  mutate(date = as.Date(date1$quarter))
train <- train %>% left_join(macro_df)
train <- train %>%
  select(d.int, HD, HAWK, DOVE) %>%
  na.omit()

#### Test set
test <- dfm_test %>% dfm_lookup(dictionary = hawk_dove_dic)
date1 <- docvars(dfm_test)
test <- test %>%
  convert(to = "data.frame") %>%
  mutate(HD = (HAWK - DOVE) / (HAWK + DOVE)) %>%
  select(HD, HAWK, DOVE) %>%
  tibble() %>%
  mutate(date = as.Date(date1$quarter))
test <- test %>% left_join(macro_df)
test <- test %>%
  select(date, d.int, HD, HAWK, DOVE) %>%
  na.omit()

### Evaluation
elastic_net <- function_elastic_net(train)
test$prediction <- predict(elastic_net, test)
mse <- mean((test$prediction - test$d.int)^2)
performance <- performance %>% add_row(model = "HD", mse = mse)


#### Hu, Lui (2004) Sentiment --------------

#### Train set
train <- dfm_train %>% dfm_lookup(dictionary = quanteda.sentiment::data_dictionary_HuLiu)
date1 <- docvars(dfm_train)
train <- train %>%
  convert(to = "data.frame") %>%
  mutate(SENTIMENT = (negative - positive) / (negative + positive)) %>%
  select(SENTIMENT, negative, positive) %>%
  tibble() %>%
  mutate(date = as.Date(date1$quarter)) %>%
  left_join(macro_df, by = "date") %>%
  select(d.int, SENTIMENT, negative, positive) %>%
  na.omit()

#### Test set
test <- dfm_test %>% dfm_lookup(dictionary = quanteda.sentiment::data_dictionary_HuLiu)
date1 <- docvars(dfm_test)
test <- test %>%
  convert(to = "data.frame") %>%
  mutate(SENTIMENT = (negative - positive) / (negative + positive)) %>%
  select(SENTIMENT, negative, positive) %>%
  tibble() %>%
  mutate(date = as.Date(date1$quarter)) %>%
  left_join(macro_df, by = "date") %>%
  select(date, d.int, SENTIMENT, negative, positive) %>%
  na.omit()

### Evaluation
elastic_net <- function_elastic_net(train)
test$prediction <- predict(elastic_net, test)
mse <- mean((test$prediction - test$d.int)^2)
performance <- performance %>% add_row(model = "HuLiu", mse = mse)

#### NRC Word-Emotion Lexicon by Mohammad and Turney (2013) --------------

#### Train set
train <- dfm_train %>% dfm_lookup(dictionary = quanteda.sentiment::data_dictionary_NRC)
date1 <- docvars(dfm_train)
train <- train %>%
  convert(to = "data.frame") %>%
  tibble() %>%
  mutate(date = as.Date(date1$quarter)) %>%
  left_join(macro_df, by = "date") %>%
  select(d.int, anger, anticipation, disgust, fear, joy, negative, positive, sadness, surprise, trust) %>%
  na.omit()

#### Test set
test <- dfm_test %>% dfm_lookup(dictionary = quanteda.sentiment::data_dictionary_NRC)
date1 <- docvars(dfm_test)
test <- test %>%
  convert(to = "data.frame") %>%
  tibble() %>%
  mutate(date = as.Date(date1$quarter)) %>%
  left_join(macro_df, by = "date") %>%
  select(d.int, anger, anticipation, disgust, fear, joy, negative, positive, sadness, surprise, trust) %>%
  na.omit()

### Evaluation
elastic_net <- function_elastic_net(train)
test$prediction <- predict(elastic_net, test)
mse <- mean((test$prediction - test$d.int)^2)
performance <- performance %>% add_row(model = "NRC", mse = mse)


#### All Dictionaries combined --------------

#### Train set
train <- dfm_train %>% dfm_lookup(dictionary = c(quanteda.sentiment::data_dictionary_NRC, quanteda.sentiment::data_dictionary_HuLiu, quanteda.sentiment::data_dictionary_LoughranMcDonald, hawk_dove_dic))
date1 <- docvars(dfm_train)
train <- train %>%
  convert(to = "data.frame") %>%
  tibble() %>%
  mutate(date = as.Date(date1$quarter)) %>%
  left_join(macro_df, by = "date") %>%
  # 24 dimensional representation!
  mutate(SENTIMENT = (negative - positive) / (negative + positive), HD = (HAWK - DOVE) / (HAWK + DOVE), SENTIMENT2 = (NEGATIVE - POSITIVE) / (NEGATIVE + POSITIVE)) %>%
  select(anger:DOVE, d.int, SENTIMENT, SENTIMENT2, HD) %>%
  na.omit() # 24 dimensions


#### Test set
test <- dfm_test %>% dfm_lookup(dictionary = c(data_dictionary_NRC, data_dictionary_HuLiu, data_dictionary_LoughranMcDonald, hawk_dove_dic))
date1 <- docvars(dfm_test)
test <- test %>%
  convert(to = "data.frame") %>%
  tibble() %>%
  # 24 dimensional representation!
  mutate(SENTIMENT = (negative - positive) / (negative + positive), HD = (HAWK - DOVE) / (HAWK + DOVE), SENTIMENT2 = (NEGATIVE - POSITIVE) / (NEGATIVE + POSITIVE)) %>%
  mutate(date = as.Date(date1$quarter)) %>%
  left_join(macro_df, by = "date") %>%
  select(anger:DOVE, d.int, SENTIMENT, SENTIMENT2, HD) %>%
  na.omit()

### Evaluation
elastic_net <- function_elastic_net(train)
test$prediction <- predict(elastic_net, test)
mse <- mean((test$prediction - test$d.int)^2)
performance <- performance %>% add_row(model = "All Dic", mse = mse)







# Word Embeddings --------------------------------------------------------------


##### GloVe ---------
# TODO: Add Word column
glove <- readRDS("data/embeddings/fullglove10w300.Rds") %>% rename(token = word)
train <- function_document_matrix(dfm_train, glove, macro_df) %>%
  select(d.int, starts_with("V")) %>%
  na.omit()
test <- function_document_matrix(dfm_test, glove, macro_df) %>%
  select(date, d.int, starts_with("V")) %>%
  na.omit()
elastic_net <- function_elastic_net(train)
test$prediction <- predict(elastic_net, test)
mse <- mean((test$prediction - test$d.int)^2)
performance <- performance %>% add_row(model = "GloVe", mse = mse)



##### LDA ---------
lda <- readRDS("data/embeddings/lda_word_embedding.rds") %>% rename(token = term)
colnames(lda) <- c("token", paste0("V", 1:300))
train <- function_document_matrix(dfm_train, lda, macro_df) %>%
  select(d.int, starts_with("V")) %>%
  na.omit()
test <- function_document_matrix(dfm_test, lda, macro_df) %>%
  select(date, d.int, starts_with("V")) %>%
  na.omit()
elastic_net <- function_elastic_net(train)
test$prediction <- predict(elastic_net, test)
mse <- mean((test$prediction - test$d.int)^2)
performance <- performance %>% add_row(model = "LDA", mse = mse)



##### Word2Vec Bow --------------------------
word2vec_bow <- word2vec::read.word2vec("data/embeddings/fullword2veccbow300.bin")
word2vec_bow <- as.matrix(word2vec_bow, which = "words") %>% as_tibble(rownames = "token")
train <- function_document_matrix(dfm_train, word2vec_bow, macro_df) %>%
  select(d.int, starts_with("V")) %>%
  na.omit()
test <- function_document_matrix(dfm_test, word2vec_bow, macro_df) %>%
  select(date, d.int, starts_with("V")) %>%
  na.omit()
elastic_net <- function_elastic_net(train)
test$prediction <- predict(elastic_net, test)
mse <- mean((test$prediction - test$d.int)^2)
performance <- performance %>% add_row(model = "word2vec_bow", mse = mse)



##### Word2Vec Skipgram ------------------
word2vec_skip <- word2vec::read.word2vec("data/embeddings/fullword2vecskipgram300.bin")
word2vec_skip <- as.matrix(word2vec_skip, which = "words") %>% as_tibble(rownames = "token")
train <- function_document_matrix(dfm_train, word2vec_skip, macro_df) %>%
  select(d.int, starts_with("V")) %>%
  na.omit()
test <- function_document_matrix(dfm_test, word2vec_skip, macro_df) %>%
  select(date, d.int, starts_with("V")) %>%
  na.omit()
elastic_net <- function_elastic_net(train)
test$prediction <- predict(elastic_net, test)
mse <- mean((test$prediction - test$d.int)^2)
performance <- performance %>% add_row(model = "word2vec_skip", mse = mse)


##### Doc2vec PVDM -----------------------
doc2vec_pvdm <- doc2vec::read.paragraph2vec(file = "data/embeddings/fulldoc2vecPVDM300.bin")
doc2vec_pvdm <- as.matrix(doc2vec_pvdm, which = "words") %>% as_tibble(rownames = "token")
train <- function_document_matrix(dfm_train, doc2vec_pvdm, macro_df) %>%
  select(d.int, starts_with("V")) %>%
  na.omit()
test <- function_document_matrix(dfm_test, doc2vec_pvdm, macro_df) %>%
  select(date, d.int, starts_with("V")) %>%
  na.omit()
elastic_net <- function_elastic_net(train)
test$prediction <- predict(elastic_net, test)
mse <- mean((test$prediction - test$d.int)^2)
performance <- performance %>% add_row(model = "doc2vec_pvdm", mse = mse)


##### Doc2vec BOW ----------------------
doc2vec_bow <- doc2vec::read.paragraph2vec(file = "data/embeddings/fulldoc2vecPVDBOW300.bin")
doc2vec_bow <- as.matrix(doc2vec_bow, which = "words") %>% as_tibble(rownames = "token")
train <- function_document_matrix(dfm_train, doc2vec_bow, macro_df) %>%
  select(d.int, starts_with("V")) %>%
  na.omit()
test <- function_document_matrix(dfm_test, doc2vec_bow, macro_df) %>%
  select(date, d.int, starts_with("V")) %>%
  na.omit()
elastic_net <- function_elastic_net(train)
test$prediction <- predict(elastic_net, test)
mse <- mean((test$prediction - test$d.int)^2)
performance <- performance %>% add_row(model = "doc2vec_bow", mse = mse)


##### Doc2vec BOW Pre ---------------------
doc2vec_bowpre <- doc2vec::read.paragraph2vec(file = "data/embeddings/fulldoc2vecPVDBOWpre300.bin")
doc2vec_bowpre <- as.matrix(doc2vec_bowpre, which = "words") %>% as_tibble(rownames = "token")
train <- function_document_matrix(dfm_train, doc2vec_bowpre, macro_df) %>%
  select(d.int, starts_with("V")) %>%
  na.omit()
test <- function_document_matrix(dfm_test, doc2vec_bowpre, macro_df) %>%
  select(date, d.int, starts_with("V")) %>%
  na.omit()
elastic_net <- function_elastic_net(train)
test$prediction <- predict(elastic_net, test)
mse <- mean((test$prediction - test$d.int)^2)
performance <- performance %>% add_row(model = "doc2vec_bow_pre", mse = mse)


##### Doc2vec PVDM Pre --------------------
doc2vec_pvdm_pre <- doc2vec::read.paragraph2vec(file = "data/embeddings/fulldoc2vecPVDMpre300.bin")
doc2vec_pvdm_pre <- as.matrix(doc2vec_pvdm_pre, which = "words") %>% as_tibble(rownames = "token")
train <- function_document_matrix(dfm_train, doc2vec_pvdm_pre, macro_df) %>%
  select(d.int, starts_with("V")) %>%
  na.omit()
test <- function_document_matrix(dfm_test, doc2vec_pvdm_pre, macro_df) %>%
  select(date, d.int, starts_with("V")) %>%
  na.omit()
elastic_net <- function_elastic_net(train)
test$prediction <- predict(elastic_net, test)
mse <- mean((test$prediction - test$d.int)^2)
performance <- performance %>% add_row(model = "doc2vec_pvdm_pre", mse = mse)


##### GloVe pretrain ---------------------
# glove6b = textdata::embedding_glove6b(dimensions = 300, manual_download = T) # download zip file at https://nlp.stanford.edu/data/glove.6B.zip and paste zip file at "/Users/johanneszahner/Library/Caches/textdata/glove6b"
#glove6b <- readRDS("/Users/johanneszahner/Library/Caches/textdata/glove6b/glove_6b_300.rds")
colnames(glove6b) <- c("token", paste0("V", 1:300))
train <- function_document_matrix(dfm_train, glove6b, macro_df) %>%
  select(d.int, starts_with("V")) %>%
  na.omit()
test <- function_document_matrix(dfm_test, glove6b, macro_df) %>%
  select(date, d.int, starts_with("V")) %>%
  na.omit()
elastic_net <- function_elastic_net(train)
test$prediction <- predict(elastic_net, test)
mse <- mean((test$prediction - test$d.int)^2)
performance <- performance %>% add_row(model = "glove6B", mse = mse)


##### Word2Vec pretrain ------------------
word2vec_google <- read_table2("data/embeddings/GoogleNews-vectors-negative300-SLIM.txt", col_names = FALSE, skip = 1)
word2vec_google <- word2vec_google %>% .[-234111, ] # remove word INVICTUS because of data error
colnames(word2vec_google) <- c("token", paste0("V", 1:300))
train <- function_document_matrix(dfm_train, word2vec_google, macro_df) %>%
  select(d.int, starts_with("V")) %>%
  na.omit()
test <- function_document_matrix(dfm_test, word2vec_google, macro_df) %>%
  select(date, d.int, starts_with("V")) %>%
  na.omit()
elastic_net <- function_elastic_net(train)
test$prediction <- predict(elastic_net, test)
mse <- mean((test$prediction - test$d.int)^2)
performance <- performance %>% add_row(model = "word2vec_google", mse = mse)


# Document Embeddings ----------------------------------------------------------

# Define samples:
training_docs <- tibble(doc_id = docid(dfm_train), date = docvars(dfm_train)$quarter) %>% left_join(macro_df)
testing_docs <- tibble(doc_id = docid(dfm_test), date = docvars(dfm_test)$quarter) %>% left_join(macro_df)


##### LDA ------------------------
lda <- readRDS("data/embeddings/lda_doc_embedding.rds") %>% rename(doc_id = document)
train <- training_docs %>%
  left_join(lda) %>%
  select(d.int, starts_with("V")) %>%
  na.omit()
test <- testing_docs %>%
  left_join(lda) %>%
  select(d.int, starts_with("V")) %>%
  na.omit()
elastic_net <- function_elastic_net(train)
test$prediction <- predict(elastic_net, test)
mse <- mean((test$prediction - test$d.int)^2)
performance_docs <- performance_docs %>% add_row(model = "lda", mse = mse)



##### Doc2Vec PVD BOW -----------------
doc2vec_bow <- doc2vec::read.paragraph2vec(file = "data/embeddings/fulldoc2vecPVDBOW300.bin")
doc2vec_bow <- as.matrix(doc2vec_bow, which = "docs") %>% as_tibble(rownames = "doc_id")
train <- training_docs %>%
  left_join(doc2vec_bow) %>%
  select(d.int, starts_with("V")) %>%
  na.omit()
test <- testing_docs %>%
  left_join(doc2vec_bow) %>%
  select(d.int, starts_with("V")) %>%
  na.omit()
elastic_net <- function_elastic_net(train)
test$prediction <- predict(elastic_net, test)
mse <- mean((test$prediction - test$d.int)^2)
performance_docs <- performance_docs %>% add_row(model = "doc2vec_bow", mse = mse)



##### Doc2Vec PVD BOW Pre ---------------
doc2vec_bow_pre <- doc2vec::read.paragraph2vec(file = "data/embeddings/fulldoc2vecPVDBOWpre300.bin")
doc2vec_bow_pre <- as.matrix(doc2vec_bow_pre, which = "docs") %>% as_tibble(rownames = "doc_id")
train <- training_docs %>%
  left_join(doc2vec_bow_pre) %>%
  select(d.int, starts_with("V")) %>%
  na.omit()
test <- testing_docs %>%
  left_join(doc2vec_bow_pre) %>%
  select(d.int, starts_with("V")) %>%
  na.omit()
elastic_net <- function_elastic_net(train)
test$prediction <- predict(elastic_net, test)
mse <- mean((test$prediction - test$d.int)^2)
performance_docs <- performance_docs %>% add_row(model = "doc2vec_bow_pre", mse = mse)



##### Doc2Vec PVDM ---------------
doc2vec_pvdm <- doc2vec::read.paragraph2vec(file = "data/embeddings/fulldoc2vecPVDM300.bin")
doc2vec_pvdm <- as.matrix(doc2vec_pvdm, which = "docs") %>% as_tibble(rownames = "doc_id")
train <- training_docs %>%
  left_join(doc2vec_pvdm) %>%
  select(d.int, starts_with("V")) %>%
  na.omit()
test <- testing_docs %>%
  left_join(doc2vec_pvdm) %>%
  select(d.int, starts_with("V")) %>%
  na.omit()
elastic_net <- function_elastic_net(train)
test$prediction <- predict(elastic_net, test)
mse <- mean((test$prediction - test$d.int)^2)
performance_docs <- performance_docs %>% add_row(model = "doc2vec_pvdm", mse = mse)



##### Doc2Vec PVDM Pre ---------------
doc2vec_pvdm_pre <- doc2vec::read.paragraph2vec(file = "data/embeddings/fulldoc2vecPVDMpre300.bin")
doc2vec_pvdm_pre <- as.matrix(doc2vec_pvdm_pre, which = "docs") %>% as_tibble(rownames = "doc_id")
train <- training_docs %>%
  left_join(doc2vec_pvdm) %>%
  select(d.int, starts_with("V")) %>%
  na.omit()
test <- testing_docs %>%
  left_join(doc2vec_pvdm) %>%
  select(d.int, starts_with("V")) %>%
  na.omit()
elastic_net <- function_elastic_net(train)
test$prediction <- predict(elastic_net, test)
mse <- mean((test$prediction - test$d.int)^2)
performance_docs <- performance_docs %>% add_row(model = "doc2vec_pvdm_pre", mse = mse)











# Table 4 ----------------------------------------------------------------------

performance %>%
  mutate(mse = mse / performance$mse[performance$model == "No Model"]) %>%
  arrange(-mse)
performance_docs %>%
  mutate(mse = mse / performance_docs$mse[performance_docs$model == "No Model"]) %>%
  arrange(-mse)
