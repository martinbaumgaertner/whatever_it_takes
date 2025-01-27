# Description ------------------------------------------------------------------

# Title: Rhetorical Stability
#
# This R code replicates Table 2 from Chapter 3.2 of the paper "Whatever it takes to
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
# The process of retrieving context words for economic terms is computationally
# intensive on the full corpus. Therefore, the following steps were taken:
#
#   1. Corpus Segmentation: The corpus was split by central bank, and the context
#   word extraction was performed separately for each. The outputs were saved as
#   individual CSV files for subsequent analysis. For the US Federal Reserve (Fed),
#   speeches were further divided into sets of 1500 due to potential memory
#   limitations with large CSV files (exceeding 100MB) on some machines. This
#   splitting is handled by the line 77.
#
#   2. Pre-computed Context Word Files: The resulting CSV files containing the
#   extracted context words are quite large and therefore NOT included in this replication set.
#   If you are interested feel free to contact us!
#
# Consequently, the code for extracting context terms from the raw text (Part 1) is
# currently commented out. Running the code in its current form will directly read
# the pre-computed CSV files in Part 2. This significantly reduces the computational
# burden for replication.

# Preparation ------------------------------------------------------------------

# Part 1: Data (Commented Out - Context Words Pre-computed) --------------------


# The following code blocks were used to generate the context word CSV files but
# are now commented out for replication purposes. They are included here for
# transparency and to demonstrate the original processing steps.

library(tidyverse)
library(quanteda)
library(tidytext)
library(SnowballC)
# ########## Central bank text --------

# # Load speech data:
# # Filter for specific central banks (e.g., Bank of Canada):
data <- readRDS("data/processed/text_data.Rds") %>%
  filter(cb %in% c("Bank of Canada")) %>%
  select(cb, date, text) %>%
  mutate(date = year(date))

# # Split Data data if necessary (example: first 1500 speeches)
data <- data[1:1500, ]

# # Create quanteda corpus
corpus <- corpus(data)
corpus_df <- cbind(docid = corpus %>% docnames(), corpus %>% docvars())

# # Create tokens object, remove stopwords and numbers
tokens <- corpus %>%
  tokens() %>%
  tokens_remove(stopwords("en")) %>%
  tokens_select(pattern = "\\b(?!\\d+\\b)\\w+\\b", valuetype = "regex")

# # Extract context words using kwic
econ_terms <- list(econ = c("inflat*", "price", "wage", "cyclical", "growth", "employ*", "unemplo*", "recover*", "cost")) %>% dictionary()
kwic <- kwic(tokens, pattern = econ_terms$econ, window = 6)
kwic <- data.frame(docid = kwic$docname, KEYWORD = kwic$keyword, TEXT = cbind(kwic$pre, kwic$post)) %>%
  mutate(TEXT = paste0(TEXT.1, " ", TEXT.2)) %>%
  select(-c(TEXT.1, TEXT.2)) %>%
  left_join(corpus_df) %>%
  unnest_tokens("TOKENS", TEXT, token = "words")
kwic$TOKENS <- kwic$TOKENS %>% wordStem(language = "en")
# kwic


# # Write context words to CSV
write.csv(kwic, "ddata/processed/stability/kwick_boc.csv")

# ########## Random Wikipedia text corpus ----------

# # Load Random Wikipedia Articles using the getwiki package:
wiki <- c()
for (i in 1:1000) {
  set.seed(124)
  txt <- getwiki::random_wiki(clean = TRUE)
  wiki <- c(wiki, txt)
}

# # Create quanteda corpus
wiki_corpus <- wiki %>% corpus()
wiki_corpus_df <- cbind(docid = wiki_corpus %>% docnames(), wiki_corpus %>% docvars())

# # Create tokens object, remove stopwords and numbers
wiki_tokens <- wiki_corpus %>%
  tokens(remove_numbers = TRUE, remove_separators = TRUE, remove_punct = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("en")) %>%
  tokens_select(wiki_tokens, pattern = "\\b(?!\\d+\\b)\\w+\\b", valuetype = "regex") # remove numbers

# # Extract context words using kwic
econ_terms <- list(econ = c("inflat*", "price", "wage", "cyclical", "growth", "employ*", "unemplo*", "recover*", "cost")) %>% dictionary()
wiki_kwic <- kwic(wiki_tokens, pattern = econ_terms$econ, window = 6)
wiki_kwic <- data.frame(docid = wiki_kwic$docname, KEYWORD = wiki_kwic$keyword, TEXT = cbind(wiki_kwic$pre, wiki_kwic$post)) %>%
  mutate(TEXT = paste0(TEXT.1, " ", TEXT.2)) %>%
  select(-c(TEXT.1, TEXT.2)) %>%
  left_join(wiki_corpus_df) %>%
  unnest_tokens("TOKENS", TEXT, token = "words")
wiki_kwic$TOKENS <- wiki_kwic$TOKENS %>% wordStem(language = "en")
wiki_kwic$docid <- "WIKIPEDIA"

# # Write context words to CSV
write.csv(wiki_kwic, "data/processed/stability/kwick_wiki_random.csv")

# ########## Economics Wikipedia text corpus ----------

# # Load Economics Wikipedia Articles using the getwiki package and the Economics target terms:
wiki <- c()
for (i in 1:length(econ_terms$econ)) {
  print(econ_terms$econ[i])
  txt <- getwiki::search_wiki(econ_terms$econ[i], clean = TRUE)
  txt <- txt$content
  wiki <- c(wiki, txt)
}

# # Create quanteda corpus
wiki_corpus <- wiki %>% corpus()
wiki_corpus_df <- cbind(docid = wiki_corpus %>% docnames(), wiki_corpus %>% docvars())

# # Create tokens object, remove stopwords and numbers
wiki_tokens <- wiki_corpus %>%
  tokens(remove_numbers = TRUE, remove_separators = TRUE, remove_punct = TRUE) %>%
  tokens_tolower() %>%
  tokens_select(pattern = "\\b(?!\\d+\\b)\\w+\\b", valuetype = "regex") # remove numbers

# # Extract context words using kwic
econ_terms <- list(econ = c("inflat*", "price", "wage", "cyclical", "growth", "employ*", "unemplo*", "recover*", "cost")) %>% dictionary()
wiki_kwic <- kwic(wiki_tokens, pattern = econ_terms$econ, window = 6)
wiki_kwic <- data.frame(docid = wiki_kwic$docname, KEYWORD = wiki_kwic$keyword, TEXT = cbind(wiki_kwic$pre, wiki_kwic$post)) %>%
  mutate(TEXT = paste0(TEXT.1, " ", TEXT.2)) %>%
  select(-c(TEXT.1, TEXT.2)) %>%
  left_join(wiki_corpus_df) %>%
  unnest_tokens("TOKENS", TEXT, token = "words")
wiki_kwic$TOKENS <- wiki_kwic$TOKENS %>% wordStem(language = "en")
wiki_kwic$docid <- "WIKIPEDIA"
wiki_kwic

# # Write context words to CSV
write.csv(wiki_kwic, "data/processed/stability/kwick_wiki_econ.csv")











# Part 2: Textual Stability ---------------------------------------------------



####### Read pre-computed context word data for central banks.
# These CSV files contain the context words extracted in Part 1 (now commented out for replication).
kwick_cb <- rbind(
  read_csv("data/processed/stability/kwick_boc.csv"),
  # read_csv("data/processed/stability/kwick_boe.csv"),
  # read_csv("data/processed/stability/kwick_boi.csv"),
  # read_csv("data/processed/stability/kwick_boj.csv"),
  # read_csv("data/processed/stability/kwick_db.csv"),
  # read_csv("data/processed/stability/kwick_ecb.csv"),
  # read_csv("data/processed/stability/kwick_fed1.csv"),
  # read_csv("data/processed/stability/kwick_fed2.csv"),
  # read_csv("data/processed/stability/kwick_fed3.csv"),
  # read_csv("data/processed/stability/kwick_riks.csv")
)

##### Read pre-computed context word data for Wikipedia Articles
# These CSV files contain the context words extracted in Part 1 (now commented out for replication).
kwick_wiki_econ <- read_csv("data/processed/stability/kwick_wiki_econ.csv")
kwick_wiki_random <- read_csv("data/processed/stability/kwick_wiki_random.csv")




###### Prepare Data sets

# Compare central bank and Wikipedia context word usage against the US Federal Reserve (Fed).
# This involves calculating the relative frequency of each token within each keyword's context.

# ECB vs US Fed
kwick_ecb <- kwick_cb %>%
  filter(cb %in% c("US Federal Reserve", "European Central Bank")) %>%
  group_by(cb) %>%
  count(TOKENS, KEYWORD) %>%
  pivot_wider(names_from = cb, values_from = n, values_fill = 0) %>%
  mutate(across(-c(TOKENS, KEYWORD), ~ . / sum(.)))

# BoJ vs US Fed
kwick_boj <- kwick_cb %>%
  filter(cb %in% c("US Federal Reserve", "Bank of Japan")) %>%
  group_by(cb) %>%
  count(TOKENS, KEYWORD) %>%
  pivot_wider(names_from = cb, values_from = n, values_fill = 0) %>%
  mutate(across(-c(TOKENS, KEYWORD), ~ . / sum(.)))

# BoE vs US Fed
kwick_boe <- kwick_cb %>%
  filter(cb %in% c("US Federal Reserve", "Bank of England")) %>%
  group_by(cb) %>%
  count(TOKENS, KEYWORD) %>%
  pivot_wider(names_from = cb, values_from = n, values_fill = 0) %>%
  mutate(across(-c(TOKENS, KEYWORD), ~ . / sum(.)))

# BoC vs US Fed
kwick_boc <- kwick_cb %>%
  filter(cb %in% c("US Federal Reserve", "Bank of Canada")) %>%
  group_by(cb) %>%
  count(TOKENS, KEYWORD) %>%
  pivot_wider(names_from = cb, values_from = n, values_fill = 0) %>%
  mutate(across(-c(TOKENS, KEYWORD), ~ . / sum(.)))

# BuBa vs US Fed
kwick_buba <- kwick_cb %>%
  filter(cb %in% c("US Federal Reserve", "Deutsche Bundesbank")) %>%
  group_by(cb) %>%
  count(TOKENS, KEYWORD) %>%
  pivot_wider(names_from = cb, values_from = n, values_fill = 0) %>%
  mutate(across(-c(TOKENS, KEYWORD), ~ . / sum(.)))

# Riksbank vs US Fed
kwick_riks <- kwick_cb %>%
  filter(cb %in% c("US Federal Reserve", "Sveriges Riksbank")) %>%
  group_by(cb) %>%
  count(TOKENS, KEYWORD) %>%
  pivot_wider(names_from = cb, values_from = n, values_fill = 0) %>%
  mutate(across(-c(TOKENS, KEYWORD), ~ . / sum(.)))

# RBI vs US Fed
kwick_rbi <- kwick_cb %>%
  filter(cb %in% c("US Federal Reserve", "Reserve Bank of India")) %>%
  group_by(cb) %>%
  count(TOKENS, KEYWORD) %>%
  pivot_wider(names_from = cb, values_from = n, values_fill = 0) %>%
  mutate(across(-c(TOKENS, KEYWORD), ~ . / sum(.)))

# Wikipedia vs US Fed
kwick_wiki_random$cb <- "Wikipedia Random"
kwick_wiki_random <- kwick_wiki_random %>%
  rbind(kwick_cb %>% filter(cb == "US Federal Reserve") %>% select(-c(date))) %>%
  group_by(cb) %>%
  count(TOKENS, KEYWORD) %>%
  pivot_wider(names_from = cb, values_from = n, values_fill = 0) %>%
  mutate(across(-c(TOKENS, KEYWORD), ~ . / sum(.)))
kwick_wiki_random %>% filter(TOKENS == "stabil", KEYWORD == "price")

# Econ Wikipedia vs US Fed
kwick_wiki_econ$cb <- "Wikipedia Economics"
kwick_wiki_econ <- kwick_wiki_econ %>%
  rbind(kwick_cb %>% filter(cb == "US Federal Reserve") %>% select(-c(date))) %>%
  group_by(cb) %>%
  count(TOKENS, KEYWORD) %>%
  pivot_wider(names_from = cb, values_from = n, values_fill = 0) %>%
  mutate(across(-c(TOKENS, KEYWORD), ~ . / sum(.)))



## Table 3 ----------------------------------------------------------------------

# This code performs OLS regressions to compare the textual similarity between the US Federal
# Reserve (Fed) and other central banks, as well as Wikipedia texts. The dependent variable in
# all regressions is the relative frequency of context words.
# Warning: Running the following regressions can take several minutes, depending on system resources.

ols <- list()
ols[["1"]] <- kwick_wiki_random %>% lm(formula = `US Federal Reserve` ~ `Wikipedia Random` + factor(KEYWORD))
ols[["2"]] <- kwick_wiki_econ %>% lm(formula = `US Federal Reserve` ~ `Wikipedia Economics` + factor(KEYWORD))
ols[["3"]] <- kwick_ecb %>% lm(formula = `US Federal Reserve` ~ `European Central Bank` + factor(KEYWORD))
ols[["4"]] <- kwick_boe %>% lm(formula = `US Federal Reserve` ~ `Bank of England` + factor(KEYWORD))
ols[["5"]] <- kwick_boj %>% lm(formula = `US Federal Reserve` ~ `Bank of Japan` + factor(KEYWORD))
ols[["6"]] <- kwick_buba %>% lm(formula = `US Federal Reserve` ~ `Deutsche Bundesbank` + factor(KEYWORD))
ols[["7"]] <- kwick_boc %>% lm(formula = `US Federal Reserve` ~ `Bank of Canada` + factor(KEYWORD))
ols[["8"]] <- kwick_riks %>% lm(formula = `US Federal Reserve` ~ `Sveriges Riksbank` + factor(KEYWORD))
ols[["9"]] <- kwick_rbi %>% lm(formula = `US Federal Reserve` ~ `Reserve Bank of India` + factor(KEYWORD))
stargazer(ols,
  type = "text", omit = c("KEYWORD"), digits = 2, omit.stat = c("f", "ser", "adj.rsq"),
  add.lines = list(c("Keyword controll", rep("Yes", 9)), c("Full Vocabluary", rep("Yes", 9)))
)







## Table A4 (Table 3 without controls) ------------------------------------------

# This code performs OLS regressions to compare the textual similarity between the US Federal
# Reserve (Fed) and other central banks, as well as Wikipedia texts. The dependent variable in
# all regressions is the relative frequency of context words. In this regression, we do not control
# for keyword specific variation.

ols2 <- list()
ols2[["1"]] <- kwick_wiki_random %>% lm(formula = `US Federal Reserve` ~ `Wikipedia Random`)
ols2[["2"]] <- kwick_wiki_econ %>% lm(formula = `US Federal Reserve` ~ `Wikipedia Economics`)
ols2[["3"]] <- kwick_ecb %>% lm(formula = `US Federal Reserve` ~ `European Central Bank`)
ols2[["4"]] <- kwick_boe %>% lm(formula = `US Federal Reserve` ~ `Bank of England`)
ols2[["5"]] <- kwick_boj %>% lm(formula = `US Federal Reserve` ~ `Bank of Japan`)
ols2[["6"]] <- kwick_buba %>% lm(formula = `US Federal Reserve` ~ `Deutsche Bundesbank`)
ols2[["7"]] <- kwick_boc %>% lm(formula = `US Federal Reserve` ~ `Bank of Canada`)
ols2[["8"]] <- kwick_riks %>% lm(formula = `US Federal Reserve` ~ `Sveriges Riksbank`)
ols2[["9"]] <- kwick_rbi %>% lm(formula = `US Federal Reserve` ~ `Reserve Bank of India`)
stargazer(ols,
  type = "text", omit = c("KEYWORD"), digits = 2, omit.stat = c("f", "ser", "adj.rsq"),
  add.lines = list(c("Keyword controll", rep("No", 9)), c("Full Vocabluary", rep("Yes", 9)))
)








## Additional Robustness Tests (not in the paper) -------------------------------

#### Only positive observations:
ols3 <- list()
ols3[["1"]] <- kwick_wiki_random %>%
  filter(`US Federal Reserve` > 0 & `Wikipedia Random` > 0) %>%
  lm(formula = `US Federal Reserve` ~ `Wikipedia Random`)
ols3[["2"]] <- kwick_wiki_econ %>%
  filter(`US Federal Reserve` > 0 & `Wikipedia Economics` > 0) %>%
  lm(formula = `US Federal Reserve` ~ `Wikipedia Economics`)
ols3[["3"]] <- kwick_ecb %>%
  filter(`US Federal Reserve` > 0 & `European Central Bank` > 0) %>%
  lm(formula = `US Federal Reserve` ~ `European Central Bank`)
ols3[["4"]] <- kwick_boe %>%
  filter(`US Federal Reserve` > 0 & `Bank of England` > 0) %>%
  lm(formula = `US Federal Reserve` ~ `Bank of England`)
ols3[["5"]] <- kwick_boj %>%
  filter(`US Federal Reserve` > 0 & `Bank of Japan` > 0) %>%
  lm(formula = `US Federal Reserve` ~ `Bank of Japan`)
ols3[["6"]] <- kwick_buba %>%
  filter(`US Federal Reserve` > 0 & `Deutsche Bundesbank` > 0) %>%
  lm(formula = `US Federal Reserve` ~ `Deutsche Bundesbank`)
ols3[["7"]] <- kwick_boc %>%
  filter(`US Federal Reserve` > 0 & `Bank of Canada` > 0) %>%
  lm(formula = `US Federal Reserve` ~ `Bank of Canada`)
ols3[["8"]] <- kwick_riks %>%
  filter(`US Federal Reserve` > 0 & `Sveriges Riksbank` > 0) %>%
  lm(formula = `US Federal Reserve` ~ `Sveriges Riksbank`)
ols3[["9"]] <- kwick_rbi %>%
  filter(`US Federal Reserve` > 0 & `Reserve Bank of India` > 0) %>%
  lm(formula = `US Federal Reserve` ~ `Reserve Bank of India`)
stargazer(ols3,
  type = "text", omit = c("KEYWORD"), digits = 2, omit.stat = c("f", "ser", "adj.rsq"),
  add.lines = list(c("Keyword controll", rep("No", 9)), c("Full Vocabluary", rep("No", 9)))
)



##### Only positive observations + controls
ols4 <- list()
ols4[["1"]] <- kwick_wiki_random %>%
  filter(`US Federal Reserve` > 0 & `Wikipedia Random` > 0) %>%
  lm(formula = `US Federal Reserve` ~ `Wikipedia Random` + factor(KEYWORD))
ols4[["2"]] <- kwick_wiki_econ %>%
  filter(`US Federal Reserve` > 0 & `Wikipedia Economics` > 0) %>%
  lm(formula = `US Federal Reserve` ~ `Wikipedia Economics` + factor(KEYWORD))
ols4[["3"]] <- kwick_ecb %>%
  filter(`US Federal Reserve` > 0 & `European Central Bank` > 0) %>%
  lm(formula = `US Federal Reserve` ~ `European Central Bank` + factor(KEYWORD))
ols4[["4"]] <- kwick_boe %>%
  filter(`US Federal Reserve` > 0 & `Bank of England` > 0) %>%
  lm(formula = `US Federal Reserve` ~ `Bank of England` + factor(KEYWORD))
ols4[["5"]] <- kwick_boj %>%
  filter(`US Federal Reserve` > 0 & `Bank of Japan` > 0) %>%
  lm(formula = `US Federal Reserve` ~ `Bank of Japan` + factor(KEYWORD))
ols4[["6"]] <- kwick_buba %>%
  filter(`US Federal Reserve` > 0 & `Deutsche Bundesbank` > 0) %>%
  lm(formula = `US Federal Reserve` ~ `Deutsche Bundesbank` + factor(KEYWORD))
ols4[["7"]] <- kwick_boc %>%
  filter(`US Federal Reserve` > 0 & `Bank of Canada` > 0) %>%
  lm(formula = `US Federal Reserve` ~ `Bank of Canada` + factor(KEYWORD))
ols4[["8"]] <- kwick_riks %>%
  filter(`US Federal Reserve` > 0 & `Sveriges Riksbank` > 0) %>%
  lm(formula = `US Federal Reserve` ~ `Sveriges Riksbank` + factor(KEYWORD))
ols4[["9"]] <- kwick_rbi %>%
  filter(`US Federal Reserve` > 0 & `Reserve Bank of India` > 0) %>%
  lm(formula = `US Federal Reserve` ~ `Reserve Bank of India` + factor(KEYWORD))

stargazer::stargazer(ols4,
  type = "text", omit = c("KEYWORD"), digits = 2, omit.stat = c("f", "ser", "adj.rsq"),
  add.lines = list(c("Keyword controll", rep("Yes", 9)), c("Full Vocabluary", rep("No", 9)))
)
