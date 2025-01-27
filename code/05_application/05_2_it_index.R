# Description ------------------------------------------------------------------

# Title: IT Index
#
# This R code replicates the results from Chapter 5.2.-3.3 of the paper "Whatever
# it takes to understand a central banker – Embedding their words using neural networks."
# To reproduce the results without modification, the following prerequisites are required:
#
#   1. Packages: All R packages listed in the project's setup file must be installed.
#   This ensures that all necessary functions and dependencies are available.
#
#   2. Text-Data: The text corpus itself is required. This corpus should be placed in
#   a subfolder named "corpus" within the same directory as this R script. This file
#   structure allows the code to locate and load the data correctly.
#
#   3. Access to the trained LLM.
#
#   4. Access to the Monetary Frameworks Database by Cobham (2021), available here
#   (last accessed: 01.10.2023): https://monetaryframeworks.org
#
#   5. Shadow Rate: The shadow Wu-Xia-Shadow Rate is necessary to replicate the main results.
#   The Rate can be obtained from the following website (last accessed: 01.10.2023):
#   https://sites.google.com/view/jingcynthiawu/shadow-rates
#
#   6. Shadow Short Rate: The shadow short rate by Krippner (2021) used in the robustness-
#   test can be obtained from the following website (last accessed: 01.10.2023):
#   www.ljkmfa.com
#
#   7. Forward Guidance Shocks: Swanson’s (2021, JME) forward guidance shocks are used
#   in the main regressions and the robustness-tests. The shocks are available on the
#   following website (last accessed: 01.04.2024):
#   https://sites.socsci.uci.edu/~swanson2/researchpublished.html
#
#   8. A personalized FRED API key (line 58). Otherwise, the datasets can be obtained
#   by hand from the FRED website. They key-identifier is supplied for every index.
#
#


# Attention: Running the whole code would take around 20 minutes on a modern computer.
# Section 3 is currently commented out, which reduces the running time to less
# than a minute.




# Preparation ------------------------------------------------------------------

library(tidyverse)
library(interactions)
library(quanteda)
library(readxl)
library(doc2vec)
library(sweater)
library(fredr)
#### Load FRED API key
# fredr_set_key("YOUR.API.KEY")


##### Hawkish/Dovish Dictionary
hawk_dove_dic <- dictionary(list(
  HAWK = c("accelerat∗", "better", "boom∗", "emerg∗", "expansion", "fast∗", "favo(u)rabl∗", "firm∗", "great∗", "high∗", "improv∗", "increas∗", "larger", "positive", "rais∗", "ris∗", "stabili∗", "stable", "strengthen∗", "strong∗", "subdued", "unsustainable", "upside", "upswing", "upturn", "upward∗", "uncertain"),
  DOVE = c("collaps∗", "contraction", "dampen∗", "decelerat∗", "declin∗", "decreas∗", "delay∗", "depression", "destabili∗", "deteriorat∗", "difficul∗", "diminish∗", "disappear∗", "downside", "downswing", "downturn", "downward∗", "fall∗", "fragil∗", "low∗", "negative", "poor", "recession∗", "slow∗", "sluggish", "small∗", "struggling", "sustainable", "unfavo(u)rabl∗", "unstable", "weak∗", "worse")
))


# Part 1: IT Index (Inflation Targeting Index) ---------------------------------


##### Cobham (2021) Data (Monetary Policy Framework Classifications)
cobham_target <- read_excel("data/foreign_data/cobham/mpfclassificationdata_revjul19.xlsx", sheet = 6, range = "A1:AS63")
cobham_target <- cobham_target %>% select(country = ...1, everything())
cobham_target <- cobham_target %>%
  mutate(country = recode(str_trim(country),
    "UK" = "United Kingdom",
    "SKorea" = "South Korea",
    "Czech Rep" = "Czech Republic",
    "Saudi Arabia" = "Saudi",
    "USA" = "United States"
  ))
cobham_target <- cobham_target %>% pivot_longer(-country, values_to = "regime")
cobham_target <- cobham_target %>%
  mutate(year = ymd(paste0(name, "-01-01"))) %>%
  select(-name) %>%
  na.omit()


#### Central Bank Communication Corpus
corpus <- readRDS("data/processed/text_data.Rds")
corpus <- corpus %>% filter(type == "speech")
corpus <- corpus %>% select(country, date, doc_id, speaker)
corpus$year <- floor_date(corpus$date, "year")


####### Merge Corpus with Monetary Policy Framework Classifications
corpus <- corpus %>% left_join(cobham_target, by = join_by(country, year))
corpus <- corpus %>% mutate(regime = if_else(is.na(regime), "undefined", regime))


###### Pre-trained Document Embeddings
doc_embedding <- read.paragraph2vec("data/embeddings/fulldoc2vecPVDBOWpre300.bin") %>% as.matrix(which = "docs")
doc_ids <- corpus %>%
  filter(regime == "LIT" | regime == "FIT" | country == "United States") %>%
  pull(doc_id)
doc_embedding <- doc_embedding[rownames(doc_embedding) %in% doc_ids, ]


###### IT Index
# Calculate the IT Index using the the relative norm distance (RND) of word
# embeddings function from the sweater package. Interpretation: Negative values
# indicate closer alignment with LIT, positive with FIT.
lit <- corpus %>%
  filter(regime == "LIT", country != "United States") %>%
  pull(doc_id) # Target Document IDs for LIT
fit <- corpus %>%
  filter(regime == "FIT", country != "United States") %>%
  pull(doc_id) # Target Document IDs for FIT
us <- corpus %>%
  filter(country == "United States") %>%
  pull(doc_id) # Target Document IDs for Fed Speeches
IT <- rnd(w = doc_embedding, A_words = lit, B_words = fit, S_words = us)
IT <- IT$P %>%
  as_tibble(rownames = "doc_id") %>%
  left_join(corpus) %>%
  filter(date < "2023-01-01")


###### IT Time Series
IT_ts <- IT %>%
  group_by(quarters = floor_date(date, "quarters")) %>%
  summarise(IT = mean(value)) %>%
  select(-quarters) %>%
  ts(start = c(1999, 1), frequency = 4)
IT_ts <- (IT_ts - mean(IT_ts)) / sd(IT_ts)


######### Figure 6: Fed’s stance on inflation targeting
plot_IT <- tibble(date = time(IT_ts), IT = IT_ts[, "IT"]) %>% filter(date > "2000-01-01")
min_values <- plot_IT %>% top_n(10, -IT)
min_values <- min_values[c(2, 5, 7), ]
min_values <- rbind(min_values, plot_IT[plot_IT$date == 2020.25, ])
min_values$name <- c("2008Q1: FFR = 0%", "2008Q4: QE1", "2009Q2: QE1 Expansion", "2020Q1: CARES")
plot_IT %>%
  ggplot(aes(date, IT)) +
  geom_line() +
  geom_abline(intercept = 0, slope = 0, col = "red") +
  ggrepel::geom_text_repel(
    data = min_values, aes(x = date, y = IT, label = format(name)),
    fill = "red", color = "grey50", label.padding = unit(0.1, "lines"),
    label.size = 0.2, show.legend = FALSE, nudge_y = c(-0.2, -0.5, 0.5, -0.4),
    nudge_x = c(-3, -1, 3, -3)
  ) +
  geom_point(data = min_values, size = 2, shape = 19) +
  geom_point(size = 1) +
  labs(
    y = "<-- LIT      IT      FIT -->",
    x = ""
  ) +
  ylim(c(-3.5, 3.5))



# Part 2: Kohn/Dudley Example --------------------------------------------------
# This section examines two specific speeches, one by Donald Kohn (associated with
# Fixed Inflation Targeting) and one by William Dudley (associated with
# Loose Inflation Targeting), to illustrate how the IT index reflects
# their respective stances.

###### Speech examples:
IT_standardized <- IT %>% mutate(value = (value - mean(value)) / sd(value)) # Standardize the IT index for easier comparison and find extreme values
IT_standardized %>% arrange(value) # 3rd lowest: -3.42 on 2013-04-22 Dudley [official regime: FIT] (doc_7147)
IT_standardized %>% arrange(-value) # 2nd highest: 2.97 on 2003-02-28 Donald Kohn [official regime: LIT] (doc_14949)


######### Calculate sentiment/hawkish-dovish stance of the Kohn and Dudley speeches
dic <- readRDS("data/processed/text_data.Rds") %>%
  filter(doc_id %in% c("doc_14949", "doc_7147")) %>%
  select(doc_id, text) %>%
  corpus() %>%
  tokens() %>%
  dfm()
dic %>%
  dfm_lookup(dictionary = quanteda.sentiment::data_dictionary_LoughranMcDonald) %>%
  convert(to = "data.frame") %>%
  mutate(SENTIMENT = (NEGATIVE - POSITIVE) / (NEGATIVE + POSITIVE))
dic %>%
  dfm_lookup(dictionary = hawk_dove_dic) %>%
  convert(to = "data.frame") %>%
  mutate(HD = (HAWK - DOVE) / (HAWK + DOVE))
#           doc_id      HAWK-DOVE (Hawk-Dov)/Sum(.)         SENTIMENT (Neg-Pos)/Sum(.)
# Kohn      doc_14949   -0.38 (dov)                         0.385 (neg.)
# Dudley    doc_7147    0.000                               -0.0786
#


######## Economic Environment Data (used for context in the speeches)
SHADOW_RATE <- read_excel("data/foreign_data/WuXiaShadowRate.xlsx", sheet = 2, skip = 1, col_types = c("date", "skip", "numeric", "skip", "skip"), col_names = c("date", "SHADOW_RATE")) # Wu-Xia Shadow Short Rate
SHADOW_RATE <- SHADOW_RATE %>%
  filter(date >= "1980-01-01") %>%
  pull(SHADOW_RATE) %>%
  ts(start = c(1980, 1), frequency = 12)
SHADOW_RATE <- SHADOW_RATE %>% aggregate.ts(nfrequency = 4, FUN = mean)
CPI <- fredr(c("CPALTT01USM659N")) %>%
  select(value) %>%
  ts(start = c(1956, 1), frequency = 12) # CPI: Total for United States (CPALTT01USM659N)
CPI <- CPI %>% aggregate.ts(nfrequency = 4, FUN = mean)

# Analyze Kohn's speech in the context of the economic environment
df <- cbind(IT_ts, SHADOW_RATE, CPI)
df <- window(df, start = c(2003, 2), end = c(2004, 2))
df %>% plot()
lm(SHADOW_RATE ~ CPI, df) %>% summary() # positive inflation coefficient

# Analyze Dudley's speech in the context of the economic environment
df <- cbind(IT_ts, SHADOW_RATE, CPI)
df <- window(df, start = c(2013, 4), end = c(2014, 4))
df %>% plot()
lm(SHADOW_RATE ~ CPI, df) %>% summary() # negative inflation coefficient




# Part 3: FIT/LIT specific wordcloud -------------------------------------------

# This section explores the topics within speeches classified as FIT or LIT
# using Latent Dirichlet Allocation (LDA). This analysis aims to identify
# distinct themes and vocabulary associated with each monetary policy regime.
#
# Attention: Running the LDA model computation can take 10-20 minutes on a
# modern computer. A pre-trained model (lda_model.rds) is provided for convenience.



###### Clean Corpus
# This code would prepare the corpus for LDA by tokenizing and removing stop words.
# lda = readRDS("corpus/dataset.Rds") %>% select(doc_id, speaker,  text)
# lda = IT %>% left_join(lda, by = "doc_id")
# corpus_tidy = lda %>% unnest_tokens(word, text) %>%  anti_join(stop_words)
# dtm = corpus_tidy %>%  count(doc_id, word, sort = TRUE) %>% cast_dtm(document = doc_id, term = word, value = n)


###### LDA Algorithm
# This section trains an LDA model with 25 topics. If you want to train a new
# model, uncomment the code below. Otherwise, the pre-trained model is loaded.
# lda_model = LDA(dtm, k = 25, control = list(seed = 1234))
# saveRDS(lda_model, file = "LDA/lda_model.rds")
lda_model <- readRDS(file = "data/helper/lda_model.rds")


#### Topic IT distribution
# This section assigns the most probable topic to each document and joins
# this information with the IT index values.
topics <- topicmodels::posterior(lda_model)$topics
topics <- data.frame(doc_id = rownames(topics), topic = apply(topics, 1, which.max))
topics <- topics %>% left_join(IT)
topics <- topics %>% mutate(value_min = if_else(value < median(value), 1, 0))


###### Table A9: Regression Results: LDA topics
# This section performs regressions to analyze the relationship between the IT
# index and the identified LDA topics.
ols1 <- lm(value ~ factor(topic), topics)
glm1 <- glm(value_min ~ factor(topic), topics, family = "binomial") #  Large RND -> FIT
stargazer::stargazer(ols1, glm1, type = "text", omit.stat = c("ser", "f"), digits = 2, no.space = T)


###### Figure 7: Worldcloud
# Attention: the command "comparison.cloud" is not deterministic, hence resulting
# wordclouds may vary from the one displayed in the paper!
lit.fit_topics <- tidy(lda_model, matrix = "beta")
lit.fit_topics <- lit.fit_topics %>% filter(topic %in% c(1, 4, 25, 2, 7, 15))
lit.fit_topics <- lit.fit_topics %>% mutate(topic = if_else(topic %in% c(1, 4, 25), "LIT", "FIT"))
lit.fit_topics <- lit.fit_topics %>%
  group_by(topic) %>%
  top_n(100, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
lit.fit_topics <- lit.fit_topics %>% filter(!term %in% 1:3000, !term %in% c("url", "vol", "pp"))
lit.fit_topics %>%
  reshape2::acast(term ~ topic, value.var = "beta", fill = 0) %>%
  wordcloud::comparison.cloud(
    colors = c("#F8766D", "#00BFC4"),
    title.bg.colors = c("#F8766D30", "#00BFC420"),
    max.words = 75,
    match.colors = TRUE, use.r.layout = TRUE,
    scale = c(2.5, .35)
  )





# Part 4: Expectations ---------------------------------------------------------
# This section analyzes the relationship between the IT index, macroeconomic
# variables, and inflation expectations. It uses data from FRED and Swanson's
# (2021) FG shocks to explore these relationships.


###### Data

# 1-Year Expected Inflation (EXPINF1YR) - Deviation from 2% target
df <- fredr(c("EXPINF1YR")) %>% select(DATE = date, value)
df$d.value <- (df$value - 2)
EXPINF1YR <- df %>%
  select(d.value) %>%
  ts(start = c(1982, 1), frequency = 12)

# 2-Year Expected Inflation (EXPINF2YR) - Deviation from 2% target
df <- fredr(c("EXPINF2YR")) %>% select(DATE = date, value)
df$d.value <- (df$value - 2)
EXPINF2YR <- df %>%
  select(d.value) %>%
  ts(start = c(1982, 1), frequency = 12)

# 10-Year Expected Inflation (EXPINF10YR) - Deviation from 2% target
df <- fredr(c("EXPINF10YR")) %>% select(DATE = date, value)
df$d.value <- (df$value - 2)
EXPINF10YR <- df %>%
  select(d.value) %>%
  ts(start = c(1982, 1), frequency = 12)

# University of Michigan: 12-month Inflation Expectation (MICH) - Deviation from 2% target
df <- fredr(c("MICH")) %>% select(DATE = date, value)
df$d.value <- (df$value - 2)
MICH <- df %>%
  select(d.value) %>%
  ts(start = c(1978, 1), frequency = 12)

# Inflation Risk Premium (TENEXPCHAINFRISPRE)
df <- fredr(c("TENEXPCHAINFRISPRE")) %>% select(DATE = date, value)
df$d.value <- df$value # - lag(df$value)
TENEXPCHAINFRISPRE <- df %>%
  select(d.value) %>%
  ts(start = c(1982, 1), frequency = 12)

# Real Risk Premium (TENEXPCHAREARISPRE)
df <- fredr(c("TENEXPCHAREARISPRE")) %>% select(DATE = date, value)
df$d.value <- df$value # - lag(df$value)
TENEXPCHAREARISPRE <- df %>%
  select(d.value) %>%
  ts(start = c(1982, 1), frequency = 12)

# 1-Month Real Interest Rate (REAINTRATREARAT1MO)
df <- fredr(c("REAINTRATREARAT1MO")) %>% select(DATE = date, value)
df$d.value <- df$value
REAINTRATREARAT1MO <- df %>%
  select(d.value) %>%
  ts(start = c(1982, 1), frequency = 12)

# 1-Year Real Interest Rate (REAINTRATREARAT1YE)
df <- fredr(c("REAINTRATREARAT1YE")) %>% select(DATE = date, value)
df$d.value <- df$value
REAINTRATREARAT1YE <- df %>%
  select(d.value) %>%
  ts(start = c(1982, 1), frequency = 12)

# 10-Year Real Interest Rate (REAINTRATREARAT10Y)
df <- fredr(c("REAINTRATREARAT10Y")) %>% select(DATE = date, value)
df$d.value <- df$value
REAINTRATREARAT10Y <- df %>%
  select(d.value) %>%
  ts(start = c(1982, 1), frequency = 12)

#  CPI (CPIAUCSL)
df <- fredr(c("CPIAUCSL"), units = "pc1") %>% select(DATE = date, value)
df$D.value <- df$value - 2
CPIAUCSL <- df %>%
  select(value) %>%
  ts(start = c(1947, 1), frequency = 12)
D.CPIAUCSL <- df %>%
  select(D.value) %>%
  ts(start = c(1947, 1), frequency = 12)

# Forward Guidance Shocks (Swanson, 2021, JME)
FG <- read_xlsx("data/foreign_data/swanson_2021.xlsx", skip = 2, col_types = c("date", "numeric", "numeric", "numeric", "numeric"), col_names = c("DATE", "FFR", "FG", "LSAP", "-LSAP"))
FG <- FG %>%
  group_by(months = floor_date(DATE, "month")) %>%
  summarise(FG = mean(FG))
FG <- FG %>% right_join(tibble(months = seq(as.Date("2000-01-01"), as.Date("2020-01-01"), by = "months")))
FG <- FG %>% arrange(months)
FG <- FG %>% mutate(FG = if_else(is.na(FG), 0, FG))
FG <- FG %>%
  select(FG) %>%
  ts(start = c(2000, 1), frequency = 12)
FG <- FG * 100

# IT
InfTar <- IT %>%
  group_by(months = floor_date(date, "month")) %>%
  summarise(IT = mean(value))
InfTar <- InfTar %>% filter(months >= "2000-01-01") # there are no speeches in Dec 1999 and I am unsure how to handle that
InfTar <- InfTar %>%
  select(-months) %>%
  ts(start = c(2000, 1), frequency = 12)
InfTar <- (InfTar - mean(InfTar)) / sd(InfTar)



###### Dictionaries
dic <- readRDS("data/processed/text_data.Rds") %>%
  filter(country == "United States", date > "1990-01-01", type == "speech") %>%
  select(date, text) %>%
  corpus() %>%
  tokens() %>%
  dfm()

# Loughran-McDonald Sentiment
LM <- dic %>% dfm_lookup(dictionary = quanteda.sentiment::data_dictionary_LoughranMcDonald)
LM <- LM %>%
  convert(to = "data.frame") %>%
  transmute(LM_SENTIMENT = (NEGATIVE - POSITIVE) / (NEGATIVE + POSITIVE), DATE = docvars(LM)$date) %>%
  tibble()
LM <- LM %>%
  group_by(months = floor_date(DATE, "month")) %>%
  summarise(LM_SENTIMENT = mean(LM_SENTIMENT))
LM <- LM %>% filter(months >= "2000-01-01")
LM <- LM %>%
  select(-months) %>%
  ts(start = c(2000, 1), frequency = 12)

# BBD's Uncertainty
dictionary <- dictionary(list(UNCERTAINTY = c("uncertainty", "uncertain")))
UNC <- dic %>% dfm_lookup(dictionary = dictionary)
UNC <- UNC %>%
  convert(to = "data.frame") %>%
  transmute(UNCERTAINTY, DATE = docvars(UNC)$date) %>%
  tibble()
UNC <- UNC %>%
  group_by(months = floor_date(DATE, "month")) %>%
  summarise(UNCERTAINTY = mean(UNCERTAINTY))
UNC <- UNC %>% filter(months >= "2000-01-01")
UNC <- UNC %>%
  select(-months) %>%
  ts(start = c(2000, 1), frequency = 12)


# BN's Hawkish/Dovish Dictionary
HD_BN <- dic %>% dfm_lookup(dictionary = hawk_dove_dic)
HD_BN <- HD_BN %>%
  convert(to = "data.frame") %>%
  transmute(HD = (HAWK - DOVE) / (HAWK + DOVE), DATE = docvars(HD_BN)$date) %>%
  tibble()
HD_BN <- HD_BN %>%
  group_by(months = floor_date(DATE, "month")) %>%
  summarise(HD = mean(HD, na.rm = T))
HD_BN <- HD_BN %>% filter(months >= "2000-01-01")
HD_BN <- HD_BN %>%
  select(-months) %>%
  ts(start = c(2000, 1), frequency = 12)




######## Final Dataset
df <- cbind(InfTar, EXPINF1YR, EXPINF2YR, EXPINF10YR, MICH, REAINTRATREARAT1MO, REAINTRATREARAT1YE, REAINTRATREARAT10Y, Infl.Risk.Premium = TENEXPCHAINFRISPRE, Real.Risk.Premium = TENEXPCHAREARISPRE, D.CPIAUCSL, LM, UNC, HD_BN, FG)
df <- window(df, start = c(1999, 1), end = c(2020, 1))
df <- df %>% na.omit()

####### Table A11: Summary Statistics
df %>%
  data.frame() %>%
  mutate(EXPINF1YR.abs = abs(EXPINF1YR), EXPINF2YR.abs = abs(EXPINF2YR), EXPINF10YR.abs = abs(EXPINF10YR)) %>%
  stargazer::stargazer(type = "text")


###### Table 9: Regression
ols <- list()
ols[["1"]] <- lm(abs(EXPINF1YR) ~ D.CPIAUCSL + lag(InfTar) + Infl.Risk.Premium + Real.Risk.Premium + UNC + LM + HD_BN + FG, df)
ols[["2"]] <- lm(abs(EXPINF2YR) ~ D.CPIAUCSL + lag(InfTar) + Infl.Risk.Premium + Real.Risk.Premium + UNC + LM + HD_BN + FG, df)
ols[["3"]] <- lm(abs(EXPINF10YR) ~ D.CPIAUCSL + lag(InfTar) + Infl.Risk.Premium + Real.Risk.Premium + UNC + LM + HD_BN + FG, df)
ols[["4"]] <- lm(abs(MICH) ~ D.CPIAUCSL + lag(InfTar) + Infl.Risk.Premium + Real.Risk.Premium + UNC + LM + HD_BN + FG, df)
ols[["5"]] <- lm(REAINTRATREARAT1MO ~ EXPINF1YR * lag(InfTar) + Infl.Risk.Premium + Real.Risk.Premium + UNC + LM + HD_BN + FG + D.CPIAUCSL, df)
ols[["6"]] <- lm(REAINTRATREARAT1YE ~ EXPINF1YR * lag(InfTar) + Infl.Risk.Premium + Real.Risk.Premium + UNC + LM + HD_BN + FG + D.CPIAUCSL, df)
ols[["7"]] <- lm(REAINTRATREARAT10Y ~ EXPINF1YR * lag(InfTar) + Infl.Risk.Premium + Real.Risk.Premium + UNC + LM + HD_BN + FG + D.CPIAUCSL, df)
ols[["8"]] <- lm(abs(Infl.Risk.Premium) ~ D.CPIAUCSL + lag(InfTar) + Real.Risk.Premium + UNC + LM + HD_BN + FG, df)
stargazer::stargazer(ols, type = "text", omit.stat = c("ser", "f"), digits = 2, no.space = FALSE, column.sep.width = "0pt", font.size = "small", keep = c("EXPINF1YR", "InfTar"))







# Part 5: Taylor Rule ----------------------------------------------------------


#### Output Gap (GDPC1)
GDP_GAP <- fredr(c("GDPC1")) %>%
  select(DATE = date, rGDP = value) %>%
  filter(DATE >= "1980-01-01")
GDP_GAP <- GDP_GAP %>%
  pull(rGDP) %>%
  ts(start = c(1980, 1), frequency = 4)
GDP_GAP <- GDP_GAP %>% log() * 100
GDP_GAP <- GDP_GAP %>%
  mFilter::hpfilter(freq = 1600) %>%
  .$cycle


########  CPI (CPIAUCSL)
CPI <- fredr(c("CPIAUCSL"), units = "pc1", frequency = "q") %>% select(DATE = date, value)
CPI <- CPI %>%
  select(value) %>%
  ts(start = c(1947, 1), frequency = 4)
CPI <- CPI - 2


#### Unemployment Rate (UNRATE)
UNRATE <- fredr(c("UNRATE")) %>%
  select(DATE = date, UNRATE = value) %>%
  filter(DATE >= "1980-01-01")
UNRATE <- UNRATE$UNRATE %>%
  as.numeric() %>%
  ts(start = c(1980, 1), frequency = 12)
UNRATE <- aggregate(UNRATE, nfrequency = 4, mean)


#### Wu-Xia Shadow Short Rate
SHADOW_RATE <- read_excel("data/foreign_data/WuXiaShadowRate.xlsx", sheet = 2, skip = 1, col_types = c("date", "skip", "numeric", "skip", "skip"), col_names = c("date", "SHADOW_RATE")) # Wu-Xia Shadow Short Rate
SHADOW_RATE <- SHADOW_RATE %>% filter(date >= "1980-01-01")
SHADOW_RATE <- SHADOW_RATE$SHADOW_RATE %>% ts(start = c(1980, 1), frequency = 12)
SHADOW_RATE <- aggregate(SHADOW_RATE, nfrequency = 4, mean)
SHADOW_RATE <- SHADOW_RATE %>% na.omit()


####### Dataset
df <- cbind(IT = IT_ts, SHADOW_RATE, CPI, UNRATE, GDP_GAP)
df <- window(df, start = c(1999, 1), end = c(2020, 1))

###### Table A11: Statistical Summary
df %>%
  data.frame() %>%
  stargazer::stargazer(type = "text", digits = 2, summary.stat = c("n", "mean", "sd", "p25", "median", "p75"))


###### Table 10: IT Taylor Rule Regression Table
ols <- list()
ols[["1"]] <- lm(SHADOW_RATE ~ CPI, df)
ols[["2"]] <- lm(SHADOW_RATE ~ CPI * lag(IT), df)
ols[["3"]] <- lm(SHADOW_RATE ~ CPI * lag(IT) + UNRATE, df)
ols[["4"]] <- lm(SHADOW_RATE ~ CPI * lag(IT) + UNRATE * lag(IT) + GDP_GAP * lag(IT), df)
stargazer::stargazer(ols,
  type = "text", omit.stat = c("ser", "f"), digits = 2, no.space = T,
  column.sep.width = "0pt", font.size = "small"
)


###### Figure 8
johnson_neyman(ols[["2"]], modx = "lag(IT)", pred = CPI, alpha = .05)$plot +
  theme(text = element_text(family = "serif")) +
  labs(y = expression(" Slope of CPI- 2%"), x = expression(" IT"["t-1"]), title = "")

# Johnson-Neyman intervals (for interpretation)
johnson_neyman(ols[["2"]], modx = "lag(IT)", pred = CPI, alpha = .05) # # When lag(rnd) is OUTSIDE the interval [-14.28, -1.66], the slope of cpi is p < .05.
johnson_neyman(ols[["2"]], modx = "lag(IT)", pred = CPI, alpha = .1) # When lag(rnd) is OUTSIDE the interval [-8.37, -1.80], the slope of cpi is p < .1.












# Part 6: Taylor Rule:  Robustness Tests ---------------------------------------
# This section performs robustness tests on the Taylor Rule regressions from
# Part 5. It explores the sensitivity of the results to different measures of
# the policy rate, the inclusion of speaker fixed effects, the incorporation
# of textual sentiment and uncertainty measures, and the use of a rolling

ols_robustness <- list()


#### Robustness Test 1: Short Shadow Short Rate
SSR <- read_excel("data/foreign_data/International_SSR_estimates_NEW_202104.xlsx", sheet = 2, skip = 7, col_types = c("date", "numeric", "skip", "skip", "skip", "skip", "skip", "skip", "skip"), col_names = c("DATE", "INTEREST.RATE"))
SSR <- SSR$INTEREST.RATE %>% ts(start = c(1995, 1), frequency = 12)
SSR <- aggregate(SSR, nfrequency = 4, mean)
df <- cbind(IT = IT_ts, SSR, CPI, UNRATE, GDP_GAP)
df <- window(df, start = c(1999, 1), end = c(2020, 1))
ols_robustness[["SSR"]] <- lm(SSR ~ CPI * lag(IT) + UNRATE * lag(IT) + GDP_GAP * lag(IT), df)






###### Robustness Test 2: Speaker Fixed Effects
speaker <- IT %>% na.omit()
speaker$speaker_res <- lm(value ~ speaker, speaker) %>% residuals()
speaker <- speaker %>%
  group_by(quarters = floor_date(date, "quarters")) %>%
  summarise(speaker_res = mean(speaker_res)) %>%
  select(-quarters) %>%
  ts(start = c(1999, 1), frequency = 4)
speaker <- (speaker - mean(speaker)) / sd(speaker)
df <- cbind(IT = speaker, SHADOW_RATE, CPI, UNRATE, GDP_GAP)
df <- window(df, start = c(1999, 1), end = c(2020, 1))
ols_robustness[["Speaker"]] <- lm(SHADOW_RATE ~ CPI * lag(IT) + UNRATE * lag(IT) + GDP_GAP * lag(IT), df)





######## Robustness Test 3: Textual Sentiment Analysis (Dictionaries)
dic <- readRDS("data/processed/text_data.Rds") %>%
  filter(country == "United States", date > "1990-01-01", type == "speech") %>%
  select(date, text) %>%
  corpus() %>%
  tokens() %>%
  dfm()

# LM
LM <- dic %>% dfm_lookup(dictionary = quanteda.sentiment::data_dictionary_LoughranMcDonald)
LM <- LM %>%
  convert(to = "data.frame") %>%
  transmute(LM_SENTIMENT = (POSITIVE - NEGATIVE) / (NEGATIVE + POSITIVE), DATE = docvars(LM)$date) %>%
  tibble()
LM <- LM %>%
  group_by(quarters = floor_date(DATE, "quarters")) %>%
  summarise(LM_SENTIMENT = mean(LM_SENTIMENT))
LM <- LM %>% filter(quarters >= "1999-01-01")
LM <- LM %>%
  select(-quarters) %>%
  ts(start = c(1999, 1), frequency = 4)

# Uncertainty
dictionary <- dictionary(list(UNCERTAINTY = c("uncertainty", "uncertain")))
UNC <- dic %>% dfm_lookup(dictionary = dictionary)
UNC <- UNC %>%
  convert(to = "data.frame") %>%
  transmute(UNCERTAINTY, DATE = docvars(UNC)$date) %>%
  tibble()
UNC <- UNC %>%
  group_by(quarters = floor_date(DATE, "quarters")) %>%
  summarise(UNCERTAINTY = mean(UNCERTAINTY))
UNC <- UNC %>% filter(quarters >= "1999-01-01")
UNC <- UNC %>%
  select(-quarters) %>%
  ts(start = c(1999, 1), frequency = 4)


# Hawk/Dove (Bennani & Neuenkirch, Applied Economics)
HD_BN <- dic %>% dfm_lookup(dictionary = hawk_dove_dic)
HD_BN <- HD_BN %>%
  convert(to = "data.frame") %>%
  transmute(HD = (DOVE - HAWK) / (HAWK + DOVE), DATE = docvars(HD_BN)$date) %>%
  tibble()
HD_BN <- HD_BN %>%
  group_by(quarters = floor_date(DATE, "quarters")) %>%
  summarise(HD = mean(HD, na.rm = T))
HD_BN <- HD_BN %>% filter(quarters >= "1999-01-01")
HD_BN <- HD_BN %>%
  select(-quarters) %>%
  ts(start = c(1999, 1), frequency = 4)

# Forward Guidance Shocks (Swanson, 2021, JME)
FG <- read_xlsx("data/foreign_data/swanson_2021.xlsx", skip = 2, col_types = c("date", "numeric", "numeric", "numeric", "numeric"), col_names = c("DATE", "FFR", "FG", "LSAP", "-LSAP"))
FG <- FG %>%
  group_by(quarter = floor_date(DATE, "quarter")) %>%
  summarise(FG = mean(FG))
FG <- FG %>% right_join(tibble(quarter = seq(as.Date("1999-01-01"), as.Date("2020-01-01"), by = "quarter")))
FG <- FG %>% arrange(quarter)
FG <- FG %>%
  select(FG) %>%
  ts(start = c(1999, 1), frequency = 4)
FG <- FG * 100


# Regression
df <- cbind(IT = IT_ts, SHADOW_RATE, CPI, UNRATE, GDP_GAP, uncertainty = UNC, hawk.dove = HD_BN, lm = LM, FG)
df <- window(df, start = c(1999, 1), end = c(2020, 1))
ols_robustness[["dictionaries"]] <- lm(SHADOW_RATE ~ CPI * lag(IT) + UNRATE * lag(IT) + GDP_GAP * lag(IT) + uncertainty + hawk.dove + lm + FG, df)







####### Robustness Test 4: Rolling Window IT Index

# Pre 2000 IT index calculation
lit <- corpus %>%
  filter(regime == "LIT", country != "United States", year <= "2000-01-01") %>%
  pull(doc_id)
fit <- corpus %>%
  filter(regime == "FIT", country != "United States", year <= "2000-01-01") %>%
  pull(doc_id)
us <- corpus %>%
  filter(country == "United States", year <= "2000-01-01") %>%
  pull(doc_id)
rolling.window <- rnd(w = doc_embedding, A_words = lit, B_words = fit, S_words = us)
rolling.window <- rolling.window$P %>%
  as_tibble(rownames = "doc_id") %>%
  left_join(corpus) %>%
  filter(date < "2023-01-01")


# Rolling window IT index calculation
for (i in 2000:2021) {
  date_start <- paste0(as.character(i), "-01-01")
  date_end <- paste0(as.character(i + 1), "-01-01")
  lit <- corpus %>%
    filter(regime == "LIT", country != "United States", year <= date_start) %>%
    pull(doc_id)
  fit <- corpus %>%
    filter(regime == "FIT", country != "United States", year <= date_start) %>%
    pull(doc_id)
  us <- corpus %>%
    filter(country == "United States", year > date_start, year <= date_end) %>%
    pull(doc_id)
  df <- rnd(w = doc_embedding, A_words = lit, B_words = fit, S_words = us)
  df <- df$P %>%
    as_tibble(rownames = "doc_id") %>%
    left_join(corpus) %>%
    filter(date < "2023-01-01")
  rolling.window <- rbind(df, rolling.window)
}
rolling.window <- rolling.window %>%
  group_by(quarters = floor_date(date, "quarters")) %>%
  summarise(rnd = mean(value)) %>%
  select(-quarters) %>%
  ts(start = c(1999, 1), frequency = 4)
rolling.window <- (rolling.window - mean(rolling.window)) / sd(rolling.window)


# Regression
df <- cbind(IT = rolling.window, SHADOW_RATE, CPI, UNRATE, GDP_GAP)
df <- window(df, start = c(1999, 1), end = c(2020, 1))
ols_robustness[["Rolling Window"]] <- lm(SHADOW_RATE ~ CPI * lag(IT) + UNRATE * lag(IT) + GDP_GAP * lag(IT), df)


######## Table A11: Statistical Summary
df <- cbind(IT_ROLLING.WINDOW = rolling.window, IT_SPEAKER.FE = speaker, SSR, uncertainty = UNC, hawk.dove = HD_BN, lm = LM, FG)
df <- window(df, start = c(1999, 1), end = c(2020, 1))
df %>%
  data.frame() %>%
  stargazer::stargazer(type = "text", digits = 2, summary.stat = c("n", "mean", "sd", "p25", "median", "p75"))




######## Table A15: Robustness Tests
stargazer::stargazer(ols_robustness,
  type = "text", omit.stat = c("ser", "f"), digits = 2, no.space = F,
  omit = c("year"), column.labels = c("Shadow Rate", "Speaker FE", "FG + Dictionaries", "Rolling Window")
)
