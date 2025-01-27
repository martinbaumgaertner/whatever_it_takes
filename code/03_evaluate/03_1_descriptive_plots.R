# Description ------------------------------------------------------------------
#
# Title: Corpus Summary
#
# This R code replicates the descriptive statistics, Table 1, and Figure 1 from Chapter 3.1
# of the paper "Whatever it takes to understand a central banker – Embedding their words
# using neural networks.". To execute this code without modification and reproduce the
# results, the following prerequisites must be met:
#
#   1. Packages: All R packages listed in the project's setup file must be installed.
#   This ensures that all necessary functions and dependencies are available.
#
#   2. Text-Data: The text corpus itself is required. This corpus should be placed in
#   a subfolder named "corpus" within the same directory as this R script. This file
#   structure allows the code to locate and load the data correctly.


library(tidyverse)

# Data -------------------------------------------------------------------------

##### Speech-data
dataset <- readRDS("data/processed/text_data.Rds") # Load speech data from an RDS file

##### Map Data


# Select relevant country codes from codelist
name_iso <- countrycode::codelist %>% select(region = country.name.en, ISO2 = iso2c, ISO3 = iso3c)
world_raw <- map_data("world") %>%
  tibble() %>%
  left_join(name_iso)

#### Merge Map Data and Corpus Data

# Count the number of speeches per country code
data_map <- dataset %>%
  count(country_code) %>%
  rename("ISO2" = country_code)

map <- world_raw %>%
  left_join(data_map) %>%
  # Create a binary variable indicating if a country is in the corpus
  mutate(n = factor(if_else(is.na(n), "No", "Yes"))) %>%
  unique()

# Descriptive Statistics -------------------------------------------------------

###### Corpus Summary Statistics
dataset %>% nrow() # 21.134 speeches
dataset %>%
  count(cb) %>%
  nrow() # 128 central banks
dataset %>%
  count(speaker) %>%
  nrow() # 892 central bankers
dataset$nwords %>% sum() # 112,770,492 words


pop <- map %>%
  select(region, ISO2, ISO3, n) %>%
  distinct() %>%
  left_join(gapminder::gapminder %>% filter(year == 2007), by = c("region" = "country")) %>%
  na.omit()


##### Population covered in our corpus
pop$pop %>% sum() # World population <- 5625941443
pop %>%
  group_by(in_corpus = n) %>%
  summarise(pop = sum(pop) / 5625941443 * 100)
# `In Corpus`   pop
# 1 Yes         82.9
# 2 No          17.1


##### GDP covered in our corpus
pop$GDP <- pop$gdpPercap * pop$pop # Calculate GDP for each country
pop$GDP %>% sum() # World GDP <- 4.113013e+13
pop %>%
  group_by(in_corpus <- n) %>%
  summarise(gdp <- sum(gdp) / 4.113013e+13 * 100)
#     `In Corpus`   GDP
# 1   Yes           89.5
# 2   No            10.5



# Table 2: Corpus Summary ------------------------------------------------------

###### Top 10:
dataset %>%
  count(cb) %>%
  arrange(-n) %>%
  mutate(nn <- n / sum(n) * 100) %>%
  top_n(10, n)

###### Bottom:
dataset %>%
  count(cb) %>%
  arrange(n) %>%
  mutate(nn <- n / sum(n) * 100) %>%
  .[1, ]


# Figure 3: Properties of the Text corpus --------------------------------------

map %>% distinct(n)

####### Plot 1: World Map
map %>%
  filter(lat > -66) %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = n)) +
  coord_fixed(1.3) +
  labs(fill = "Covered in the corpus?") +
  scale_fill_grey(start = 0.2) +
  theme_classic(base_family = "serif") +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill <- "white")
  )
ggsave("Worldmap_corpus.pdf", width = 10, height = 5)


####### Plot 2: Type of documents / Number of speeches
nword_plot <- dataset %>% count(type = "speech") %>%
  mutate(type = if_else(type == FALSE, "Other", "Speech")) %>%
  ggplot(aes(n, type)) +
  geom_col(show.legend = FALSE, fill = "grey20") +
  theme(base_family = "serif") +
  labs(y = "Type of document", x = "Number of Documents")


####### Plot 3: Number of words per document
n_speech_plot <- dataset %>%
  filter(nwords < 70000) %>%
  ggplot(aes(nwords)) +
  geom_histogram(binwidth = 500, fill = "grey20") +
  labs(y = "Number of Documents", x = "Number of Words")


####### Plot 4: Temporal Variation
time_plot <- dataset %>%
  count(date = year(date)) %>%
  filter(date > 1950, date < 2025)  %>%
  ggplot(aes(date, n)) +
  geom_col(fill = "grey20") +
  labs(y = "Number of Documents", x = "")

ggpubr::ggarrange(time_plot, nword_plot, n_speech_plot, ncol = 3)
ggsave("Corpus_cummary.pdf", width = 10, height = 2.5)
