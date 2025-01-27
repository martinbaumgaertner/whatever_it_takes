# Description ------------------------------------------------------------------

# Title: Intrinsic Evaluation
#
# This R code replicates the intrinsic evaluations from Chapter 4.3 of the paper
# "Whatever it takes to understand a central banker – Embedding their words using
# neural networks." To reproduce the results without modification, the following
# prerequisites are required:
#
#   1. Packages: All R packages listed in the project's setup file must be installed.
#   This ensures that all necessary functions and dependencies are available.
#
#   2. Access to the trained LLM.






# Preparation ------------------------------------------------------------------

library(tidyverse)
library(doc2vec)
library(textdata)


# Data -------------------------------------------------------------------------

#### Our pre-trained Word Embeddings
doc2vec <- read.paragraph2vec("data/embeddings/fulldoc2vecPVDBOWpre300.bin") %>% as.matrix(which = "words")


#### Our pre-trained Document Embeddings
doc_embedding <- read.paragraph2vec("data/embeddings/fulldoc2vecPVDBOWpre300.bin") %>% as.matrix(which = "docs")
doc_embedding <- as_tibble(doc_embedding, rownames = "doc_id") %>% filter(grepl("doc_", doc_id))


##### GloVe6B
# You might need to install the glove package from http://nlp.stanford.edu/data/glove.6B.zip. See return_path at https://www.rdocumentation.org/packages/textdata/versions/0.4.4/topics/embedding_glove
glove <- embedding_glove6b(dimensions = 300, manual_download = TRUE) %>%
  column_to_rownames(var = "token") %>%
  as.matrix()


##### GoogleNews
google <- read_table2("data/embeddings/GoogleNews-vectors-negative300-SLIM.txt", col_names = FALSE, skip = 1)
google <- google %>% .[-234111, ] # remove word INVICTUS because of data error
google <- google %>%
  rename_at(vars(paste0("X", 2:301)), function(x) paste0("V", 1:300)) %>%
  rename(token = X1) %>%
  filter(!is.na(token)) %>%
  column_to_rownames(var = "token") %>%
  as.matrix()


#### Speech-data
dataset <- readRDS("data/processed/text_data.Rds")
dataset$year <- dataset$date %>% floor_date("year")
dataset$ISO3 <- countrycode::countrycode(sourcevar = dataset$country, origin = "country.name", destination = "iso3c")
dataset <- dataset %>% mutate(ISO3 = if_else(country == "Euro area", "EUR", ISO3))
dataset <- dataset %>%
  select(ISO3, year, doc_id) %>%
  na.omit()


##### Worldmap Data
world_raw <- map_data("world") # Load world map data
name_iso <- countrycode::codelist %>% select(region = country.name.en, ISO2 = iso2c, ISO3 = iso3c) # Select relevant country codes from codelist
world_raw <- world_raw %>% left_join(name_iso) # Join map data with country codes
world_raw <- world_raw %>% mutate(ISO3 = if_else(is.na(ISO3), countrycode::countrycode(sourcevar = region, origin = "country.name", destination = "iso3c"), ISO3))




# Intrinsic Evaluation 1 and 2 -------------------------------------------------


# The top_embeddings function is designed to find the n_output most similar words
# to a given word within a list of word embedding models (models).

top_embeddings <- function(models, word, n_output) {
  output <- list()

  for (i in 1:length(models)) {
    model <- models[[i]]
    rownumber <- which(rownames(model) == word)
    name <- names(models)[i]
    if (length(rownumber) == 0) {
      message("Word does not exist in Corpus")
      output[[name]] <- NA
    } else {
      input_vector <- as.matrix(t(model[rownumber, ]))

      output[[name]] <- as_tibble(text2vec::sim2(model, input_vector), rownames = "Word") %>%
        arrange(desc(V1)) %>%
        slice(-1) %>%
        top_n(n_output, V1) %>%
        mutate(Rank = 1:n_output) %>%
        rename(Similarity = V1) %>%
        relocate(Rank)
    }
  }

  return(enframe(output) %>%
    unnest(value) %>%
    select(-Similarity) %>%
    pivot_wider(names_from = name, values_from = Word))
}



#### Table 6:
top_embeddings(list("doc2vec" = doc2vec), "inflation", 10)
top_embeddings(list("doc2vec" = doc2vec), "unemployment", 10)
top_embeddings(list("doc2vec" = doc2vec), "output", 10)



##### Table 7:
top_embeddings(list("doc2vec" = doc2vec, "glove" = glove, "google" = google), "basel", 10)


# Not in the published paper
top_embeddings(list("doc2vec" = doc2vec, "glove" = glove, "google" = google), "greening", 10)



# Intrinsic Evaluation 3 -------------------------------------------------------


#### Join speech data with document embeddings and reshape for pairwise comparison.
similarity <- dataset %>%
  left_join(doc_embedding) %>%
  pivot_longer(V1:V300, names_to = "dimension") %>%
  na.omit()


#### Calculate average central bank (CB) embedding for each country.
similarity <- similarity %>%
  group_by(ISO3, dimension) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  ungroup()


#### Calculate pairwise similarity between CB embeddings.
similarity <- similarity %>%
  select(ISO3, dimension, value) %>%
  na.omit()
similarity <- similarity %>% widyr::pairwise_similarity(ISO3, dimension, value)
similarity <- similarity %>%
  select(target_country = item1, ISO3 = item2, everything()) %>%
  ungroup()

#### Filter to keep only similarities with the European Central Bank (ECB) as the target country.
similarity <- similarity %>% filter(target_country == "EUR")


#### Join pairwise similarity data with world map data for visualization.
map <- world_raw %>% left_join(similarity)



#######  Figure 5: Map of Similarity to the ECB
map %>%
  filter(lat > -66) %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = similarity)) +
  scale_fill_distiller(palette = "Blues", direction = 1) + # or direction=1
  coord_fixed(1.3) +
  labs(fill = "Similarity") +
  theme_classic(base_family = "serif") +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "white")
  )
