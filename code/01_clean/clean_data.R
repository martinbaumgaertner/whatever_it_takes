library(tidyverse)

raw_text <- read_rds("data/raw/text_data_raw.Rds")

source("code/01_clean/helper/collocations.R")

collocations <- raw_text %>%
  # This process is memory intensive, uncomment the following line to test it
  # sample_n(100) %>%
  find_collocations()

clean_text <- raw_text %>%
  # This process is memory intensive, uncomment the following line to test it
  # sample_n(100) %>%
  replace_collocations(collocations)

filter_text <- clean_text %>%
  dplyr::filter(language == "en") %>%
  dplyr::filter(nwords < 20000 & nchar(text) > 0) %>%
  # use collocations
  select(-text) %>%
  rename(text = text_colloc)

saveRDS(filter_text, file = "data/processed/text_data.Rds")
