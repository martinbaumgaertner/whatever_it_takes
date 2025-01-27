library(tidyverse)

find_collocations <- function(data,
                              min_z_caps = 25, min_count_caps = 100,
                              min_z_lower = 100, min_count_lower = 500,
                              store = TRUE, tolower = TRUE,
                              path = "data/helper/clean") {
  colloc_file <- file.path(path, "collocations.Rds")

  if (!file.exists(colloc_file)) {
    process_collocations <- function(data, pattern, min_count, min_z) {
      data %>%
        dplyr::pull(text, doc_id) %>%
        quanteda::tokens() %>%
        quanteda::tokens_remove(stopwords::stopwords("en"), padding = TRUE) %>%
        quanteda::tokens_select(
          pattern = pattern,
          valuetype = "regex",
          case_insensitive = FALSE,
          padding = TRUE
        ) %>%
        quanteda.textstats::textstat_collocations(min_count = min_count, tolower = tolower, size = 2) %>%
        dplyr::filter(z >= min_z)
    }

    caps <- process_collocations(data, "^[A-Z\\.]", min_count_caps, min_z_caps)
    lower <- process_collocations(data, "^[a-z\\.]", min_count_lower, min_z_lower)

    full <- dplyr::bind_rows(caps, lower) %>%
      dplyr::mutate(
        collocation = tolower(collocation),
        collocation_bound = stringi::stri_replace_all_regex(collocation, "\\s+", "_")
      ) %>%
      dplyr::distinct() %>%
      dplyr::as_tibble()

    if (store) {
      saveRDS(full, file = colloc_file)
    }
  } else {
    full <- readr::read_rds(colloc_file)
  }

  return(full)
}

replace_collocations <- function(
    data,
    collocations,
    chunk_size = 10000 # Adjust to suit your data size
    ) {
  collocations <- collocations %>%
    dplyr::distinct(collocation, collocation_bound) %>%
    dplyr::mutate(
      collocation = paste0("\\b", tolower(collocation), "\\b"),
      collocation_bound = tolower(collocation_bound)
    )

  speaker_collocation <- data %>%
    dplyr::select(speaker) %>%
    dplyr::mutate(
      speaker = stringr::str_remove_all(speaker, ","),
      speaker = stringr::str_split(speaker, " and ")
    ) %>%
    tidyr::unnest(speaker) %>%
    dplyr::distinct(speaker) %>%
    dplyr::filter(!is.na(speaker)) %>%
    dplyr::mutate(
      speaker = tolower(speaker),
      speaker_bound = stringi::stri_replace_all_regex(speaker, "\\s+", "_"),
      speaker = paste0("\\b", speaker, "\\b")
    )

  patterns <- c(collocations$collocation, speaker_collocation$speaker)
  replacements <- c(collocations$collocation_bound, speaker_collocation$speaker_bound)

  replacement_dict <- setNames(replacements, patterns)

  data_dt <- data.table::as.data.table(data)

  data_dt[, text_colloc := tolower(text)]

  # 6. Process in chunks (helps with very large data)
  total_rows <- nrow(data_dt)
  row_indices <- seq_len(total_rows)

  index_chunks <- split(row_indices, ceiling(row_indices / chunk_size))

  # Perform replacements on each chunk
  for (chunk_ix in seq_along(index_chunks)) {
    idx <- index_chunks[[chunk_ix]]

    # Single-pass replacement on each chunk using stringi
    data_dt[idx, text_colloc := stringi::stri_replace_all_regex(
      str = text_colloc,
      pattern = names(replacement_dict),
      replacement = replacement_dict,
      vectorize_all = FALSE
    )]
  }

  data <- tibble::as_tibble(data_dt)

  return(data)
}
