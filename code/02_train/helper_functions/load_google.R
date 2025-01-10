load_google_vectors <- function(
    file_path,
    remove_row = NULL,
    skip = 1,
    rename_prefix = "V",
    word_column = "word") {
  # Read the Google News vectors file
  google <- readr::read_table2(file_path, col_names = FALSE, skip = skip) %>%
    {
      if (!is.null(remove_row)) {
        .[-remove_row, ]
      } else {
        .
      }
    } %>%
    # Rename columns X2 to X301 to V1 to V300
    dplyr::rename_with(
      .cols = tidyselect::starts_with("X"),
      .fn = function(x) {
        if (grepl("^X[2-9][0-9]*$", x)) {
          paste0(rename_prefix, as.numeric(sub("X", "", x)) - 1)
        } else if (x == "X1") {
          word_column
        } else {
          x
        }
      }
    )

  # Assign row names and convert to matrix
  rownames <- google %>% dplyr::pull({{ word_column }})
  google_matrix <- google %>%
    dplyr::select(-{{ word_column }}) %>%
    as.matrix()
  rownames(google_matrix) <- rownames

  return(google_matrix)
}
