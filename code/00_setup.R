# Objective --------------------------------------------------------------------
# establish basic working environment
# - load packages
# - tidyverse on top
# - create folder and path
# - ggplot settings

# Packages ---------------------------------------------------------------------
pacman::p_load(rvest, tidyRSS, spacyr, progress, pdftools, tidytext, word2vec, doc2vec, tokenizers.bpe, udpipe, lubridate,
  quanteda, widyr, zoo, cld3, readtext, sandwich, tidyverse, lmtest, texreg, quanteda, umap, mlr3verse, data.table,
  progressr, qdapRegex, furrr, tidymodels, gapminder, countrycode, quanteda, quanteda.textstats, SnowballC,
  getwiki, stargazer, keras, textrecipes, quanteda.sentiment, glmnet, caret, tseries, fredr, readxl,
  textdata, text2vec, mFilter, sweater, wordcloud, reshape2, interactions, text2vec, topicmodels,
  install = T, update = F
)

# citation("doc2vec")
# citation("word2vec")
# citation("text2vec")
# citation("tidymodels")
# citation("ranger")
# citation("topicmodels")
# citation("tidytext")
# citation("topicdoc")
# citation("workflowsets")


# devtools::install_github("https://github.com/martinbaumgaertner/hfdshocks.git")
# devtools::install_github("https://github.com/martinbaumgaertner/doc2vec.git")

# library(hfdshocks) #get Altavilla shocks

select <- dplyr::select
filter <- dplyr::filter
rename <- dplyr::rename
options(dplyr.summarise.inform = FALSE)

# Path/Folder ------------------------------------------------------------------
base_path <- getwd()
data_path <- paste0(base_path, "/data/")
#  dir.create(file.path(data_path), showWarnings = FALSE)
# writeLines(c("*.pdf","*.html","*.txt","*.htm","*.xlsx","*.bin","*.Rds"),paste0(file.path(data_path),"/.gitignore"))
#
# bis_path<-paste0(base_path, "/data/bis/")
# dir.create(file.path(bis_path), showWarnings = FALSE)
# macro_path<-paste0(base_path, "/data/macro/")
# dir.create(file.path(macro_path), showWarnings = FALSE)
# embeddings_path<-paste0(base_path, "/data/embeddings/")
# dir.create(file.path(embeddings_path), showWarnings = FALSE)
# models_path<-paste0(base_path, "/data/models/")
# dir.create(file.path(models_path), showWarnings = FALSE)
# hyperparameter_path<-paste0(base_path, "/data/embeddings/hyperparameter/")
# dir.create(file.path(hyperparameter_path), showWarnings = FALSE)
# lda_path<-paste0(base_path, "/data/lda/")
# dir.create(file.path(lda_path), showWarnings = FALSE)
# evaluation_path<-paste0(base_path, "/data/evaluation/")
# dir.create(file.path(evaluation_path), showWarnings = FALSE)
#
# graphics_path<-paste0(base_path, "/graphics/")
# dir.create(file.path(graphics_path), showWarnings = FALSE)
# tables_path<-paste0(base_path, "/tables/")
# dir.create(file.path(tables_path), showWarnings = FALSE)

# Graphic ----------------------------------------------------------------------
#
# theme_set(theme_bw(base_family = "serif")) # also echt Martin ;-)
# theme_update(plot.margin = unit(c(0,1,0,1), "mm"))
#
# theme_set(ggthemes::theme_fivethirtyeight(base_family = "serif"))
# theme_update(plot.background = element_blank(),
#              panel.background = element_blank(),
#              legend.background = element_blank(),
#              legend.key = element_blank(),
#              axis.title = element_text())
# h <- 6.8
# w <- 11.8
# units <- 'in'
# dpi <- 500
#
# # Various ----------------------------------------------------------------------
# coltypes<-cols(.default = col_double(),date = col_datetime(format = ""))
# Sys.setlocale("LC_ALL","English")
