# Description ------------------------------------------------------------------

# Title: MP Classification
#
# This R code replicates the results from Chapter 5 of the paper "Whatever it takes to
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
#   3. Access to the trained LLM.
#
#   4. Access to the Monetary Frameworks Database by Cobham (2021), available here
#   (last accessed: 01.10.2023): https://monetaryframeworks.org


# Data Collection --------------------------------------------------------------


## 1) Similarity -----------

###### Pre-trained Document Embeddings
doc_embedding <- read.paragraph2vec("/Users/johanneszahner/Research/Whatever_it_takes/data/embeddings/fulldoc2vecPVDBOWpre300.bin") %>% as.matrix(which = "docs")
doc_embedding <- as_tibble(doc_embedding, rownames = "doc_id")


#### Prepare Speech-data
corpus <- readRDS("corpus/dataset.Rds")
corpus <- corpus %>% filter(type == "speech")
corpus$ISO3 <- countrycode(sourcevar = corpus$country, origin = "country.name", destination = "iso3c")
corpus <- corpus %>% mutate(ISO3 = if_else(country == "Euro area", "EUR", ISO3))
corpus$year <- floor_date(corpus$date, "year")
corpus <- corpus %>% select(ISO3, year, doc_id)


#### Merge speech data with document embeddings
doc_embedding <- corpus %>%
  left_join(doc_embedding) %>%
  pivot_longer(V1:V300, names_to = "dimension") %>%
  na.omit()


#### Average annual pairwise similarity
similarity <- doc_embedding %>%
  group_by(ISO3, year, dimension) %>%
  summarise(value = mean(value, na.rm = T)) %>%
  ungroup()
similarity <- similarity %>%
  group_by(year) %>%
  select(ISO3, dimension, value) %>%
  na.omit()
similarity <- similarity %>% pairwise_similarity(ISO3, dimension, value)
similarity <- similarity %>%
  select(target_country = item1, ISO3 = item2, everything()) %>%
  ungroup()

#### Focuses the analysis on New Zealand (NZL), United States (USA), and Euro Area (EUR):
similarity <- similarity %>% filter(target_country %in% c("NZL", "USA", "EUR"))
similarity <- similarity %>% filter(year >= "1999-01-01")


## 2) Cobham MP Classification -------


###### Cobham's Targets
cobham <- read_excel("Cobham (2021)/mpfclassificationdata_revjul19.xlsx", sheet = 7, range = "A1:AS63")
cobham_africa <- read_excel("Cobham (2021)/africaclassificationdecember2021.xlsx", sheet = 2, range = "A1:AS55")
cobham_latinam <- read_excel("Cobham (2021)/latamclassificationfeb2020.xlsx", sheet = 2, range = "A1:AS23")
cobham_asia <- read_excel("Cobham (2021)/asiaedclassificationaugust2020.xlsx", sheet = 2, range = "A1:AS26")
cobham_mena <- read_excel("Cobham (2021)/menaclassificationjul2019.xlsx", sheet = 2, range = "A1:AS20")
cobham_target <- rbind(cobham, cobham_africa, cobham_latinam, cobham_asia, cobham_mena)
cobham_target <- cobham_target %>% select(country = ...1, everything())
cobham_target <- cobham_target %>% pivot_longer(-c(country), values_to = "cobham_target", names_to = "year")


###### Cobham's MP Framework
cobham <- read_excel("Cobham (2021)/mpfclassificationdata_revjul19.xlsx", sheet = 6, range = "A1:AS63")
cobham_africa <- read_excel("Cobham (2021)/africaclassificationdecember2021.xlsx", sheet = 1, range = "A1:AS55")
cobham_latinam <- read_excel("Cobham (2021)/latamclassificationfeb2020.xlsx", sheet = 1, range = "A1:AS23")
cobham_asia <- read_excel("Cobham (2021)/asiaedclassificationaugust2020.xlsx", sheet = 1, range = "A1:AS26")
cobham_mena <- read_excel("Cobham (2021)/menaclassificationjul2019.xlsx", sheet = 1, range = "A1:AS20")
cobham_framework <- rbind(cobham, cobham_africa, cobham_latinam, cobham_asia, cobham_mena)
cobham_framework <- cobham_framework %>% select(country = ...1, everything())
cobham_framework <- cobham_framework %>% pivot_longer(-c(country), values_to = "cobham_framework", names_to = "year")


######## Final Dataset
cobham <- cobham_target %>% left_join(cobham_framework)
cobham$year <- ymd(paste0(cobham$year, "-01-01"))


###### Add and correct ISO3 country codes
cobham$ISO3 <- countrycode(sourcevar = cobham$country, origin = "country.name", destination = "iso3c")
cobham <- cobham %>% mutate(ISO3 = if_else(country == "Euro area", "EUR", ISO3))
cobham <- cobham %>% mutate(ISO3 = if_else(country == "SKorea", "KOR", ISO3))
cobham <- cobham %>% mutate(ISO3 = if_else(country == "Equatorial Giunea", "GNQ", ISO3))
cobham <- cobham %>% mutate(ISO3 = if_else(country == "Saudi", "SAU", ISO3))


####### Remove non-classifications
cobham <- cobham %>% filter(!cobham_framework == "X")


##### Add control for Euro area Members
member_ea <- c("Germany", "France", "Austria", "Belgium", "Finland", "Greece", "Ireland", "Italy", "Luxembourg", "Netherlands", "Portugal", "Spain", "Cyprus", "Estonia", "Latvia", "Lithuania", "Malta", "Slovakia", "Slovenia", "Euro area")
cobham <- cobham %>% mutate(ea_member = if_else(cobham_framework == "CU" & country %in% member_ea & year >= "1999-01-01" | ISO3 == "EUR", "member", "non-member"))
cobham <- cobham %>% mutate(cobham_target = if_else(is.na(cobham_target) & ea_member == "member", "ITs", cobham_target), cobham_framework = if_else(cobham_framework == "CU" & ea_member == "member", "LIT", cobham_framework))


###### Add non_IT dummy for non-IT MP Frameworks
cobham$cobham_framework_IT.regime <- if_else(cobham$cobham_framework %in% c("FIT", "FCIT", "LIT", "LCIT"), cobham$cobham_framework, cobham$cobham_target)





## 3) Macro Data ----------------
# We are using the World Bank's World Development Indicators (WDI) as primary
# data source. For the ECB, we rely on the FRED database.


####### Inflation Rate
CPI <- read_csv("World Bank Development Indicators/CPI.csv", skip = 3) %>% select(ISO3 = `Country Code`, "1960":"2022")
CPI <- CPI %>% pivot_longer(-c(ISO3), names_to = "year", values_to = "CPI")
CPI$year <- paste(CPI$year, "-01-01") %>% ymd()
CPI.EA <- fredr(c("FPCPITOTLZGEMU")) %>% select(year = date, CPI = value)
CPI.EA$ISO3 <- "EUR"
CPI <- CPI %>% rbind(CPI.EA)


###### Unemployment Rate
UNRATE <- read_csv("World Bank Development Indicators/UNRATE.csv", skip = 3) %>% select(ISO3 = `Country Code`, "1960":"2022")
UNRATE <- UNRATE %>% pivot_longer(-c(ISO3), names_to = "year", values_to = "UNRATE")
UNRATE$year <- paste(UNRATE$year, "-01-01") %>% ymd()
UNRATE.EA <- fredr(c("LRHUTTTTEZA156S")) %>% select(year = date, UNRATE = value)
UNRATE.EA$ISO3 <- "EUR"
UNRATE <- UNRATE %>% rbind(UNRATE.EA)


####### GDP
GDP <- read_csv("World Bank Development Indicators/GDP.csv", skip = 3) %>% select(ISO3 = `Country Code`, "1960":"2022")
GDP <- GDP %>% pivot_longer(-c(ISO3), names_to = "year", values_to = "GDP")
GDP$year <- paste(GDP$year, "-01-01") %>% ymd()
GDP.EA <- fredr(c("LRHUTTTTEZA156S")) %>% select(year = date, GDP = value)
GDP.EA$ISO3 <- "EUR"
GDP <- GDP %>% rbind(GDP.EA)


##### Macro Dataset
economic_indicator <- CPI %>%
  full_join(UNRATE) %>%
  full_join(GDP)


##### Variable Transformations
funct <- function(variable) {
  variable <- na.locf(variable, fromLast = F, na.rm = F)
} # Fill variables
economic_indicator <- economic_indicator %>%
  group_by(ISO3) %>%
  mutate_all(funct) %>%
  ungroup()
economic_indicator <- economic_indicator %>% na.omit()
economic_indicator$GDP <- economic_indicator$GDP %>% log() # Use log GDP


###### Add Difference of an indicator between a country and the US/NZ/EA
economic_indicator <- economic_indicator %>%
  mutate(

    # Euro area
    CPI.diff.EA = CPI - CPI[ISO3 == "EUR"],
    GDP.diff.EA = GDP - GDP[ISO3 == "EUR"],
    UNRATE.diff.EA = UNRATE - UNRATE[ISO3 == "EUR"],

    # New Zealand
    CPI.diff.NZ = CPI - CPI[ISO3 == "NZL"],
    GDP.diff.NZ = GDP - GDP[ISO3 == "NZL"],
    UNRATE.diff.NZ = UNRATE - UNRATE[ISO3 == "NZL"],

    # United States
    CPI.diff.US = CPI - CPI[ISO3 == "USA"],
    GDP.diff.US = GDP - GDP[ISO3 == "USA"],
    UNRATE.diff.US = UNRATE - UNRATE[ISO3 == "USA"]
  ) %>%
  ungroup()





## 4) Merge Data & fix factor variables -----------

# Merge similarity data with economic indicators and Cobham classifications
similarity <- similarity %>%
  left_join(economic_indicator) %>%
  left_join(cobham) %>%
  na.omit()

# Relevel factor variables for regression analysis
similarity$cobham_target <- relevel(factor(similarity$cobham_target), ref = "ERfix")
similarity$cobham_framework <- relevel(factor(similarity$cobham_framework), ref = "FERT")
similarity$cobham_framework_IT.regime <- relevel(factor(similarity$cobham_framework_IT.regime), ref = "ERfix")
similarity$ea_member <- relevel(factor(similarity$ea_member), ref = "non-member")
similarity <- similarity %>% distinct()






# Statistical Summary ----------------------------------------------------------

#### Summary Statistic Dataset
summary_statistic <- similarity %>%
  select(ISO3, year, cobham_target) %>%
  distinct()
summary_statistic <- summary_statistic %>%
  mutate(n = 1) %>%
  pivot_wider(names_from = cobham_target, values_from = n, values_fill = 0)
summary_statistic <- summary_statistic %>%
  select(-c(year, ISO3)) %>%
  as.data.frame()
summary_statistic %>% stargazer(type = "text")

#### Economy Specific Data set
nz <- similarity %>%
  filter(target_country == "NZL") %>%
  distinct() %>%
  select(similarity, contains("nz")) %>%
  data.frame()
us <- similarity %>%
  filter(target_country == "USA") %>%
  distinct() %>%
  select(similarity, contains("us")) %>%
  data.frame()
ea <- similarity %>%
  filter(target_country == "EUR") %>%
  distinct() %>%
  select(similarity, contains("EA")) %>%
  data.frame()


#### Table A11
stargazer(summary_statistic, nz, us, ea, type = "text", digits = 2, summary.stat = c("n", "mean", "sd", "p25", "median", "p75"), title = "Summary Statistics Main Reg. 1")




# Regression Analysis ----------------------------------------------------------

reg_results <- list()

# New Zealand Regressions
reg_results[["A1"]] <- similarity %>%
  filter(target_country == "NZL") %>%
  distinct() %>%
  lm(formula = similarity ~ cobham_target + factor(year))
reg_results[["A2"]] <- similarity %>%
  filter(target_country == "NZL") %>%
  lm(formula = similarity ~ cobham_target + factor(year) + CPI.diff.NZ + UNRATE.diff.NZ + GDP.diff.NZ)
reg_results[["A4"]] <- similarity %>%
  filter(target_country == "NZL") %>%
  lm(formula = similarity ~ cobham_framework_IT.regime + factor(year) + CPI.diff.NZ + UNRATE.diff.NZ + GDP.diff.NZ)

# United States Regressions
reg_results[["B1"]] <- similarity %>%
  filter(target_country == "USA") %>%
  lm(formula = similarity ~ cobham_target + factor(year))
reg_results[["B2"]] <- similarity %>%
  filter(target_country == "USA") %>%
  lm(formula = similarity ~ cobham_target + factor(year) + CPI.diff.US + UNRATE.diff.US + GDP.diff.US)
reg_results[["B4"]] <- similarity %>%
  filter(target_country == "USA") %>%
  lm(formula = similarity ~ cobham_framework_IT.regime + factor(year) + CPI.diff.US + UNRATE.diff.US + GDP.diff.US)

# Euro Area Regressions
reg_results[["C1"]] <- similarity %>%
  filter(target_country == "EUR") %>%
  lm(formula = similarity ~ cobham_target + factor(year))
reg_results[["C2"]] <- similarity %>%
  filter(target_country == "EUR") %>%
  lm(formula = similarity ~ cobham_target + factor(year) + CPI.diff.EA + UNRATE.diff.EA + GDP.diff.EA)
reg_results[["C4"]] <- similarity %>%
  filter(target_country == "EUR") %>%
  lm(formula = similarity ~ cobham_framework_IT.regime + factor(year) + CPI.diff.EA + UNRATE.diff.EA + GDP.diff.EA)


##### Table 5: Regression results: Monetary Policy Regime classification
stargazer(reg_results,
  type = "text", no.space = T, omit.stat = c("f", "ser"), digits = 2, column.labels = c("RBNZ", "Fed", "ECB"), column.separate = c(3, 3, 3),
  keep = c("cobham_targetITs", "cobham_framework_IT.regimeFIT", "cobham_framework_IT.regimeLIT", "cobham_framework_IT.regimeFCIT", "cobham_framework_IT.regimeLCIT", "Constant"),
  dep.var.caption = "Similarity", dep.var.labels.include = F, add.lines = list(c("Macro-controls", rep(c("No", "Yes", "Yes"), 4)), c("Year Fixed Effects", rep("Yes", 12)))
)




##### Table A12: Regression results: Monetary Policy Framework classification
stargazer(reg_results,
  type = "text", no.space = T, omit.stat = c("f", "ser"), digits = 2, column.labels = c("RBNZ", "Fed", "ECB"), column.separate = c(3, 3, 3),
  keep = c("cobham_target", "cobham_framework", "cobham_framework_IT.regime", "Constant"), dep.var.caption = "Similarity", dep.var.labels.include = F,
  add.lines = list(c("Macro-controls", rep(c("No", "Yes", "Yes"), 4)), c("Year Fixed Effects", rep("Yes", 12)))
)
