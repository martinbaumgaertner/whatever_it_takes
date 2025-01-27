# Replication Code of  
**_"Whatever it takes to understand a central banker"_**

## Authors
Martin Baumgärtner and Johannes Zahner

---

## Abstract

Dictionary approaches are at the forefront of current techniques for quantifying central bank communication. This paper proposes **embeddings**—a language model trained using machine learning techniques—to locate words and documents in a multidimensional vector space.  

To accomplish this, we gather a text corpus that is unparalleled in size and diversity within the central bank communication literature, as well as introduce a novel approach to text quantification from computational linguistics.  

The combination of both allows us to provide high-quality, central bank-specific textual representations and demonstrate their applicability by developing an index that tracks deviations in the Fed's communication towards inflation targeting.  

Our findings indicate that these deviations in communication significantly affect market expectations and impact monetary policy actions, substantially reducing the inflation response parameter in an estimated Taylor Rule.

---

## Preparation

### Install packages with renv

To install the packages used in this project, you can use the `renv` package. Install the package with the following command:

```r
install.packages("renv")
renv::restore()
```

### Code
We publish our Code at Mendeley and Github:

- Mendeley: [Link](https://data.mendeley.com/datasets/xxxx)
- Github: [Link](https://github.com/martinbaumgaertner/whatever_it_takes)

The Mendely link is a static representation of the 1.0 Release of the code. The Github link is the most current version of the code.


Download the contents of the `.zip` archive into your desired folder on your computer. The folder will contain:

1. **readme.md** — This file.  
2. **R-Scripts** (`clean_data.R`, `03_1_descriptive_plots.R`, ...): Scripts named according to the chapters they replicate. For example, the script `"03_1_descriptive_plots.R"` replicates the descriptive plots of Chapter 3.1. The scripts can be run independently. Detailed descriptions of which table/figure is replicated by each script are provided below.

3. Data
We publish the core Textdata and all foreign data. 

#### **data/foreign_data** contains all foreign data used in the analysis. The data is stored in the following formats: `.csv`, `.xlsx`, and `.rds`. The data is necessary for running the scripts. It contains the following data:

- Folder **cobham** contains five XLSX datasets from the Monetary Frameworks Database by Cobham (2021). The data files can also be accessed at: [https://monetaryframeworks.org](https://monetaryframeworks.org) (last accessed: 01.10.2023).

- Folder **krippner** contains an XLSX dataset with the shadow short rate from Krippner (2021). Access the dataset at: [www.ljkmfa.com](www.ljkmfa.com) (last accessed: 01.10.2023).

- Folder **world_bank** contains three CSV datasets from the WDI database.

- File **swanson_2021.xlsx** contains the Forward Guidance Shocks by Swanson (2021). Access the dataset at: [Swanson Research Published](https://sites.socsci.uci.edu/~swanson2/researchpublished.html) (last accessed: 01.04.2024).

- File **WuXiaShadowRate.xlsx**/**shadowrate_ecb.csv** contains the Wu-Xia Shadow Rates for the Federal Reserve (XLSX) and ECB (CSV). Access the data at: [Wu-Xia Shadow Rates](https://sites.google.com/view/jingcynthiawu/shadow-rates) (last accessed: 01.10.2023). 

#### **data/raw** contains the core text data used in the analysis. The data is stored in the the Rds format. The data is used to train the language models.


### FRED-API
To replicate all figures and tables, a FRED API key is required. This allows the code to directly load data from the FRED database, minimizing user intervention. You can obtain a free FRED API key here: [https://www.stlouisfed.org](https://www.stlouisfed.org).

---

## Contact
For questions, please contact **Johannes Zahner** at johannes.zahner@googlemail.com.


