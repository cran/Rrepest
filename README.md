# Rrepest
>A way to run estimations with weighted replicate samples and plausible values

## Table of Contents

* Description
* Installation
* Documentation
* Examples of use cases
* Authors
* Contributing

## Description

It estimates statistics using replicate weights (*Balanced Repeated Replication (BRR) weights*, *Jackknife replicate weights*,...), thus accounting for complex survey designs in the estimation of sampling variances. It is designed specifically for use with the international education datasets produced by the **OECD** (e.g. PIAAC, PISA, SSES, TALIS, etc.), but works for any educational large-scale assessment and survey that uses replicated weights (e.g. ICCS, ICILS, PIRLS, TIMSS - all produced by IEA). It also allows for analyses with multiply imputed variables (plausible values); where plausible values are used, the average estimator across plausible values is reported and the imputation error is added to the variance estimator.

## Installation

### Using CRAN (latest official version)

Run the following code:
``` r
install.packages("Rrepest")
```

### Using tar.gz file (latest development version)

[Download Rrepest](https://gitlab.algobank.oecd.org/edu_data/rrepest/-/blob/main/Development/Rrepest.tar.gz),
then run

Run the following code replacing "You_R_Name" with your username:
``` r
install.packages("C:/Users/You_R_Name/Downloads/Rrepest.tar.gz",
repos = NULL,
type ="source")
```

Run:
``` r
library(Rrepest)
```

### Using a GitLab token (latest development version)

Run the following code replacing "MY_TOKEN" with your gitlab token:
``` r
remotes::install_gitlab("edu_data/rrepest", host = "https://algobank.oecd.org:4430", upgrade = "never", auth_token = "MY_TOKEN")
```
Note: It will take a few minutes to install.

Run:
``` r
library(Rrepest)
```
Note: Ensure you have the package `data.table` installed. For a complete list of the dependencies used, consult the [Description](https://gitlab.algobank.oecd.org/edu_data/rrepest/-/blob/main/DESCRIPTION) file.

## Documentation

* **Full documentation** of `Rrepest` is available [here](https://CRAN.R-project.org/package=Rrepest/Rrepest.pdf).
* **Cheat sheet** including an overview of the syntax and auxiliaries of `Rrepest` is available [here](https://gitlab.algobank.oecd.org/edu_data/rrepest/-/blob/main/Development/Rrepest_cheat_sheet.pdf).
* Information on **how to incorporate analyses that are not pre-programmed** into `Rrepest` is available in the following [wiki](https://gitlab.algobank.oecd.org/edu_data/rrepest/-/wikis/General-Analysis).

## Examples of use cases
`Rrepest` supports **summary statistics** (i.e. mean, variance, standard deviation, quantiles, inter-quantile range), **frequency count**, **correlation**, **linear regression** and any other statistics that are not pre-programmed into `Rrepest` but take a data frame and weights as parameters (see **General analysis** below). `Rrepest` also has optional features that provide means, among others, to specify the level of analysis, obtain estimates for each level of a given categorical variable, test for differences, flag estimates that are based on fewer observations than required for reporting, compute averages. More detail on the optional features of `Rrepest` can be found [here](https://CRAN.R-project.org/package=Rrepest/Rrepest.pdf).

### Summary statistics
``` r
# PISA 2018 Data
# df.qqq <- readRDS("//oecdmain/asgenedu/EDUCATION_DATALAKE/sources/PISA/PISA 2018/R/STU/CY07_MSU_STU_QQQ.rds")

Rrepest::Rrepest(data = df.qqq,
        svy = "PISA2015",
        est = est(c("mean","var","std","quant",0.5,"iqr",c(.9,.1)),"age"),
        by = c("cnt"))

```
### Frequency count
``` r
# TALIS 2018 Data
# df.t <- readRDS("//oecdmain/asgenedu/EDUCATION_DATALAKE/sources/TALIS/2018/R/International/TTGINTT3.rds")

Rrepest::Rrepest(data = df.t,
                 svy = "TALISTCH",
                 est = est("freq","tt3g01"),
                 by = "cntry")
```
### Correlation
``` r
# PISA 2018 Data
# df.qqq <- readRDS("//oecdmain/asgenedu/EDUCATION_DATALAKE/sources/PISA/PISA 2018/R/STU/CY07_MSU_STU_QQQ.rds")

Rrepest::Rrepest(data = df.qqq,
        svy = "PISA2015",
        est = est("corr",c("pv@math","pv@read")),
        by = c("cnt"))

```

### Linear regression
``` r
# TALIS 2018 Data
# df.t <- readRDS("//oecdmain/asgenedu/EDUCATION_DATALAKE/sources/TALIS/2018/R/International/TTGINTT3.rds")

df.t <- df.t %>% 
        mutate(TT3G01_rec = case_when(TT3G01 == 2 ~ 1,
                                      TT3G01 == 1 ~ 0))

Rrepest::Rrepest(data = df.t,
        svy = "TALISTCH",
        est = est("lm","tt3g01_rec","tt3g39c"),
        by = "cntry")
```

Further examples can be found in the [Examples.R](https://gitlab.algobank.oecd.org/edu_data/rrepest/-/blob/main/Development/Examples.R) file.

### General analysis
To incorporate analyses that are not pre-programmed into Rrepest, you can utilize the **'gen'** option within the `est()` function of Rrepest. Any line of code that takes a data frame and weights as parameters can be used with the **'gen'** option. For more information, please see the following [wiki](https://gitlab.algobank.oecd.org/edu_data/rrepest/-/wikis/General-Analysis).

# Authors

[Francesco Avvisati](mailto:francesco.avvisati@oecd.org), [Rodolfo Ilizaliturri](mailto:Rodolfo.ILIZALITURRI@oecd.org) and  [François Keslair](mailto:francois.keslair@oecd.org). 

Contact us if you want to join!


## Contributing

Do you have suggestions or comments? Please open an [issue](https://gitlab.algobank.oecd.org/edu_data/rrepest/-/issues).
