# Rrepest
>R package to run estimations with weighted replicate samples and plausible values

## Description

`Rrepest` estimates statistics using replicate weights (*balanced repeated replication*/*brr weights*, *jackknife replicate weights*,...), thus accounting for complex survey designs in the estimation of sampling variances. It is especially designed for use with the international education datasets produced by the **OECD** and the **IEA**, and also allows for analyses with multiply imputed variables (plausible values); where plausible values are used, the average estimator across plausible values is reported and the imputation error is added to the variance estimator.

## Cheat Sheet

A "cheat sheet" including an overview of the syntax and uses of `Rrepest` is available [here](https://gitlab.algobank.oecd.org/edu_data/rrepest/-/blob/main/Development/Rrepest_cheat_sheet.pdf)

## Installation

### Using CRAN

Run the following code:
``` r
install.packages("Rrepest")
```

### Using tar.gz file

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

### Using a GitLab token

Run the following code replacing "MY_TOKEN" with your gitlab token:
``` r
remotes::install_gitlab("edu_data/rrepest", host = "https://algobank.oecd.org:4430", upgrade = "never", auth_token = "MY_TOKEN")
```
Note: It will take a few minutes to install

Run:
``` r
library(Rrepest)
```
Note: Ensure you have the package `data.table` installed. For a complete list of the dependencies used, consult the [Description](https://gitlab.algobank.oecd.org/edu_data/rrepest/-/blob/main/DESCRIPTION) file.

## Usage
The current version supports uni-variate statistics (e.g. mean, variance, standard deviation, quantiles), frequencies,
linear regression and covariance.

### Statistics
``` r
# PISA 2018 Data
# df.qqq <- readRDS("//oecdmain/asgenedu/EDUCATION_DATALAKE/sources/PISA/PISA 2018/R/STU/CY07_MSU_STU_QQQ.rds")

Rrepest::Rrepest(data = df.qqq,
        svy = "PISA2015",
        est = est(c("mean","var","quant",0.5,"iqr",c(.9,.1)),"CNTSCHID"),
        by = c("cnt"))

```
### Frequencies
``` r
# TALIS 2018 Data
# df.t <- readRDS(file = "V:/TALIS/BACKUP/DATA/TALIS2018/R/PUF/TTGINTT3_demo.rds")

Rrepest::Rrepest(data = df.t,
                 svy = "TALISTCH",
                 est = est("freq","TT3G06I2"),
                 over = c("TT3G06A2","TT3G52J"),
                 by = "cntry",
                 test = T,
                 isced = 2)
```

### Linear Regression
``` r
# TALIS 2018 Data
# df.t <- readRDS(file = "V:/TALIS/BACKUP/DATA/TALIS2018/R/PUF/TTGINTT3_demo.rds")

df.t <- df.t %>% 
  mutate(TT3G01_rec = case_when(TT3G01 == 2 ~ 1,
                                TT3G01 == 1 ~ 0))

Rrepest::Rrepest(data = df.t,
        svy = "TALISTCH",
        est = est("lm","TT3G01_rec",'TT3G39C'),
        by = "cntry")
```

### Correlation
``` r
# PISA 2018 Data
# df.qqq <- readRDS("//oecdmain/asgenedu/EDUCATION_DATALAKE/sources/PISA/PISA 2018/R/STU/CY07_MSU_STU_QQQ.rds")

Rrepest::Rrepest(data = df.qqq,
        svy = "PISA2015",
        est = est("corr",c("pv@math","pv@read")),
        by = c("CNT"))

```
Further examples can be found in the [Examples.R](https://gitlab.algobank.oecd.org/edu_data/rrepest/-/blob/main/Development/Examples.R) file

### General Analysis
To incorporate analyses that are not pre-programmed into Rrepest, you can utilize the **'gen'** option within the `est()` function of Rrepest. More about it is presented in the following [wiki](https://gitlab.algobank.oecd.org/edu_data/rrepest/-/wikis/General-Analysis).

# Authors

[Francesco Avvisati](mailto:francesco.avvisati@oecd.org), [Rodolfo Ilizaliturri](mailto:Rodolfo.ILIZALITURRI@oecd.org) and  [François Keslair](mailto:francois.keslair@oecd.org). 

Contact us if you want to join!


## Contributing

Do you have suggestions or comments? Please open an [issue](https://gitlab.algobank.oecd.org/edu_data/rrepest/-/issues)

 
## Project status
First public release (30 June 2023).
