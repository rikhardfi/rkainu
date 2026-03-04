# rkainu

Finnish spirometry reference values from Kainu et al. (2015) as an R package.

Covers 10 parameters (FVC, FEV1, FEV1/FVC, FEV6, FEV1/FEV6, PEF, MMEF, MEF75, MEF50, MEF25) for Finnish adults aged 18–90. The API mirrors [rspiro](https://cran.r-project.org/package=rspiro) for easy side-by-side comparison with GLI-2012.

## Installation

```r
# install.packages("devtools")
devtools::install_github("rikhardfi/rkainu")
```

## Quick start

```r
library(rkainu)

# Predicted FEV1 for a 50-year-old male, 177 cm
kainu_pred(50, 177, 1, "FEV1")
#> [1] 4.046

# Z-score for observed FEV1 = 3.5 L
kainu_zscore(50, 177, 1, FEV1 = 3.5)
#> [1] -1.02

# Lower limit of normal
kainu_lln(50, 177, 1, "FEV1")
#> [1] 3.168

# Percent predicted
kainu_pctpred(50, 177, 1, FEV1 = 3.5)
#> [1] 86.5

# Multiple parameters
kainu_pred(50, 177, 1, c("FEV1", "FVC", "FEV1FVC"))
```

## Data.frame workflow

```r
patients <- data.frame(
  age = c(35, 55, 72),
  height = c(182, 170, 168),
  sex = c(1, 2, 1),
  FEV1 = c(4.2, 2.5, 2.1),
  FVC = c(5.3, 3.2, 3.0)
)

kainu_df(patients, params = c("FEV1", "FVC"))
# Adds columns: pred_FEV1, lln_FEV1, z_FEV1, pctpred_FEV1, pred_FVC, ...
```

## Sex encoding

- `1` = male, `2` = female (same as rspiro)

## Height

Height in cm. Values below 3 are assumed to be in metres and automatically converted.

## Reference

Kainu A, Timonen KL, Toikka J, et al. Reference values of spirometry for Finnish adults. *Clin Physiol Funct Imaging*. 2016;36(5):346-358.
