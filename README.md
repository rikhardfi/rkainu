# rkainu

Finnish spirometry (Kainu et al. 2015) and DLCO (Kainu et al. 2017) reference values as an R package.

Spirometry covers 10 parameters for Finnish adults aged 18–90. DLCO covers DLCOc, DLCOc/VA (KCO), and VA. The spirometry API mirrors [rspiro](https://cran.r-project.org/package=rspiro) for easy side-by-side comparison with GLI-2012.

**Web calculator:** [rikhardfi.github.io/spirometry-calculator](https://rikhardfi.github.io/spirometry-calculator/)

## Parameters

| Parameter | Description | Unit |
|-----------|-------------|------|
| FEV1 | Forced expiratory volume in 1 second | L |
| FVC | Forced vital capacity | L |
| FEV1/FVC | FEV1 to FVC ratio | ratio |
| FEV6 | Forced expiratory volume in 6 seconds | L |
| FEV1/FEV6 | FEV1 to FEV6 ratio | ratio |
| PEF | Peak expiratory flow | L/s |
| MMEF | Maximal mid-expiratory flow | L/s |
| MEF75 | MEF at 75% of FVC | L/s |
| MEF50 | MEF at 50% of FVC | L/s |
| MEF25 | MEF at 25% of FVC | L/s |

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

# Multiple parameters at once
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

## Notes

- **Sex encoding:** `1` = male, `2` = female (same as rspiro)
- **Height:** In cm. Values below 3 are assumed to be in metres and automatically converted.
- **Age range:** 18–90. Spline data covers 18–84; values outside are clamped with a warning.
- **No dependencies:** Base R only. Suggests testthat, knitr, rmarkdown for development.

## References

Kainu A, Timonen KL, Toikka J, et al. Reference values of spirometry for Finnish adults. *Clin Physiol Funct Imaging*. 2016;36(5):346-358. doi: [10.1111/cpf.12237](https://doi.org/10.1111/cpf.12237)

Kainu A, Timonen KL, Toikka J, et al. Reference values of diffusing capacity for Finnish adults. *Clin Physiol Funct Imaging*. 2018;38(3):413-420. doi: [10.1111/cpf.12432](https://doi.org/10.1111/cpf.12432)

## License

GPL (>= 3).

## See also

- [Web calculator](https://rikhardfi.github.io/spirometry-calculator/) — browser-based tool using the same equations
- [rspiro](https://cran.r-project.org/package=rspiro) — GLI-2012 reference values for R
