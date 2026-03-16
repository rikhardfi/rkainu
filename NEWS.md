# rkainu 0.2.0
* Added DLCO reference equations (Kainu et al. 2017)
* New functions: dlco_pred(), dlco_zscore(), dlco_lln()
* Parameters: DLCOc, DLCOc/VA (KCO), VA
* Added CITATION file with both Kainu 2015 and 2017 references
* Added R-CMD-check GitHub Actions workflow
* Added edge case tests for input validation
* Web calculator: added DLCO tab with accessibility improvements

# rkainu 0.1.0
* Initial release with Finnish spirometry reference values (Kainu et al. 2015)
* Functions: kainu_pred(), kainu_zscore(), kainu_lln(), kainu_pctpred(), kainu_df()
* 10 parameters: FVC, FEV1, FEV1/FVC, FEV6, FEV1/FEV6, PEF, MMEF, MEF75, MEF50, MEF25
* Validated against published reference values
* Web calculator: https://rikhardfi.github.io/spirometry-calculator/
