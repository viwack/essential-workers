# NaNDA Essential Workers Project

Analysis of neighborhood built-environment characteristics across US census tracts stratified by essential worker concentration, with a focus on conditions present during the COVID-19 pandemic (2020).

## Research Overview

The central exposure is **EWRKR_PROP** — the proportion of residents in a census tract employed as essential workers, derived from 2016–2020 American Community Survey estimates. Tracts are ranked into quintiles (Q1 = lowest, Q5 = highest essential worker concentration) and compared on a range of neighborhood resource and environmental burden outcomes.

Analyses are additionally stratified by urbanicity using 4-category RUCA codes (metropolitan vs. non-metropolitan).

## Outcome Variables

| Variable | Description |
|---|---|
| `den_totalfoodstores` | Density of total food stores |
| `den_allphysicians` | Density of physicians |
| `den_mentalhealthphys` | Density of mental health physicians |
| `den_mentalhealthpractitioners` | Density of mental health practitioners |
| `den_pharmacies` | Density of pharmacies |
| `den_allrescarefacilities` | Density of residential care facilities |
| `den_conveniencestores` | Density of convenience stores |
| `den_dollarstores` | Density of dollar stores |
| `COUNT_TRI_FACILITIES` | Count of Toxic Release Inventory (TRI) pollution sites |
| `PROP_PARK_AREA_TRACT` | Proportion of tract area covered by parks |
| `MEDFAMINC16_20` | Median family income (2016–2020) |

## Data Sources

All neighborhood data is linked at the **2020 census tract** level (`TRACT_FIPS20`).

| Dataset | Source | Notes |
|---|---|---|
| Essential Workers + Socio-demographics | ICPSR 38528 (NaNDA) | 2016–2020 ACS estimates |
| Pollution / TRI Facilities | ICPSR 38597 | Filtered to 2020 |
| Grocery Stores | NaNDA (`nanda_grocery_Tract20_1990-2021_01P.csv`) | Filtered to 2020 |
| Healthcare Services | `healthcare_services.csv` | Ambulatory / walk-in care; filtered to 2020 |
| Parks | ICPSR 38586 | `38586-0003-Data.tsv` |
| Convenience Stores | `convenience_stores.csv` | Filtered to 2020 |
| Dollar Stores | `dollar_stores.csv` | Filtered to 2020 |
| RUCA Codes (urbanicity) | `RUCA-codes-2020-tract.xlsx` | Collapsed to 4-category RUCA4 |
| Broadband Access | ICPSR 38567 | Loaded but not yet merged |

The merged analytic file is written to `everything.csv`.

## Scripts

| Script | Purpose |
|---|---|
| `ew_vs_non-ew analysis.R` | Loads all raw data sources, merges to tract level, and produces `everything.csv` and `df_structure.csv` |
| `descriptive_each_var.R` | Descriptive statistics and visualizations by essential worker quintile; statistical tests |

## Statistical Methods

- **Descriptive statistics**: means, SDs, and 95% CIs by quintile
- **Trend tests**: Jonckheere-Terpstra and Mann-Kendall tests for monotonic trend across quintiles
- **Group comparisons**: Welch two-sample t-tests (Q1 vs. Q5)
- **Correlation**: Spearman rank correlation with RUCA urbanicity codes
- **Stratified analysis**: Metropolitan (RUCA4 = 1) vs. non-metropolitan tracts analyzed separately

## Requirements

```r
install.packages(c("dplyr", "readr", "readxl", "ggplot2", "purrr", "clinfun", "trend"))
```

## Notes

- Large raw data files (`.csv`, `.dta`, `.tsv`, `.shp`) are tracked via Git LFS and are not included directly in this repository.
- The unit of analysis is the 2020 US census tract.
- Essential worker proportions reflect 2016–2020 ACS averages; neighborhood environment variables are anchored to 2020 to reflect pandemic-era conditions.
