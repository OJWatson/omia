# Provenance: cat_ba_week1

## Dataset
- Object name: `cat_ba_week1`
- File: `data/cat_ba_week1.rda`
- Build script: `data-raw/build_cat_ba_week1.R`

## Source document
Daily counts used for this dataset were provided in the project specification:

`1,0,1,4,1,2,0,0,3,2,4,1,5,1,3,4,4,5,9,3,8,6,12`

## Processing
- Day index created as integers 1 through 23.
- Date sequence set to consecutive days starting `2024-01-01`.
- Incidence recorded exactly as provided.

## Reproducibility
To rebuild:

```r
source("data-raw/build_cat_ba_week1.R")
```
