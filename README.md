## tidycps

__tidycps__ is an R package that allows users to interface with the US Census' Current Population Survey (CPS) basic monthly microdata, and return tidyverse-ready data frames. 

This package is still in the early stages of development, but can be used to retive basic monthly CPS microdata. When I am happy with the development stage I will post version 1 on CRAN. For now, install from GitHub for the latest updates. 

```r
remotes::install_github("glenem/tidycps")
```

## Example

```r
# Pull December 2025 basic monthly CPS data for Florida
df <- get_basic_cps(year = 2025, month = 'dec',
weight = "person",
variables = c(
"PRPERTYP", # person type
"PRTAGE", # age
"PESEX", # sex
"PTDTRACE", # race
  "PEHSPNON", # Hispanic
 "PEEDUCA", # educational attainment
  "PEMLR" # labor force status
),
state = 'fl'
)

# Summarize the employment, unemployment and unemployment rate with the helper functions
fl_labforce_emp <- df |>
  labforce_emp_status() |>
  summarize_emp_unr(by=c('state', 'DATE'))
```

Note: This product uses the Census Bureau Data API but is not endorsed or certified by the Census Bureau.
