# Test for CPS function
rm(list = ls())

library(pacman)
pacman::p_load('devtools',
               'tidyverse',
               'httr2',
               "glue",
               "jsonlite"
)

load_all()

vars <- get_basic_cps_vars()

df <- get_basic_cps(year_val = 2025, month_val = 'dec',
  variables = c(
  "PRPERTYP", # person type
  "PRTAGE", # age
  "PESEX", # sex
  "PTDTRACE", # race
  "PEHSPNON", # Hispanic
  "PEEDUCA", # educational attainment
  "PEMLR", # labor force status
  "COUNTY"
),
state_val = 'fl'
)

# get an annual time series for 2025
# Note OCT is not available
df <- map_dfr(c(c(1:9), c(11:12)), ~{
  get_basic_cps(year_val = 2025,
                   month_val=.x,
                   variables = c(
    "PRPERTYP", # person type
    "PRTAGE", # age
    "PESEX", # sex
    "PTDTRACE", # race
    "PEHSPNON", # Hispanic
    "PEEDUCA", # educational attainment
    "PEMLR", # labor force status
    "COUNTY"
  ),
  state_val = 'fl'
  )
}, .id = "month_val")



df <- df %>% labforce_emp_status()


df <- df %>% sex_indicators()
