## code to prepare `fips_abb` dataset goes here

###########################
# Author: Glen Martin
# Downloads the FIPS to Abbreviation crosswalk
# and save it as a file for internal use
#
###########################

library(pacman)
pacman::p_load('devtools',
               'tidyverse',
               'httr2',
               "glue",
               "jsonlite"
)

load_all()

fips_abb <- get_fip_abb_cw(year_val = 2025, month_val = 'dec')

usethis::use_data(fips_abb, overwrite = TRUE, internal = TRUE)
