###########################
# Author: Glen Martin
# Downloads the FIPS to Abbreviation crosswalk
# and save it as a file for internal use
#
###########################

source("R/cpsR.R")

fips_abb <- get_fip_abb_cw()

saveRDS(fips_abb, 'Data/fips_abb_crosswalk.rds')
