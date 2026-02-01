library(pacman)
pacman::p_load('devtools', 'available')

getwd()

# Check availability of package name
available('tidycps')

# to work on the package
use_r('cpsR')

# packages, just need to run once. Already ran: look at the description
use_package("dplyr")
use_package("lubridate")
use_package("stringr")
use_package("magrittr")
use_package("httr2")
use_package("glue")
use_package("jsonlite")

# connect to github
usethis::use_github()

# Check directory
getwd()

# load the package
load_all()

# Run documentation
document()

# Check for issues
check()
