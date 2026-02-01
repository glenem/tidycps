library('devtools')

getwd()

# to work on the package
use_r('cpsR')

source("R/cpsR.R") # works

rename_files("R/cpsR.R", "cpsR.R")

list.files("R")
devtools::load_all() # does not work


document()

devtools::check()
