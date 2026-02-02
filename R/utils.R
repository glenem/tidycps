#' @import lubridate
#' @import stringr
#' @import httr2
#' @import glue
#' @import jsonlite
#' @importFrom utils stack



# Function to be used for the defaults of get_basic_cps_vars and get_basic_cps -----
# to transform them into a format usable for the API call
schedule_month_year <- function(year, month){
  if (month == 1){
    month <- str_to_lower(month.abb[12])
    year <- year - 1
  }else{
    month <- str_to_lower(month.abb[month-1])
  }
  list(month=month, year=year)
}

# Function to get the fips number abbreviation cross-walk ----

get_fip_abb_cw <- function(year=2000:2025,
                           month = c("jan","feb","mar","apr","may","jun",
                                     "jul","aug","sep","oct","nov","dec")
){

  if (is.numeric(month)){
    month <- str_to_lower(month.abb[month])
  }else if (is.character(month)){
    month <- str_to_lower(month)
  }


  lol <- request(
    glue("https://api.census.gov/data/{year}/cps/basic/{month}/variables/STATE.json")
  ) |>
    req_headers("Accept" = "application/json") |>
    req_perform() |>
    resp_body_json()

  lol <- stack(lol$values$item)
  return(lol)
}

fip_abb_crosswalk<- function(abb){
  # fips_abb, internal data loaded
  abb_num <- as.character(fips_abb[match(str_to_upper(abb), fips_abb$values), "ind"])
  return(abb_num)
}


# Note: need to rework this function to make it more useful!!
# Function to get a data frame of the variables of the basic monthly cps -----
# That is, a two field df with the name of the variable and the label.
# get_basic_cps_vars <- function(year_val=year(today()),
#                          month_val = month(today())
#                          ){
#   if (missing(month_val)){
#   l <- schedule_month_year(year_val,
#                            month_val)
#   month_val <- l$month
#   year_val <- l$year
#   }else if (is.numeric(month_val)){
#     month_val <- str_to_lower(month.abb[month_val])
#   }else if (is.character(month_val)){
#     month_val <- str_to_lower(month_val)
#   }
#
#
# lol <- request(
#   glue("https://api.census.gov/data/{year_val}/cps/basic/{month_val}/variables")
#                ) |>
#   req_headers("Accept" = "application/json") |>
#   req_perform() |>
#   resp_body_json()
#
# df <- as_tibble(
#   do.call(rbind, lol[-1]),
#   .name_repair = "minimal"
# )
#
# names(df) <- lol[[1]] |>
#   unlist(use.names =  FALSE) |>
#   as.character()
#
# df <- df |>
#   select(-concept)
#
# df <- df |>
#   mutate(
#     across(
#       everything(),
#                 ~unlist(
#                 .x,
#                 use.names = FALSE
#                 )
#     )
#   )
# }
