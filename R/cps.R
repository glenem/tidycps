#'  Get basic monthly CPS micro data
#' @import dplyr
#' @import lubridate
#' @import stringr
#' @import httr2
#' @import glue
#' @import jsonlite
#' @param year Numeric, the year you want to pull the data for
#' @param month Character string, the month you want to pull the data for
#' @param variables Character string or vector of characters strings of variables IDs. tidycps uses these IDs to pull the micro data.
#' @param weight Character string to indicate the type of weight to use in your analysis: person, household or veteran.
#' @param state Character string for the state abbreviation or numeric for the state FIPS code. If nothing is entered it pulls the micro data for every state in the United States.
#' @param api_key US Census API Key. Recommend to put your census key in .Renviorn for easy access.
#' @return A tibble or tidyverse-ready data frame of CPS micro data.
#' @examples
#' # Load libraries
#' library('tidycps')
#' library('dplyr')
#'
#' # Pull December 2025 basic monthly CPS data for Florida
#' df <- get_basic_cps(year = 2025, month = 'dec',
#' weight = "person",
#' variables = c(
#' "PRPERTYP", # person type
#' "PRTAGE", # age
#' "PESEX", # sex
#' "PTDTRACE", # race
#'   "PEHSPNON", # Hispanic
#'  "PEEDUCA", # educational attainment
#'   "PEMLR", # labor force status
#'   "COUNTY"
#' ),
#' state = 'fl'
#' )
#'
#' # Calculate the Florida's December 2025 unemployment rate
#' fl_labforce_emp <- df |>
#'   labforce_emp_status() |>
#'   group_by(state, DATE) |>
#'   summarize(
#'     unemployment = sum(unemp*(PWCMPWGT)),
#'     labor_force = sum(labforce*(PWCMPWGT)),
#'     unemployment_rate = (unemployment/labor_force)*100
#'   )
#'@export


# Function to get data from the basic monthly cps in a df -----
get_basic_cps <- function(year=2000:2025,
                          month = c("jan","feb","mar","apr","may","jun",
                                    "jul","aug","sep","oct","nov","dec"),
                          variables = NULL,
                          weight = c("person", "household", "veteran"),
                          state = NULL,
                          api_key = Sys.getenv("USCENSUS_KEY")
){

  # Month Year control flow
  # decide what happens to the month and year variables depending on if the default arguments are used
  if(is.numeric(month)){
    month <- str_to_lower(month.abb[month])
  }else if(is.character(month)){
    month <- str_to_lower(month)
  }

  # Error message for October 2025 CPS
  if (year == 2025 && month == "oct") {

    msg_cps <- c(crayon::red(stringr::str_wrap("Due to the Federal Government shutdown in 2025, there are not estimates available for the October 2025 CPS.")),
                 crayon::cyan(stringr::str_wrap("If you are trying to pull monthly data for the entire year of 2025, please make sure to omit October in your API call.")),
                 crayon::cyan(stringr::str_wrap("Thank you!"))
                 )
    rlang::abort(msg_cps)

  }

  message(sprintf("Getting data from the %s baisc monthly CPS", paste0(str_to_title(month), "-", year)))

  # State Control Flow
  # if the default argument is used get all state data
  if(missing(state)){
    state <- "*"
  }else if(is.numeric(state)){
    if (str_length(as.character(state) == 1)){
      state <- paste0("0", state)
    }
  }else if (is.character(state)){
    # enter in the code if the user enters the state abbreviation, i.e., FL
    state <- fip_abb_crosswalk(state)
  }

  # weight control flow
  weight <- match.arg(weight)
  wt <- switch(
    weight,
    person    = "PWCMPWGT",
    household = "HWHHWGT",
    veteran   = "PWVETWGT"
  )

  vars_val <- paste0(c(wt, variables), collapse=",")

  lol <- request(
    glue("https://api.census.gov/data/{year}/cps/basic/{month}?get={vars_val}&for=state:{state}&key={api_key}")
  )  |>
    req_headers("Accept" = "application/json") |>
    req_perform() |>
  resp_body_json()
  Sys.sleep(3)


  df <- as_tibble(
    do.call(rbind, lol[-1]),
    .name_repair = "minimal"
  )

  names(df) <- lol[[1]] |>
    unlist(use.names =  FALSE) |>
    as.character()

  # transforms the df of lists to the usual df of vectors
  df <- df |>
    mutate(
      across(
        everything(),
        ~unlist(
          .x,
          use.names = FALSE
        )
      )

    )

  # ensure all data is transformed to numeric
  df <- df |>
    mutate_all(as.numeric)

  df <- df |> mutate(
    DATE = as.Date(
      paste0(year, '-',
      ifelse(match(str_to_title(month), month.abb)<10,
      paste0('0', match(str_to_title(month), month.abb)),
      match(str_to_title(month), month.abb)),
      '-01'),
      )
  )
}

#' Get a Time Series of Basic Monthly CPS micro data
#' @param year Numeric, the year you want to pull the data for
#' @param month Character string, the month you want to pull the data for
#' @param variables Character string or vector of characters strings of variables IDs. tidycps uses these IDs to pull the micro data.
#' @param weight Character string to indicate the type of weight to use in your analysis: person, household or veteran.
#' @param state Character string for the state abbreviation or numeric for the state FIPS code. If nothing is entered it pulls the micro data for every state in the United States.
#' @param api_key US Census API Key. Recommend to put your census key in .Renviorn for easy access.
#' @return A tibble or tidyverse-ready data frame of CPS micro data.
#' @export
get_cps_micro_ts <- function(year=2000:2025,
                          month =1:12,
                          variables = NULL,
                          weight = c("person", "household", "veteran"),
                          state = NULL,
                          api_key = Sys.getenv("USCENSUS_KEY")
){

  grid <- tidyr::expand_grid(
    year_val  = year,
    month_val = month
  )


  purrr::pmap_dfr(
    grid,
    function(year_val, month_val) {
      get_basic_cps(
        year      = year_val,
        month     = month_val,
        variables = variables,
        weight    = weight,
        state     = state,
        api_key   = api_key
      )
    }
  )
}


# Function to help make CPS variables more useful ----
sex_indicators <- function(df){
  df <- df |>
    mutate(
      male = ifelse(.data$PESEX==1, 1, 0),
      female = ifelse(.data$PESEX==2, 1, 0)
    )
}

#' Create laborforce and employment status indicators
#' @param df A tibble or data frame that contains the variables PRPERTYP, PRTAGE, and PEMLR
#' @return A tibble or data frame with the new variables added
#' @export
labforce_emp_status <- function(df){
  df <- df |>
    mutate(
      civ_noninst_pop = ifelse(.data$PRPERTYP == 2 & .data$PRTAGE>=16, 1, 0),
      unemp = ifelse(3 <= .data$PEMLR & .data$PEMLR <= 4 , 1, 0),
      emp = ifelse(1 <= .data$PEMLR & .data$PEMLR <= 2 , 1, 0),
      labforce = ifelse(1 <= .data$PEMLR & .data$PEMLR <= 4 , 1, 0),
      not_in_labforce = ifelse(5 <= .data$PEMLR & .data$PEMLR <= 7 , 1, 0),
      not_in_universe = ifelse(.data$PEMLR==-1, 1, 0)
    )
  return(df)
}

age_indicators <- function(df){
  df <- df |>
    mutate(
      age_16to24 = ifelse(.data$PRTAGE>=16 & .data$PRTAGE<=24, 1, 0),
      age_25to34 = ifelse(.data$PRTAGE>=25 & .data$PRTAGE<=34, 1, 0),
      age_35to44 = ifelse(.data$PRTAGE>=35 & .data$PRTAGE<=44, 1, 0),
      age_45to54 = ifelse(.data$PRTAGE>=45 & .data$PRTAGE<=54, 1, 0),
      age_55to64 = ifelse(.data$PRTAGE>=55 & .data$PRTAGE<=64, 1, 0),
      age_65over = ifelse(.data$PRTAGE>=65, 1, 0),
      prime_age_25to54 = ifelse(.data$PRTAGE>=25 & .data$PRTAGE<=54, 1, 0),
    )
}
