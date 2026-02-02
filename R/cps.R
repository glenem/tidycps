#' @import dplyr
#' @import lubridate
#' @import stringr
#' @import httr2
#' @import glue
#' @import jsonlite




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

# Function to help make CPS variables more useful ----
sex_indicators <- function(df){
  df <- df |>
    mutate(
      male = ifelse(PESEX==1, 1, 0),
      female = ifelse(PESEX==2, 1, 0)
    )
}

labforce_emp_status <- function(df){
  df <- df |>
    mutate(
      civ_noninst_pop = ifelse(PRPERTYP == 2 & PRTAGE>=16, 1, 0),
      unemp = ifelse(3 <= PEMLR & PEMLR <= 4 , 1, 0),
      emp = ifelse(1 <= PEMLR & PEMLR <= 2 , 1, 0),
      labforce = ifelse(1 <= PEMLR & PEMLR <= 4 , 1, 0),
      not_in_labforce = ifelse(5 <= PEMLR & PEMLR <= 7 , 1, 0),
      not_in_universe = ifelse(PEMLR==-1, 1, 0)
    )
  return(df)
}

age_indicators <- function(df){
  df <- df |>
    mutate(
      age_16to24 = ifelse(PRTAGE>=16 & PRTAGE<=24, 1, 0),
      age_25to34 = ifelse(PRTAGE>=25 & PRTAGE<=34, 1, 0),
      age_35to44 = ifelse(PRTAGE>=35 & PRTAGE<=44, 1, 0),
      age_45to54 = ifelse(PRTAGE>=45 & PRTAGE<=54, 1, 0),
      age_55to64 = ifelse(PRTAGE>=55 & PRTAGE<=64, 1, 0),
      age_65over = ifelse(PRTAGE>=65, 1, 0),
      prime_age_25to54 = ifelse(PRTAGE>=25 & PRTAGE<=54, 1, 0),
    )
}
