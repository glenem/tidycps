###########################
# Author: Glen Martin
# Functions to access data from the basic monthly current population survey (CPS)
#
#
############################


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

get_fip_abb_cw <- function(year_val=year(today()),
                       month_val = month(today())
                       ){

  if (missing(month_val)){
    l <- schedule_month_year(year_val,
                             month_val)
    month_val <- l$month
    year_val <- l$year
  }else if (is.numeric(month_val)){
    month_val <- str_to_lower(month.abb[month_val])
  }else if (is.character(month_val)){
    month_val <- str_to_lower(month_val)
  }


  lol <- request(
    glue("https://api.census.gov/data/{year_val}/cps/basic/{month_val}/variables/STATE.json")
  ) %>%
    req_headers("Accept" = "application/json") %>%
    req_perform() %>%
    resp_body_json()

  lol <- stack(lol$values$item)
  return(lol)
}

fip_abb_crosswalk<- function(abb){
  # fips_abb, internal data loaded
  abb_num <- as.character(fips_abb[match(str_to_upper(abb), fips_abb$values), "ind"])
  return(abb_num)
}


# Function to get a data frame of the variables of the basic monthly cps -----
# That is, a two field df with the name of the variable and the label.
get_basic_cps_vars <- function(year_val=year(today()),
                         month_val = month(today())
                         ){
  if (missing(month_val)){
  l <- schedule_month_year(year_val,
                           month_val)
  month_val <- l$month
  year_val <- l$year
  }else if (is.numeric(month_val)){
    month_val <- str_to_lower(month.abb[month_val])
  }else if (is.character(month_val)){
    month_val <- str_to_lower(month_val)
  }


lol <- request(
  glue("https://api.census.gov/data/{year_val}/cps/basic/{month_val}/variables")
               ) %>%
  req_headers("Accept" = "application/json") %>%
  req_perform() %>%
  resp_body_json()

df <- as_tibble(
  do.call(rbind, lol[-1]),
  .name_repair = "minimal"
)

names(df) <- lol[[1]] %>%
  unlist(use.names =  FALSE) %>%
  as.character()

df <- df %>% select(-concept)

df <- df %>%
  mutate(
    across(
      everything(),
                ~unlist(
                .x,
                use.names = FALSE
                )
    )
  )
}

# Function to get data from the basic monthly cps in a df -----

get_basic_cps <- function(year_val=year(today()),
                          month_val = month(today()),
                          variables = NULL,
                          wt = "PWCMPWGT",
                          veterans = FALSE,
                          household = FALSE,
                          state_val = NULL,
                          key_val = Sys.getenv("USCENSUS_KEY")
){

  # Month Year control flow
  # decide what happens to the month and year variables depending on if the default arguments are used
  if (missing(month_val)){
    l <- schedule_month_year(year_val,
                             month_val)
    month_val <- l$month
    year_val <- l$year
  }else if(is.numeric(month_val)){
    month_val <- str_to_lower(month.abb[month_val])
  }else if(is.character(month_val)){
    month_val <- str_to_lower(month_val)
  }

  # State Control Flow
  # if the default argument is used get all state data
  if(missing(state_val)){
    state_val <- "*"
  }else if(is.numeric(state_val)){
    if (str_length(as.character(state_val) == 1)){
      state_val <- paste0("0", state_val)
    }
  }else if (is.character(state_val)){
    # enter in the code if the user enters the state abbreviation, i.e., FL
    state_val <- fip_abb_crosswalk(state_val)
  }


  # wt control flow
  if(veterans==TRUE){
    wt <- "PWVETWGT"
  }else if(household==TRUE){
    wt <- "HWHHWGT"
  }


  vars_val <- paste0(c(wt, variables), collapse=",")

  lol <- request(
    glue("https://api.census.gov/data/{year_val}/cps/basic/{month_val}?get={vars_val}&for=state:{state_val}&key={key_val}")
  )  %>%
    req_headers("Accept" = "application/json")  %>%
    req_perform() %>%
  resp_body_json()

  df <- as_tibble(
    do.call(rbind, lol[-1]),
    .name_repair = "minimal"
  )

  names(df) <- lol[[1]] %>%
    unlist(use.names =  FALSE) %>%
    as.character()

  # transforms the df of lists to the usual df of vectors
  df <- df %>%
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
  df <- df %>% mutate_all(as.numeric)

  df <- df %>% mutate(
    DATE = as.Date(
      paste0(year_val, '-',
      ifelse(match(str_to_title(month_val), month.abb)<10,
      paste0('0', match(str_to_title(month_val), month.abb)),
      match(str_to_title(month_val), month.abb)),
      '-01'),
      )
  )
}

# Function to help make CPS variables more useful ----
sex_indicators <- function(df){
  df <- df %>%
    mutate(
      male = ifelse(PESEX==1, 1, 0),
      female = ifelse(PESEX==2, 1, 0)
    )
}

labforce_emp_status <- function(df){
  df <- df %>%
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
