

#' @title Get data from selected api type and schema provided available arguments
#' @description General function to get data from selected NFZ API type (agreements, ...) and schema (ex. agreements or providers), provided list of available arguments. Function to be run inside outer function for selected API User given arguments are retrieved from appropriate environment.
#' @param available_args Dictionary of function arguments and their corresponding equivalents in NFZ API.
#' @param api_type A type of NFZ api to be queried.
#' @param schema A name of schema of appropriate api.
#' @param url_args A vector of obligatory arguments to be passed without keys as a part of the url <https://api.nfz.gov.pl/api_type/schema/url_args/url_args>.
#' @return  Returns a data.table for requested query.
get_request <- function(available_args, api_type="app-umw-api", schema="agreements", url_args=NULL){

  schemas_dict <- list(`app-umw-api`=c('agreements', "months", "plans", "providers", "service-types", "contract-products"))
  if(!api_type %in% names(schemas_dict)){
    stop(paste0("Wrong API type, ", api_type, " doesn't exist."))
  } else if(!schema %in% schemas_dict[[api_type]]){
    stop(paste0(schema, " schema doesn't exist within ", api_type, " API."))
  }

  # Retrieve arguments and check their corectness
  given_args <- check_req_args(sys.parent(0)) # get_api_data,
  # browser()
  functions <- c("agr_get_agreements", "agr_get_agreement", "agr_get_plan", "agr_get_month_plan", "agr_get_providers", "agr_get_prov_by_year", "agr_get_provider",
                 "agr_get_serivces", "agr_get_products")
  if(!as.character(given_args$call_func) %in% functions){
    stop(paste("Wrong context call; get_request should be called within one of higher level functions designed for communication with an API."))
  }


  formal_names <- given_args$formal_args
  given_args <- given_args$given_args


  # Prepare query provided arguments
  formal_args = c(api_type, schema)
  if(!is.null(url_args)){
    n_formals = if(length(url_args) == length(given_args)) 0 else length(intersect(names(given_args), url_args))
    optional_args = if(length(url_args) == length(given_args)) list() else given_args[(n_formals+1):length(given_args)]

    formal_args = c(formal_args, unlist(setdiff(given_args, optional_args)))

  } else {
    optional_args = given_args
  }
  optionals_matcher <- if(rlang::is_empty(optional_args)) "" else "="

  api_agrs <- paste0(sapply(names(optional_args), function(x) available_args[[x]]),
                     optionals_matcher,
                     sapply(names(optional_args), function(x) optional_args[[x]]))
  api_agrs <- convert_pl_signs(api_agrs)

  api_query <- paste0("https://api.nfz.gov.pl/", paste(formal_args, collapse = "/"), "?",
                      paste0(api_agrs, collapse = "&"), "&limit=1&format=json&api-version=1.2")

  # Get scope request (to know how much data is available)
  request <- httr::GET(api_query, httr::timeout(20))

  status_code <- request$status_code
  if(status_code != 200){
    message(paste0('Request error, status code:', as.character(status_code), ". Check the arguments."))
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }

  # Extract data from the request
  request_data <- jsonlite::fromJSON(httr::content(request, "text"), flatten=TRUE)

  data <- data.table::data.table()
  if(!is.null(request_data$data)){

    items_found <- request_data$meta$count
    message(paste0(items_found, ' items of data meeting requested criteria found.'))

    # Get requests for all available data (max items per page 25 from API doc.)
    for(page in 1:ceiling(items_found/25)){
      api_query <- gsub("&limit=1", paste0("&page=",page,"&limit=25"), api_query)
      print(api_query)

      request <- httr::GET(api_query, httr::timeout(20))
      request_data <- jsonlite::fromJSON(httr::content(request, "text"), flatten=TRUE)
      data_attribute = sapply(request_data$data, class)
      data_attribute = match("data.frame", data_attribute)

      data <- rbind(data, request_data$data[[data_attribute]])
      page <- page + 1
      Sys.sleep(0.1)

    }
  } else {
    message('No availabe API data for queried scope. Please make sure the arguements are correct.')
  }
  # browser()
  return(data)
}



#' @title Check for function's arguments
#' @description To be run in local environment (inside another function). Checks whether all required arguments of function run in global env were provided during a function call. If a vector of possible arguments was given also checks whether all provided in a function call arguments are supported.
#' @param gen The stack number of a frame (environment) at which the check_req_args was called in.
#' @return  Returns a vector of arguments provided in a function call if all arguments are correct, otherwise function stops
#' @details `gen` describes a relative position or a stack number of check_req_args parent environment relative to a global environment (global has number 0). Can't be 0 or negative.
#' @examples
#' \dontrun{
#'  foo <- function(a, b, ...){
#'    given_args = check_req_args(foo, c("a", "b", "c"))
#'    return(given_args)}
#'
#'  foo("a", "b", "c", "d")
#' }
#'
check_req_args <- function(gen=NULL){

  if(!is.numeric(gen)){
    stop("A stack number of an active frame/enviornemnt must be integer.")
  } else if(gen!=round(gen)){ # gen%%1 != 0
    stop("A stack number of an active frame/enviornemnt must be integer.")
  }

  if(gen < 0){
    stop("A stack number of an active frame/enviornemnt can't be negative.")
  }

  gen = if(is.null(gen)) sys.parent(1) else gen # The stack number of an env in which check_req_args was called at
  # browser()
  func_call <- as.list(match.call(definition = sys.function(sys.parent(gen)),
                                  call = sys.call(sys.parent(gen)),
                                  envir = sys.frame(-gen))) # parent.frame(gen)

  given_args <- func_call[2:length(func_call)] # Arguments given in a func call
  # required_args <- setdiff(do.call(methods::formalArgs, list(func_call[[1]])), "...")
  # Get arguments that are required, and don't have default value
  possible_args <- formals(fun = sys.function(sys.parent(gen)), envir = parent.frame(gen))
  required_args <- names(unlist(possible_args))# While unlisting a pairList arguments with default NULL are dropped out
  possible_args <- names(possible_args)


  missing_args <- setdiff(required_args, names(given_args))
  if(!rlang::is_empty(missing_args)){
    msg <- paste0("You did not provide all reqired arguments. Missing: ", paste(missing_args, collapse = ", "))
    message(msg)
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }

  return(list(call_func = func_call[[1]], formal_args=required_args, available_args=possible_args, given_args=given_args))
}


#' @title Check for compatibility of LC_CTYPE environment variable
#' @description To be able to download data from NFZ api service LC_CTYPE env. var. has to be set to Polish language. This function checks for that, and suggest code to run, to be able to use NFZ api. If LC_CTYPE doesn't match, function stops the call.
check_env_lang <- function(){
  char_set <- Sys.getlocale("LC_CTYPE")
  if(char_set != "Polish_Poland.1250"){
    msg = paste0("Your LC_CTYPE environment variable has the value '", char_set,"', but to be sure that nfzRapi functions work correctly it has to be changed to 'Polish_Poland.1250'.\n",
    "1. Before using nfzRapi functions run following command: Sys.setlocale('LC_CTYPE', 'Polish_Poland.1250')\n",
    "2. After using nfzRapi, run following command to restore original setting: Sys.setlocale('LC_CTYPE', '", char_set,"')")
    message(msg)
    # message(paste0("Your character set does not support Polish non ASCII characters.",
    # "Your LC_CTYPE environment variable has value '", char_set,"', but to be sure that nfzRapi functions work correctly it has to be changed to 'Polish_Poland.1250'.\n
    # 1. Before using nfzRapi functions run following command: Sys.setlocale('LC_CTYPE', 'Polish_Poland.1250')\n
    # 2. After using nfzRapi, run following command to restore original setting: Sys.setlocale('LC_CTYPE', '", char_set,"')"))
    # opt <- options(show.error.messages = FALSE)
    # on.exit(options(opt))
    stop("The character set defined by LC_CTYPE environemt variable does not support Polish non ASCII characters.")
  }
}



#' @title Convert Polish non ASCII characters to NFZ API accpted utf-8 representation of Unicode
#' @description Function accpets a string and converts Polish diacretic non ASCII characters
#' @param char A character where the signs are to be converted
#' @return  Returns a character vector
#' @examples
#' \dontrun{
#'  convert_pl_signs("Gąśłękówiżań")
#' }
#'
convert_pl_signs <- function(char){


  if(!is.character(char)){
    warning("Non-character vector given.")
  }

  unicode_dict <- list(
    `%C4%84`="\u104",
    `%C4%86`="\u106",
    `%C4%98`="\u118",
    `%C5%81`="\u141",
    `%C5%83`="\u143",
    `%C5%93`="\uD3",
    `%C5%9A`="\u15A",
    `%C5%B9`="\u179",
    `%C5%BB`="\u17B",
    `%C5%85`="\u105",
    `%C5%87`="\u107",
    `%C5%99`="\u119",
    `%C5%82`="\u142",
    `%C5%84`="\u144",
    `%C3%B3`="\uF3",
    `%C5%9B`="\u15B",
    `%C5%BA`="\u17A",
    `%C5%BC`="\u17C"
  )
  char2 <- c()
  for(character in char){
    for(letter in unicode_dict){
      change <- names(unicode_dict)[match(letter, unicode_dict)]
      character <- gsub(letter, change[!is.na(change)], character)
    }
    char2 <- c(char2, character)
  }
  return(char2)
}

