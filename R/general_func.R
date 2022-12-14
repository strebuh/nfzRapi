

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
  given_args <- check_req_args(sys.parent(1))
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
  api_agrs <- paste0(sapply(names(optional_args), function(x) eval(available_args[[x]])),
                     optionals_matcher,
                     sapply(names(optional_args), function(x) eval(optional_args[[x]])))
  api_agrs <- convert_pl_signs(api_agrs)

  api_query <- paste0("https://api.nfz.gov.pl/", paste(formal_args, collapse = "/"), "?",
                      paste0(api_agrs, collapse = "&"), "&limit=1&format=json&api-version=1.2")
  print(api_query)
  # Get scope request (to know how much data is available)
  request <- httr::GET(api_query, httr::timeout(20))
  request_data <- jsonlite::fromJSON(httr::content(request, "text"), flatten=TRUE)

  status_code <- request$status_code
  if(status_code != 200){
    msg=paste0("No availabe data for queried scope. Please make sure the arguements are correct.\n",
               "Request error, status code:", as.character(status_code))
    message(msg)
    return(NULL)
  }

  # Extract data from the request
  data <- data.table::data.table()
  if(!is.null(request_data$data)){

    items_found <- request_data$meta$count
    message(paste0(items_found, ' items of data meeting requested criteria found.'))
    api_query <- gsub("&limit=1", paste0("&page=", 1,"&limit=25"), api_query)
    # Get requests for all available data (max items per page 25 from API doc.)
    for(page in 1:ceiling(items_found/25)){

      api_query <- gsub("&page=\\d+&limit=25", paste0("&page=", page,"&limit=25"), api_query)
      request <- httr::GET(api_query, httr::timeout(20))
      request_data <- jsonlite::fromJSON(httr::content(request, "text"), flatten=TRUE)
      data_attribute = sapply(request_data$data, class)
      data_attribute = match("data.frame", data_attribute)
      data <- rbind(data, request_data$data[[data_attribute]])
      page <- page + 1
      Sys.sleep(0.1)

    }
  } else {
    msg=paste0("No availabe data for queried scope. Please make sure the arguements are correct.\n",
                 "If any of the provided arguments contains Polish characters make sure the local encoding is one of 1250 variations. Run Sys.getlocale('LC_CTYPE') command; if it doesn't show any of 1250's ",
                 "You may call Sys.setlocale('LC_CTYPE', 'Polish_Poland.1250') command.")
    message(msg)
    # browser()
    return(NULL)
  }
  # browser()
  return(data)
}



#' @title Check for function's arguments
#' @description To be run in local environment (inside another function). Checks whether all required arguments of function run in global env were provided during a function call. If a vector of possible arguments was given also checks whether all provided in a function call arguments are supported.
#' @param gen The stack number of a frame (environment) which arguments are to be verified
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
check_req_args <- function(gen){


  if(!is.numeric(gen)){
    stop("A stack number of an active frame/enviornemnt must be integer.")
  } else if(gen!=round(gen)){
    stop("A stack number of an active frame/enviornemnt must be integer.")
  }
  if(gen < 0){
    stop("A stack number of an active frame/enviornemnt can't be negative.")
  }

  func_call <- as.list(match.call(definition = sys.function(gen), # stack number of api function env (usually 1, but if nested into antohter funtion not 1)
                                  call = sys.call(gen), # sys.call(sys.parent(gen)) # stack number of api function env (usually 1, but if nested into antohter funtion not 1)
                                  envir = sys.frame(gen))) # parent.frame(gen) -gen

  given_args <- func_call[2:length(func_call)]
  # !!!This won't pass test when test's run in a loop in devtools, because iterated element't is hidden in some env  and can't be evaluated
  gen.2 = gen
  while("name" %in% sapply(given_args, class)){
    gen.2 <-  gen.2-1
    given_args <- lapply(given_args, function(e) ifelse(is.name(e), eval(e, envir=sys.frame(gen.2)), e))
  }
  # Get arguments that are required, and don't have default value
  possible_args <- formals(fun = sys.function(gen), envir = sys.frame(gen))
  required_args <- names(unlist(possible_args))# While unlisting a pairList arguments with default NULL are dropped out
  possible_args <- names(possible_args)

  missing_args <- setdiff(required_args, names(given_args))
  if(!rlang::is_empty(missing_args)){
    msg <- paste0("You did not provide all reqired arguments. Missing: ", paste(missing_args, collapse = ", "))
    stop(msg)
  }

  return(list(call_func = func_call[[1]], formal_args=required_args, available_args=possible_args, given_args=given_args))
}


agr_check_arg_types <- function(year=NULL, admin_branch=NULL, service_type=NULL, service_code=NULL, service_name=NULL, product_code=NULL, prod_code=NULL,
                                provider_code=NULL, provider_name=NULL, product_name=NULL, nip=NULL, regon=NULL, post_code=NULL, street=NULL, town=NULL,
                                teryt=NULL, id_agreement=NULL, id_plan=NULL){


  err = "Wrong format or type of argument(s):"
  msg = ""
  if(!is.null(year)){
    if(!nchar(year) == 4){
      err = paste(err, "year - 4 digits expected.", sep="\n")
    } else if(!year %in% agr_get_years()){
      err = paste(err, "Year out of range. To check available years call agr_get_years function.", sep="\n")
    }
  }

  if(!is.null(admin_branch)){
    if(grepl("\\D", admin_branch) | admin_branch=="" | !is.character(admin_branch)){
      err = paste(err, "admin_branch - A code should be a number between 1 and 16 (for precise results 2 digits character including leading zero).", sep="\n")
    } else if(nchar(admin_branch) > 2){
      err = paste(err, "admin_branch - out of range. A code should be a number between 1 and 16 (for precise results 2 digits character including leading zero).", sep="\n")
    }
  }

  if(!is.null(service_type)){
    if(!is.null(year)){
      possible_services = agr_get_serivces_year(year)$attributes.code
      if(!nchar(service_type) == 2){
        err = paste(err, "service_type - 2 digits character code expected (including leading zero if needed).", sep="\n")
      } else if(!service_type %in% possible_services){
        err = paste(err, paste0("service_type - out of range. Possible values are: ", paste(possible_services, collapse = ", ")), sep="\n")
      }
    } else {
      stop("year missing.")
    }
  }

  if(!is.null(service_code)){
    if(grepl("\\D", service_code) | service_code==""){
      err = paste(err, "service_code - 2 digits character code expected (including leading zero if needed).", sep="\n")
    } else if(nchar(service_code) > 2){
      err = paste(err, "service_code - 2 digits character code expected (including leading zero if needed).", sep="\n")
    } else if(nchar(service_code) < 2){
      msg = paste(msg, paste0("Given service_code length is ", nchar(service_code)," which may be ambiguous. Results returned for all codes that match given subset."), sep="\n")
    }
  }

  if(!is.null(service_name)){
    if(!is.character(service_name)){
      err = paste(err, "service_name - character expected.", sep="\n")
    }
  }

  if(!is.null(product_code)){
    if(!is.character(product_code) | !grepl("\\d{2}\\.\\d{4}\\.\\d{3}\\.\\d{2}", product_code)){
      err = paste(err, "product_code - expected full code of 11 digits separated by dots, format: 'dd.dddd.ddd.dd'.", sep="\n")
    }
  }

  if(!is.null(prod_code)){
    if(!is.character(prod_code) | grepl("(\\.(\\d{1}|\\d{5,})\\.)|((.*\\..*){4,})", prod_code)){
      err = paste(err, "prod_code - character code of 11 digits separated by dots, full code correct format is: 'dd.dddd.ddd.dd' (subset can be provided).", sep="\n")
    } else if(nchar(prod_code)>14){
      err = paste(err, paste0("Given prod_code length is ", nchar(prod_code)," which is too long, full code correct format is: 'dd.dddd.ddd.dd' (subset can be provided)."), sep="\n")
    } else if(nchar(prod_code)<14){
      msg = paste(msg, paste0("Given prod_code length is ", nchar(prod_code)," which is not a full code, full code correct format is: 'dd.dddd.ddd.dd' (subset can be provided)."), sep="\n")
    }
  }

  if(!is.null(product_name)){
    if(!is.character(product_name)){
      err = paste(err, "product_name - character expected.", sep="\n")
    }
  }

  if(!is.null(provider_code)){
    if(!is.character(provider_code) | !grepl("\\d+", provider_code)){
      err = paste(err, "provider_code - character expected.", sep="\n")
    }
  }

  if(!is.null(provider_name)){
    if(!is.character(provider_name)){
      err = paste(err, "provider_name - character expected.", sep="\n")
    }
  }

  if(!is.null(nip)){
    if(nchar(nip)!=10 | grepl("\\D", nip)){
      err = paste(err, "nip - 10 digits code expected.", sep="\n")
    }
  }

  if(!is.null(regon)){
    if(nchar(regon)>14 | grepl("\\D", regon)){
      err = paste(err, "regon - code too long or contaning non-numeric signs. The max length of the code is 14 digits. If code's subset provided, all records that match the subset will be returned.", sep="\n")
    }
  }

  if(!is.null(post_code)){
    if(nchar(post_code)>5 | grepl("\\D", post_code)){
      err = paste(err, "post_code - 5 digits post code without a hyphen ('-') expected.", sep="\n")
    }
  }

  if(!is.null(town)){
    if(!is.character(town)){
      err = paste(err, "town - character expected.", sep="\n")
    }
  }

  if(!is.null(teryt)){
    if(nchar(teryt)>7 | grepl("\\D", teryt)){
      err = paste(err, "teryt - should consist of max 7 digits.", sep="\n")
    }
  }

  if(!is.null(id_agreement)){
    if(!is.character(id_agreement)){
      err = paste(err, "id_agreement - character expected.", sep="\n")
    }
  }

  if(!is.null(id_plan)){
    if(!is.character(id_plan)){
      err = paste(err, "id_plan - character expected.", sep="\n")
    }
  }

  if(msg!=""){
    warning(msg)
  }

  if(err!="Wrong format or type of argument(s):"){
    stop(err)
  }

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
    stop("The character set defined by LC_CTYPE environemt variable does not support Polish non ASCII characters.")
  }
}



#' @title Convert Polish non ASCII characters to NFZ API accpted utf-8 representation of Unicode
#' @description Function accpets a string and converts Polish diacretic non ASCII characters
#' @param char A character where the signs are to be converted
#' @return  Returns a character vector
#' @examples
#' \dontrun{
#'  convert_pl_signs("G????????k??wi??a??")
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
    `%C5%BC`="\u17C",
    `%20`=" "
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


