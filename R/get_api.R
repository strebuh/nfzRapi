

#' @title Get data from selected api type and schema provided available arguments
#' @description General function to get data from selected NFZ API type (agreements, ...) and schema (ex. agreements or providers), provided lsit of available arguments. Function to be run inside outer function for selected api. User given arugments are retrieved from appropriate environemt.
#' @param available_args List of available arguments for selected api sechema
#' @param api_type A type of NFZ api to be queried.
#' @param schema A name of schema of appropriate api
#' @return  Returns a data.table for requested query
get_request <- function(available_args,  api_type="app-umw-api", schema="agreements", url_args=NULL){

  # Retrieve arguments and check their corectrness
  given_args <- check_req_args(possible_args=names(available_args)) # get_api_data,
  formal_names <- given_args$formal_names
  given_args <- given_args$given_args

  # Prepare query provided arguments
  formal_args = c(api_type, schema)
  if(!is.null(url_args)){
    n_formals = if(length(url_args) == length(given_args)) 0 else length(intersect(names(given_args), url_args))
    optional_args = if(length(url_args) == length(given_args)) list() else given_args[(n_formals+1):length(given_args)]
    # browser()

    formal_args = c(formal_args, unlist(setdiff(given_args, optional_args)))

  } else {
    optional_args = given_args
  }
  optionals_matcher <- if(rlang::is_empty(optional_args)) "" else "="
  # browser()

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
      # browser()
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


#' @title Query an agreement schema from Agreements api
#' @description Function to
#' @param year A year of agreements
#' @param ow The number of one of 16 voivodship branches of NFZ; character with leading zero between 02-32, increased by 2
#' @return  Returns a data.table for requested query
#' @export
#' @examples
#' \dontrun{
#'  get_agreements(year=2021, admin_branch='07', town='Siedlce')
#' }
# TODO: separate functions or a dictionary of apis addresses and possible arguments for each of them
agr_get_agrmts <- function(year, ow, ...){

  check_env_lang() # Check the settings of the language
  available_args <- list(year="year",
                     admin_branch="branch",
                     service_type="serviceType",
                     product_code="productCode",
                     provider_code="providerCode",
                     provider_name="providerName",
                     town="place",
                     nip="nip",
                     regon="regon")

  data <- get_request(available_args=available_args, schema="agreements", api_type="app-umw-api")
  return(data)
}



agr_agrmts_by_id <- function(id_agreement){
  check_env_lang() # Check the settings of the language
  data <- get_request(available_args=list(id_agreement="id"), schema="agreements", api_type="app-umw-api", url_args="id_agreement")
  return(data)
}


agr_plans_by_id <- function(id_plan){
  check_env_lang() # Check the settings of the language
  data <- get_request(available_args=list(id_plan="id"), schema="plans", api_type="app-umw-api", url_args="id_plan")
  return(data)
}


agr_mnth_pl_by_id <- function(id_month){
  check_env_lang() # Check the settings of the language
  data <- get_request(available_args=list(id_month="id"), schema="months", api_type="app-umw-api", url_args="id_month")
  return(data)
}


agr_providers <- function(...){

  check_env_lang() # Check the settings of the language
  available_args <- list(year="year",
                         admin_branch="branch",
                         provider_code="code",
                         provider_name="name",
                         nip="nip",
                         regon="regon",
                         post_code="postCode",
                         street="street",
                         town="place",
                         teryt="commune")

  data <- get_request(available_args=available_args, schema="providers", api_type="app-umw-api")
  return(data)
}


agr_providers_year <- function(year, ...){

  check_env_lang() # Check the settings of the language
  available_args <- list(year="year",
                         admin_branch="branch",
                         provider_code="code",
                         provider_name="name",
                         nip="nip",
                         regon="regon",
                         post_code="postCode",
                         street="street",
                         town="place",
                         teryt="commune",
                         product_code="productCode",
                         service_type="serviceType")

  data <- get_request(available_args=available_args, schema="providers", api_type="app-umw-api", url_args="year")
  return(data)
}

agr_provider <- function(year, provider_code, admin_branch){

  check_env_lang() # Check the settings of the language
  available_args <- list(year="year",
                         admin_branch="branch",
                         provider_code="code")

  data <- get_request(available_args=available_args, schema="providers", api_type="app-umw-api",
                      url_args=c("year", "provider_code"))
  return(data)
}


agr_get_serivces <- function(year, ...){

  check_env_lang() # Check the settings of the language
  available_args <- list(year="year",
                         service_type="code",
                         service_name="name")

  data <- get_request(available_args=available_args, schema="service-types", api_type="app-umw-api")
  return(data)
}


agr_get_products <- function(year, ...){

  check_env_lang() # Check the settings of the language
  available_args <- list(year="year",
                         product_code="code",
                         product_name="name")

  data <- get_request(available_args=available_args, schema="contract-products", api_type="app-umw-api")
  return(data)
}
