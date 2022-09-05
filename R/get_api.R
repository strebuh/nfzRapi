# source("R/check_arguments.R")

#' @title Get data from selected api type and schema provided available arguments
#' @description General function to get data from selected NFZ API type (agreements, ...) and schema (ex. agreements or providers), provided list of available arguments. Function to be run inside outer function for selected API User given arguments are retrieved from appropriate environment.
#' @param available_args List of available arguments for selected api schema.
#' @param api_type A type of NFZ api to be queried.
#' @param schema A name of schema of appropriate api.
#' @param url_args A vector of obligatory arguments to be passed without keys as a part of the url <https://api.nfz.gov.pl/api_type/schema/url_args/url_args>.
#' @return  Returns a data.table for requested query.
get_request <- function(available_args,  api_type="app-umw-api", schema="agreements", url_args=NULL){

  # Retrieve arguments and check their corectness
  given_args <- check_req_args(possible_args=names(available_args)) # get_api_data,
  formal_names <- given_args$formal_names
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


#' @title Get all agreements meeting criteria
#' @description Query an agreement schema from agreements API to retrieve data about NFZ agreements meeting specified arguments.
#' @param year A year of agreements.
#' @param admin_branch The number of one of 16 voivodships branches of NFZ; character with leading zero between 02-32, increased by 2.
#' @param ... One or multiple argument to filter providers.
#' @return  Returns data.table.
#' @export
#' @examples
#' \dontrun{
#'  get_agreements(year=2021, admin_branch='07', town='Siedlce')
#' }
# TODO: separate functions or a dictionary of API's addresses and possible arguments for each of them
agr_get_agreements <- function(year, admin_branch, ...){

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


#' @title Get a particular agreement general details
#' @description Query an agreement schema from agreements API for contract's number of units, prices and average price split by products.
#' @param id_agreement A hashed id of NFZ agreement.
#' @return  Returns data.table containing data about particular agreements.
#' @export
agr_get_agreement <- function(id_agreement){
  check_env_lang() # Check the settings of the language
  data <- get_request(available_args=list(id_agreement="id"), schema="agreements", api_type="app-umw-api", url_args="id_agreement")
  return(data)
}


#' @title Get a particular agreement general data by months
#' @description Query an agreement schema from agreements API for data about about a particular agreement split by months. Data contains number of units, price and average price without.
#' @param id_agreement A hashed id of a NFZ agreement.
#' @return  Returns data.table.
#' @export
agr_get_plan <- function(id_agreement){
  check_env_lang() # Check the settings of the language
  data <- get_request(available_args=list(id_agreement="id"), schema="plans", api_type="app-umw-api", url_args="id_agreement")
  return(data)
}


#' @title Get an agreement particular month's plan
#' @description Query an agreement schema from agreements API for data about a particular agreement month plan split by products. Data contains number of units, price and average price by products.
#' @param id_plan A hashed id of a month plan of a NFZ agreement.
#' @return  Returns data.table.
#' @export
agr_get_month_plan <- function(id_plan){
  check_env_lang() # Check the settings of the language
  data <- get_request(available_args=list(id_plan="id"), schema="months", api_type="app-umw-api", url_args="id_plan")
  return(data)
}


#' @title Get a list of providers
#' @description Query a providers schema from agreements API for data about providers with contracts with NFZ meeting with arguments criteria.
#' @param ... One or multiple argument to filter providers.
#' @return  Returns data.table.
#' @export
agr_get_providers <- function(...){

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


#' @title Get a list of providers from a year
#' @description Query a providers schema from agreements API for data about providers having contract with NFZ in a particular year and meeting any other specified with arguments criteria. A table contains contract's amount.
#' @param year A year of a contract.
#' @param ... One or multiple argument to filter providers.
#' @return  Returns data.table.
#' @export
agr_get_prov_by_year <- function(year, ...){

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


#' @title Get particular provider's contracted services
#' @description Query a providers schema from agreements API for data about a provider's NFZ contracted services in a particular year.
#' @param year A year of a contract.
#' @param provider_code NFZ assigned provider code (KOD_SWIADCZENIODAWCY).
#' @param admin_branch NFZ voivodship branch code (OW).
#' @return  Returns data.table.
#' @export
agr_get_provider <- function(year, provider_code, admin_branch){

  check_env_lang() # Check the settings of the language
  available_args <- list(year="year",
                         admin_branch="branch",
                         provider_code="code")

  data <- get_request(available_args=available_args, schema="providers", api_type="app-umw-api",
                      url_args=c("year", "provider_code"))
  return(data)
}


#' @title Get a list of service types (Rodzaje Swiadczen)
#' @description Query a service-types schema from agreements API to get a list of service types (Rodzaje Swiadczen) available in a given year.
#' @param year A year of a contract.
#' @param service_type A full or part of NFZ a service type code (Kod rodzaju swiadczen)
#' @param service_name A full or part of NFZ a service type name (Nazwa rodzaju swiadczen)
#' @return  Returns data.table.
#' @export
agr_get_serivces <- function(year, service_type, service_name){

  check_env_lang() # Check the settings of the language
  available_args <- list(year="year",
                         service_type="code",
                         service_name="name")

  data <- get_request(available_args=available_args, schema="service-types", api_type="app-umw-api")
  return(data)
}


#' @title Get a list of product types (Produkty Kontraktowe)
#' @description Query a product-types schema from agreements API to get a list of product types (Produkty Kontraktowe) available in a given year.
#' @param year A year of a contract.
#' @param product_code A full or part of NFZ a product type code (Kod produktu kontraktowego)
#' @param product_name A full or part of NFZ a product type name (Nazwa produktu kontraktowego)
#' @return  Returns data.table.
#' @export
agr_get_products <- function(year, product_code, product_name){

  check_env_lang() # Check the settings of the language
  available_args <- list(year="year",
                         product_code="code",
                         product_name="name")

  data <- get_request(available_args=available_args, schema="contract-products", api_type="app-umw-api")
  return(data)
}
