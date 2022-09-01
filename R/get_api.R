

#' @title Get data from selected api type and schema provided available arguments
#' @description General function to get data from selected NFZ API type (agreements, ...) and schema (ex. agreements or providers), provided lsit of available arguments. Function to be run inside outer function for selected api. User given arugments are retrieved from appropriate environemt.
#' @param available_args List of available arguments for selected api sechema
#' @param api_type A type of NFZ api to be queried.
#' @param schema A name of schema of appropriate api
#' @return  Returns a data.table for requested query
get_request <- function(available_args,  api_type="app-umw-api", schema="agreements"){

  # Retrieve arguments and check their corectrness
  given_args <- check_req_args(possible_args=names(available_args)) # get_api_data,

  # Prepare query provided arguments
  api_agrs <- paste0(sapply(names(given_args), function(x) available_args[[x]]),
                     "=",
                     sapply(names(given_args), function(x) given_args[[x]]))
  api_agrs <- convert_pl_signs(api_agrs)
  api_query <- paste0("https://api.nfz.gov.pl/", api_type, "/", schema, "?",
                      paste0(api_agrs, collapse = "&"),
                      "&limit=1&format=json&api-version=1.2")

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
      data <- rbind(data, request_data$data[[1]])
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
#'  get_agreements(year=2021, ow='07', MIEJSCOWOSC='Siedlce')
#' }
# TODO: separate functions or a dictionary of apis addresses and possible arguments for each of them
agr_agreements <- function(year, ow, ...){

  check_env_lang() # Check the settings of the language
  available_args <- list(year="year",
                     ow="branch",
                     RODZAJ_SWIADCZEN="serviceType",
                     PRODUKT_KONTRAKTOWY="productCode",
                     KOD_SWIADCZENIODAWCY="providerCode",
                     NAZWA_SWIADCZENIODAWCY="providerName",
                     MIEJSCOWOSC="place",
                     NIP="nip",
                     REGON="regon")

  data <- get_request(available_args=available_args, schema="agreements", api_type="app-umw-api")
  return(data)
}




agr_providers <- function(year, ow, ...){

  check_env_lang() # Check the settings of the language
  available_args <- list(year="year",
                         ow="branch",
                         KOD_SWIADCZENIODAWCY="code",
                         NAZWA_SWIADCZENIODAWCY="name",
                         NIP="nip",
                         REGON="regon",
                         KOD_POCZTOWY="postCode",
                         ULICA="street",
                         MIEJSCOWOSC="place",
                         TERYT="commune")

  data <- get_request(available_args=available_args, schema="providers", api_type="app-umw-api")
  return(data)
}


# agr_providers_year <- function(year, ow, ...){
#
#   check_env_lang() # Check the settings of the language
#   available_args <- list(year="year",
#                          ow="branch",
#                          KOD_SWIADCZENIODAWCY="code",
#                          NAZWA_SWIADCZENIODAWCY="name",
#                          NIP="nip",
#                          REGON="regon",
#                          KOD_POCZTOWY="postCode",
#                          ULICA="street",
#                          MIEJSCOWOSC="place",
#                          TERYT="commune")
#
#   data <- get_request(available_args=available_args, schema="providers", api_type="app-umw-api")
#   return(data)
# }
