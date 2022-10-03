

#' @title Get all agreements meeting criteria
#' @description Query an agreement schema from agreements API to retrieve data about NFZ agreements meeting specified criteria
#' @param year A year of agreements.
#' @param admin_branch The number of one of 16 voivodships branches of NFZ; character type, a number in a range of 01-16 with leading zero.
#' @param service_type Full NFZ service type code (kod rodzaju swiadczen)
#' @param product_code Full NFZ product type code (kod produktu kontraktowego)
#' @param provider_code Part of or full provider's code assigned by NFZ for contracting purposes (kod swiadczeniodawcy)
#' @param provider_name Part of or full provider's name (nazwa swiadczeniodawcy)
#' @param town A name of the provider's location town or city
# @param nip Part of or full provider's NIP code. NIP is a ten-digit code used to identify taxpayers in Poland.
#' @param regon Part of or full REGON identification number (National Official Business Register).
#' @return  Returns data.table.
#' @details `gen` describes a relative position or a stack number of check_req_args parent environment relative to a global environment (global has number 0). Can't be 0 or negative.
#' @export
#' @examples
#' \dontrun{
#'  get_agreements(year=2021, admin_branch='07', town='Siedlce')
#' }
# TODO: separate functions or a dictionary of API's addresses and possible arguments for each of them
agr_get_agreements <- function(year, admin_branch, service_type=NULL, product_code=NULL, provider_code=NULL, #nip=NULL,
                               provider_name=NULL, town=NULL, regon=NULL){

  available_args <- list(year="year",
                     admin_branch="branch",
                     service_type="serviceType",
                     product_code="productCode",
                     provider_code="providerCode",
                     provider_name="providerName",
                     town="place",
                     # nip="nip",
                     regon="regon")
  agr_check_arg_types(year=year, admin_branch=admin_branch, service_type=service_type, product_code=product_code, provider_code=provider_code, # nip=nip,
                      provider_name=provider_name, town=town, regon=regon)

  data <- get_request(available_args=available_args, schema="agreements", api_type="app-umw-api")#, given_args=given_args)
  return(data)
}


#' @title Get a list of providers
#' @description Query providers schema from agreements API for data about providers having contracts with NFZ, meeting specified criteria.
#' @param year A year of agreements.
#' @param admin_branch The number of one of 16 voivodships branches of NFZ; character type, a number in a range of 01-16 with leading zero.
#' @param provider_code Part of or full provider's code assigned by NFZ for contracting purposes (kod swiadczeniodawcy).
#' @param provider_name Part of or full provider's name (nazwa swiadczeniodawcy).
#' @param nip Part of or full provider's NIP code. NIP is a ten-digit code used to identify taxpayers in Poland.
#' @param regon Part of or full REGON identification number (National Official Business Register).
#' @param post_code Postal code of a provider's main location (headquarter). Given without a dash, just 5 digits.
#' @param street Street name of provider's main location (headquarter).
#' @param town A name of the provider's location town or city.
#' @param teryt Location code (TREC) of the National Official Register of the Territorial Division of the Country (TERYT).
#' @return  Returns data.table.
#' @export
agr_get_providers <- function(year=NULL, admin_branch=NULL, provider_code=NULL, provider_name=NULL, nip=NULL, regon=NULL,
                              post_code=NULL, street=NULL, town=NULL, teryt=NULL){

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

  agr_check_arg_types(year=year, admin_branch=admin_branch, provider_code=provider_code, provider_name=provider_name, town=town,
                      nip=nip, regon=regon, post_code=post_code, street=street, teryt=teryt)

  data <- get_request(available_args=available_args, schema="providers", api_type="app-umw-api")
  return(data)
}


#' @title Get a list of providers available in given year
#' @description Query providers schema from agreements API for data about providers having contract with NFZ in a particular year and meeting any other specified criteria. A table contains contract's amount.
#' @param year A year of agreements.
#' @param admin_branch The number of one of 16 voivodships branches of NFZ; character type, a number in a range of 01-16 with leading zero.
#' @param provider_code Part of or full provider's code assigned by NFZ for contracting purposes (kod swiadczeniodawcy).
#' @param provider_name Part of or full provider's name (nazwa swiadczeniodawcy).
#' @param nip Part of or full provider's NIP code. NIP is a ten-digit code used to identify taxpayers in Poland.
#' @param regon Part of or full REGON identification number (National Official Business Register).
#' @param post_code Postal code of a provider's main location (headquarter). Given without a dash, just 5 digits.
#' @param street Street name of provider's main location (headquarter).
#' @param town A name of the provider's location town or city.
#' @param teryt Location code (TREC) of the National Official Register of the Territorial Division of the Country (TERYT).
#' @return  Returns data.table.
#' @export
agr_get_prov_by_year <- function(year, admin_branch=NULL, provider_code=NULL, provider_name=NULL, nip=NULL,
                                 regon=NULL, post_code=NULL, street=NULL, town=NULL, teryt=NULL){

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

  agr_check_arg_types(year=year, admin_branch=admin_branch, provider_code=provider_code, provider_name=provider_name, town=town,
                      nip=nip, regon=regon, post_code=post_code, street=street, teryt=teryt)

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
agr_get_provider <- function(year, provider_code, admin_branch){ # prov code 2nd, to correctly identify optional args

  available_args <- list(year="year",
                         provider_code="code",
                         admin_branch="branch")

  agr_check_arg_types(year=year, admin_branch=admin_branch, provider_code=provider_code)

  data <- get_request(available_args=available_args, schema="providers", api_type="app-umw-api",
                      url_args=c("year", "provider_code"))
  return(data)
}


#' @title Get a list of service types (Rodzaje Swiadczen)
#' @description Query a service-types schema from agreements API to get a list of service types (Rodzaje Swiadczen) available in a given year.
#' @param year A year of a contract.
#' @param service_code Full or part of NFZ a service type code (Kod rodzaju swiadczen)
#' @param service_name Full or part of NFZ a service type name (Nazwa rodzaju swiadczen)
#' @return  Returns data.table.
#' @export
agr_get_serivces <- function(year, service_code=NULL, service_name=NULL){

  available_args <- list(year="year",
                         service_code="code",
                         service_name="name")

  agr_check_arg_types(year=year, service_code=service_code, service_name=service_name)

  data <- get_request(available_args=available_args, schema="service-types", api_type="app-umw-api")#, given_args=given_args)
  return(data)
}

#' @title Get service types available for a given year
#' @description Function to retrieve all service types valid for a given year
#' @param year a year for valid services
#' @return  Returns a data.table
#' @export
agr_get_serivces_year <- function(year){
  agr_check_arg_types(year=year)
  request <- httr::GET(paste0("https://api.nfz.gov.pl/app-umw-api/service-types?year=", year,"&page=1&limit=25&format=json&api-version=1.2"), httr::timeout(20))
  data <- jsonlite::fromJSON(httr::content(request, "text"), flatten=TRUE)
  data <- data$data$entries
  return(data)
}



#' @title Get a list of product types (Produkty Kontraktowe)
#' @description Query a product-types schema from agreements API to get a list of product types (Produkty Kontraktowe) available in a given year.
#' @param year A year of a contract.
#' @param prod_code Full or part of NFZ a product type code (Kod produktu kontraktowego)
#' @param product_name Full or part of NFZ a product type name (Nazwa produktu kontraktowego)
#' @return  Returns data.table.
#' @export
agr_get_products <- function(year, prod_code=NULL, product_name=NULL){

  available_args <- list(year="year",
                         prod_code="code",
                         product_name="name")

  agr_check_arg_types(year=year, prod_code=prod_code, product_name=product_name)

  data <- get_request(available_args=available_args, schema="contract-products", api_type="app-umw-api")
  return(data)
}


#' @title Get years of available data
#' @description Function to retrieve a all years for which data is available
#' @return  Returns integer vector
#' @export
agr_get_years <- function(){
  request <- httr::GET("https://api.nfz.gov.pl/app-umw-api/available-years?format=json&api-version=1.2", httr::timeout(20))
  data <- jsonlite::fromJSON(httr::content(request, "text"), flatten=TRUE)
  data <- data$`start-year`:data$`end-year`
  return(data)
}

