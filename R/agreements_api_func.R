

#' @title Get all agreements meeting criteria
#' @description Query an agreement schema from agreements API to retrieve data about NFZ agreements meeting specified criteria
#' @param year A year of agreements.
#' @param admin_branch The number of one of 16 voivodships branches of NFZ; character type, a number in a range of 01-16 with leading zero.
#' @param service_type A part of or a full NFZ service type code (kod rodzaju swiadczen)
#' @param product_code A part of or a full NFZ product type code (kod produktu kontraktowego)
#' @param provider_code A part of or a full provider's code assigned by NFZ for contracting purposes (kod swiadczeniodawcy)
#' @param provider_name A part of or a full provider's name (nazwa swiadczeniodawcy)
#' @param town A name of the provider's location town or city
#' @param nip A part of or a full provider's NIP code. NIP is a ten-digit code used to identify taxpayers in Poland.
#' @param regon A part of or full REGON identification number (National Official Business Register).
#' @return  Returns data.table.
#' @details `gen` describes a relative position or a stack number of check_req_args parent environment relative to a global environment (global has number 0). Can't be 0 or negative.
#' @export
#' @examples
#' \dontrun{
#'  get_agreements(year=2021, admin_branch='07', town='Siedlce')
#' }
# TODO: separate functions or a dictionary of API's addresses and possible arguments for each of them
agr_get_agreements <- function(year, admin_branch, service_type=NULL, product_code=NULL, provider_code=NULL,
                               provider_name=NULL, town=NULL, nip=NULL, regon=NULL){

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

  agr_check_arg_types(year=year, admin_branch=admin_branch, service_type=service_type, product_code=product_code, provider_code=provider_code,
                      provider_name=provider_name, town=town, nip=nip, regon=regon)

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
#' @description Query a providers schema from agreements API for data about providers having contracts with NFZ, meeting specified criteria.
#' @param year A year of agreements.
#' @param admin_branch The number of one of 16 voivodships branches of NFZ; character type, a number in a range of 01-16 with leading zero.
#' @param provider_code A part of or a full provider's code assigned by NFZ for contracting purposes (kod swiadczeniodawcy).
#' @param provider_name A part of or a full provider's name (nazwa swiadczeniodawcy).
#' @param nip A part of or a full provider's NIP code. NIP is a ten-digit code used to identify taxpayers in Poland.
#' @param regon A part of or full REGON identification number (National Official Business Register).
#' @param post_code Postal code of a provider's main location (headquarter). Given without a dash, just 5 digits.
#' @param street Street name of provider's main location (headquarter).
#' @param town A name of the provider's location town or city.
#' @param teryt Location code (TREC) of the National Official Register of the Territorial Division of the Country (TERYT).
#' @return  Returns data.table.
#' @export
agr_get_providers <- function(year=NULL, admin_branch=NULL, provider_code=NULL, provider_name=NULL, nip=NULL, regon=NULL,
                              post_code=NULL, street=NULL, town=NULL, teryt=NULL){

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

  agr_check_arg_types(year=year, admin_branch=admin_branch, provider_code=provider_code, provider_name=provider_name, town=town,
                      nip=nip, regon=regon, post_code=post_code, street=street, teryt=teryt)


  data <- get_request(available_args=available_args, schema="providers", api_type="app-umw-api")
  return(data)
}


#' @title Get a list of providers available in given year
#' @description Query a providers schema from agreements API for data about providers having contract with NFZ in a particular year and meeting any other specified criteria. A table contains contract's amount.
#' @param year A year of agreements.
#' @param admin_branch The number of one of 16 voivodships branches of NFZ; character type, a number in a range of 01-16 with leading zero.
#' @param provider_code A part of or a full provider's code assigned by NFZ for contracting purposes (kod swiadczeniodawcy).
#' @param provider_name A part of or a full provider's name (nazwa swiadczeniodawcy).
#' @param nip A part of or a full provider's NIP code. NIP is a ten-digit code used to identify taxpayers in Poland.
#' @param regon A part of or full REGON identification number (National Official Business Register).
#' @param post_code Postal code of a provider's main location (headquarter). Given without a dash, just 5 digits.
#' @param street Street name of provider's main location (headquarter).
#' @param town A name of the provider's location town or city.
#' @param teryt Location code (TREC) of the National Official Register of the Territorial Division of the Country (TERYT).
#' @return  Returns data.table.
#' @export
agr_get_prov_by_year <- function(year, admin_branch=NULL, provider_code=NULL, provider_name=NULL, nip=NULL,
                                 regon=NULL, post_code=NULL, street=NULL, town=NULL, teryt=NULL){

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
agr_get_provider <- function(year, provider_code, admin_branch){

  check_env_lang() # Check the settings of the language
  available_args <- list(year="year",
                         admin_branch="branch",
                         provider_code="code")

  agr_check_arg_types(year=year, admin_branch=admin_branch, provider_code=provider_code)

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
agr_get_serivces <- function(year, service_type=NULL, service_name=NULL){

  check_env_lang() # Check the settings of the language
  available_args <- list(year="year",
                         service_type="code",
                         service_name="name")

  agr_check_arg_types(year=year, service_type=service_type, service_name=service_name)

  data <- get_request(available_args=available_args, schema="service-types", api_type="app-umw-api")
  return(data)
}

#' @title Get service types available for a given year
#' @description Function to retrieve all service types valid for a given year
#' @param year
#' @return  Returns a data.table
#' @export
agr_get_serivces_year <- function(year){
  request <- httr::GET(paste0("https://api.nfz.gov.pl/app-umw-api/service-types?year=", year,"&page=1&limit=10&format=json&api-version=1.2"), httr::timeout(20))
  data <- jsonlite::fromJSON(httr::content(request, "text"), flatten=TRUE)
  data <- data$data$entries
  return(data)
}


#' @title Get product types available for a given year
#' @description Function to retrieve a all product types valid for a given year
#' @param year
#' @return  Returns a data.table
#' @export
agr_get_products_year <- function(year){
  request <- httr::GET(paste0("https://api.nfz.gov.pl/app-umw-api/contract-products?year=", year, "&page=1&limit=10&format=json&api-version=1.2"), httr::timeout(20))
  data <- jsonlite::fromJSON(httr::content(request, "text"), flatten=TRUE)
  data <- data$data$entries
  return(data)
}


#' @title Get a list of product types (Produkty Kontraktowe)
#' @description Query a product-types schema from agreements API to get a list of product types (Produkty Kontraktowe) available in a given year.
#' @param year A year of a contract.
#' @param product_code A full or part of NFZ a product type code (Kod produktu kontraktowego)
#' @param product_name A full or part of NFZ a product type name (Nazwa produktu kontraktowego)
#' @return  Returns data.table.
#' @export
agr_get_products <- function(year, product_code=NULL, product_name=NULL){

  check_env_lang() # Check the settings of the language
  available_args <- list(year="year",
                         product_code="code",
                         product_name="name")

  if(!nchar(year) == 4){
    stop("Wrong format or type of year argument. Full 4 digits year expected.")
  } else if(!year %in% agr_get_years()){
    stop("Year arguemnt value out of range. To check available years call agr_get_years function.")
  }

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

