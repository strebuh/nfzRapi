# source("R/agreements_api_func.R")
# source("R/general_func.R")


# if running test code getst to
test_that("agr_get_agreements wrong argument error", {

  # year, admin_branch, service_type, product_code, provider_code, provider_name town, regon
  expect_error(agr_get_agreements(year=2020, admin_branch='07', unexcpected_arg=3), "unused argument")

  # agr_get_agreements(year=2020, admin_branch='07', town="Huszlew")
  for(wrong in list(18, 'string', 2025)){
    expect_error(agr_get_agreements(year=wrong, admin_branch='07'), "Wrong format or type of argument\\(s\\):")
  }


  for(wrong in list(127, 'string', '')){
    expect_error(agr_get_agreements(year=2020, admin_branch=wrong), "Wrong format or type of argument\\(s\\):")
  }
  # agr_get_agreements(year=2020, admin_branch='')

  for(wrong in list(777, '312')){
    expect_error(agr_get_agreements(year=2020, admin_branch='07', service_type=wrong), "Wrong format or type of argument\\(s\\):")
  }


  # agr_get_agreements(year=2020, admin_branch='07', service_type="03", product_code="03.4000.030.02",  town="Siedlce")
  for(wrong in list("03.4000", "03.40000.30.02", 03.40000)){
    expect_error(agr_get_agreements(year=2020, admin_branch='07', service_type="03", product_code=wrong), "Wrong format or type of argument\\(s\\):")
  }


  # agr_get_agreements(year=2020, admin_branch='07', service_type="03", product_code="03.4000.030.02", provider_code="70603563")
  for(wrong in list(70603563, "some string")){
    expect_error(agr_get_agreements(year=2020, admin_branch='07', service_type="03", product_code="03.4000.030.02", provider_code=wrong), "Wrong format or type of argument\\(s\\):")
  }


  # agr_get_agreements(year=2020, admin_branch='07', service_type="03", product_code="03.4000.030.02", provider_name="SZPITAL", town="siedlce")
  for(wrong in list(222)){
    expect_error(agr_get_agreements(year=2020, admin_branch='07', service_type="03", product_code="03.4000.030.02", provider_code=wrong), "Wrong format or type of argument\\(s\\):")
  }

  # agr_get_agreements(year=2020, admin_branch='07', service_type="03", town="Łosice")
  # agr_get_agreements(year=2020, admin_branch='03', service_type="03", town="Biała Podlaska")
  for(wrong in list(777)){
    expect_error(agr_get_agreements(year=2020, admin_branch='07', service_type="03", town=wrong), "Wrong format or type of argument\\(s\\):")
  }

  # agr_get_agreements(year=2020, admin_branch='03', service_type="03", regon='000676708') # can be a part
  # agr_get_agreements(year=2020, admin_branch='03', service_type="03", regon='676708')
  for(wrong in list('abd', '777456456654777', 'some string')){
    expect_error(agr_get_agreements(year=2020, admin_branch='07', service_type="03", regon=wrong), "Wrong format or type of argument\\(s\\):")
  }

})


test_that("agr_get_agreements wrong argument messsage or warning", {

  expect_message(agr_get_agreements(year=2020, admin_branch='07', service_type="03", product_code="03.4000.030.02", town="siedlce", provider_name="some string"), "No availabe data for queried scope.")
  expect_message(agr_get_agreements(year=2020, admin_branch='07', service_type="03", town="Biała-Podlaska"), "No availabe data for queried scope.")
  expect_message(agr_get_agreements(year=2020, admin_branch='07', service_type="03", town="some string"), "No availabe data for queried scope.")

})



test_that("agr_get_providers wrong argument error", {

# year, admin_branch, provider_code, provider_name, nip, regon, post_code, street, town, teryt
expect_error(agr_get_providers(year=2020, admin_branch='07', unexcpected_arg=3), "unused argument")

for(wrong in list(18, 2025, 'some sting')){
  expect_error(agr_get_providers(year=wrong, admin_branch='07'), "Wrong format or type of argument\\(s\\):")
}

# agr_get_providers(year=2020, admin_branch='07')
for(wrong in list(777, 'string', '')){
  expect_error(agr_get_providers(year=2020, admin_branch=wrong), "Wrong format or type of argument\\(s\\):")
}

# agr_get_providers(year=2020, admin_branch='07', provider_code="70603563")
for(wrong in list(70603563, 'some string')){
  expect_error(agr_get_providers(year=2020, admin_branch='07', provider_code=wrong), "Wrong format or type of argument\\(s\\):")
}

# agr_get_providers(year=2020, admin_branch='07', provider_name="SZPITAL", town="siedlce")
for(wrong in list(222)){
  expect_error(agr_get_providers(year=2020, admin_branch='07', town="siedlce", provider_name=wrong), "Wrong format or type of argument\\(s\\):")
}

# agr_get_providers(year=2020, admin_branch='07', nip="8212577607") # can be a part
for(wrong in list("789456", "774564566547", "some string")){
  expect_error(agr_get_providers(year=2020, admin_branch='07', town="siedlce", nip=wrong), "Wrong format or type of argument\\(s\\):")
}

# agr_get_providers(year=2020, admin_branch='03', regon=30206988) # can be a part
for(wrong in list("abd", "777456456654777", "some string")){
  expect_error(agr_get_providers(year=2020, admin_branch='07', town="siedlce", regon=wrong), "Wrong format or type of argument\\(s\\):")
}

# agr_get_providers(year=2020, admin_branch='03', post_code=2150)
for(wrong in list(215000, "21-50", "some string")){
  expect_error(agr_get_providers(year=2020, admin_branch='03', post_code=wrong), "Wrong format or type of argument\\(s\\):")
}

# agr_get_providers(year=2020, admin_branch='07', town="Huszlew")
for(wrong in list(150000)){
  expect_error(agr_get_providers(year=2020, admin_branch='07', town=wrong), "Wrong format or type of argument\\(s\\):")
}

  # agr_get_providers(year=2020, admin_branch='07', teryt=1410)
  for(wrong in list('some string', 14650111, "???")){
    expect_error(agr_get_providers(year=2020, admin_branch='07', teryt=wrong), "Wrong format or type of argument\\(s\\):")
  }
})


test_that("agr_get_providers wrong argument message or warning", {

  # agr_get_providers(year=2020, admin_branch='07', post_code=82, street="-")
  expect_message(agr_get_providers(year=2020, admin_branch='07', town="siedlce", provider_name="some string"), "No availabe data for queried scope.")
  expect_message(agr_get_providers(year=2020, admin_branch='07', town="Siedlce", street="15000000000"), "No availabe data for queried scope.")
  expect_message(agr_get_providers(year=2020, admin_branch='07', town="Siedlce", street="???"), "No availabe data for queried scope.")

})




test_that("agr_get_prov_by_year wrong argument error", {

  # year, admin_branch, provider_code, provider_name, nip, regon, post_code, street, town, teryt
  expect_error(agr_get_prov_by_year(year=2020, admin_branch='07', unexcpected_arg=3), "unused argument")

  for(wrong in list(18, 2025, 'some sting')){
    expect_error(agr_get_prov_by_year(year=wrong, admin_branch='07'), "Wrong format or type of argument\\(s\\):")
  }

  # agr_get_prov_by_year(year=2020, admin_branch='07')
  for(wrong in list(123, 'string', '')){
    expect_error(agr_get_prov_by_year(year=2020, admin_branch=wrong), "Wrong format or type of argument\\(s\\):")
  }

  # agr_get_prov_by_year(year=2020, admin_branch='07', provider_code="70603563")
  for(wrong in list(70603563, 'some string')){
    expect_error(agr_get_prov_by_year(year=2020, admin_branch='07', provider_code=wrong), "Wrong format or type of argument\\(s\\):")
  }

  # agr_get_prov_by_year(year=2020, admin_branch='07', provider_name="SZPITAL", town="siedlce")
  for(wrong in list(222)){
    expect_error(agr_get_prov_by_year(year=2020, admin_branch='07', town="siedlce", provider_name=wrong), "Wrong format or type of argument\\(s\\):")
  }

  # agr_get_prov_by_year(year=2020, admin_branch='07', nip="8212577607") # can be a part
  for(wrong in list("789456", "774564566547", "some string")){
    expect_error(agr_get_prov_by_year(year=2020, admin_branch='07', town="siedlce", nip=wrong), "Wrong format or type of argument\\(s\\):")
  }

  # agr_get_prov_by_year(year=2020, admin_branch='03', regon=30206988) # can be a part
  for(wrong in list("abd", "777456456654777", "some string")){
    expect_error(agr_get_prov_by_year(year=2020, admin_branch='07', town="siedlce", regon=wrong), "Wrong format or type of argument\\(s\\):")
  }

  # agr_get_prov_by_year(year=2020, admin_branch='03', post_code=2150)
  for(wrong in list(215000, "21-50", "some string")){
    expect_error(agr_get_prov_by_year(year=2020, admin_branch='03', post_code=wrong), "Wrong format or type of argument\\(s\\):")
  }

  # agr_get_prov_by_year(year=2020, admin_branch='07', town="Huszlew")
  for(wrong in list(150000)){
    expect_error(agr_get_prov_by_year(year=2020, admin_branch='07', town=wrong), "Wrong format or type of argument\\(s\\):")
  }

  # agr_get_prov_by_year(year=2020, admin_branch='07', teryt=1410)
  for(wrong in list('some string', 14650111, "???")){
    expect_error(agr_get_prov_by_year(year=2020, admin_branch='07', teryt=wrong), "Wrong format or type of argument\\(s\\):")
  }

  })

test_that("agr_get_prov_by_year wrong argument mesage or warning", {

  # agr_get_prov_by_year(year=2020, admin_branch='07', post_code=82, street="-")
  expect_message(agr_get_prov_by_year(year=2020, admin_branch='07', town="siedlce", provider_name="some string"), "No availabe data for queried scope.")
  expect_message(agr_get_prov_by_year(year=2020, admin_branch='07', town="Siedlce", street="15000000000"), "No availabe data for queried scope.")
  expect_message(agr_get_prov_by_year(year=2020, admin_branch='07', town="Siedlce", street="???"), "No availabe data for queried scope.")
  expect_message(agr_get_prov_by_year(year=2020, admin_branch='07', street="???"), "No availabe data for queried scope.")

})



test_that("agr_get_provider wrong argument error", {

  # agr_get_provider(year, provider_code, admin_branch)
  expect_error(agr_get_provider(year=2020, admin_branch='07', unexcpected_arg=3), "unused argument")

  for(wrong in list(18, 2025, 'some sting')){
    expect_error(agr_get_provider(year=wrong, admin_branch='07', provider_code='70603563'), "Wrong format or type of argument\\(s\\):")
  }

  # agr_get_provider(year=2020, admin_branch='07', provider_code='70603563')
  for(wrong in list(777, 'string', '')){
    expect_error(agr_get_provider(year=2020, admin_branch=wrong, provider_code='70603563'), "Wrong format or type of argument\\(s\\):")
  }

  # agr_get_provider(year=2020, admin_branch='07', provider_code="70603563")
  for(wrong in list(70603563, 'some string')){
    expect_error(agr_get_provider(year=2020, admin_branch='07', provider_code=wrong), "Wrong format or type of argument\\(s\\):")
  }

})


test_that("agr_get_serivces wrong argument error", {

  # agr_get_serivces(year, service_type=NULL, service_name=NULL)
  expect_error(agr_get_serivces(year=2020, service_type='04', unexcpected_arg=3), "unused argument")

  for(wrong in list(18, 2025, 'some sting')){
    expect_error(agr_get_serivces(year=wrong), "Wrong format or type of argument\\(s\\):")
  }

  # agr_get_serivces(year=2020, service_type='07')
  for(wrong in list(123, 'string', '')){
    expect_error(agr_get_serivces(year=2020, service_type=wrong), "Wrong format or type of argument\\(s\\):")
  }

  # agr_get_serivces(year=2020, admin_branch='07', provider_name="SZPITAL", town="siedlce")
  for(wrong in list(222)){
    expect_error(agr_get_serivces(year=2020, service_name=wrong), "Wrong format or type of argument\\(s\\):")
  }

})


test_that("agr_get_serivces wrong argument message or warning", {

  expect_message(agr_get_serivces(year=2020, service_name='some string'), "No availabe data for queried scope.")

})


test_that("agr_get_serivces_year wrong argument error", {

  # agr_get_serivces_year(year)
  expect_error(agr_get_serivces_year(year=2020, unexcpected_arg=3), "unused argument")

  for(wrong in list(18, 2025, 'some sting')){
    expect_error(agr_get_serivces_year(year=wrong), "Wrong format or type of argument\\(s\\):")
  }
})


test_that("agr_get_products wrong argument error", {

  # agr_get_products(year, prod_code=NULL, product_name=NULL)
  expect_error(agr_get_products(year=2020, service_type='04', unexcpected_arg=3), "unused argument")

  for(wrong in list(18, 2025, 'some sting')){
    expect_error(agr_get_products(year=wrong), "Wrong format or type of argument\\(s\\):")
  }

  # agr_get_products(year=2020, service_type='07')
  for(wrong in list(03.40000, "03.40000.030.02", "03.4000.3.02", "3.4000.030.02.")){
    expect_error(agr_get_products(year=2020, prod_code=wrong), "Wrong format or type of argument\\(s\\):")
  }

  for(wrong in list(03.40000)){
    expect_error(agr_get_products(year=2020, product_name=wrong), "Wrong format or type of argument\\(s\\):")
  }
})

test_that("agr_get_products wrong argument message or warning", {

  expect_warning(agr_get_products(year=2020, prod_code="03.4000.030"), "Given prod_code length is \\d+ which is")
  expect_warning(agr_get_products(year=2020, prod_code="03.4000.30.02"), "Given prod_code length is \\d+ which is")
  # TODO: check for correct format, also when part of the code provided

})

# source("R/agreements_api_func.R")
# source("R/general_func.R")

# test_that("town Polish language characters intepretation", {
#
#   # org_lang <-  Sys.getlocale("LC_CTYPE")
#   Sys.setlocale("LC_CTYPE", "English_United Kingdom.1252")
#   exp_message <- "No availabe data for queried scope. Please make sure the arguements are correct."
#   expect_message(agr_get_agreements(year=2020, admin_branch='05', town="Łódź", product_code='03.4000.130.02'), exp_message) # trzeba caly kod
#   # Sys.setlocale("LC_CTYPE", org_lang)
#
#
# })


