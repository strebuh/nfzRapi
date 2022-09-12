
# context("check_req_args")
test_that("check_req_args wrong argument type or value error", {

  expect_error(check_req_args(gen="character"), "A stack number of an active frame/enviornemnt must be integer.")
  expect_error(check_req_args(gen=-1), "A stack number of an active frame/enviornemnt can't be negative.")
  expect_error(check_req_args(gen=1.5), "A stack number of an active frame/enviornemnt must be integer.")

})

# https://stackoverflow.com/questions/8771942/how-can-i-reference-the-local-environment-within-a-function-in-r

# context("convert_pl_signs")
test_that("convert_pl_signs wrong argument type warning", {

  expect_warning(convert_pl_signs(char=-12), "Non-character vector given.")

})


test_that("convert_pl_signs corectness of conversion", {

  expect_identical(convert_pl_signs(char="Ą Ć Ę Ł Ń Ó Ś Ź Ż ą ć ę ł ń ó ś ź ż"),
                 "%C4%84 %C4%86 %C4%98 %C5%81 %C5%83 %C5%93 %C5%9A %C5%B9 %C5%BB %C5%85 %C5%87 %C5%99 %C5%82 %C5%84 %C3%B3 %C5%9B %C5%BA %C5%BC")

})

# context("check_env_lang")
test_that("check_env_lang language settings check", {

  org_lang = Sys.getlocale("LC_CTYPE")
  Sys.setlocale("LC_CTYPE", "Hungarian_Hungary.1250")
  expect_error(check_env_lang(), "The character set defined by LC_CTYPE environemt variable does not support Polish non ASCII characters.")
  Sys.setlocale("LC_CTYPE", org_lang)

})



# context("get_request")
test_that("get_request wrong context call error", {

  expect_error(get_request(available_args=list(year=2020, ow=6), api_type="app-umw-api", schema="agreements"),
               "Wrong context call; get_request should be called within one of higher level functions designed for communication with an API.")

})
