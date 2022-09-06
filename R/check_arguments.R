
#' @title Check for function's arguments
#' @description To be run in local environment (inside another function). Checks whether all required arguments of function run in global env were provided during a function call. If a vector of possible arguments was given also checks whether all provided in a function call arguments are supported.
#' @param gen Indicates on how many environments up is the function call which arguments are to be checked, relative to where check_req_args is called at. Equivalent to sys.parent's 'n' argument.
#' @return  Returns a vector of arguments provided in a function call if all arguments are correct, otherwise function stops
#' @examples
#' \dontrun{
#'  foo <- function(a, b, ...){
#'    given_args = check_req_args(foo, c("a", "b", "c"))
#'    return(given_args)}
#'
#'  foo("a", "b", "c", "d")
#' }
#'
check_req_args <- function(gen=0){

  # browser()
  func_call <- as.list(match.call(definition = sys.function(sys.parent(gen)),
                                  call = sys.call(sys.parent(gen)),
                                  envir = parent.frame(gen)))
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

  return(list(formal_args=required_args, available_args=possible_args, given_args=given_args))
}


#' @title Check for compatibility of LC_CTYPE environment variable
#' @description To be able to download data from NFZ api service LC_CTYPE env. var. has to be set to Polish language. This function checks for that, and suggest code to run, to be able to use NFZ api. If LC_CTYPE doesn't match, function stops the call.
check_env_lang <- function(){
  char_set <- Sys.getlocale("LC_CTYPE")
  if(char_set != "Polish_Poland.1250"){
    message(paste0("Your character set does not support Polish non ASCII characters.",
    "Your LC_CTYPE environment variable has value '", char_set,"', but to be sure that nfzRapi functions work correctly it has to be changed to 'Polish_Poland.1250'.\n
    1. Before using nfzRapi functions run following command: Sys.setlocale('LC_CTYPE', 'Polish_Poland.1250')\n
    2. After using nfzRapi, run following command to restore original setting: Sys.setlocale('LC_CTYPE', '", char_set,"')"))
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
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
  # browser()
  char2 <- c()
  for(ch in char){
    for(letter in unicode_dict){
      # browser()
      change<-names(unicode_dict)[match(letter, unicode_dict)]
      ch<-gsub(letter, change[!is.na(change)], ch)
    }
    char2 <- c(char2, ch)
  }
  # browser()
  return(char2)
}


