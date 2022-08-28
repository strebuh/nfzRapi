
#' @title Check for function's arguments
#' @description To be run in local environment (inside another function). Checks whether all required arguments of function run in global env were provided during a function call. If a vector of possible arguments was given also checks whether all provided in a function call arguments are supported.
#' @param func The main function bare name
#' @param possible_args A vector of all available but not required arguments names (acceptable optional arguments defined as '...')
#' @return  Returns a vector of arguments provided in a function call if all arguments are correct, otherwise function stops
#' @examples
#' check_req_args(a, b) <- example to run
#' \dontrun{
#'  foo <- function(a, b, ...){
#'    given_args = check_req_args(foo, c("a", "b", "c"))
#'    return(given_args)}
#'
#'  foo("a", "b", "c", "d")
#' }
#'
check_req_args <- function(func, possible_args=NULL){

  given_args = as.list(match.call(definition = sys.function(sys.parent(1)),
                                  call = sys.call(sys.parent(1)),
                                  envir = parent.frame(1L)))
  given_args = given_args[2:length(given_args)]
  required_args <- setdiff(formalArgs(func), "...")

  msg = ""
  missing_args <- setdiff(required_args, names(given_args))
  if(!rlang::is_empty(missing_args)){
    msg = paste0(msg, paste0("You did not provide all reqired arguments. Missing: ", paste(missing_args, collapse = ", ")))
  }


  if(!is.null(possible_args)){
    wrong_arguments = setdiff(names(given_args), possible_args)
    if("" %in% wrong_arguments){
      msg = paste(msg, "OPTIONAL ARGUMENTS NEED TO BE NAMED!", paste0("Possible arguments are: ", paste(possible_args, collapse = ", ")), sep="\n")
    }

    wrong_arguments = setdiff(wrong_arguments, "")
    if(!rlang::is_empty(wrong_arguments)){
      msg = paste(msg, paste0("Some of provided arguments are incorrect: ", paste(wrong_arguments, collapse = ", ")), sep="\n")
    }
  }

  if(msg!=""){
    message(msg)
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }
  return(given_args)
}


#' @title Check for compatibility of LC_CTYPE environment variable
#' @description To be able to download data from NFZ api service LC_CTYPE env. var. has to be set to Polish language. This function checks for that, and suggest code to run, to be able to use NFZ api. If LC_CTYPE doesn't match, function stops the call.
check_env_lang <- function(){
  char_set = Sys.getlocale("LC_CTYPE")
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



#' @title Convert Polish non ASCII characters to NFZapi accpted utf-8 representation of Unicode
#' @description Function accpets a string and converts Polish diacretic non ASCII characters
#' @param char A character where the signs are to be converted
#' @return  Returns a character vector
#' @examples
#' check_req_args(a, b) <- example to run
#' \dontrun{
#'  convert_pl_signs("Gąśłękówiżań")
#' }
#'
convert_pl_signs <- function(char){ #, to_pl_lc_type=F

  unicode_dict = list(
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
  char2 = c()
  for(ch in char){
    for(letter in unicode_dict){
      # browser()
      change=names(unicode_dict)[match(letter, unicode_dict)]
      ch=gsub(letter, change[!is.na(change)], ch)
    }
    char2 = c(char2, ch)
  }
  # browser()
  return(char2)
}


