% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/general_func.R
\name{check_env_lang}
\alias{check_env_lang}
\title{Check for compatibility of LC_CTYPE environment variable}
\usage{
check_env_lang()
}
\description{
To be able to download data from NFZ api service LC_CTYPE env. var. has to be set to Polish language. This function checks for that, and suggest code to run, to be able to use NFZ api. If LC_CTYPE doesn't match, function stops the call.
}
