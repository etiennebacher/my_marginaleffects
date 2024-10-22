#' @section Global options:
#' 
#' The behavior of `my_marginaleffects` functions can be modified by setting global options.
#' 
#' Disable some safety checks:
#' 
#' ```r
#' options(my_marginaleffects_safe = FALSE)
#' ```
#'
#' Omit some columns from the printed output:
#'
#' ```r
#' options(my_marginaleffects_print_omit = c("p.value", "s.value"))`
#' ```
