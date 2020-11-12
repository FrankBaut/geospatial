#' Remove decimals from a value (significant numbers)
#'
#' @param x float value
#' @param ...
#' @param prec number of removed decimals in the x value
#'
#' @return
#' @export
#'
#' @examples
#' x<-129.317313
#' trunc(x,prec=4)
trunc <- function(x, ..., prec = 0){
  base::trunc(x * 10^prec, ...) / 10^prec
  }
