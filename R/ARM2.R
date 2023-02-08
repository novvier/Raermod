#' @title Estimate NO2 with NOX/NO2 ratop
#' @description Estimate NO2 with NOX/NO2 ratop
#' @param nox Concentration NOx in ug/m3
#' @param rmin Minimum ratio
#' @param rmax Maximum ratio
#' @return A vector numeric with NO2 cconcnetration
#' @export create_domian
#' @examples
#' no2 <- ARM2(1:10)
#' print(no2)
#'

ARM2 <- function(nox, rmin=0.5, rmax=0.9){
  ratio = -1.1723E-17*(nox**6) + 4.2795E-14*(nox**5) - 5.8345E-11*(nox**4) +
    3.4555E-08*(nox**3) - 5.6062E-06*(nox**2) - 2.7383E-03*(nox) + 1.2441E+00
  if(ratio < rmin){
    ratio = rmin
  }
  if(ratio > rmax){
    ratio = rmax
  }
  print(ratio)
  return(nox*ratio)
}
ARM2 <- Vectorize(ARM2, "nox")
