#' @title Estimate NO2 with NOX/NO2 ratop
#' @description Estimate NO2 with NOX/NO2 ratop
#' @param nox Concentration NOx in ug/m3
#' @param rmin Minimum ratio
#' @param rmax Maximum ratio
#' @param aprof Profile of ratio, must be 'EPA', 'All', 'Urban', 'Rural', 'Industrial' or 'Coastal'
#' @return A vector numeric with NO2 cconcnetration
#' @export create_domian
#' @examples
#' no2 <- ARM2(1:10)
#' print(no2)
#'

ARM2 <- function(nox, rmin=0.5, rmax=0.9, aprof="EPA"){
  aprof <- toupper(aprof)
  if(aprof == "EPA"){
    C0 = +1.2441E+00
    C1 = -2.7383E-03
    C2 = -5.6062E-06
    C3 = +3.4555E-08
    C4 = -5.8345E-11
    C5 = +4.2795E-14
    C6 = -1.1723E-17
  } else if(aprof == "ALL"){
    C0 = +1.4217e+00
    C1 = -9.0043e-03
    C2 = +2.8689e-05
    C3 = -5.1310e-08
    C4 = +6.2556e-11
    C5 = -5.5299e-14
    C6 = +2.4169e-17
  } else if(aprof == "URBAN"){
    C0 = +1.4081e+00
    C1 = -8.4309e-03
    C2 = +2.2008e-05
    C3 = -1.8692e-08
    C4 = -1.4082e-11
    C5 = +2.976e-14
    C6 = -1.1526e-17
  } else if(aprof == "RURAL"){
    C0 = +7.0908e-01
    C1 = +1.8014e-02
    C2 = -4.0219e-04
    C3 = +3.1248e-06
    C4 = -1.1639e-08
    C5 = +2.0910e-11
    C6 = -1.4534e-14
  } else if(aprof == "INDUSTRIAL"){
    C0 = +9.7054e-01
    C1 = +2.7563e-03
    C2 = -8.0316e-05
    C3 = +4.4204e-07
    C4 = -1.0885e-09
    C5 = +1.2663e-12
    C6 = -5.6578e-16
  } else if(aprof == "COASTAL"){
    C0 = +1.4097e+00
    C1 = -8.6617e-03
    C2 = +2.6443e-05
    C3 = -5.9049e-08
    C4 = +1.3159e-10
    C5 = -2.0684e-13
    C6 = +1.3132e-16
  } else {
    stop("'aprof' must be 'EPA', 'All', 'Urban', 'Rural', 'Industrial' or 'Coastal'")
  }

  ratio = C0 + C1*nox + C2*(nox**2) + C3*(nox**3) + C4*(nox**4) + C5*(nox**5) + C6*(nox**6)

  print(ratio)

  if(ratio < rmin){
    ratio = rmin
  }
  if(ratio > rmax){
    ratio = rmax
  }
  return(nox*ratio)
}
ARM2 <- Vectorize(ARM2, "nox")
