#' @title Read SFC file
#' @description Import SURFACE output AERMET
#' @param file Path file
#' @return A data frame
#' @export read_sfc
#' @examples
#' file_sfc <- system.file("extdata", "AERMET2.SFC", package="Raermod")
#' read_sfc(file_sfc)
#'

read_sfc <- function(file){
  sfc <- utils::read.table(file, header = FALSE, skip = 1, fill = TRUE)
  names(sfc) <- c("YR"  , "MO"  , "DY"  , "JDAY", "HR"  , "HFLX", "USTR",
                  "WSTR", "DTDZ", "Z_IC", "Z_IM", "L_MO", "Z0_R", "BWNR",
                  "ALBD", "WSPD", "WDIR", "WHGT", "TEMP", "THGT", "PRCD",
                  "PRCP", "RHUM", "PRES", "CLDC", "WSADJ", "FLAG")
  sfc$PRCD <- as.factor(sfc$PRCD)
  # Considerar valores nulos
  sfc$WSTR <- ifelse(sfc$WSTR == -9, NA, sfc$WSTR)
  sfc$DTDZ <- ifelse(sfc$DTDZ == -9, NA, sfc$DTDZ)
  sfc$Z_IC <- ifelse(sfc$Z_IC == -999, NA, sfc$Z_IC)
  sfc$Z_IM <- ifelse(sfc$Z_IM == -999, NA, sfc$Z_IM)
  sfc$L_MO <- ifelse(sfc$L_MO == -99999, NA, sfc$L_MO)
  sfc$WSPD <- ifelse(sfc$WSPD == 999, NA, sfc$WSPD)
  #
  # Corregir el formato de hora. De 1-24 a 0-23
  sfc$HR <- sfc$HR - 1
  #
  date <- paste(sfc$YR, sfc$MO, sfc$DY, sfc$HR, sep = "-")
  date <- as.POSIXct(strptime(date, format = "%y-%m-%d-%H"))
  sfc <- sfc[, 6:27]
  sfc <- cbind(date, sfc)
  return(sfc)
}
