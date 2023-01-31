#' @title Create domain and parameters to "gridcard" AERMOD
#' @description Create a shape object (sfc) and parameters of GRIDCARD keyword to AERMOD.
#' @param coords Numeric vector with coordinates center (X, Y)
#' @param utm Numeric value of UTM zone for DATUM WGS84
#' @param ext Numeric vector with extension in X and Y, or one numeric value to duplicated fo X and Y.
#' @param res Numeric vector with resolution in X and Y, or one numeric value to duplicated fo X and Y.
#' @param pts Numeric vector with points in X and Y, or one numeric value to duplicated fo X and Y.
#' @return A list with parameters and shape object
#' @export create_domian
#' @examples
#' create_domian(coords=c(313860, 8617880), ext=4, res=50)
#'

create_domian <- function(coords, utm=18, ext=NULL, res=NULL, pts=NULL){
  checkExt <- function(x, y, txt){
    if(x%%1 > 0){
      x1 <- ceiling(x)
      x2 <- (x1 - x)*y
      cat("The final grid in", txt, "is", x2, "meters bigger to grid original\n")
    } else {
      x1 <- x
    }
    return(x1)
  }
  # ext in km
  if (is.null(pts)){
    # Check extension
    if(length(ext)==1){
      ext <- c(ext, ext)
    }
    ext <- ext*1000
    # Check resolution
    if(length(res)==1){
      res <- c(res, res)
    }
    Xdelta = res[1]
    Ydelta = res[2]
    Xnum <- checkExt(ext[1]/res[1] + 1, res[1], "X")
    Ynum <- checkExt(ext[2]/res[2] + 1, res[2], "Y")
  } else if(is.null(res)){
    # Check extension
    if(length(ext)==1){
      ext <- c(ext, ext)
    }
    ext <- ext*1000
    if(length(pts)==1){
      pts <- c(pts, pts)
    }
    # Check points in X
    if(pts[1]%%1 > 0){
      stop("Points in X must be integer")
    }
    if(pts[2]%%1 > 0){
      stop("Points in Y must be integer")
    }
    # Check resolution
    Xnum = pts[1]
    Ynum = pts[2]
    Xdelta = checkExt(ext[1]/(pts[1]-1), pts[1]-1, "X")
    Ydelta = checkExt(ext[2]/(pts[2]-1), pts[2]-1, "Y")
  } else if(is.null(ext)){
    if(length(pts)==1){
      pts <- c(pts, pts)
    }
    if(length(res)==1){
      res <- c(res, res)
    }
    # Check points in X
    if(pts[1]%%1 > 0){
      stop("Points in X must be integer")
    }
    if(pts[2]%%1 > 0){
      stop("Points in Y must be integer")
    }
    Xnum = pts[1]
    Ynum = pts[2]
    Xdelta = res[1]
    Ydelta = res[2]
  }
  ext[1] = (Xnum-1)*Xdelta
  ext[2] = (Ynum-1)*Ydelta
  Xinit = coords[1] - ext[1]/2
  Yinit = coords[2] - ext[2]/2
  cat("Points total:", Xnum*Ynum, "\n")
  cat("Extension: ", ext[1]/1000, "km in X ;", ext[2]/1000,"km in Y  \n")

  # Create sf object
  utm = 32700 + utm
  points_sf <- c(Xinit, Xinit, Xinit+ext[1], Xinit+ext[1], Xinit,
                 Yinit, Yinit+ext[2], Yinit+ext[2], Yinit, Yinit) |>
    matrix(ncol=2) |> list() |> sf::st_polygon() |> sf::st_sfc(crs=utm) |>
    sf::st_as_sf(domain=1)
  sf::st_geometry(points_sf) <- "geometry"

  return(list(PARAM=c("Xinit"=Xinit, "Xnum"=Xnum, "Xdelta"=Xdelta,
                      "Yinit"=Yinit, "Ynum"=Ynum, "Ydelta"=Ydelta),
              SHP = points_sf))
}
