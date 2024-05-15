#' @title Create input raster to AERSURFACE
#' @description Import SURFACE output AERMET
#' @param file Path file input
#' @param export Path file output
#' @return A data frame
#' @export input_aersurface
#' @examples
#' out_tif <- paste0(tempfile(), ".tif")
#' file_tif <- system.file("extdata", "land.tif", package="Raermod")
#' input_aersurface(file_tif, out_tif)
#' terra::rast(out_tif)
#'

input_aersurface <- function(file, export){

  identNAmargin <- function(x){
    x_matrix <- as.matrix(x)
    x_dim <- dim(x_matrix)
    na_check_row <- as.vector(x_matrix[c(1,x_dim[1]), ])
    na_check_col <- as.vector(x_matrix[, c(1,x_dim[2])])
    na_count <- sum(is.na(c(na_check_row, na_check_col)))
    cat("Values with NA in margin: ", na_count, "\n")
    if(na_count > 0){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  removeNAmargin <- function(x){
    id_na <- identNAmargin(x)
    while(id_na){
      res_x <- terra::res(x)
      x_modified <- x
      delNA = -1
      extent_delta = c(delNA,delNA,delNA,delNA)
      # Crop
      ul_mod <- extent_delta[c(1,3)] * res_x
      ul_mod[ul_mod > 0] <- 0
      lr_mod <- extent_delta[c(2,4)] * res_x
      lr_mod[lr_mod > 0] <- 0
      crop_extent <- c(terra::xmin(x), terra::xmax(x),
                       terra::ymin(x), terra::ymax(x))
      crop_extent[c(1,3)] <- crop_extent[c(1,3)] - ul_mod
      crop_extent[c(2,4)] <- crop_extent[c(2,4)] + lr_mod

      x <- terra::crop(x,crop_extent)
      id_na <- identNAmargin(x)
    }
    return(x)
  }

  cat("Convert landcover WGS84 to NAD83\n")
  lc_rst <- terra::rast(file)

  crs_txt <- terra::crs(lc_rst)
  crs_txt <- strsplit(crs_txt, "\n")[[1]]
  crs_txt_ref <- substr(crs_txt[1], 10, 15)

  if(crs_txt_ref != "WGS 84"){
    cat(crs_txt$input, "\n")
    stop("The raster must be in WGS 84")
  }

  proj_txt <- substr(crs_txt[1], 19, 21)

  if(proj_txt == "UTM"){
    lc_rst_ll <- terra::project(lc_rst, terra::crs("+init=epsg:4326"),
                                method = "near")
  } else {
    lc_rst_ll <- lc_rst
  }

  cat("Create personal projection...\n")
  lon_0 = (terra::xmin(lc_rst_ll) + terra::xmax(lc_rst_ll)) / 2
  lat_0 = (terra::ymin(lc_rst_ll) + terra::ymax(lc_rst_ll)) / 2
  lat_1 = lat_0 + 5
  lat_2 = lat_0 - 5
  srUser = paste0("+proj=aea +lat_1=", as.character(lat_1),
                  " +lat_2=", as.character(lat_2),
                  " +lat_0=", as.character(lat_0),
                  " +lon_0=", as.character(lon_0),
                  " +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
                  " ellps=WGS84 +towgs84=0,0,0")

  cat("Create input landcover for aersurface..\n")

  lc_user <- terra::project(lc_rst_ll, srUser,
                            res = c(30,30), method = "near")

  lc_user <- removeNAmargin(lc_user)

  terra::writeRaster(lc_user, export, overwrite = T,
                     datatype="INT1U", gdal = c("COMPRESS=NONE"))

  return(lc_user)
}
