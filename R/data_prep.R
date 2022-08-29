#' Import a user's aoi shape file
#'
#' `user_aoi` reads in a shape file from a file path to be used to query the
#' DBCA Fire History data.
#'
#' Under the hood, `user_aoi`, will on reading in the data, repair geometry and
#' transform the original coordinate reference system to GDA 2020 (Albers
#' Australia, EPSG:7844).
#'
#' @param aoi_path A character vector of the full file path to the user's shape
#' file. It will include the name of the shapefile as well as its file extension.
#'
#' @param name A character vector name representing the aoi. This name will be
#' used in titles of outputs and output file names.
#'
#' @returns A named list containing `aoi` (an sf class object) and `aoi_name`
#' (the name of the aoi).
#'
#'
#'@examples
#' \dontrun{
#' aoi <- user_aoi(aoi_filepath = "C:/path/to/data/myshape.shp", name = "Great Shape")
#'}
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' @import sf
#'
#' @export
user_aoi <- function(aoi_path, name){
  shp <- sf::st_read(dsn = aoi_path, quiet = TRUE)|>
    sf::st_make_valid() |>
    sf::st_transform(7844)
  aoi_list <- list(aoi = shp,
                   aoi_name = name)
  return(aoi_list)
}
