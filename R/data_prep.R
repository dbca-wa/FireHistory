#' Import a user's area of interest shape file
#'
#' `user_aoi` reads in a shape file from a file path to be used to query the
#' DBCA Fire History dataset.
#'
#' Under the hood, `user_aoi`, will on reading in the data, repair geometry and
#' transform the original coordinate reference system to GDA 2020 (Geodetic,
#' EPSG:7844).
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

#' Find a DBCA tenure name
#'
#' `find_tenure` helps find the correct tenure name form all of the DBCA estate.
#'
#' Used with the default parameter, `find_tenure` returns a character vector of
#' the 1000+ full names of all tenure in the DBCA estate. A character vector can
#' be used for a fuzzy search, returning all close matches. Use this function to
#' obtain the correct search term for [FireHistory::DBCA_aoi()].
#'
#' @param like default is NULL which returns all DBCA tenure names. Use a
#' character vector to find matches. Case ambivalent.
#'
#' @returns A character vector containg one or many DBCA tenure names.
#'
#'@examples
#' \dontrun{
#' all_tenure <- find_tenure(like = NULL)
#'
#' choice <- find_tenure(like = "wandoo")[1]
#' }
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' @export
find_tenure <- function(like = NULL){
  if(is.null(like)){
    return(DBCA_tenure)
  } else {
    lc <- tolower(like)
    result <- DBCA_tenure[grepl(lc, DBCA_tenure, ignore.case = TRUE)]
    return(result)
  }
}

#' Make a DBCA tenure area of interest
#'
#' `DBCA_aoi` construct a spatial object from DBCA tenure which will be used to
#' query the DBCA Fire History dataset.
#'
#' @param choice a  character vector of a single DBCA tenure name as determined by
#' previously running [FireHistory::find_tenure()].
#'
#' @returns A named list containing `aoi` (an sf class object) and `aoi_name`
#' (the tenure name of the aoi).
#'
#' @examples
#' \dontrun{
#' aoi <- DBCA_aoi(choice = "Wandoo National Park")
#' }
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' @importFrom sf st_transform
#' @importFrom dplyr filter
#'
#' @export
DBCA_aoi <- function(choice){
  aoi <- tenure_shp |>
    sf::st_transform(7844) |>
    dplyr::filter(LEG_NAME %in% choice)
  aoi_list <- list(aoi = aoi,
                   aoi_name = choice)
  return(aoi_list)
}

#' Make well known text string from bounding box of an area of interest
#'
#' `make_wkt` translates a bounding box of an area of interest (aoi) into WKT for
#' use in filtering the DBCA Fire History dataset.
#'
#' @param aoi an aoi list object created from running either [FireHistory::user_aoi()]
#' or [FireHistory::DBCA_aoi()].
#'
#' @param fh_crs crs of the Fire History input data.
#'
#' @returns A WKT representation of the area of interest's bounding box.
#'
#' @examples
#' \dontrun{
#' wkt_filter <- make_wkt(aoi = aoi)
#' }
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' @import sf
make_wkt <- function(aoi, fh_crs){
  aoi_wkt <- sf::st_transform(aoi[['aoi']], fh_crs) |>
    sf::st_bbox() |>
    sf::st_as_sfc() |>
    sf::st_geometry() |>
    sf::st_as_text()
  return(aoi_wkt)
}

#' Assemble data for calculating fire metrics
#'
#' `assemble_data` brings together the DBCA Fire History data, the area of
#' interest (aoi) and the aoi name into a list.
#'
#' Under the hood, `assemble_data` reads from the very large Fire History shape
#' file only those polygons that intersect the aoi. The user must have previously
#' downloaded the Fire History shape file [DBCA_Fire_History_DBCA_060](https://catalogue.data.wa.gov.au/dataset/dbca-fire-history).
#' The function stores the subset of the Fire History, the aoi and
#' the aoi name in a named list which will be used by other functions for
#' calculating various fire metrics. Note all spatial objects have class SpatVector
#' and data is now reprojected to Albers GDA2020 (epsg:9473).
#'
#' @param fire_path a character vector of the full file path to the previously
#' downloaded DBCA Fire History DBCA 060 shape file (see details). File path includes
#' file extension ".shp"
#' @param from numeric representing the starting year for the analysis and
#' generation of metrics.
#' @param to numeric representing the current year for the analysis and
#' generation of metrics.
#' @param aoi the aoi object previously created by using either [FireHistory::user_aoi()]
#' or [FireHistory::DBCA_aoi()]
#'
#' @returns A list containing the spatially and temporally subsetted Fire History
#' polygons, the aoi polygon/s, the aoi name and period. Spatial data are of class
#' SpatVector and are reprojected to Albers GDA2020 (epsg:9473). Note if there is no
#' data in the Fire History for the aoi, the function returns an error with a
#' message.
#'
#' @examples
#' \dontrun{
#' fie_data <- assemble_data(fire_path = "C:/path/to/data/DBCA_Fire_History_DBCA_060.shp",
#' from = 1988, to = 2022, aoi = aoi)
#' }
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' @import sf
#' @importFrom terra vect
#'
#' @export
assemble_data <- function(fire_path, from, to, aoi){
  # find crs of fh input
  fname <- tools::file_path_sans_ext(basename(fire_path))
  fquery <- paste0('SELECT * from ', fname, ' LIMIT 1')
  fh_crs <- sf::st_crs(sf::read_sf(fire_path, query = fquery))
  # match crs and proceed
  wkt_flt <- make_wkt(aoi, fh_crs)
  fh <- sf::st_read(dsn = fire_path, quiet = TRUE, wkt_filter = wkt_flt)
  names(fh) <- tolower(names(fh))
  if(dim(fh)[1] != 0){
    fh_alb <- fh |>
      dplyr::filter(fih_year1 >= from & fih_year1 <= to) |>
      sf::st_make_valid() |>
      sf::st_transform(9473) |>
      terra::vect()
    aoi_alb <- aoi[['aoi']] |>
      sf::st_make_valid() |>
      sf::st_transform(9473) |>
      terra::vect()
    dat_list <- list(fh_alb = fh_alb,
                     aoi_alb = aoi_alb,
                     aoi_name = aoi[['aoi_name']],
                     period = c(from,to))
  } else {
    stop("There is no fire history data for that location")
  }
  return(dat_list)
}
