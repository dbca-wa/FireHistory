#' Metric - Year Since Last Burn
#'
#' `yslb` calculates the year since last burn (yslb) metrics from the data stored
#' in the list constructed from the [FireHistory::assemble_data()] function.
#'
#' The function calculates yslb both spatially and as area statements for the
#' area of interest (aoi) and for the time period defined in the previously constructed
#' data list. The function will create a list object containing four items:
#'
#' * A yslb raster object (class SpatRaster)
#' * A ggplot object of a 'map' showing the yslb raster
#' * A data frame of yslb area stats
#' * A ggplot object of a column plot showing area by yslb
#'
#' Using default parameters, the function also outputs the four items to a
#' created folder ("outputs") in the working directory. All outputs are named by
#' aoi and time period to differentiate products. Note the yslb raster and product
#' are projected to Albers GDA2020 (epsg:9473).
#'
#' @param data a data list object created from [FireHistory::assemble_data()]
#' @param products default TRUE. Renders yslb products to `./outputs`
#'
#' @returns A list containing the four yslb products and if `products = TRUE` a
#' folder containing rendered products.
#'
#' @examples
#' \dontrun{
#' yslb_list <- yslb(data = data_list, products = TRUE)
#' }
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' @import terra
#' @importFrom snakecase to_parsed_case to_mixed_case
#' @importFrom rasterVis gplot
#' @import ggplot2
#' @importFrom dplyr as_tibble mutate rename select
#' @importFrom readr write_csv
#' @importFrom magrittr %>%
#'
#' @export
yslb <- function(data, products = TRUE){
  # yslb
  template <- terra::rast(data[["aoi_alb"]], res = 30)
  yr_rst <- terra::rasterize(data[["fh_alb"]], template,
                             field = "fih_year1", fun = "max")
  yr_crp <- terra::crop(terra::mask(yr_rst, mask = data[["aoi_alb"]]),
                        data[["aoi_alb"]])
  current <- max(data[["fh_alb"]]$fih_year1)
  yslb <- current - yr_crp
  # products
  # folder
  if(!dir.exists("outputs")){dir.create("outputs")}
  # naming
  name <- paste0(snakecase::to_parsed_case(data[["aoi_name"]]), "_",
                 data[["period"]][1], "-", data[["period"]][2], "_")
  # map
  yslb_map <- rasterVis::gplot(yslb) +
    geom_tile(aes(fill = value)) +
    scale_fill_viridis_c(na.value = "transparent", name = "yslb") +
    labs(x = "",
         y = "",
         title = paste0(snakecase::to_mixed_case(name, sep_out = " "), " YSLB"),
         caption = expression(italic("Data: DBCA_Fire_History_DBCA_060"))) +
    coord_equal() +
    theme_bw()
  # stats
  yslb_stats <- dplyr::as_tibble(terra::freq(yslb)) %>%
    dplyr::mutate(area_ha = count * 0.09) %>%
    dplyr::rename(yslb = value) %>%
    dplyr::select(-layer, -count)
  # plot
  yslb_plot <- ggplot(yslb_stats) +
    geom_col(aes(x = yslb, y = area_ha)) +
    labs(x = "years",
         y = "area (ha)",
         title = paste0(snakecase::to_mixed_case(name, sep_out = " "), " YSLB"),
         caption = expression(italic("Data: DBCA_Fire_History_DBCA_060"))) +
    theme_bw()
  yslb_list <- list(yslb = yslb,
                    yslb_map = yslb_map,
                    yslb_stats = yslb_stats,
                    yslb_plot = yslb_plot)
  if(products == TRUE){
    # terra::writeRaster(yslb, paste0("./outputs/", name, "YSLB.tif"))
    # raster work around to get geotiff playing nicely in ArcMAP
    raster::writeRaster(raster::raster(yslb), paste0("./outputs/", name, "YSLB.tif"))
    ggsave(filename = paste0("./outputs/", name, "YSLB_map.png"), yslb_map)
    ggsave(filename = paste0("./outputs/", name, "YSLB_plot.png"), yslb_plot)
    readr::write_csv(yslb_stats, paste0("./outputs/", name, "YSLB_stats.csv"))
  }
  return(yslb_list)
}

#' Metric - Fire Frequency
#'
#' `fire_freq` calculates the fire frequency metrics from the data stored
#' in the list constructed from the [FireHistory::assemble_data()] function.
#'
#' The function calculates fire frequency both spatially and as area statements
#' for the area of interest (aoi) and for the time period defined in the previously
#' constructed data list. The function will create a list object containing four
#' items:
#'
#' * A fire frequency raster object (class SpatRaster)
#' * A ggplot object of a 'map' showing the fire frequency raster
#' * A data frame of fire frequency area stats
#' * A ggplot object of a column plot showing area by fire frequency
#'
#' Using default parameters, the function also outputs the four items to a
#' created folder ("outputs") in the working directory. All outputs are named by
#' aoi and time period to differentiate products. Note the fire frequency raster
#' and product are projected to Albers GDA2020 (epsg:9473).
#'
#' @inheritParams yslb
#'
#' @returns A list containing the four fire frequency products and if
#' `products = TRUE` a folder containing rendered products.
#'
#' @examples
#' \dontrun{
#' ffreq_list <- fire_freq(data = data_list, products = TRUE)
#' }
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' @import terra
#' @importFrom snakecase to_parsed_case to_mixed_case
#' @importFrom rasterVis gplot
#' @import ggplot2
#' @importFrom dplyr as_tibble mutate rename select
#' @importFrom readr write_csv
#' @importFrom magrittr %>%
#'
#' @export
fire_freq <- function(data, products = TRUE){
  template <- terra::rast(data[["aoi_alb"]], res = 30)
  data[["fh_alb"]]$n <- 1
  frq_rst <- terra::rasterize(data[["fh_alb"]], template,
                              field = "n", sum = TRUE)
  fire_frq <- terra::crop(frq_rst, data[["aoi_alb"]], mask = TRUE)

  # products
  # folder
  if(!dir.exists("outputs")){dir.create("outputs")}
  # naming
  name <- paste0(snakecase::to_parsed_case(data[["aoi_name"]]), "_",
                 data[["period"]][1], "-", data[["period"]][2], "_")
  # map
  freq_map <- rasterVis::gplot(fire_frq) +
    geom_tile(aes(fill = value)) +
    scale_fill_viridis_c(na.value = "transparent", name = "Fire Frequency") +
    labs(x = "",
         y = "",
         title = paste0(snakecase::to_mixed_case(name, sep_out = " "), " Fire Frequency"),
         caption = expression(italic("Data: DBCA_Fire_History_DBCA_060"))) +
    coord_equal() +
    theme_bw()
  # stats
  freq_stats <- dplyr::as_tibble(terra::freq(fire_frq)) %>%
    dplyr::mutate(area_ha = count * 0.09) %>%
    dplyr::rename(yslb = value) %>%
    dplyr::select(-layer, -count)
  # plot
  freq_plot <- ggplot(freq_stats) +
    geom_col(aes(x = yslb, y = area_ha)) +
    labs(x = "times burnt in period",
         y = "area (ha)",
         title = paste0(snakecase::to_mixed_case(name, sep_out = " "), " Fire Frequency"),
         caption = expression(italic("Data: DBCA_Fire_History_DBCA_060"))) +
    theme_bw()
  freq_list <- list(fire_freq = fire_frq,
                    fire_freq_map = freq_map,
                    fire_freq_stats = freq_stats,
                    fire_freq_plot = freq_plot)
  if(products == TRUE){
    # terra::writeRaster(fire_frq, paste0("./outputs/", name, "FFREQ.tif"))
    # raster work around to get geotiff playing nicely in ArcMAP
    raster::writeRaster(raster::raster(fire_frq), paste0("./outputs/", name, "FFREQ.tif"))
    ggsave(filename = paste0("./outputs/", name, "FFREQ_map.png"), freq_map)
    ggsave(filename = paste0("./outputs/", name, "FFREQ_plot.png"), freq_plot)
    readr::write_csv(freq_stats, paste0("./outputs/", name, "FFREQ_stats.csv"))
  }
  return(freq_list)
}

#' Metric - Fire Interval
#'
#' `fire_interval` calculates  fire interval metrics from the data stored
#' in the list constructed from the [FireHistory::assemble_data()] function.
#'
#' The function calculates fire intervals both spatially and as area statements
#' for the area of interest (aoi) and for the time period defined in the previously
#' constructed data list. The user has a choice of one of 3 interval measures to
#' choose from. The minimum ("min"), the maximum ("max") or the mean ("mean").
#'
#' The function will create a list object containing four
#' items:
#'
#' * A fire interval raster object of the chosen measure (class SpatRaster)
#' * A ggplot object of a 'map' showing the fire interval raster of the chosen measure
#' * A data frame of fire interval area stats of the chosen measure
#' * A character vector of the user specified measure
#'
#' Using default parameters, the function also outputs the first three items to a
#' created folder ("outputs") in the working directory. All outputs are named by
#' aoi and time period to differentiate products. Note the fire interval raster
#' and product are projected to Albers GDA2020 (epsg:9473).
#'
#' @inheritParams yslb
#' @param measure a character indicating which fire interval measure to calculate.
#' Can be one of "min" for minimum, "max" for maximum or "mean" for mean.
#'
#' @returns A list containing four fire interval products and if
#' `products = TRUE` a folder containing three rendered products.
#'
#' @examples
#' \dontrun{
#' finterval_list <- fire_interval(data = data_list, measure = "min", products = TRUE)
#' }
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' @import terra
#' @import ggplot2
#' @importFrom snakecase to_parsed_case to_mixed_case
#' @importFrom rasterVis gplot
#' @importFrom dplyr as_tibble mutate rename select filter
#' @importFrom readr write_csv
#' @importFrom magrittr %>%
#' @importFrom cli cli_progress_step
#' @importFrom sf st_crop
#' @importFrom stars st_as_stars st_apply
#' @importFrom rlang as_name
#' @importFrom raster writeRaster raster
#'
#' @export
fire_interval <- function(data, measure = c("min", "max", "mean"), products = TRUE){
  # aoi vector
  aoi_dat <- data[["aoi_alb"]]

  # fh vector
  fh_dat <- data[["fh_alb"]]

  # extent raster and stack filler for 0 fires in year
  blnk_rst <- terra::rast(terra::ext(aoi_dat), res = c(30,30), crs = "EPSG:9473")
  blnk_rst[] <- 0
  blnk_rst_mskd <- blnk_rst %>%
    terra::mask(terra::vect(aoi_dat))

  # intended period of search
  yrs <- data$period[1]:data$period[2]

  # measure
  measure <- tolower(measure)

  ## create earliest layer for stack
  cli::cli_progress_step("Stacking fires")
  rst1 <- fh_dat %>%
    dplyr::filter(fih_year1 == yrs[1]) %>%
    dplyr::mutate(n = 1) %>%
    sf::st_crop(blnk_rst) %>%
    terra::vect()

  # catch if 1st year no fire
  if(dim(rst1)[1] == 0){
    rst_stck <- blnk_rst_mskd
  } else {
    rst_stck <- terra::rasterize(rst1, blnk_rst_mskd, field = "n", fun = "first", background = NA_real_)
    rst_stck[is.na(rst_stck)] <- 0
  }

  # name layer
  names(rst_stck) <- yrs[1]

  ## create all other layers for stack
  for(i in 2:length(yrs)){
    yr <- yrs[i]
    rst <- fh_dat %>%
      dplyr::filter(fih_year1 == yr) %>%
      dplyr::mutate(n = 1) %>%
      sf::st_crop(blnk_rst_mskd) %>%
      terra::vect()
    if(dim(rst)[1] == 0){
      rst_out <- blnk_rst_mskd
    } else {
      rst_out <- terra::rasterize(rst, blnk_rst_mskd, field = "n", fun = "first", background = NA_real_)
      rst_out[is.na(rst_out)] <- 0
    }

    # name layer
    names(rst_out) <- yrs[i]

    # add to stack
    rst_stck <- c(rst_stck, rst_out)
  }

  # mask stack to aoi
  rst_stck_mskd <- rst_stck %>%
    terra::mask(terra::vect(aoi_dat))

  # functions for measures
  max_int <- function(x){
    if(all(is.na(x))){
      NA
    } else {
      seq <- rle(as.numeric(x))
      out <- max(seq$lengths[seq$values == 0])
      return(out)
    }
  }
  mean_int <- function(x){
    if(all(is.na(x))){
      NA
    } else {
      seq <- rle(as.numeric(x))
      out <- mean(seq$lengths[seq$values == 0])
      return(out)
    }
  }
  min_int <- function(x){
    if(all(is.na(x))){
      NA
    } else {
      seq <- rle(as.numeric(x))
      out <- min(seq$lengths[seq$values == 0])
      return(out)
    }
  }

  # convert to stars object to minimise calc times
  stars_stck_mskd <- stars::st_as_stars(rst_stck_mskd)

  # calculate measure
  if(measure == "max"){
    cli::cli_progress_step("Calculating maximum interval")
    int_dat <- stars::st_apply(stars_stck_mskd, MARGIN = 1:2, FUN = max_int) %>%
      terra::rast()
  } else {
    if(measure == "mean"){
      cli::cli_progress_step("Calculating mean interval")
      int_dat <- stars::st_apply(stars_stck_mskd, MARGIN = 1:2, FUN = mean_int) %>%
        terra::rast()
    } else {
      if(measure == "min"){
        cli::cli_progress_step("Calculating minimum interval")
        int_dat <- stars::st_apply(stars_stck_mskd, MARGIN = 1:2, FUN = min_int) %>%
          terra::rast()
      } else {
        cli::cli_alert_danger("That measure is not supported. Choose either 'max', 'mean' or 'min'.")
        stop("Choose another measure")
      }
    }
  }

  ## products
  cli::cli_progress_step("Organising products")
  # folder
  if(!dir.exists("outputs")){dir.create("outputs")}
  # naming
  name <- paste0(snakecase::to_parsed_case(data[["aoi_name"]]), "_",
                 data[["period"]][1], "-", data[["period"]][2], "_",
                 measure, "FireInterval")

  # map
  int_map <- rasterVis::gplot(int_dat) +
    geom_tile(aes(fill = value)) +
    scale_fill_viridis_c(na.value = "transparent", name = paste(measure, "\nFire Interval")) +
    labs(x = "",
         y = "",
         title = snakecase::to_mixed_case(name, sep_out = " "),
         caption = expression(italic("Data: DBCA_Fire_History_DBCA_060"))) +
    coord_equal() +
    theme_bw()

  # stats
  rname <- rlang::as_name(measure)
  int_stats <- dplyr::as_tibble(terra::freq(int_dat)) %>%
    dplyr::mutate(area_ha = count * 0.09) %>%
    dplyr::rename(!!rname:= value) %>%
    dplyr::select(-layer, -count)

  # output list
  int_list <- list(interval = int_dat,
                   interval_map = int_map,
                   interval_stats = int_stats,
                   interval_measure = measure)

  if(products == TRUE){
    # terra::writeRaster(int_dat, paste0("./outputs/", name, ".tif"))
    # raster work around to get geotiff playing nicely in ArcMAP
    raster::writeRaster(raster::raster(int_dat), paste0("./outputs/", name, ".tif"))
    ggsave(filename = paste0("./outputs/", name, "_map.png"), int_map)
    readr::write_csv(int_stats, paste0("./outputs/", name, "_stats.csv"))
  }
  return(int_list)
}
