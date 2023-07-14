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
#' @importFrom cli cli_progress_step
#' @importFrom dplyr as_tibble mutate rename select
#' @importFrom readr write_csv
#' @importFrom magrittr %>%
#'
#' @export
yslb <- function(data, products = TRUE){
  # yslb
  cli::cli_progress_step("Calculating YSLB")
  # template for all rasters
  template <- terra::rast(data[["aoi_alb"]], res = 30)
  # fh vector
  vec <- terra::vect(data[["fh_alb"]])
  # aoi raster mask
  aoi_msk <- data[["aoi_msk"]]
  # rasterize fire history and crop/mask to aoi
  yr_crp <- vec[order(vec$fin_y), ] %>% 
    terra::rasterize(template, field = "fin_y") %>%
    terra::crop(aoi_msk, mask = TRUE)
  # take current from user specified date range
  current <- data[["FYperiod"]][2]
  # calculate yslb
  yslb <- current - yr_crp
  
  # find unburnt
  zero_msk <- aoi_msk %>%
    terra::subst(1, 0) %>%
    terra::crop(yslb) %>%
    terra::mask(yslb, inverse = TRUE)
  
  # currently can't plot the zero (unknown) with sensible legend - sticking with
  # vector at present for display
  zero_vec <- terra::as.polygons(zero_msk)
  
  # products
  cli::cli_progress_step("Organising products")
  
  # naming
  name <- paste0(snakecase::to_parsed_case(data[["aoi_name"]]), "_",
                 data[["FYperiod"]][1], "-", data[["FYperiod"]][2], "_")
  # data caption
  ddate <- data[["data_date"]]
  dcap <- paste0("Data: DBCA_Fire_History_DBCA_060\nDownloaded on ", ddate)
  
  
  # yslb map spat rast and vect
  yslb_map <- ggplot(zero_vec) +
    tidyterra::geom_spatraster(data = yslb) +
    tidyterra::geom_spatvector(aes(color = "grey")) +
    scale_color_discrete(name = "unburnt",
                         labels = "") +
    scale_fill_viridis_c(na.value = "transparent", name = "yslb") +
    labs(x = "",
         y = "",
         title = paste0(snakecase::to_mixed_case(name, sep_out = " "), " YSLB"),
         caption = dcap) +
    coord_sf(crs = 9473) +
    theme_bw()
  
  
  # stats
  aoi_area <- dplyr::as_tibble(terra::freq(aoi_msk)) %>%
    dplyr::mutate(aoi_area = count * 0.09) %>%
    dplyr::pull(aoi_area)
  
  unburnt <- dplyr::as_tibble(terra::freq(zero_msk)) %>%
    dplyr::mutate(value = "unknown",
                  area_ha = count * 0.09)
  
  yslb_stats <- dplyr::as_tibble(terra::freq(yslb)) %>%
    dplyr::mutate(value = as.character(value),
                  area_ha = count * 0.09) %>%
    dplyr::bind_rows(unburnt) %>%
    dplyr::mutate(aoi_ha = aoi_area,
                  percent = area_ha/aoi_area * 100) %>%
    dplyr::rename(yslb = value) %>%
    dplyr::select(-layer, -count)
  
  lvs <- yslb_stats[['yslb']] # levels for factor conversion to aid ordering in plot
  
  # plot
  yslb_plot <- ggplot(yslb_stats) +
    geom_col(aes(x = factor(yslb, levels = lvs, ordered = TRUE), y = area_ha)) +
    labs(x = "years",
         y = "area (ha)",
         title = paste0(snakecase::to_mixed_case(name, sep_out = " "), " YSLB"),
         caption = dcap) +
    theme_bw()
  
  yslb_list <- list(yslb = yslb,
                    yslb_map = yslb_map,
                    yslb_stats = yslb_stats,
                    yslb_plot = yslb_plot)
  
  if(products == TRUE){
    # folder
    if(!dir.exists("outputs")){dir.create("outputs")}
    # terra::writeRaster(yslb, paste0("./outputs/", name, "YSLB.tif"))
    # raster work around to get geotiff playing nicely in ArcMAP
    raster::writeRaster(raster::raster(yslb), paste0("./outputs/", name, "YSLB.tif"))
    suppressMessages(ggsave(filename = paste0("./outputs/", name, "YSLB_map.png"), yslb_map))
    suppressMessages(ggsave(filename = paste0("./outputs/", name, "YSLB_plot.png"), yslb_plot))
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
#' @importFrom cli cli_progress_step
#' @importFrom dplyr as_tibble mutate rename select
#' @importFrom readr write_csv
#' @importFrom magrittr %>%
#'
#' @export
fire_freq <- function(data, products = TRUE){
  # fire freq
  cli::cli_progress_step("Calculating fire frequency")
  # template for all rasters
  template <- terra::rast(data[["aoi_alb"]], res = 30)
  data[["fh_alb"]]$n <- 1
  # aoi raster mask
  aoi_msk <- data[["aoi_msk"]]
  fire_frq <- terra::rasterize(data[["fh_alb"]], template,
                               field = "n", fun = "sum") %>%
    terra::crop(aoi_msk, mask = TRUE)
  
  # find unburnt
  zero_msk <- aoi_msk %>%
    terra::subst(1, 0) %>%
    terra::crop(fire_frq) %>%
    terra::mask(fire_frq, inverse = TRUE)
  
  # products
  cli::cli_progress_step("Organising products")
  
  # naming
  name <- paste0(snakecase::to_parsed_case(data[["aoi_name"]]), "_",
                 data[["FYperiod"]][1], "-", data[["FYperiod"]][2], "_")
  
  # data caption
  ddate <- data[["data_date"]]
  dcap <- paste0("Data: DBCA_Fire_History_DBCA_060\nDownloaded on ", ddate)
  
  # map
  freq_map <- ggplot() +
    tidyterra::geom_spatraster(data = sum(fire_frq, zero_msk, na.rm = TRUE)) +
    
    scale_fill_viridis_c(na.value = "transparent", name = "Fire Frequency") +
    labs(x = "",
         y = "",
         title = paste0(snakecase::to_mixed_case(name, sep_out = " "), " Fire Frequency"),
         caption = dcap) +
    coord_sf(crs = 9473) +
    theme_bw()
  
  # stats
  aoi_area <- dplyr::as_tibble(terra::freq(aoi_msk)) %>%
    dplyr::mutate(aoi_area = count * 0.09) %>%
    dplyr::pull(aoi_area)
  
  unburnt <- dplyr::as_tibble(terra::freq(zero_msk)) %>%
    dplyr::mutate(value = 0,
                  area_ha = count * 0.09)
  
  freq_stats <- dplyr::as_tibble(terra::freq(fire_frq)) %>%
    dplyr::mutate(value = value,
                  area_ha = count * 0.09) %>%
    dplyr::bind_rows(unburnt) %>%
    dplyr::mutate(aoi_ha = aoi_area,
                  percent = area_ha/aoi_area * 100) %>%
    dplyr::rename(n = value) %>%
    dplyr::arrange(n) %>%
    dplyr::select(-layer, -count)
  
  # plot
  freq_plot <- ggplot(freq_stats) +
    geom_col(aes(x = n, y = area_ha)) +
    labs(x = "times burnt in period",
         y = "area (ha)",
         title = paste0(snakecase::to_mixed_case(name, sep_out = " "), " Fire Frequency"),
         caption = dcap) +
    theme_bw()
  freq_list <- list(fire_freq = fire_frq,
                    fire_freq_map = freq_map,
                    fire_freq_stats = freq_stats,
                    fire_freq_plot = freq_plot)
  if(products == TRUE){
    # folder
    if(!dir.exists("outputs")){dir.create("outputs")}
    # terra::writeRaster(fire_frq, paste0("./outputs/", name, "FFREQ.tif"))
    # raster work around to get geotiff playing nicely in ArcMAP
    raster::writeRaster(raster::raster(fire_frq), paste0("./outputs/", name, "FFREQ.tif"))
    suppressMessages(ggsave(filename = paste0("./outputs/", name, "FFREQ_map.png"), freq_map))
    suppressMessages(ggsave(filename = paste0("./outputs/", name, "FFREQ_plot.png"), freq_plot))
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
#' @importFrom fasterize fasterize
#'
#' @export
fire_interval <- function(data, measure = c("min", "max", "mean"), products = TRUE){
  # aoi raster mask
  aoi_msk <- data[["aoi_msk"]]
  
  # fh vector
  fh_dat <- data[["fh_alb"]]
  
  # extent raster and stack filler for 0 fires in year
  blnk_rst <- terra::rast(terra::ext(aoi_msk), res = c(30,30), crs = "EPSG:9473")
  blnk_rst[] <- 0
  blnk_rst_mskd <- blnk_rst %>%
    terra::crop(aoi_msk, mask = TRUE)
  
  # intended period of search
  yrs <- data[["FYperiod"]][1]:data[["FYperiod"]][2]
  
  # measure
  measure <- tolower(measure)
  
  ## create earliest layer for stack
  cli::cli_progress_step("Stacking fires")
  # create sf version for fasterize
  suppressWarnings(
    rst1sf <- fh_dat %>%
      dplyr::filter(fin_y == yrs[1]) %>%
      dplyr::mutate(n = 1) %>%
      sf::st_crop(blnk_rst) %>%
      sf::st_make_valid() %>%
      sf::st_cast()
  )
  
  # catch if 1st year no fire
  if(dim(rst1sf)[1] == 0){
    rst_stck <- blnk_rst_mskd
  } else {
    rst_stck <- fasterize::fasterize(rst1sf, raster::raster(blnk_rst_mskd), 
                                     field = "n", fun = "first",
                                     background = NA_real_) %>%
      terra::rast()
    rst_stck[is.na(rst_stck)] <- 0
  }
  
  # name layer
  names(rst_stck) <- yrs[1]
  
  ## create all other layers for stack
  for(i in 2:length(yrs)){
    yr <- yrs[i]
    # create sf version for fasterize
    suppressWarnings(
      rstsf <- fh_dat %>%
        dplyr::filter(fih_year1 == yr) %>%
        dplyr::mutate(n = 1) %>%
        sf::st_crop(blnk_rst_mskd) %>%
        sf::st_make_valid() %>%
        sf::st_cast()
    )
    
    if(dim(rstsf)[1] == 0){
      rst_out <- blnk_rst_mskd
    } else {
      rst_out <- fasterize::fasterize(rstsf, raster::raster(blnk_rst_mskd), 
                                      field = "n", fun = "first", 
                                      background = NA_real_)  %>%
        terra::rast()
      rst_out[is.na(rst_out)] <- 0
    }
    
    # name layer
    names(rst_out) <- yrs[i]
    
    # add to stack
    rst_stck <- c(rst_stck, rst_out)
  }
  
  # mask stack to aoi
  rst_stck_mskd <- rst_stck %>%
    terra::crop(aoi_msk, mask = TRUE)
  
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
  
  # naming
  name <- paste0(snakecase::to_parsed_case(data[["aoi_name"]]), "_",
                 data[["FYperiod"]][1], "-", data[["FYperiod"]][2], "_",
                 measure, "FireInterval")
  
  # data caption
  ddate <- data[["data_date"]]
  dcap <- paste0("Data: DBCA_Fire_History_DBCA_060\nDownloaded on ", ddate)
  
  # map
  int_map <- ggplot() +
    tidyterra::geom_spatraster(data = int_dat) +
    scale_fill_viridis_c(na.value = "transparent", name = "Fire Frequency") +
    labs(x = "",
         y = "",
         title = snakecase::to_mixed_case(name, sep_out = " "),
         caption = dcap) +
    coord_sf(crs = 9473) +
    theme_bw()
  
  # stats
  rname <- rlang::as_name(measure)
  
  aoi_area <- dplyr::as_tibble(terra::freq(aoi_msk)) %>%
    dplyr::mutate(aoi_area = count * 0.09) %>%
    dplyr::pull(aoi_area)
  
  int_stats <- dplyr::as_tibble(terra::freq(int_dat)) %>%
    dplyr::mutate(area_ha = count * 0.09,
                  aoi_ha = aoi_area,
                  percent = area_ha/aoi_area * 100) %>%
    dplyr::rename(!!rname:= value) %>%
    dplyr::select(-layer, -count)
  
  # plot
  int_plot <- ggplot(int_stats) +
    geom_col(aes(x = .data[[measure]], y = area_ha)) +
    labs(x = "years relating to interval",
         y = "area (ha)",
         title = snakecase::to_mixed_case(name, sep_out = " "),
         caption = dcap) +
    theme_bw()
  
  # output list
  int_list <- list(interval = int_dat,
                   interval_map = int_map,
                   interval_stats = int_stats,
                   interval_plot = int_plot,
                   interval_measure = measure)
  
  if(products == TRUE){
    # folder
    if(!dir.exists("outputs")){dir.create("outputs")}
    # terra::writeRaster(int_dat, paste0("./outputs/", name, ".tif"))
    # raster work around to get geotiff playing nicely in ArcMAP
    raster::writeRaster(raster::raster(int_dat), paste0("./outputs/", name, ".tif"))
    suppressMessages(ggsave(filename = paste0("./outputs/", name, "_map.png"), int_map))
    suppressMessages(ggsave(filename = paste0("./outputs/", name, "_plot.png"), int_plot))
    readr::write_csv(int_stats, paste0("./outputs/", name, "_stats.csv"))
  }
  return(int_list)
}
