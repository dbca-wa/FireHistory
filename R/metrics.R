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
  yslb_map <- gplot(yslb) +
    geom_tile(aes(fill = value)) +
    scale_fill_viridis_c(na.value = "transparent", name = "yslb") +
    labs(x = "",
         y = "",
         title = paste0(snakecase::to_mixed_case(name, sep_out = " "), " YSLB"),
         caption = expression(italic("Data: DBCA_Fire_History_DBCA_060"))) +
    coord_equal() +
    theme_bw()
  # stats
  yslb_stats <- dplyr::as_tibble(terra::freq(yslb)) |>
    dplyr::mutate(area_ha = count * 0.09) |>
    dplyr::rename(yslb = value) |>
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
    terra::writeRaster(yslb, paste0("./outputs/", name, "YSLB.tif"))
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
  freq_map <- gplot(fire_frq) +
    geom_tile(aes(fill = value)) +
    scale_fill_viridis_c(na.value = "transparent", name = "Fire Frequency") +
    labs(x = "",
         y = "",
         title = paste0(snakecase::to_mixed_case(name, sep_out = " "), " Fire Frequency"),
         caption = expression(italic("Data: DBCA_Fire_History_DBCA_060"))) +
    coord_equal() +
    theme_bw()
  # stats
  freq_stats <- dplyr::as_tibble(terra::freq(fire_frq)) |>
    dplyr::mutate(area_ha = count * 0.09) |>
    dplyr::rename(yslb = value) |>
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
    terra::writeRaster(fire_frq, paste0("./outputs/", name, "FFREQ.tif"))
    ggsave(filename = paste0("./outputs/", name, "FFREQ_map.png"), freq_map)
    ggsave(filename = paste0("./outputs/", name, "FFREQ_plot.png"), freq_plot)
    readr::write_csv(freq_stats, paste0("./outputs/", name, "FFREQ_stats.csv"))
  }
  return(freq_list)
}
