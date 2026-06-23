#' Generate a simple random spatial sample from a polygon layer
#'
#' @param path Path to directory containing the shapefile
#' @param layer Layer name (string)
#' @param n Number of sample points
#' @param site_code Code used for naming samples (e.g., "UGOH")
#' @param year Year for naming samples
#' @param crs_in Input CRS (default = EPSG:3338)
#' @param crs_out Output CRS (default = EPSG:4326)
#'
#' @return A tibble with columns Site, Latitude, Longitude
#' @export

generate_srs <- function(path, layer, n, site_code, year,
                         crs_in = 3338, crs_out = 4326) {
  
  # read
  poly <- sf::st_read(dsn = path, layer = layer)
  
  # sample
  pts <- sf::st_sample(poly, size = n)
  
  # transform
  pts <- sf::st_transform(pts, crs_out)
  
  # convert to df
  df <- as.data.frame(pts) |>
    tidyr::separate(geometry, c("Longitude", "Latitude"), ", ") |>
    dplyr::mutate(
      Longitude = substr(Longitude, 3, nchar(Longitude) - 1),
      Latitude  = substr(Latitude, 1, nchar(Latitude) - 1),
      Sample    = seq_len(dplyr::n()),
      Site      = paste0(site_code, "_", Sample, "_", year)
    ) |>
    dplyr::select(Site, Latitude, Longitude)
  
  return(df)
}