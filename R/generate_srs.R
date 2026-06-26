#' Generate a Simple Random Sample (SRS) of Points Within Polygons
#'
#' Creates a simple random sample of points inside a polygon layer.
#' This function supports two modes of input:
#'
#' \enumerate{
#'   \item \strong{In-memory input:} supply an \code{sf} object via \code{sf}.
#'   \item \strong{File-based input:} supply \code{path} and \code{layer},
#'         which are passed to \code{sf::read_sf()}.
#' }
#'
#' The output is a tibble containing random point coordinates and
#' basic metadata such as site code and year.
#'
#' @param sf Optional. An \code{sf} polygon object. If provided, the
#'   \code{path} and \code{layer} arguments are ignored.
#'
#' @param path Optional. Directory or file path where the spatial layer
#'   is stored. Must be used together with \code{layer} if \code{sf} is
#'   not provided.
#'
#' @param layer Optional. Layer name within the file or directory
#'   specified by \code{path}. Required if \code{sf} is not used.
#'
#' @param n Number of random points to generate.
#'
#' @param site_code A short code used to build unique sample IDs.
#'
#' @param year Year attributed to the sampling effort.
#'
#' @details
#' If an \code{sf} object is provided, it is used directly as the polygon
#' source. Otherwise, the function reads spatial data using
#' \code{sf::read_sf(path, layer)}.  
#'
#' All geometries are validated and transformed to Alaska Albers
#' (EPSG:3338) for sampling operations, and final sample coordinates are
#' returned in WGS84 (EPSG:4326).
#'
#' @return
#' A tibble containing:
#' \itemize{
#'   \item \code{Site} – unique sample ID
#'   \item \code{Year} – year of sampling
#'   \item \code{Longitude}, \code{Latitude} – point coordinates
#'   \item geometry column as \code{sf} object
#' }
#'
#' @examples
#' \dontrun{
#'
#' # --- File-based workflow ---
#' generate_srs(
#'   path = "data",
#'   layer = "Ugashik",
#'   n = 120,
#'   site_code = "UGOH",
#'   year = 2025
#' )
#'
#' # --- In-memory sf workflow ---
#' library(sf)
#' ugoh_sf <- read_sf("data/Ugashik.gpkg", "Ugashik")
#'
#' generate_srs(
#'   sf = ugoh_sf,
#'   n = 120,
#'   site_code = "UGOH",
#'   year = 2025
#' )
#' }
#'
#' @export


generate_srs <- function(sf = NULL,
                         path = NULL,
                         layer = NULL,
                         n,
                         site_code,
                         year) {
  
  # --- 1. INPUT HANDLING ----------------------------------------------------
  
  # Case A: user supplied an sf object
  if (!is.null(sf)) {
    if (!inherits(sf, "sf")) {
      stop("Argument `sf` must be an sf object.")
    }
    poly <- sf
    
    # Case B: user supplied path + layer
  } else {
    if (is.null(path) || is.null(layer)) {
      stop("Must provide either `sf` OR both `path` and `layer`.")
    }
    
    poly <- sf::read_sf(path, layer = layer)
  }
  
  # Ensure polygon geometry
  if (!any(sf::st_geometry_type(poly) %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("Input layer must contain polygon geometries.")
  }
  
  # --- 2. SAMPLE GENERATION -------------------------------------------------
  
  poly <- sf::st_make_valid(poly)
  poly <- sf::st_transform(poly, 3338)  # Alaska Albers
  
  # Random sample of n points
  pts <- sf::st_sample(poly, size = n, type = "random")
  
  pts <- sf::st_as_sf(pts)
  pts <- sf::st_transform(pts, 4326)  # WGS84 output
  
  # --- 3. ATTRIBUTE ASSEMBLY ------------------------------------------------
  
  tib <- tibble::tibble(
    Site = paste0(site_code, "-", seq_len(n)),
    Year = year,
    Longitude = sf::st_coordinates(pts)[, 1],
    Latitude  = sf::st_coordinates(pts)[, 2]
  )
  
  tib <- dplyr::bind_cols(tib, pts)
  
  return(tib)
}