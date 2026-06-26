#' Batch Sampling for Multiple Spatial Layers
#'
#' Applies `generate_srs()` to a list of species or polygon inputs and
#' returns a single combined tibble of all sampling results.
#'
#' This function supports **two input modes** for each element of `specs`:
#' 
#' 1. **In-memory sf object**  
#'    Supply an `sf` object via the `sf` argument:
#'    ```
#'    list(sf = my_layer, n = 120, site_code = "UGOH", year = 2025)
#'    ```
#'
#' 2. **File-based input**  
#'    Supply a file `path` and `layer` name:
#'    ```
#'    list(path = "data", layer = "Ugashik", n = 120,
#'         site_code = "UGOH", year = 2025)
#'    ```
#'
#' Each list entry must contain either:
#'   • `sf`, `n`, `site_code`, `year`  
#' **OR**  
#'   • `path`, `layer`, `n`, `site_code`, `year`
#'
#' @param specs A named list where each element is a list of arguments
#'   passed to `generate_srs()`. The names of `specs` (e.g., species codes)
#'   will be used to populate the output column `Group`.
#'
#' @return A tibble combining all output rows from each call to
#'   `generate_srs()`, with an additional column `Group` indicating
#'   the originating list element.
#'
#' @examples
#' \dontrun{
#' # Using path + layer
#' species_list <- list(
#'   UGOH = list(path=".", layer="Ugashik", n=120,
#'               site_code="UGOH", year=2025),
#'   CBTH = list(path=".", layer="Thistle", n=120,
#'               site_code="CBTH", year=2025)
#' )
#'
#' multi_srs(species_list)
#'
#' # Using sf objects
#' library(sf)
#' ugoh_sf <- read_sf("data/Ugashik.gpkg", "Ugashik")
#' cbth_sf <- read_sf("data/Thistle.gpkg", "Thistle")
#'
#' species_list2 <- list(
#'   UGOH = list(sf = ugoh_sf, n=120, site_code="UGOH", year=2025),
#'   CBTH = list(sf = cbth_sf, n=120, site_code="CBTH", year=2025)
#' )
#'
#' multi_srs(species_list2)
#' }
#'
#' @export

multi_srs <- function(specs) {
  
  results <- purrr::imap_dfr(
    specs,
    ~ {
      args <- .x
      
      # Check for sf object input
      if (!is.null(args$sf)) {
        # nothing to change — pass arguments through
        df <- do.call(generate_srs, args)
        
      } else {
        # fallback: expect path + layer
        if (is.null(args$path) || is.null(args$layer)) {
          stop("Each entry must include either `sf` or both `path` and `layer`.")
        }
        
        df <- do.call(generate_srs, args)
      }
      
      df$Group <- .y
      df
    }
  )
  
  return(results)
}