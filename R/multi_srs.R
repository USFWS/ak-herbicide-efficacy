#' Batch Sampling for Multiple Spatial Layers
#'
#' Applies \code{generate_srs()} to a list of species or polygons and
#' returns a single combined tibble of all sampling results.
#'
#' This is useful when processing many invasive plant polygons or any
#' group of spatial layers that follow the same sampling workflow.
#'
#' @param specs A named list. Each element must itself be a list of arguments
#'   that will be passed to \code{generate_srs()}. Each list must include:
#'   \code{path}, \code{layer}, \code{n}, \code{site_code}, and \code{year}.
#'
#'   Example structure:
#'   \code{
#'   list(
#'     UGOH = list(path = "data", layer = "Ugashik", n = 120,
#'                 site_code = "UGOH", year = 2025),
#'     CBTH = list(path = "data", layer = "Thistle", n = 120,
#'                 site_code = "CBTH", year = 2025)
#'   )
#'   }
#'
#' @return A tibble combining all output rows from each call to
#'   \code{generate_srs()}, with an additional column \code{Group} that
#'   identifies the list element (e.g., species name).
#'
#' @examples
#' \dontrun{
#' species_list <- list(
#'   UGOH = list(path=".", layer="UgashikOrangeHawkweedJul2021",
#'               n=120, site_code="UGOH", year=2025),
#'   CBTH = list(path=".", layer="2022ThistleCombinedPolygon",
#'               n=120, site_code="CBTH", year=2025)
#' )
#'
#' multi_srs(species_list)
#' }
#'
#' @export

multi_srs <- function(specs) {
  
  # Iterate through each list element and pass parameters to generate_srs()
  results <- purrr::imap_dfr(
    specs,
    ~ {
      df <- do.call(generate_srs, .x)
      df$Group <- .y     # species or layer name
      df
    }
  )
  
  return(results)
}
