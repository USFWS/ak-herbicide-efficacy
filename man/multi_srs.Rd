\name{multi_srs}
\alias{multi_srs}
\title{Batch Sampling for Multiple Spatial Layers}

\description{
Applies \code{generate_srs()} to a list of species or polygon inputs and
returns a single combined tibble of all sampling results.

Each element of \code{specs} may supply either:
\itemize{
  \item an in-memory \code{sf} object via the \code{sf} argument, or
  \item a file-based input via \code{path} and \code{layer}.
}
This enables flexible workflows for both disk-based layers and already
loaded spatial objects.
}

\usage{
multi_srs(specs)
}

\arguments{
  \item{specs}{A named list where each element is itself a list of
  arguments passed to \code{generate_srs()}. The list name is stored in
  the output column \code{Group}.  

  Each element must include \code{n}, \code{site_code}, and \code{year},
  and must additionally provide either:
  \itemize{
    \item \code{sf} (an \code{sf} polygon object), or
    \item \code{path} and \code{layer} (for file-based input).
  }}
}

\details{
The function loops over each element of \code{specs}, executes
\code{generate_srs()} with the provided arguments, and binds all results
into a single tibble.  

An additional column, \code{Group}, stores the name of the list
element—often a species code or layer identifier.
}

\value{
A tibble combining all output rows from each call to
\code{generate_srs()}.  
Includes a \code{Group} column to indicate the source list element.
}

\examples{
\dontrun{

# --- File-based workflow ---
species_list <- list(
  UGOH = list(path = ".", layer = "Ugashik",
              n = 120, site_code = "UGOH", year = 2025),
  CBTH = list(path = ".", layer = "Thistle",
              n = 120, site_code = "CBTH", year = 2025)
)

multi_srs(species_list)


# --- In-memory sf workflow ---
library(sf)

ugoh_sf <- read_sf("data/Ugashik.gpkg", "Ugashik")
cbth_sf <- read_sf("data/Thistle.gpkg", "Thistle")

species_list2 <- list(
  UGOH = list(sf = ugoh_sf, n = 120,
              site_code = "UGOH", year = 2025),
  CBTH = list(sf = cbth_sf, n = 120,
              site_code = "CBTH", year = 2025)
)

multi_srs(species_list2)

}
}

\seealso{
\code{\link{generate_srs}}
}

\author{USFWS Alaska Region}
