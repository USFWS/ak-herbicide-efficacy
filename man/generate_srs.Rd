\name{generate_srs}
\alias{generate_srs}
\title{Generate a Simple Random Sample (SRS) of Points Within Polygons}

\description{
Creates a simple random sample of points within a polygon layer.  
Supports two input modes:
\itemize{
  \item Providing an in-memory \code{sf} object via \code{sf}
  \item Providing a file path and layer name via \code{path} and \code{layer}
}
The function returns a tibble containing sampled coordinates and
associated metadata.
}

\usage{
generate_srs(
  sf = NULL,
  path = NULL,
  layer = NULL,
  n,
  site_code,
  year
)
}

\arguments{
  \item{sf}{An optional \code{sf} polygon object. If supplied, the
  \code{path} and \code{layer} arguments are ignored.}

  \item{path}{Directory or file path containing a spatial layer.
  Must be used together with \code{layer} if \code{sf} is not provided.}

  \item{layer}{Name of the layer within the data source specified by
  \code{path}. Required if \code{sf} is not provided.}

  \item{n}{Number of random points to generate.}

  \item{site_code}{A short site or species code used to build unique
  sample IDs.}

  \item{year}{Year associated with the sampling effort.}
}

\details{
If an \code{sf} object is provided through \code{sf}, the function uses it
directly. Otherwise, the function reads spatial data from \code{path}
and \code{layer} using \code{sf::read_sf()}. Input geometries must be
polygons or multipolygons.

All geometries are transformed into Alaska Albers Equal Area
(EPSG:3338) for spatial operations and returned in WGS84 (EPSG:4326).
}

\value{
A tibble containing:
\itemize{
  \item \code{Site} -- a unique site ID
  \item \code{Year} -- year of sampling
  \item \code{Longitude}, \code{Latitude} -- coordinates of sampled points
  \item geometry column returned as an \code{sf} object
}
}

\examples{
\dontrun{
# --- File-based example ---
generate_srs(
  path = "data",
  layer = "Ugashik",
  n = 120,
  site_code = "UGOH",
  year = 2025
)

# --- In-memory sf example ---
library(sf)
ugoh_sf <- read_sf("data/Ugashik.gpkg", "Ugashik")

generate_srs(
  sf = ugoh_sf,
  n = 120,
  site_code = "UGOH",
  year = 2025
)
}
}

\seealso{
\code{\link{multi_srs}}
}

\author{USFWS Alaska Region}
