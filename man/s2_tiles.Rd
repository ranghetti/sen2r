% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s2_tiles.R
\name{s2_tiles}
\alias{s2_tiles}
\title{Load Sentinel-2 tiles}
\usage{
s2_tiles()
}
\value{
An sf spatial object containing the extent of the tiles.
}
\description{
Load the vector object of the Sentinel-2 tiles.
When the function is run for the first time, it downloads the vector file
from the sen2r GitHub repository and it saves it on disk.
}
\note{
License: GPL 3.0
}
\examples{
\donttest{
# Retrieve all the tiles
s2tiles <- s2_tiles()

# Extract a subset of all the tiles
s2tiles_ch <- s2tiles[grepl("32T[LMN][ST]", s2tiles$tile_id),]
s2_coords <- sf::st_coordinates(suppressWarnings(sf::st_centroid(s2tiles_ch)))

# Show the tiles
plot(s2tiles_ch$geometry, border = "black")
text(s2_coords[,1], s2_coords[,2], s2tiles_ch$tile_id, cex = .75)
}
}
\references{
L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
"sen2r": An R toolbox for automatically downloading and preprocessing
Sentinel-2 satellite data. \emph{Computers & Geosciences}, 139, 104473.
\doi{10.1016/j.cageo.2020.104473}, URL: \url{https://sen2r.ranghetti.info/}.
}
\author{
Luigi Ranghetti, phD (2019)
}
