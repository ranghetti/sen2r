% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/st_as_text_2.R
\name{st_as_text_2}
\alias{st_as_text_2}
\title{Return WKT or WKT2 basing on the installed rgdal version}
\usage{
st_as_text_2(x, pretty = FALSE)
}
\arguments{
\item{x}{object of class \code{sfg}, \code{sfc} or \code{crs}}

\item{pretty}{logical; if TRUE, print human-readable well-known-text
representation of a coordinate reference system}
}
\value{
Well-known Text representation of simple feature geometry or
coordinate reference system
}
\description{
This is a convenience temporary function which returns the WKT
representation of a CRS, using \code{sf::st_as_text()} in case PROJ < 3,
\code{rgdal::CRS()} otherwise.
This has the advantage to perform precise transformations with PROJ >=3,
and to avoid conversion errors
(see \href{https://rsbivand.github.io/ECS530_h19/ECS530_III.html}{here}).
This function will be deleted whenever \code{sf} will manage WKT2.
}
\examples{
sen2r:::st_as_text_2(sf::st_crs(32632))
}
\author{
Luigi Ranghetti, phD (2019)
}
\keyword{internal}
