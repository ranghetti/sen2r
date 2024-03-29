% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mountpoint.R
\name{mountpoint}
\alias{mountpoint}
\title{Return the mountpoint of the input directory (if it is mounted)}
\usage{
mountpoint(path, protocol = NA)
}
\arguments{
\item{path}{The path to be checked}

\item{protocol}{(optional) Vector of protocol types. If NA (default),
all the protocols are considered.}
}
\value{
The path of the parent mountpoint for mounted directories;
if the input directory is not mounted, NULL is returned.
NULL is returned also if the operating system is not unix
(together with a warning message).
An attribute "protocol" contains the protocol of the mountpoint.
}
\description{
The function checks if the input directory is a subdirectory
of a mountpoint of a certain protocol. At the moment, it works only on unix
operating systems.
}
\note{
License: GPL 3.0
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
