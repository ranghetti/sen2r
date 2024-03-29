% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/expand_path.R
\name{expand_path}
\alias{expand_path}
\title{Expand a path with a parent directory}
\usage{
expand_path(path, parent = getwd(), silent = TRUE, normalize = TRUE)
}
\arguments{
\item{path}{The path name (\code{character}) to check ad eventually expand.}

\item{parent}{The parent directory (\code{character}) to use if \code{path} is
relative (default value: the working directory).}

\item{silent}{Logical value: if TRUE (default), no message are shown;
if FALSE, a message inform if \code{parent} were applied or not;
if NA, a warning is returned if \code{path} is expanded, nothing if it
is already an absolute path.}

\item{normalize}{Logical value: if TRUE (default), the path is normalised
(\code{normalizePath()} is applied); if FALSE it is simply
appended.}
}
\value{
The path eventually expanded.
}
\description{
Accessory function which checks if a path is absolute or relative;
if relative, use a specified parent directory instead than the working
directory to expand it.
Useful for functions which accept more than one path as arguments,
in which one of them contains the absolute position, and the others
do not.
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
