% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mask.R
\name{mask_clouds}
\alias{mask_clouds}
\title{Replace the clouds with a value.}
\usage{
mask_clouds(img, bands, replacement_value, out_dir, param, tmp_dir = tempdir())
}
\arguments{
\item{img}{A one-row tibble with a the fields 'sat_image' and 'files'.}

\item{bands}{A character. The names of the bands to mask.}

\item{replacement_value}{A numeric. The pixel value to fill in the clouds.}

\item{out_dir}{A length-one character. Path to store results.}

\item{param}{A list. A list of GDAL tranformation parameters.}

\item{tmp_dir}{A length-one character. Path to store temporal files.}
}
\value{
A tibble.
}
\description{
Replace the clouds with a value.
}
\author{
Alber Sanchez, \email{alber.ipia@inpe.br}
}
