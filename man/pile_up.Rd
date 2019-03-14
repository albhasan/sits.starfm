% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{pile_up}
\alias{pile_up}
\title{Pile images into files by band.}
\usage{
pile_up(brick_imgs, file_col, brick_bands, brick_prefix, brick_scene,
  out_dir, no_data = -9999)
}
\arguments{
\item{brick_imgs}{A tibble of images.}

\item{file_col}{Name of the column in brick_imgs with the paths to the images to pile up.}

\item{brick_bands}{A character. Names of the bands.}

\item{brick_prefix}{A lengh-one character. Prefix to append to produced file names.}

\item{brick_scene}{A length-one character. The scene or tile of the brick.}

\item{out_dir}{A character. Path to a directory for storing results.}

\item{no_data}{A length-one numeric. The value for no data.}
}
\value{
A tibble of the matching PRODES dates and NA for the
}
\description{
Pile images into files by band.
}
\author{
Alber Sanchez, \email{alber.ipia@inpe.br}
}