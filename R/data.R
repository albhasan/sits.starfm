#' Satellite images used to build the SITS briks described in the vignettes
#'
#' A dataset with the characteristics of 266 Landsat 8 images. These images were
#' represent the Earth's Surface Reflectance and belong to the Collection 1. The
#' images were processed and downloaded from February to August of 2017.
#'
#' @format A tibble with 266 rows and 10 variables:
#' \describe{
#'   \item{sat_image}{name of the image}
#'   \item{files}{path to the image's files}
#'   \item{scene}{scene id (in WRS-2 notation)}
#'   \item{img_date}{image's date of acquisition}
#'   \item{proc_date}{image's date of processing}
#'   \item{prodes_year}{PRODES year}
#'   \item{neigh}{image's neighbors (NA)}
#'   \item{cloud_cov}{cloud coverage}
#'   \item{tile}{metadata of the spatially matching MODIS images}
#'   \item{year}{images' year of acquisition}
#' }
#' @source \url{http://www.diamondse.info/}
"BRICK_IMAGES"
