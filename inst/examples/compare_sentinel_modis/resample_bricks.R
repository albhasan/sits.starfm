library(gdalUtils)
stop("Deprecated - use vrt instead!")

gdalUtils::align_rasters(unaligned = "/disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_20m.tif",
                         reference = "/disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_10m.tif",
                         dstfile   = "/disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_10m.tif",
                         nThreads = 8,
                         r = "bilinear")

gdalUtils::align_rasters(unaligned = "/disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_20m.tif",
                         reference = "/disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_10m.tif",
                         dstfile   = "/disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_10m.tif",
                         nThreads = 8,
                         r = "bilinear")

gdalUtils::align_rasters(unaligned = "/disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_20m.tif",
                         reference = "/disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_10m.tif",
                         dstfile   = "/disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_10m.tif",
                         nThreads = 8,
                         r = "bilinear")
