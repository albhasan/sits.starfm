#!/bin/bash
#-------------------------------------------------------------------------------
# UPLOAD BRICKS TO AMAZON S3
#-------------------------------------------------------------------------------

mc="/home/scidb/shared/rolf/./mc"

if [ "$USER" != "scidb" ]; then
  echo "ERROR: This script must be run as user scidb"
  exit 1
fi

if [ "$HOSTNAME" != "esensing-006" ]; then
  echo "ERROR: This script must be run on esensing-006"
fi

# create buckets
#$mc mb s3/sentinel2-approx

$mc cp /disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_10m.tif   s3/sentinel2-approx
$mc cp /disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_10m.tif   s3/sentinel2-approx
$mc cp /disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_10m.tif   s3/sentinel2-approx
$mc cp /disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_10m.vrt   s3/sentinel2-approx
$mc cp /disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_20m.tif   s3/sentinel2-approx
$mc cp /disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_10m.tif   s3/sentinel2-approx
$mc cp /disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_10m.vrt   s3/sentinel2-approx
$mc cp /disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_20m.tif   s3/sentinel2-approx
$mc cp /disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_10m.vrt   s3/sentinel2-approx
$mc cp /disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_20m.tif   s3/sentinel2-approx
$mc cp /disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_evi_10m.tif   s3/sentinel2-approx
$mc cp /disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndmi_10m.tif  s3/sentinel2-approx
$mc cp /disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_10m.tif  s3/sentinel2-approx
$mc cp /disks/d3/brick_sentinel2_interpolated/approx/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_10m.tif  s3/sentinel2-approx

