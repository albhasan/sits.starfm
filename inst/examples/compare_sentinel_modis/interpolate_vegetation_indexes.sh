#!/bin/bash
#------------------------------------------------------------------------------
# INTERPOLATE THE HOLES IN THE POROUS BRICKS OF VEGETATION INDEXES.
#------------------------------------------------------------------------------

script=/home/alber/Documents/ghProjects/sits.starfm/inst/examples/compare_sentinel_modis/interp_sentinel-2.R
in_dir=/disks/d3/brick_sentinel2_interpolated/porous
out_dir=/disks/d3/brick_sentinel2_interpolated/approx

Rscript "$script" approx "$in_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_evi_10m.tif  "$out_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_evi_10m.tif
Rscript "$script" approx "$in_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndmi_10m.tif "$out_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndmi_10m.tif
Rscript "$script" approx "$in_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_mtvi_masked_10m.tif  "$out_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_mtvi_10m.tif
Rscript "$script" approx "$in_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_masked_10m.tif  "$out_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_10m.tif
Rscript "$script" approx "$in_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_osavi_masked_10m.tif "$out_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_osavi_10m.tif
script "$script" approx "$in_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_rdvi_masked_10m.tif  "$out_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_rdvi_10m.tif
Rscript "$script" approx "$in_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_masked_10m.tif  "$out_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_10m.tif
exit 0
