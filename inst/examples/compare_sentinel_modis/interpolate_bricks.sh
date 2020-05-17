#!/bin/bash
#------------------------------------------------------------------------------
# INTERPOLATE THE HOLES IN THE POROUS BRICKS.
#------------------------------------------------------------------------------
script=/home/alber/Documents/ghProjects/sits.starfm/inst/examples/compare_sentinel_modis/interp_sentinel-2.R
in_dir=/disks/d3/brick_sentinel2_interpolated/porous
out_dir=/disks/d3/brick_sentinel2_interpolated/approx

Rscript  "$script" approx "$in_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_10m.tif "$out_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_10m.tif;
Rscript  "$script" approx "$in_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_10m.tif "$out_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_10m.tif;
Rscript  "$script" approx "$in_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_10m.tif "$out_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_10m.tif;
Rscript  "$script" approx "$in_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B05_20m.vrt "$out_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B05_20m.tif;
Rscript  "$script" approx "$in_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B06_20m.vrt "$out_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B06_20m.tif;
Rscript  "$script" approx "$in_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B07_20m.vrt "$out_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B07_20m.tif;
Rscript  "$script" approx "$in_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_10m.tif "$out_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_10m.tif;
Rscript  "$script" approx "$in_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_20m.tif "$out_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_20m.tif;
Rscript  "$script" approx "$in_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_20m.tif "$out_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_20m.tif;
Rscript  "$script" approx "$in_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_20m.tif "$out_dir"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_20m.tif;

exit 0
