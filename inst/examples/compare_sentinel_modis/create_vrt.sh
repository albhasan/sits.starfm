#!/bin/bash

# create approx bricks 0f 10 meters from bricks of 20 mts
in_dir=/disks/d3/brick_sentinel2_interpolated/approx/
/usr/bin/gdalbuildvrt -resolution user -tr 10 10 -r bilinear $in_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_10m.vrt $in_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_20m.tif 
/usr/bin/gdalbuildvrt -resolution user -tr 10 10 -r bilinear $in_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_10m.vrt $in_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_20m.tif 
/usr/bin/gdalbuildvrt -resolution user -tr 10 10 -r bilinear $in_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_10m.vrt $in_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_20m.tif 

