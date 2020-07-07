#!/bin/bash

# Build a tiff with of the frequency of clouds.

sentinel_l1c_dir="/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/raster/sentinel_L1C";


files=("${sentinel_l1c_dir}"/*/GRANULE/*/FMASK_DATA/*_Fmask4.tif)
vrt_file=/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/raster/fmasks.vrt


/usr/bin/gdalbuildvrt -separate -overwrite -resolution user -tr 10 10 -r bilinear -q  "${vrt_files[@]}"

