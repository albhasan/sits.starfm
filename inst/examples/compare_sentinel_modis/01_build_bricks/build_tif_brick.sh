#!/bin/bash

shopt -s nullglob

out_dir=/disks/d3/brick_sentinel2_raw

band="$1"
if [ "$band" == "" ] ; then
    echo "Expected 1 paramter. The name of a band."
    exit 1
fi

files=("${out_dir}"/*"${band}".vrt)

if [ ${#files[@]} -eq 0 ] ; then
    echo "Band not found!"
    exit 1
fi

/usr/bin/gdalwarp -overwrite -of "GTiff" -co "COMPRESS=LZW" -co "BIGTIFF=YES" -ot "Int16" -dstnodata -9999  "${files}" "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_"${band}".tif

