#!/bin/bash
shopt -s nullglob

files=(/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/raster/sentinel_L2A/*/GRANULE/*/IMG_DATA/*/*.jp2)
out_dir=/disks/d3/brick_sentinel2_raw/vrt

rm -rf ${out_dir}/*.vrt 2> /dev/null
mkdir -p "${out_dir}" 2> /dev/null

for file in "${files[@]}" ; do
    out_file=("${file##*/}")
    /usr/bin/gdalbuildvrt -resolution user -tr 10 10 -r bilinear -overwrite "${out_dir}/${out_file%_*}"_10m.vrt "${file}"
done
