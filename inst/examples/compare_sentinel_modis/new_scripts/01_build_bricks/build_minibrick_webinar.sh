#!/bin/bash
# Build mini bricks.

#in_dir="$1"
#out_dir="$2"
#coords=$3
in_dir=/disks/d3/brick_sentinel2
out_dir=/disks/d3/brick_sentinel2/mini_webinar
coords="269631 8882286 279631 8892286" # WGS84/UTM20s

/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_approx_10m.vrt  "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_approx_10m.vrt  "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_approx_10m.vrt  "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_approx_10m.vrt  "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_approx_10m.vrt  "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_approx_10m.vrt  "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_approx_10m.vrt  "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_evi_approx_10m.vrt  "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_evi_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndmi_approx_10m.vrt "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndmi_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_approx_10m.vrt "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_approx_10m.vrt "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_approx_10m.tif

/usr/bin/gdalwarp -overwrite -of "GTiff" -co "COMPRESS=LZW" -co "BIGTIFF=YES" -ot "Int16" -dstnodata -9999 -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_approx_10m.vrt  "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_approx_10m.tif
/usr/bin/gdalwarp -overwrite -of "GTiff" -co "COMPRESS=LZW" -co "BIGTIFF=YES" -ot "Int16" -dstnodata -9999 -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_approx_10m.vrt  "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_approx_10m.tif
/usr/bin/gdalwarp -overwrite -of "GTiff" -co "COMPRESS=LZW" -co "BIGTIFF=YES" -ot "Int16" -dstnodata -9999 -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_approx_10m.vrt  "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_approx_10m.tif
/usr/bin/gdalwarp -overwrite -of "GTiff" -co "COMPRESS=LZW" -co "BIGTIFF=YES" -ot "Int16" -dstnodata -9999 -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_approx_10m.vrt  "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_approx_10m.tif
/usr/bin/gdalwarp -overwrite -of "GTiff" -co "COMPRESS=LZW" -co "BIGTIFF=YES" -ot "Int16" -dstnodata -9999 -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_approx_10m.vrt  "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_approx_10m.tif
/usr/bin/gdalwarp -overwrite -of "GTiff" -co "COMPRESS=LZW" -co "BIGTIFF=YES" -ot "Int16" -dstnodata -9999 -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_approx_10m.vrt  "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_approx_10m.tif
/usr/bin/gdalwarp -overwrite -of "GTiff" -co "COMPRESS=LZW" -co "BIGTIFF=YES" -ot "Int16" -dstnodata -9999 -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_approx_10m.vrt  "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_approx_10m.tif
/usr/bin/gdalwarp -overwrite -of "GTiff" -co "COMPRESS=LZW" -co "BIGTIFF=YES" -ot "Int16" -dstnodata -9999 -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_evi_approx_10m.vrt  "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_evi_approx_10m.tif
/usr/bin/gdalwarp -overwrite -of "GTiff" -co "COMPRESS=LZW" -co "BIGTIFF=YES" -ot "Int16" -dstnodata -9999 -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndmi_approx_10m.vrt "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndmi_approx_10m.tif
/usr/bin/gdalwarp -overwrite -of "GTiff" -co "COMPRESS=LZW" -co "BIGTIFF=YES" -ot "Int16" -dstnodata -9999 -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_approx_10m.vrt "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_approx_10m.tif
/usr/bin/gdalwarp -overwrite -of "GTiff" -co "COMPRESS=LZW" -co "BIGTIFF=YES" -ot "Int16" -dstnodata -9999 -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_approx_10m.vrt "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_approx_10m.tif

find ${out_dir}/*.vrt -type f -exec rm {} \;

exit 0
