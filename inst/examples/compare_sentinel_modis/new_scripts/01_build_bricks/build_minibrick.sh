#!/bin/bash
# Build mini bricks.

in_dir="$1"
out_dir="$2"
coords=$3
#in_dir=/disks/d3/brick_sentinel2
#out_dir=/disks/d3/brick_sentinel2/mini
#coords="269631.8475151583552361 8882286.6762695331126451 278186.6487185038859025 8890289.1282500829547644" # WGS84/UTM20s

if [ "$#" -ne 3 ]; then
    echo "ERROR: Expecting 3 parameters: in_dir, out_dir, and coords"
    exit 1
fi

/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_10m.vrt         "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_approx_10m.vrt  "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_10m.vrt         "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_approx_10m.vrt  "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_10m.vrt         "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_approx_10m.vrt  "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_10m.vrt         "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_approx_10m.vrt  "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_10m.vrt         "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_approx_10m.vrt  "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_10m.vrt         "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_approx_10m.vrt  "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_10m.vrt         "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_approx_10m.vrt  "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_evi_10m.vrt         "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_evi_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_evi_approx_10m.vrt  "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_evi_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndmi_10m.vrt        "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndmi_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndmi_approx_10m.vrt "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndmi_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_10m.vrt        "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_approx_10m.vrt "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_10m.vrt        "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q "${out_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_approx_10m.vrt "${in_dir}"/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_approx_10m.tif
exit 0
