#!/bin/bash
# Build mini bricks.

#coords="269631.8475151583552361 -1117713.3237304671201855 278186.6487185038859025 -1109710.8717499168124050" # WGS84/UTM20n
coords="269631.8475151583552361 8882286.6762695331126451 278186.6487185038859025 8890289.1282500829547644"   # WGS84/UTM20s

/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q /disks/d3/brick_sentinel2/mini/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_10m.vrt         /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q /disks/d3/brick_sentinel2/mini/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_approx_10m.vrt  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q /disks/d3/brick_sentinel2/mini/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_10m.vrt         /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q /disks/d3/brick_sentinel2/mini/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_approx_10m.vrt  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q /disks/d3/brick_sentinel2/mini/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_10m.vrt         /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q /disks/d3/brick_sentinel2/mini/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_approx_10m.vrt  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q /disks/d3/brick_sentinel2/mini/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_10m.vrt         /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q /disks/d3/brick_sentinel2/mini/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_approx_10m.vrt  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q /disks/d3/brick_sentinel2/mini/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_10m.vrt         /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q /disks/d3/brick_sentinel2/mini/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_approx_10m.vrt  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q /disks/d3/brick_sentinel2/mini/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_10m.vrt         /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q /disks/d3/brick_sentinel2/mini/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_approx_10m.vrt  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q /disks/d3/brick_sentinel2/mini/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_10m.vrt         /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q /disks/d3/brick_sentinel2/mini/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_approx_10m.vrt  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q /disks/d3/brick_sentinel2/mini/S2A_MSIL2A_R096_T20LKP_20180812T143751_evi_10m.vrt         /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_evi_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q /disks/d3/brick_sentinel2/mini/S2A_MSIL2A_R096_T20LKP_20180812T143751_evi_approx_10m.vrt  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_evi_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q /disks/d3/brick_sentinel2/mini/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndmi_10m.vrt        /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndmi_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q /disks/d3/brick_sentinel2/mini/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndmi_approx_10m.vrt /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndmi_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q /disks/d3/brick_sentinel2/mini/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_10m.vrt        /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q /disks/d3/brick_sentinel2/mini/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_approx_10m.vrt /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_approx_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q /disks/d3/brick_sentinel2/mini/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_10m.vrt        /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_10m.tif
/usr/bin/gdalbuildvrt -te ${coords} -overwrite -q /disks/d3/brick_sentinel2/mini/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_approx_10m.vrt /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_approx_10m.tif
exit 0
