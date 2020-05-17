#!/bin/bash
# Build a brick from the first 2 images of another brick.

in_dir=/disks/d3/brick_sentinel2_interpolated/approx
out_dir=/disks/d3/brick_sentinel2_interpolated/first_2

/usr/bin/gdalbuildvrt -b 1 -b 2                                        $out_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_10m.vrt   $in_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_10m.tif
/usr/bin/gdalbuildvrt -b 1 -b 2                                        $out_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_10m.vrt   $in_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_10m.tif
/usr/bin/gdalbuildvrt -b 1 -b 2                                        $out_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_10m.vrt   $in_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_10m.tif
/usr/bin/gdalbuildvrt -b 1 -b 2                                        $out_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_10m.vrt   $in_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_10m.tif
/usr/bin/gdalbuildvrt -b 1 -b 2 -resolution user -tr 10 10 -r bilinear $out_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_10m.vrt   $in_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_20m.tif
/usr/bin/gdalbuildvrt -b 1 -b 2                                        $out_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_10m.vrt   $in_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_10m.vrt
/usr/bin/gdalbuildvrt -b 1 -b 2                                        $out_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_10m.vrt   $in_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_10m.vrt
/usr/bin/gdalbuildvrt -b 1 -b 2                                        $out_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_evi_10m.vrt   $in_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_evi_10m.tif
/usr/bin/gdalbuildvrt -b 1 -b 2                                        $out_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_mtvi_10m.vrt  $in_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_mtvi_10m.tif
/usr/bin/gdalbuildvrt -b 1 -b 2                                        $out_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndmi_10m.vrt  $in_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndmi_10m.tif
/usr/bin/gdalbuildvrt -b 1 -b 2                                        $out_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_10m.vrt  $in_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_10m.tif
/usr/bin/gdalbuildvrt -b 1 -b 2                                        $out_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_osavi_10m.vrt $in_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_osavi_10m.tif
/usr/bin/gdalbuildvrt -b 1 -b 2                                        $out_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_rdvi_10m.vrt  $in_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_rdvi_10m.tif
/usr/bin/gdalbuildvrt -b 1 -b 2                                        $out_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_10m.vrt  $in_dir/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_10m.tif

exit 0
