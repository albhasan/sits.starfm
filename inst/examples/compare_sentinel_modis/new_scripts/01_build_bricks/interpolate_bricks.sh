#/bin/bash
echo "Deprecated. Code included in run_experiment.sh"
exit 1
script_dir="/home/alber/Documents/ghProjects/sits.starfm/inst/examples/compare_sentinel_modis"

Rscript "${script_dir}"/interp_sentinel-2.R approx /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_masked_10m.tif  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_approx_10m.tif 
Rscript "${script_dir}"/interp_sentinel-2.R approx /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_masked_10m.tif  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_approx_10m.tif
Rscript "${script_dir}"/interp_sentinel-2.R approx /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_masked_10m.tif  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_approx_10m.tif
Rscript "${script_dir}"/interp_sentinel-2.R approx /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_masked_10m.tif  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_approx_10m.tif
Rscript "${script_dir}"/interp_sentinel-2.R approx /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_masked_10m.tif  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_approx_10m.tif
Rscript "${script_dir}"/interp_sentinel-2.R approx /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_masked_10m.tif  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_approx_10m.tif
Rscript "${script_dir}"/interp_sentinel-2.R approx /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_masked_10m.tif  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_approx_10m.tif
Rscript "${script_dir}"/interp_sentinel-2.R approx /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_evi_masked_10m.tif  /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_evi_approx_10m.tif
Rscript "${script_dir}"/interp_sentinel-2.R approx /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndmi_masked_10m.tif /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndmi_approx_10m.tif
Rscript "${script_dir}"/interp_sentinel-2.R approx /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_masked_10m.tif /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_approx_10m.tif
Rscript "${script_dir}"/interp_sentinel-2.R approx /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_masked_10m.tif /disks/d3/brick_sentinel2/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_approx_10m.tif

exit 0
