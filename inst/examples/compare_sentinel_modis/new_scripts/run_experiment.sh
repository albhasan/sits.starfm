#!/bin/bash

script_dir="/home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/scripts_bricks/new_scripts"


#---- Build bricks ----

# Build VRTs from the bands.
"${script_dir}"/01_build_bricks/./build_vrt_base.sh bilinear /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/raster/sentinel_L2A/*/GRANULE/*/IMG_DATA/*/*.jp2

# Build VRTs from the FMASKs.
"${script_dir}"/01_build_bricks/./build_vrt_base.sh nearest /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/raster/sentinel_L1C/S2A_MSIL1C_201*/GRANULE/*/FMASK_DATA/*.tif

#NOTE: build_index.sh already uses parallel to run.
parallel -j 1 "${script_dir}"/01_build_bricks/./build_index.sh ::: evi_10m ndmi_10m ndvi_10m savi_10m

#NOTE: build_index.sh already uses parallel to run.
parallel -j 1 "${script_dir}"/01_build_bricks/./mask_images.sh ::: B02_10m B03_10m B04_10m B08_10m B11_10m B12_10m B8A_10m evi_10m ndmi_10m ndvi_10m savi_10m

parallel -j 8 "${script_dir}"/01_build_bricks/./build_vrt_brick.sh ::: B02_10m B03_10m B04_10m B08_10m B11_10m B12_10m B8A_10m evi_10m ndmi_10m ndvi_10m savi_10m

# NOTE: Build VRTs for masked bands and indeces
parallel -j 8 "${script_dir}"/01_build_bricks/./build_vrt_brick.sh ::: B02_masked_10m B03_masked_10m B04_masked_10m B08_masked_10m B11_masked_10m B12_masked_10m B8A_masked_10m evi_masked_10m ndmi_masked_10m ndvi_masked_10m savi_masked_10m

parallel -j 8 "${script_dir}"/01_build_bricks/./build_tif_brick.sh ::: B02_10m B03_10m B04_10m B08_10m B11_10m B12_10m B8A_10m evi_10m ndmi_10m ndvi_10m savi_10m

# NOTE: Build TIFs for masked bands and indeces
parallel -j 8 "${script_dir}"/01_build_bricks/./build_tif_brick.sh ::: B02_masked_10m B03_masked_10m B04_masked_10m B08_masked_10m B11_masked_10m B12_masked_10m B8A_masked_10m evi_masked_10m ndmi_masked_10m ndvi_masked_10m savi_masked_10m

#---- Interpolate masked brikcs ----

#TODO: fix paths.
Rscript "${script_dir}"/01_build_bricks/interp_sentinel-2.R approx /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_masked_10m.tif  /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_approx_10m.tif 
Rscript "${script_dir}"/01_build_bricks/interp_sentinel-2.R approx /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_masked_10m.tif  /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_approx_10m.tif
Rscript "${script_dir}"/01_build_bricks/interp_sentinel-2.R approx /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_masked_10m.tif  /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_approx_10m.tif
Rscript "${script_dir}"/01_build_bricks/interp_sentinel-2.R approx /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_masked_10m.tif  /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_approx_10m.tif
Rscript "${script_dir}"/01_build_bricks/interp_sentinel-2.R approx /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_masked_10m.tif  /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_approx_10m.tif
Rscript "${script_dir}"/01_build_bricks/interp_sentinel-2.R approx /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_masked_10m.tif  /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_approx_10m.tif
Rscript "${script_dir}"/01_build_bricks/interp_sentinel-2.R approx /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_masked_10m.tif  /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_approx_10m.tif
Rscript "${script_dir}"/01_build_bricks/interp_sentinel-2.R approx /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_evi_masked_10m.tif  /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_evi_approx_10m.tif
Rscript "${script_dir}"/01_build_bricks/interp_sentinel-2.R approx /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndmi_masked_10m.tif /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndmi_approx_10m.tif
Rscript "${script_dir}"/01_build_bricks/interp_sentinel-2.R approx /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_masked_10m.tif /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_approx_10m.tif
Rscript "${script_dir}"/01_build_bricks/interp_sentinel-2.R approx /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_masked_10m.tif /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_approx_10m.tif

#---- Bricks of the first two images ----

/usr/bin/gdalbuildvrt -b 1 -b 2 /disks/d3/brick_sentinel2_raw/first_2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_approx_10m.vrt  /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_B02_approx_10m.tif 
/usr/bin/gdalbuildvrt -b 1 -b 2 /disks/d3/brick_sentinel2_raw/first_2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_approx_10m.vrt  /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_B03_approx_10m.tif 
/usr/bin/gdalbuildvrt -b 1 -b 2 /disks/d3/brick_sentinel2_raw/first_2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_approx_10m.vrt  /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_B04_approx_10m.tif 
/usr/bin/gdalbuildvrt -b 1 -b 2 /disks/d3/brick_sentinel2_raw/first_2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_approx_10m.vrt  /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_B08_approx_10m.tif 
/usr/bin/gdalbuildvrt -b 1 -b 2 /disks/d3/brick_sentinel2_raw/first_2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_approx_10m.vrt  /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_B11_approx_10m.tif 
/usr/bin/gdalbuildvrt -b 1 -b 2 /disks/d3/brick_sentinel2_raw/first_2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_approx_10m.vrt  /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_B12_approx_10m.tif 
/usr/bin/gdalbuildvrt -b 1 -b 2 /disks/d3/brick_sentinel2_raw/first_2/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_approx_10m.vrt  /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_B8A_approx_10m.tif 
/usr/bin/gdalbuildvrt -b 1 -b 2 /disks/d3/brick_sentinel2_raw/first_2/S2A_MSIL2A_R096_T20LKP_20180812T143751_evi_approx_10m.vrt  /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_evi_approx_10m.tif 
/usr/bin/gdalbuildvrt -b 1 -b 2 /disks/d3/brick_sentinel2_raw/first_2/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndmi_approx_10m.vrt /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndmi_approx_10m.tif
/usr/bin/gdalbuildvrt -b 1 -b 2 /disks/d3/brick_sentinel2_raw/first_2/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_approx_10m.vrt /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_ndvi_approx_10m.tif
/usr/bin/gdalbuildvrt -b 1 -b 2 /disks/d3/brick_sentinel2_raw/first_2/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_approx_10m.vrt /disks/d3/brick_sentinel2_raw/S2A_MSIL2A_R096_T20LKP_20180812T143751_savi_approx_10m.tif

#---- Get time series ----

"${script_dir}"/02_get_time_series/02_get_time_series.R /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/samples_all_bands.csv approx /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/approx_samples_all_bands_csv.rds &
sleep 10
"${script_dir}"/02_get_time_series/02_get_time_series.R /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/samples_indices.csv   approx /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/approx_samples_indices_csv.rds

"${script_dir}"/02_get_time_series/02_get_time_series.R /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/samples_all_bands.csv raw /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/raw_samples_all_bands_csv.rds &
sleep 10
"${script_dir}"/02_get_time_series/02_get_time_series.R /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/samples_indices.csv   raw /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/raw_samples_indices_csv.rds

#---- Compute K-Folds ----

#TODO run again 
"${script_dir}"/03_kfolds/03a_k-folds_analysis.R /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/approx_samples_all_bands_csv.rds /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/approx_samples_indices_csv.rds  /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/plot/kfold_approx

#TODO kfolds of raw samples (save with anoteher name)
"${script_dir}"/03_kfolds/03a_k-folds_analysis.R /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/raw_samples_all_bands_csv.rds /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/raw_samples_indices_csv.rds /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/plot/kfold_raw

#---- Classify brikcs ----

"${script_dir}"/04_classify/04_classify_bricks.R         /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/approx_samples_all_bands_csv.rds
"${script_dir}"/04_classify/04_classify_bricks.R         /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/approx_samples_indices_csv.rds
"${script_dir}"/04_classify/04_classify_partial_bricks.R /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/approx_samples_all_bands_csv.rds
"${script_dir}"/04_classify/04_classify_partial_bricks.R /home/alber/Documents/data/experiments/prodes_reproduction/papers/deforestation/data/validation/approx_samples_indices_csv.rds

#---- Post processing ----

exit 0