#!/usr/bin/env Rscript
# compute bricks of vegetation indexes of interpolated bricks
# See compute_veg_index.R




dump_file <- file.path(paste0("last.dump_",
                              stringr::str_replace(
                                  system('uname -n', intern = TRUE),
                                  "-", "_")))
dump_and_quit <- function() {
    # Save debugging info to file last.dump.rda
    dump.frames(dumpto = dump_file, to.file = TRUE)
    # Quit R with error status
    q(status = 1)
}
options(error = dump_and_quit)

library(tidyverse)
library(devtools)
setwd("/home/alber/Documents/data/experiments/l8mod-fusion/Rpackage/sits.starfm")
devtools::build()
devtools::install()

library(sits.starfm)
vi_bricks <- compute_vi(brick_path = "/home/alber/shared/brick_interp",
                        brick_pattern = "^LC8SR-MOD13Q1-MYD13Q1_.*[.]tif$",
                        vi_index = "ndvi")
print(vi_bricks)
