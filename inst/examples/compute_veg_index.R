#!/usr/bin/env Rscript
# compute bricks of vegetation indexes


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
devtools::load_all()


library(sits.starfm)

brick_type <- "interpolated"
#brick_type <- "starfm"


vi_index <- "ndvi"

if (brick_type == "starfm") {
    brick_path <- "/home/alber/shared/brick"
    brick_pattern <- "^LC8SR-MOD13Q1-STARFM_.*[.]tif$"
}else if (brick_type == "interpolated") {
    brick_path = "/home/alber/shared/brick_interp"
    brick_pattern = "^LC8SR-MOD13Q1-MYD13Q1_.*[.]tif$"
}else{
    stop("Unknown brick")
}

vi_bricks <- compute_vi(brick_path = brick_path,
                        brick_pattern = brick_pattern,
                        vi_index = vi_index)
print(paste(vi_bricks, collapse = " "))


