.validate_input_files <- function(input_files) {

    print(input_files)

    stopifnot(is.character(input_files))
    if (any(is.na(input_files))) {
        warning(sprintf("NAs found among the input files: ", input_files))
        input_files <- input_files[!is.na(input_files)]
    }
    return(input_files)
}


#' @title Do calculations on images.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description R wrapper for gdal_calc.py
#'
#' @param input_files           A character. Paths to the image files.
#' @param out_filename          A length-one character. The path to the
#' destination file. The default is out.tif in the current working directory.
#' @param expression            A character. The expresion to compute on the
#' images using upper case A-Z letters to represent the images in the same order
#' as input_files.
#' @param band_number           An integer. The band numbers in input_files.
#' @param dstnodata             A length-one integer. Output nodata value.
#' @param data_type             A length-one character. Ouput datatype. It must
#' be one of c('Int32', 'Int16', 'Float64', 'UInt16', 'Byte', 'UInt32',
#' 'Float32').
#' @param out_format            A length-one character. The output format.
#' @param creation_option       A character. Creation options for output file
#' e.g. c('NAME1=VALUE1', 'NAME2=VALUE2')
#' @param all_bands             A character. Process all bands of given raster
#' (A-Z).
#' @param overwrite             A length-one logical. The default is FALSE.
#' @param verbose               A length-one logical. Generate a verbose output.
#' The default is FALSE.
#' @param quiet                 A length-one logical. The default is FALSE.
#' @return out_filename A length-one character.
#' @export
gdal_calc <- function(input_files, out_filename = NULL, expression,
                      band_number = NULL, dstnodata = NULL, data_type = NULL,
                      out_format = NULL, creation_option = NULL,
                      all_bands = NULL, overwrite = FALSE, verbose = FALSE,
                      quiet = FALSE) {
    input_files <- .validate_input_files(input_files)
    params <- paste0("--calc=\"", expression, "\"")
    if (is.null(out_filename))
        out_filename <- file.path(getwd(), "out.tif")
    params <- append(params, paste0("--outfile=", out_filename))
    params <- append(params, paste(paste0("-", LETTERS[1:length(input_files)]),
                                   input_files))
    if (!is.null(band_number)) {
        params <- append(params,
                         paste0(paste0("--", LETTERS[1:length(band_number)],
                                       "_band="), band_number))
    }
    if (!is.null(dstnodata))
        params <- append(params, paste0("--NoDataValue=", dstnodata))
    if (!is.null(data_type))
        params <- append(params, paste0("--type=", data_type))
    if (!is.null(out_format))
        params <- append(params, paste0("--format=", out_format))
    if (!is.null(creation_option))
        params <- append(params, paste0("--co=", creation_option))
    if (!is.null(all_bands))
        params <- append(params, paste0("--allBands=", all_bands))
    if (overwrite)
        params <- append(params, "--overwrite")
    if (verbose)
        params <- append(params, "--debug")
    if (quiet)
        params <- append(params, "--quiet")

    error <- call_os(command = "gdal_calc.py", args = params)
    if (error) {
        warning("Failed call to gdal_calc")
        return(NA_character_)
    }
    return(out_filename)
}


#' @title Convert raster between formats
#' @author Alber Sanchez, \email{albequietr.ipia@@inpe.br}
#' @description R wrapper for gdal_translate
#'
#' @param input_files  A character. Paths to the image files.
#' @param out_filename A length-one character. The path to the destination file.
#' @param of        A length-one character.
#' @param creation_option A character. Creation options for output file e.g.
#' c('NAME1=VALUE1', 'NAME2=VALUE2').
#' @param ot        A length-one character.
#' @param ps        A length-two numeric.
#' @param tap       A length-one logical. The default is FALSE.
#' @param ul_lr     A length-four numeric.
#' @param v         A length-one logical. The default is FALSE.
#' @param separate  A length-one logical. The default is FALSE.
#' @param pct       A length-one logical. The default is FALSE.
#' @param nodata_value A length-one numeric.
#' @param a_nodata  A length-one numeric.
#' @param init      A numeric.
#' @param createonly A length-one logical. The default is FALSE.
#' @return          A length-one character. out_filename.
#' @export
gdal_merge <- function(input_files, out_filename = NULL, of = NULL,
                       creation_option = NULL, ot = NULL, ps = NULL,
                       tap = FALSE, ul_lr = NULL, v = FALSE, separate = FALSE,
                       pct = FALSE, nodata_value = NULL, a_nodata = NULL,
                       init = NULL, createonly = FALSE){
    params <- character()
    if (!is.null(of))
        params <- append(params, paste("-of", of))
    if (!is.null(creation_option))
        params <- append(params, paste("-co", creation_option))
    if (!is.null(ot))
        params <- append(params, paste("-ot", ot))
    if (!is.null(ps))
        params <- append(params, paste("-ps", paste(ps)))
    if (tap)
        params <- append(params, "-tap")
    if (!is.null(ul_lr))
        params <- append(params, paste("-ul_lr", paste(ul_lr)))
    if (v)
        params <- append(params, "-v")
    if (separate)
        params <- append(params, "-separate")
    if (pct)
        params <- append(params, "-pct")
    if (!is.null(nodata_value))
        params <- append(params, paste("-nodata_value", nodata_value))
    if (!is.null(a_nodata))
        params <- append(params, paste("-a_nodata", a_nodata))
    if (!is.null(init))
        params <- append(params, paste("-init", paste(init)))
    if (createonly)
        params <- append(params, "-createonly")
    #
    params <- append(params, input_files)
    if (is.null(out_filename)) {
        out_filename <- file.path(getwd(), "out.tif")
    }
    params <- append(paste("-o", out_filename), params)
    error <- call_os(command = "gdal_merge.py", args = params)
    if (error) {
        warning("Failed call to gdal_merge")
        return(NA_character_)
    }
    return(out_filename)
}





#' @title Convert raster between formats
#' @author Alber Sanchez, \email{albequietr.ipia@@inpe.br}
#' @description R wrapper for gdal_translate
#'
#' @param input_files  A character. Paths to the image files.
#' @param out_filename A length-one character. The path to the destination file.
#' @param ot        A length-one character.
#' @param strict    A length-one logical. The default is FALSE.
#' @param of        A length-one character.
#' @param b         A character.
#' @param mask      A length-one character.
#' @param expand    A length-one character.
#' @param outsize   A length-two numeric. Output size in pixels.
#' @param tr        A length-two numeric. Target resolution in georeferenced
#' @param r         A length-one logical. The default is FALSE.
#' @param scale     A length two or four numeric.
#' @param exponent  A length-one numeric.
#' @param unscale   A length-one logical. The default is FALSE.
#' @param srcwin    A length-four numeric.
#' @param projwin   A length-four numeric.
#' @param projwin_srs A length-one character.
#' @param epo       A length-one logical. The default is FALSE.
#' @param eco       A length-one logical. The default is FALSE.
#' @param a_srs     A length-one character.
#' @param a_scale   A length-one numeric.
#' @param a_offset  A length-one numeric.
#' @param a_ullr    A length-four numeric.
#' @param a_nodata  A length-one numeric.
#' @param colorinterp_X A character.
#' @param colorinterp A length-one character.
#' @param mo        A character.
#' @param creation_option A character. Creation options for output file e.g.
#' c('NAME1=VALUE1', 'NAME2=VALUE2').
#' @param gcp       A list of length-five numeric.
#' @param q         A length-one logical. The default is FALSE.
#' @param sds       A length-one logical. The default is FALSE.
#' @param stats     A length-one logical. The default is FALSE.
#' @param norat     A length-one logical. The default is FALSE.
#' @param oo     A character. Open options for input files e.g.
#' c('NAME1=VALUE1', 'NAME2=VALUE2').
#' @return          A length-one character. out_filename.
gdal_translate <- function(
    input_files, out_filename = NULL, ot = NULL, strict = FALSE, of = NULL,
    b = NULL, mask = NULL, expand = NULL, outsize = NULL, tr = NULL, r = NULL,
    scale = NULL, exponent = NULL, unscale = FALSE, srcwin = NULL, projwin = NULL,
    projwin_srs = NULL, epo = FALSE, eco = FALSE, a_srs = NULL, a_scale = NULL,
    a_offset = NULL, a_ullr = NULL, a_nodata = NULL, colorinterp_X = NULL,
    colorinterp = NULL, mo = NULL, creation_option = NULL, gcp = NULL, q = FALSE,
    sds = FALSE, stats = FALSE, norat = FALSE, oo = NULL ){

    input_files <- .validate_input_files(input_files)
    params <- character()
    if (!is.null(ot))
        params <- append(params, paste("-ot", ot))
    if (strict)
        params <- append(params, "-strict")
    if (!is.null(of))
        params <- append(params, paste("-of", of))
    if (!is.null(b))
        params <- append(params, paste("-b", b))
    if (!is.null(mask))
        params <- append(params, paste("-b", mask))
    if (!is.null(expand))
        params <- append(params, paste("-expand", expand))
    if (!is.null(outsize))
        params <- append(params, paste("-outsize", paste(outsize)))
    if (!is.null(tr))
        params <- append(params, paste("-tr", paste(tr)))
    if (!is.null(r))
        params <- append(params, paste("-r", r))
    if (!is.null(scale)) {
        params <- append(params, paste("-scale", paste(scale)))
        if (!is.null(exponent))
            params <- append(params, paste("-exponent", paste(exponent)))
    }
    if (!is.null(unscale))
        params <- append(params, paste("-unscale", unscale))
    if (unscale)
        params <- append(params, "-unscale")
    if (!is.null(srcwin))
        params <- append(params, paste("-srcwin", paste(srcwin)))
    if (!is.null(projwin))
        params <- append(params, paste("-projwin", paste(projwin)))
    if (!is.null(projwin_srs))
        params <- append(params, paste("-projwin_srs", projwin))
    if (epo)
        params <- append(params, "-epo")
    if (eco)
        params <- append(params, "-eco")
    if (!is.null(a_srs))
        params <- append(params, paste("-a_srs", a_srs))
    if (!is.null(a_scale))
        params <- append(params, paste("-a_scale", a_scale))
    if (!is.null(a_offset))
        params <- append(params, paste("-a_offset", a_offset))
    if (!is.null(a_ullr))
        params <- append(params, paste("-a_ullr", paste(a_ullr)))
    if (!is.null(a_nodata))
        params <- append(params, paste("-a_nodata", paste(a_nodata)))
    if (!is.null(colorinterp_X))
        params <- append(params, paste(paste0("-colorinterp_", colorinterp_X, sep = ""), colorinterp_X))
    if (!is.null(colorinterp))
        params <- append(params, paste("-colorinterp", colorinterp))
    if (!is.null(mo))
        params <- append(params, paste("-mo '", mo, "'", sep = ""))
    if (!is.null(creation_option))
        params <- append(params, paste("-co", creation_option))
    if (!is.null(gcp))
        for (l in gcp) {
            params <- append(params, paste("-gcp", paste(l)))
        }
    if (q)
        params <- append(params, "-q")
    if (sds)
        params <- append(params, "-sds")
    if (stats)
        params <- append(params, "-stats")
    if (norat)
        params <- append(params, "-norat")
    if (!is.null(oo))
        params <- append(params, paste("-oo", oo))
    #
    params <- append(params, input_files)
    if (is.null(out_filename)) {
        out_filename <- file.path(getwd(), "out.tif")
    }
    params <- append(params, out_filename)
    error <- call_os(command = "gdal_translate", args = params)
    if (error) {
        warning("Failed call to gdal_translate")
        return(NA_character_)
    }
    return(out_filename)
}


#' @title Process images
#' @author Alber Sanchez, \email{albequietr.ipia@@inpe.br}
#' @description R wrapper for gdalwarp
#'
#' @param input_files  A character. Paths to the image files.
#' @param out_filename A length-one character. The path to the destination file.
#' The default is out.tif in the current working directory.
#' @param s_srs        A length-one character.
#' @param target_srs   A length-one character. The target spatial reference set.
#' @param to           A character. e.g c('NAME1=VALUE1', 'NAME2=VALUE2')
#' @param novshiftgrid A length-one logical. The default is FALSE.
#' @param order        A length-one numeric.
#' @param tps          A length-one logical. The default is FALSE.
#' @param rpc          A length-one logical. The default is FALSE.
#' @param geoloc       A length-one logical. The default is FALSE.
#' @param et           A length-one numeric.
#' @param refine_gcps  A length-one numeric.
#' @param extent_output A length-four numeric. The extents of the output file in
#' target SRS by default, or in the SRS specified with extent_output_srs.
#' @param extent_output_srs       A length-one character. SRS of extent_output
#' (when it is different from input_files' SRS).
#' @param tr           A length-two numeric.
#' @param tap          A length-one logical. The default is FALSE.
#' @param size_ouput   A length-two integer. Set output file size in pixels and
#' lines.
#' @param ovr          A length-one character.
#' @param wo           A character. e.g c('NAME1=VALUE1', 'NAME2=VALUE2')
#' @param ot           A length-one character.
#' @param wt           A length-one character.
#' @param resampling   A length-one character. Resampling method to use.
#' @param srcnodata    A numeric.
#' @param dstnodata    A numeric.
#' @param srcalpha     A length-one logical. The default is FALSE.
#' @param nosrcalpha   A length-one logical. The default is FALSE.
#' @param dstalpha     A length-one logical. The default is FALSE.
#' @param wm           A length-one numeric.
#' @param multi        A length-one logical. The default is FALSE.
#' @param quiet        A length-one logical. The default is FALSE.
#' @param out_format   A length-one character. The output format.
#' @param creation_option A character. Creation options for output file e.g.
#' c('NAME1=VALUE1', 'NAME2=VALUE2')
#' @param cutline      A length-one character.
#' @param cl           A length-one character.
#' @param cwhere       A length-one character.
#' @param csql         A length-one character.
#' @param cblend       A length-one numeric.
#' @param crop_to_cutline A length-one logical. The default is FALSE.
#' @param overwrite    A length-one logical. The default is FALSE.
#' @param nomd         A length-one logical. The default is FALSE.
#' @param cvmd         A length-one character.
#' @param setci        A length-one logical. The default is FALSE.
#' @param oo           A character. e.g. c('NAME1=VALUE1', 'NAME2=VALUE2')
#' @param doo          A character. e.g. c('NAME1=VALUE1', 'NAME2=VALUE2')
#' @return out_filename A length-one character.
gdal_warp <- function(input_files, out_filename = NULL, s_srs = NULL,
                      target_srs = NULL, to = NULL, novshiftgrid = FALSE,
                      order = NULL, tps = FALSE, rpc = FALSE, geoloc = FALSE,
                      et = NULL, refine_gcps = NULL, extent_output = NULL,
                      extent_output_srs = NULL, tr = NULL, tap = FALSE,
                      size_ouput = NULL, ovr = NULL, wo = NULL, ot = NULL,
                      wt = NULL, resampling = NULL, srcnodata = NULL,
                      dstnodata = NULL, srcalpha = FALSE, nosrcalpha = FALSE,
                      dstalpha = FALSE, wm = NULL, multi = FALSE, quiet = FALSE,
                      out_format = NULL, creation_option = NULL, cutline = NULL,
                      cl = NULL, cwhere = NULL, csql = NULL, cblend = NULL,
                      crop_to_cutline = FALSE, overwrite = FALSE, nomd = FALSE,
                      cvmd = NULL, setci = FALSE, oo = NULL, doo = NULL) {
    input_files <- .validate_input_files(input_files)
    params <- character()
    if (!is.null(s_srs))
        params <- append(params, paste("-s_srs", s_srs))
    if (!is.null(target_srs))
        params <- append(params, paste("-t_srs", target_srs))
    if (!is.null(to))
        params <- append(params, paste("-to", to))
    if (novshiftgrid)
        params <- append(params, "-novshiftgrid")
    if (!is.null(order))
        params <- append(params, paste("-order", order))
    if (tps)
        params <- append(params, "-tps")
    if (rpc)
        params <- append(params, "-rpc")
    if (geoloc)
        params <- append(params, "-geoloc")
    if (!is.null(et))
        params <- append(params, paste("-et", et))
    if (!is.null(refine_gcps))
        params <- append(params, paste("-refine_gcps", refine_gcps))
    if (!is.null(extent_output))
        params <- append(params, paste("-te", paste(extent_output,
                                                    collapse = " ")))
    if (!is.null(extent_output_srs))
        params <- append(params, paste("-te_srs ", extent_output_srs))
    if (!is.null(tr))
        params <- append(params, paste("-tr ", tr))
    if (tap)
        params <- append(params, "-tap")
    if (!is.null(size_ouput))
        params <- append(params, paste("-ts", paste(size_ouput,
                                                    collapse = " ")))
    if (!is.null(ovr))
        params <- append(params, paste("-ovr ", ovr))
    if (!is.null(wo))
        params <- append(params, paste("-wo", wo))
    if (!is.null(ot))
        params <- append(params, paste("-ot", ot))
    if (!is.null(wt))
        params <- append(params, paste("-ot", wt))
    if (!is.null(resampling))
        params <- append(params, paste("-r", resampling))
    if (!is.null(srcnodata))
        params <- append(params, paste0("-srcnodata '", paste(srcnodata,
                                                              collapse = " "),
                                        "'"))
    if (!is.null(dstnodata))
        params <- append(params, paste0("-dstnodata '", paste(dstnodata,
                                                              collapse = " "),
                                        "'"))
    if (srcalpha)
        params <- append(params, "-srcalpha")
    if (nosrcalpha)
        params <- append(params, "-nosrcalpha")
    if (dstalpha)
        params <- append(params, "-dstalpha")
    if (!is.null(wm))
        params <- append(params, paste("-wm", wm))
    if (multi)
        params <- append(params, "-multi")
    if (quiet)
        params <- append(params, "-q")
    if (!is.null(out_format))
        params <- append(params, paste("-of", out_format))
    if (!is.null(creation_option))
        params <- append(params, paste("-co", creation_option))
    if (!is.null(cutline))
        params <- append(params, paste("-cutline", cutline))
    if (!is.null(cl))
        params <- append(params, paste("-cl", cl))
    if (!is.null(cwhere))
        params <- append(params, paste("-cwhere", cwhere))
    if (!is.null(csql))
        params <- append(params, paste("-csql", csql))
    if (!is.null(cblend))
        params <- append(params, paste("-cblend", cblend))
    if (crop_to_cutline)
        params <- append(params, "-crop_to_cutline")
    if (overwrite)
        params <- append(params, "-overwrite")
    if (nomd)
        params <- append(params, "-nomd")
    if (!is.null(cvmd))
        params <- append(params, paste("-cvmd", cvmd))
    if (setci)
        params <- append(params, "-setci")
    if (!is.null(oo))
        params <- append(params, paste("-oo", oo))
    if (!is.null(doo))
        params <- append(params, paste("-doo", doo))
    #
    params <- append(params, input_files)
    if (is.null(out_filename)) {
        out_filename <- file.path(getwd(), "out.tif")
    }
    params <- append(params, out_filename)
    error <- call_os(command = "gdalwarp", args = params)
    if (error) {
        warning("Failed call to gdalwarp")
        return(NA_character_)
    }
    return(out_filename)
}
