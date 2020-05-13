#' @title Check GDAL installation
#' @description The function check that GDAL is installed and updated to
#'  the minimum required version (2.1.2).
#' @param abort Logical parameter: if TRUE (default), the function aborts
#'  in case no GDAL installation is found; if FALSE, a warning is shown
#'  and FALSE is returned.
#' @param gdal_path (optional) Character: the path in which GDAL must be
#'  searched in. If NULL (default), search is performed in the whole file system.
#' @param force (optional) Logical: if TRUE, install even if it is already
#'  installed (default is FALSE). Notice that, defining `gdal_path`, GDAL is
#'  searched again even if `"force" = FALSE` in case the existing installation
#'  is not in `gdal_path`.
#' @param full_scan (optional) Logical: in Linux and MacOS, if `gdal_path` was
#'  not manually defined, GDAL is searched within the system path in case this
#'  argument is left to default value FALSE; instead, if TRUE, a full search is
#'  performed. In Windows, if the default OSGeo directory `C:\\OSGeo4W64` exists,
#'  GDAL is searched there, instead in the main directory `C:\\`; setting
#'  `full_scan` to TRUE, is is always searched in the whole `C:\\`.
#'  This argument takes no effect if `gdal_path` was defined, since, in that case,
#'  a full search is always performed in `gdal_path`.
#' @return Logical (invisible): TRUE in case the installation is ok, FALSE
#'  if GDAL is missing and abort=FALSE (otherwise, the function stops).
#'
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. DOI: 
#'  \href{https://doi.org/10.1016/j.cageo.2020.104473}{10.1016/j.cageo.2020.104473}, 
#'  URL: \url{http://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0
#' @importFrom jsonlite toJSON
#' @export
#' @examples
#' \donttest{
#'
#' # Use function
#' check_gdal()
#'
#' # Check GDAL was imported
#' load_binpaths()$gdalinfo
#' }

check_gdal <- function(abort = TRUE, gdal_path = NULL, force = FALSE, full_scan = FALSE) {
  
  # set minimum GDAL version
  gdal_minversion <- package_version("2.1.2")
  
  # load the saved GDAL path, if exists
  binpaths <- load_binpaths()
  
  # set message method
  message_type <- ifelse(abort==TRUE, "error", "warning")
  
  # check gdal_path
  if (!is.null(gdal_path)) {
    tryCatch(
      gdal_path <- normalize_path(gdal_path, mustWork = TRUE),
      error = function(e) {
        print_message(
          type = message_type,
          "The directory passed as main GDAL path does not exist."
        )
        return(invisible(FALSE))
      }
    )
  } else {
    gdal_path <- ""
  }
  
  # If GDAL is not found, search for it
  if (any(
    is.null(binpaths$gdalinfo), !file.exists(nn(binpaths$gdalinfo)),
    force == TRUE,
    !grepl(gdal_path, normalize_path(nn(binpaths$gdalinfo)), fixed = TRUE)
  )) {
    # nocov start
    print_message(
      type="message",
      "Searching for a valid GDAL installation",
      if (gdal_path != "") {paste0(
        " (this could take a long time, ",
        "depending on the content of \"gdal_path\")"
      )} else if (full_scan == TRUE) {paste0(
        " (a full scan in the whole file system was required, ",
        "this generally takes a long time)"
      )},
      "..."
    )
    
    if (Sys.info()["sysname"] %in% c("Linux", "Darwin")) {
      paths_gdalinfo <- if (all(full_scan == FALSE, gdal_path == "")) {
        list.files(
          file.path(
            system(paste0(Sys.which("gdal-config")," --prefix"), intern = TRUE),
            "bin"
          ),
          "^gdalinfo$", recursive = TRUE, full.names = TRUE
        )
      } else {
        list.files(
          if (gdal_path == "") {"/"} else {gdal_path},
          "^gdalinfo$", recursive = TRUE, full.names = TRUE
        )
      }
    } else if (Sys.info()["sysname"] == "Windows") {
      if (gdal_path == "") {
        if (all(full_scan == FALSE, dir.exists("C:\\OSGEO4~1"))) {
          gdal_path <- "C:\\OSGEO4~1\\"
        } else {
          gdal_path <- "C:\\"
        }
      }
      paths_gdalinfo <- list.files(
        gdal_path, "gdalinfo\\.exe$", recursive = TRUE, full.names = TRUE
      )
    }
    paths_gdalinfo <- normalize_path(paths_gdalinfo)
    
    if (any(length(paths_gdalinfo) == 0, paths_gdalinfo == "")) {
      print_message(
        type=message_type,
        "GDAL was not found",
        if (Sys.info()["sysname"] == "Windows") {
          paste0("within path \"",gdal_path,"\" ")
        },
        if (gdal_path != "") {" within the defined GDAL path"},
        ", please install it",
        if (gdal_path != "") {
          " or define another value for argument \"gdal_path\""
        } else {paste0(
          " (see the documentation at ",
          "https://sen2r.ranghetti.info/articles/installation ",
          "to see how to install it) ",
          " or define its location using argument \"gdal_path\""
        )},
        "."
      )
      return(invisible(FALSE))
    }
    
  } else {
    paths_gdalinfo <- binpaths$gdalinfo
  } # end of path retrieval
  
  # nocov end
  
  ## Check requisite 1: minimum version
  gdal_versions <- package_version(gsub(
    "^.*GDAL ([0-9\\.]+)[^0-9].*$", "\\1",
    sapply(paste(paths_gdalinfo, "--version"), system, intern = TRUE)
  ))
  if (all(gdal_versions < gdal_minversion)) {
    print_message(
      type=message_type,
      "GDAL version must be at least ", as.character(gdal_minversion),
      ". Please update it, ",
      if (gdal_path != "") {
        " or define another value for argument \"gdal_path\"."
      } else {paste0(
        " or define a different GDAL environment which satisfies ",
        "this requirement by setting the argument \"gdal_path\"."
      )},
    )
    return(invisible(FALSE))
  }
  paths_gdalinfo <- paths_gdalinfo[!gdal_versions < gdal_minversion]
  gdal_versions <- gdal_versions[!gdal_versions < gdal_minversion]
  # order by version
  paths_gdalinfo <- paths_gdalinfo[order(gdal_versions, decreasing = TRUE)]
  gdal_versions <- sort(gdal_versions, decreasing = TRUE)
  
  
  ## Check requisite 2: OpenJPEG support
  gdal_check_jp2 <- sapply(paths_gdalinfo, function(path) {
    gdal_formats <- system(paste0(path," --formats"), intern = TRUE)
    any(grepl("JP2OpenJPEG", gdal_formats))
  })
  if (!any(gdal_check_jp2)) {
    print_message(
      type=message_type,
      "Your local GDAL installation does not support JPEG2000 (JP2OpenJPEG) format. ",
      "Please install JP2OpenJPEG support and recompile GDAL.",
      if (Sys.info()["sysname"] == "Windows") {paste0(
        "\nUsing the OSGeo4W installer ",
        "(http://download.osgeo.org/osgeo4w/osgeo4w-setup-x86",
        if (Sys.info()["machine"]=="x86-64") {"_64"},".exe), ",
        "choose the \"Advanced install\" and ",
        "check the packages \"gdal-python\" and \"openjpeg\"."
      )}
    )
    return(invisible(FALSE))
  }
  paths_gdalinfo <- paths_gdalinfo[gdal_check_jp2]
  gdal_versions <- gdal_versions[gdal_check_jp2]
  
  
  ## Check requisite 3: in Windows and Mac, use only OSGeo version
  if (Sys.info()["sysname"] != "Linux") {
    gdal_osgeo_order <- if (Sys.info()["sysname"] == "Windows") {
      unique(c(
        # 1: C:\OSGeo4W64
        grep("^C:\\\\OSGEO4~1", paths_gdalinfo),
        # 2: paths containing OSGEO4~1
        grep("OSGEO4", paths_gdalinfo, perl=TRUE),
        # 3. paths in which osgeo4w-setup.exe exists wtogether with gdalinfo.exe
        which(file.exists(file.path(dirname(paths_gdalinfo),"osgeo4w-setup.exe")))#,
        # # 4: all other paths (disabled to avoid selecting other installations)
        # grep("^((?!OSGEO4).)*$", gdal_dirs, perl=TRUE)
      ))
    } else if (Sys.info()["sysname"] == "Darwin") {
      grep("/osgeo\\-gdal/", paths_gdalinfo)
    }
    if (length(gdal_osgeo_order) == 0) {
      print_message(
        type=message_type,
        if (Sys.info()["sysname"] == "Windows") {paste0(
          "You do not have a local GDAL environment installed with OSGeo4W, ",
          "which is a mandatory requirement since sen2r v. 1.1.0. ",
          "\nPlease install GDAL using the OSGeo4W installer ",
          "(http://download.osgeo.org/osgeo4w/osgeo4w-setup-x86",
          if (Sys.info()["machine"]=="x86-64") {"_64"},".exe), ",
          "choosing the \"Advanced install\" and ",
          "checking the packages \"gdal-python\" and \"openjpeg\"."
        )} else if (Sys.info()["sysname"] == "Darwin") {paste0(
          "You do not have a local GDAL environment installed with OSGeo Homebrew, ",
          "which is a mandatory requirement since sen2r v. 1.1.0. ",
          "\nPlease install GDAL from the OSGeo repository: ",
          "\n1. install Homebrew (https://brew.sh/);",
          "\n2. open a terminal and type ",
          "\n   \"brew install osgeo-gdal-python\""
        )}
      )
      return(invisible(FALSE))
    }
    paths_gdalinfo <- paths_gdalinfo[gdal_osgeo_order]
    gdal_versions <- gdal_versions[gdal_osgeo_order]
  }
  
  
  ## Check requisite 3: Python executables exist
  paths_gdalcalc <- file.path(
    if (Sys.info()["sysname"] == "Darwin") {
      gsub("/osgeo\\-gdal/", "/osgeo-gdal-python/", dirname(paths_gdalinfo))
    } else {
      dirname(paths_gdalinfo)
    },
    "gdal_calc.py"
  )
  gdal_check_py <- file.exists(paths_gdalcalc)
  
  
  if (!any(gdal_check_py)) {
    print_message(
      type=message_type,
      "You do not have installed the Python GDAL executables",
      if (Sys.info()["sysname"] == "Windows") {paste0(
        ".\nUsing the OSGeo4W installer ",
        "(http://download.osgeo.org/osgeo4w/osgeo4w-setup-x86",
        if (Sys.info()["machine"]=="x86-64") {"_64"},".exe), ",
        "choose the \"Advanced install\" and ",
        "check the package \"gdal-python\"."
      )} else if (Sys.info()["sysname"] == "Darwin") {paste0(
        " (to do it, open a terminal and type ",
        "\"brew install osgeo-gdal-python\")."
      )} else if (Sys.info()["sysname"] == "Linux") {paste0(
        " (see the documentation at ",
        "https://sen2r.ranghetti.info/articles/installation#on-linux-systems ",
        "to see how to install the required dependency ",
        "based on your Linux distribution.",
      )}
    )
    return(invisible(FALSE))
  }
  paths_gdalinfo <- paths_gdalinfo[gdal_check_py]
  paths_gdalcalc <- paths_gdalcalc[gdal_check_py]
  gdal_versions <- gdal_versions[gdal_check_py]
  
  
  
  # save the path for use with external calls
  gdal_dir <- dirname(paths_gdalinfo)[1]
  gdal_py_dir <- dirname(paths_gdalcalc)[1]
  gdal_version <- gdal_versions[1]
  bin_ext <- ifelse(Sys.info()["sysname"] == "Windows", ".exe", "")
  binpaths$gdalinfo <- normalize_path(file.path(gdal_dir,paste0("gdalinfo",bin_ext)))
  if (Sys.info()["sysname"] == "Windows") {
    binpaths$python <- normalize_path(file.path(gdal_dir,paste0("python",bin_ext)))
  }
  binpaths$gdal_calc <- if (Sys.info()["sysname"] == "Windows") {
    paste0(binpaths$python," ",normalize_path(file.path(gdal_py_dir,"gdal_calc.py")))
  } else {
    normalize_path(file.path(gdal_py_dir,"gdal_calc.py"))
  }
  binpaths$gdal_fillnodata <- if (Sys.info()["sysname"] == "Windows") {
    paste0(binpaths$python," ",normalize_path(file.path(gdal_py_dir,"gdal_fillnodata.py")))
  } else {
    normalize_path(file.path(gdal_py_dir,"gdal_fillnodata.py"))
  }
  writeLines(jsonlite::toJSON(binpaths, pretty=TRUE), attr(binpaths, "path"))
  
  # set PATH
  if (Sys.info()["sysname"] == "Windows") {
    path_exi <- Sys.getenv("PATH")
    if (!any(grepl(
      normalize_path(gdal_dir), 
      normalize_path(unlist(strsplit(path_exi, ";")), mustWork = FALSE), 
      fixed=TRUE
    ))) {
      Sys.setenv(PATH = paste0(gdal_dir,";",Sys.getenv("PATH")))
    }
    if (!any(grepl(
      normalize_path(gdal_py_dir), 
      normalize_path(unlist(strsplit(path_exi, ";")), mustWork = FALSE), 
      fixed=TRUE
    ))) {
      Sys.setenv(PATH = paste0(gdal_py_dir,";",Sys.getenv("PATH")))
    }
    # on.exit(Sys.setenv(PATH = path_exi))
  }
  
  
  # path for rgdal version 1.5.2 (missing proj.db)
  # ("ERROR 1: PROJ: proj_create_from_database: Cannot find proj.db")
  if (all(
    Sys.info()["sysname"] == "Windows",
    length(list.files(Sys.getenv("PROJ_LIB"), "^proj\\.db$")) == 0
  )) {
    proj_dir_osgeo <- file.path(dirname(gdal_dir),"share/proj")
    if (length(list.files(proj_dir_osgeo, "^proj\\.db$")) > 0) {
      proj_lib_rgdal <- Sys.getenv("PROJ_LIB")
      Sys.setenv(PROJ_LIB = proj_dir_osgeo)
      # on.exit(Sys.setenv(PROJ_LIB = proj_lib_rgdal))
    }
  }
  
  print_message(
    type="message",
    "GDAL version in use: ", as.character(gdal_version)
  )
  return(invisible(TRUE))
  
}
