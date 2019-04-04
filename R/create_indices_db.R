#' @title Create the indices database
#' @description The internal function checks if indices.json (the
#'  database of spectral indices) already exists; if not, it
#'  downloads source files and creates it.
#'  Since this function depends on xsltproc executable (available
#'  inly for Linux), this function can be used only from from
#'  Linux. It is not necessary, since a indices.json file is
#'  present in the package.
#' @param xslt_path (optional) The path where to install `xsltml`,
#'  an external `xsltproc` script used to convert MathML index formulas
#'  to LaTeX (default: a subdirectory of the package).
#' @param json_path (optional) The path of the output JSON file.
#'  *Warning*: to create a file which wil be usable by the package,
#'  this option must be left to NA (default location is within the
#'  package installation). Edit this only to create the file in another
#'  place for external use.
#' @param force (optional) Logical: if FALSE (default), the db is created only
#'  if missing or not updated; if TRUE, it is created in any case.
#' @return NULL
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @import data.table
#' @importFrom XML htmlTreeParse xmlRoot readHTMLTable xmlAttrs saveXML
#' @importFrom magrittr "%>%"
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom stats runif
#' @importFrom utils capture.output download.file unzip packageVersion


create_indices_db <- function(xslt_path = NA,
                              json_path = NA,
                              force = FALSE) {
  
  # to avoid NOTE on check
  n_index <- name <- longname <- . <- s2_formula <- type <- NULL
  
  # check if indices.json already exists, and if the version is updated
  # we assume that a new version of indices.json is created at every new ackage update
  if (is.na(json_path)) {
    json_path <- file.path(system.file("extdata",package="sen2r"),"indices.json")
  }
  if (system.file("extdata","indices.json", package="sen2r") == json_path) {
    if (force == FALSE) {
      return(invisible(NULL))
    }
  }
  
  # check the presence of xsltproc
  if (Sys.which("xsltproc")=="") {
    print_message(
      type="error",
      "\"xsltproc\" was not found in your system; ",
      "please install it or update your system PATH.")
  }
  
  # set XSLT path
  if (is.na(xslt_path)) {
    xslt_path <- file.path(system.file(package="sen2r"),"extdata","xslt")
  }
  
  # if missing, download xsltml to convert from MathML to LaTeX: http://fhoerni.free.fr/comp/xslt.html
  if (any(!file.exists(file.path(xslt_path,c("cmarkup.xsl","entities.xsl","glayout.xsl","mmltex.xsl","scripts.xsl","tables.xsl","tokens.xsl"))))) {
    dir.create(xslt_path, recursive=FALSE, showWarnings=FALSE)
    download.file("https://netix.dl.sourceforge.net/project/xsltml/xsltml/v.2.1.2/xsltml_2.1.2.zip",
                  file.path(xslt_path,"xsltml_2.1.2.zip"),
                  quiet = TRUE)
    unzip(file.path(xslt_path,"xsltml_2.1.2.zip"), exdir=xslt_path)
    unlink(file.path(xslt_path,"xsltml_2.1.2.zip"))
  }
  
  # Read HTML indices database from indexdatabase.de
  idb_url <- "http://www.indexdatabase.de"
  idb_s2indices_url <- file.path(idb_url,"db/is.php?sensor_id=96")
  download.file(idb_s2indices_url, s2_path <- tempfile(), method = "wget", quiet = TRUE)
  s2_html <- htmlTreeParse(s2_path, useInternalNodes = FALSE) %>% xmlRoot()
  s2_htmlinternal <- htmlTreeParse(s2_path, useInternalNodes = TRUE) %>% xmlRoot()
  s2_html_table <- s2_html[["body"]][["div"]][["table"]]
  s2_htmlinternal_table <- s2_htmlinternal[["body"]][["div"]][["table"]]
  unlink(s2_path)
  
  s2_table <- data.table(readHTMLTable(s2_htmlinternal_table, header=TRUE, stringsAsFactors=FALSE)[,1:3])
  setnames(s2_table, c( "Nr.\r\n      ", "Name\r\n      ","Abbrev.\r\n      "),
           c("n_index","longname","name"))
  
  s2_table$link <- paste0(idb_url, sapply(seq_along(s2_html_table)[-1], function(x) {
    xmlAttrs(s2_html_table[[x]][[2]][[1]])["href"]
  }))
  s2_formula_mathml <- lapply(seq_along(s2_html_table)[-1], function(x) {
    s2_html_table[[x]][[5]][[1]]
  })
  s2_formula_mathml_general <- lapply(seq_along(s2_html_table)[-1], function(x) {
    s2_html_table[[x]][[4]][[1]]
  }) # this is used for automatic band substitution
  
  # Build table
  s2_table$s2_formula <- as.character(NA)
  s2_table[,n_index:=as.integer(n_index)]
  if (any(s2_table$n_index != seq_len(nrow(s2_table)))) {
    print_message(
      type="error",
      "The index numbering in Index DataBase is altered; ",
      "please report this to a maintainer.")
  }
  
  # clean database
  n_index_toremove <- c()
  # change name to some indices
  s2_table[grep("MIR/NIR Normalized Difference",s2_table$longname),name:="NDVI2"]
  s2_table[longname=="Transformed Soil Adjusted Vegetation Index 2",name:="TSAVI2"]
  s2_table[longname=="Modified Soil Adjusted Vegetation Index",name:="MSAVI2"]
  
  # Change names containing "/"
  s2_table[,name:=gsub("/","-",name)]
  
  # remove indices without name
  n_index_toremove <- c(n_index_toremove, s2_table[name=="",n_index])
  # replacing duplicated indices
  n_index_toremove <- c(
    n_index_toremove,
    s2_table[longname %in% c(
      "RDVI",
      "RDVI2",
      "Normalized Difference NIR/Green Green NDVI",
      "Enhanced Vegetation Index 2"
    ),n_index])
  # (duplicated_indices <- unique(s2_table$name[duplicated(s2_table$name)]))
  # removing indices with incorrect formulas
  n_index_toremove <- c(
    n_index_toremove,
    s2_table[name %in% c(
      "CRI550","CRI700","GEMI","IR550","IR700","LWCI","mCRIG","mCRIRE","CCCI","Ctr6",
      "ND800:680","NLI","RARSa1","RARSa2","RARSa3","RARSa4","RARSc3","RARSc4",
      "mARI","NDVIc","RSR","SRSWIRI:NIR","SARVI","SQRT(IR:R)","TNDVI"
    ),n_index]) # TODO some indices can be reintegrated
  # clean
  n_index_toremove <- sort(as.integer(n_index_toremove))
  s2_formula_mathml <- s2_formula_mathml[!s2_table$n_index %in% n_index_toremove]
  s2_formula_mathml_general <- s2_formula_mathml_general[!s2_table$n_index %in% n_index_toremove]
  s2_table <- s2_table[!n_index %in% n_index_toremove,]
  
  
  ## Convert MathML to LaTeX on each row, using external tool
  parent_regex <- "\\{((?>[^{}]+)|(?R))*\\}"
  max_iter = 7 # maximum numbero of iterations for nested fractions
  
  for (sel_row in seq_len(nrow(s2_table))) {
    
    
    saveXML(s2_formula_mathml[[sel_row]],
            tmp_infile <- tempfile())
    
    system(
      paste0(
        Sys.which("xsltproc")," ",
        file.path(xslt_path,"mmltex.xsl")," ",
        "\"",tmp_infile,"\" > ",
        "\"",tmp_outfile <- tempfile(),"\""),
      intern = Sys.info()["sysname"] == "Windows"
    )
    
    # convert manually from latex to formula
    tmp_latex <- suppressWarnings(readLines(tmp_outfile)) %>%
      gsub("^\\$ *(.*) *\\$$","\\1",.) %>% # remove math symbols
      gsub("\\\\textcolor\\[rgb\\]\\{[0-9\\.\\,]+\\}", "\\\\var", .) %>% # RGB indications are variable names
      gsub(paste0("\\\\mathrm",parent_regex), "\\1", ., perl=TRUE) %>% # remove mathrm
      gsub("\\\\left\\|([^|]+)\\\\right\\|", "abs(\\1)", .) %>% # abs
      gsub("\u00B7", "*", .)  %>% # replace muddle point
      gsub("\\\\times", "*", .)  %>% # replace times
      gsub("\\\\&InvisibleTimes;", "*", .)  %>% # remove invisibles multiplications
      gsub("\u0096", "-band_", .)  %>% # unicode START OF GUARDED AREA as "-"
      gsub("\\\\var\\{([0-9][0-9a]?)\\}", "band\\_\\1", .)  %>% # recognise band names
      gsub("\\\\var\\{([^}]+)\\}", "par\\_\\1", .)  %>% # recognise other elements as parameters
      gsub("par\\_([^0-9A-Za-z])", "\\1", .)  %>% # error in two indices
      gsub("\\\\left\\(", "(", .)  %>% # parenthesis
      gsub("\\\\right\\)", ")", .) # parenthesis
    
    # remove temporary files
    unlink(tmp_infile)
    unlink(tmp_outfile)
    
    n_iter <- 1
    while (length(grep("[{}]", tmp_latex))>0 & n_iter<=max_iter) {
      tmp_latex <- gsub(paste0("\\\\frac",parent_regex,parent_regex), "(\\1)/(\\2)", tmp_latex, perl=TRUE) # convert fractions
      tmp_latex <- gsub(paste0("\\\\sqrt",parent_regex), "sqrt(\\1)", tmp_latex, perl=TRUE) # convert sqrt
      tmp_latex <- gsub(paste0(parent_regex,"\\^",parent_regex),"power\\(\\1,\\2\\)", tmp_latex, perl=TRUE) # square
      n_iter <- n_iter+1
    }
    
    s2_table[sel_row,"s2_formula"] <- tmp_latex
    # print(sel_row)
    
  }
  
  # last manual corrections on formulas
  s2_table[,s2_formula:=gsub("par\\_([0-9])", "band_\\1", s2_table$s2_formula)] # some bands were wrongly classified as parameters
  s2_table$name[s2_table$name=="TCI"] <- "TCIdx" # in order not to mess with TCI True Color Image product
  s2_table$name[s2_table$name=="NDSI"] <- "NDSaI" # in order not to mess with Normalized Difference Snow Index
  
  # rename parameters (A, B, ...)
  s2_table[,s2_formula:=gsub("par\\_([aALyY]r?)", "par_a", s2_table$s2_formula)] # first parameters (a, A, ar, y, Y, L) -> "a"
  s2_table[,s2_formula:=gsub("par\\_([bB])", "par_b", s2_table$s2_formula)] # second parameters (b, B) -> "b"
  s2_table[,s2_formula:=gsub("par\\_([X])", "par_c", s2_table$s2_formula)] # third parameters ("X") -> "c"
  
  ## Test expressions
  # build a data.frame with random values
  test_df <- runif(17,0,1)
  names(test_df) <- c(paste0("band_",c(1:12,"8a")),"par_a","par_b","par_c","par_d")
  test_df <- as.data.frame(t(test_df))
  # define power() as in numpy
  power <- function(x,y){x^y}
  
  test_results <-with(test_df,
                      sapply(s2_table$s2_formula,
                             function(x) {
                               tryCatch(eval(parse(text=x)),
                                        error = function(y){return(y)},
                                        warning = function(y){return(y)})
                             }
                      ))
  test_type <-with(test_df,
                   sapply(s2_table$s2_formula,
                          function(x) {
                            tryCatch(ifelse(is.numeric(eval(parse(text=x))),return("ok")),
                                     error = function(y){return("error")},
                                     warning = function(y){return("warning")})
                          }
                   ))
  test_out <- data.table("formula"=s2_table$s2_formula,
                         "n_index"=s2_table$n_index,
                         "index"=s2_table$name,
                         "output"=ifelse(test_type=="ok",test_results,NA),
                         "type"=test_type)
  
  # # These indices contain errors:
  # test_out[type=="error",]
  # # for now, remove them.
  # # TODO check them and correct manually
  # # TODO2 check all the aoutmatic parsings (maybe expression do not provide errors but they are different from originals)
  n_index_toremove <- test_out[type%in%c("error","warning"),n_index]
  s2_formula_mathml <- s2_formula_mathml[!s2_table$n_index %in% n_index_toremove]
  s2_formula_mathml_general <- s2_formula_mathml_general[!s2_table$n_index %in% n_index_toremove]
  s2_table <- s2_table[!n_index %in% n_index_toremove,]
  
  
  ## Check indices manually
  # (this is necessary because most of the indices is associated to wrong
  # Sentinel-2 bands, and because parameter values are missing)
  # logical: FALSE for not checked, TRUE for checked
  s2_table$checked <- FALSE
  # numerics: values for index parameters
  s2_table$d <- s2_table$c <- s2_table$b <- s2_table$a <- as.numeric(NA)
  
  # by default, make all these changes
  # (only bands named as "RED", "BLUE" and "NIR" are changed, because
  # these are the wrong ones, while normally bands named in other ways
  # - like "700nm" - are correctly associated)
  s2_table[grepl("RED",s2_formula_mathml_general),
           s2_formula := gsub("band_5","band_4",s2_formula)] # B5 to B4 (Red)
  s2_table[grepl("NIR",s2_formula_mathml_general),
           s2_formula := gsub("band_9","band_8",s2_formula)] # B9 to B8 (NIR)
  s2_table[grepl("BLUE",s2_formula_mathml_general),
           s2_formula := gsub("band_1","band_2",s2_formula)] # B2 to B1 (Blue)
  
  # set as checked for indices ok after previous changes
  s2_table[name %in% c("NDVI","SAVI","MCARI","MCARI2","TCARI","ARVI","NDRE",
                       "BNDVI","GNDVI","NDII","TCIdx","MSAVI2","OSAVI",
                       "NBR","EVI2",
                       "MTVI2","MCARI:MTVI2","TCARI:OSAVI"),checked:=TRUE]
  
  # set default parameter values
  s2_table[name=="SAVI", a:=0.5] # default value for L (here "a") parameter
  s2_table[name=="ARVI", a:=1] # default value for gamma (here "a") parameter
  
  # add missing indices
  s2_table_new <- rbindlist(list(
    "NDFI" = data.frame(
      n_index = 301,
      longname = "Normalized Difference Flood Index B1B7",
      name = "NDFI",
      link = "https://doi.org/10.1371/journal.pone.0088741",
      s2_formula = "(band_4-band_12)/(band_4+band_12)",
      checked = TRUE
    ),
    "NDFI2" = data.frame(
      n_index = 302,
      longname = "Normalized Difference Flood Index B1B6",
      name = "NDFI2",
      link = "https://doi.org/10.1371/journal.pone.0088741",
      s2_formula = "(band_4-band_11)/(band_4+band_11)",
      checked = TRUE
    ),
    "NDSI" = data.frame(
      n_index = 303,
      longname = "Normalize Difference Snow Index",
      name = "NDSI",
      link = "https://doi.org/10.1007/978-90-481-2642-2_376",
      s2_formula = "(band_3-band_11)/(band_3+band_11)",
      checked = TRUE
    ),
    "NBR2" = data.frame(
      n_index = 304,
      longname = "Normalized Burn Ratio 2",
      name = "NBR2",
      link = "https://landsat.usgs.gov/sites/default/files/documents/si_product_guide.pdf",
      s2_formula = "(band_11-band_12)/(band_11+band_12)",
      checked = TRUE
    ),
    "MIRBI" = data.frame(
      n_index = 305,
      longname = "Mid-Infrared Burn Index",
      name = "MIRBI",
      link = "https://doi.org/10.1080/01431160110053185",
      s2_formula = "(1E-3*band_12)-(9.8E-4*band_11)+2E-4",
      checked = TRUE
    ),
    "CSI" = data.frame(
      n_index = 306,
      longname = "Char Soil Index",
      name = "CSI",
      link = "https://doi.org/10.1016/j.rse.2005.04.014",
      s2_formula = "(band_8)/(band_12)",
      checked = TRUE
    ),
    "CRred" = data.frame(
      n_index = 307,
      longname = "Continuum Removal in the red",
      name = "CRred",
      link = "Panigada et al. 2019 (in press)",
      s2_formula = "(band_4)/(band_3+par_a*(band_6-band_3))",
      checked = TRUE
    ),
    "CRred_2A" = data.frame(
      n_index = 308,
      longname = "Continuum Removal in the red (Sentinel-2A reflectances)",
      name = "CRred_2A",
      link = "Panigada et al. 2019 (in press)",
      s2_formula = "(band_4)/(band_3+par_a*(band_6-band_3))",
      checked = FALSE,
      a = ((664.6-559.8)/(740.5-559.8)) # reflectances for S2A
    ),
    "CRred_2B" = data.frame(
      n_index = 309,
      longname = "Continuum Removal in the red (Sentinel-2B reflectances)",
      name = "CRred_2B",
      link = "Panigada et al. 2019 (in press)",
      s2_formula = "(band_4)/(band_3+par_a*(band_6-band_3))",
      checked = FALSE,
      a = ((664.9-559.0)/(739.1-559.0)) # reflectances for S2B 
    ),
    "CRred_0" = data.frame(
      n_index = 310,
      longname = "Continuum Removal in the red (standard reflectances)",
      name = "CRred_0",
      link = "Panigada et al. 2019 (in press)",
      s2_formula = "(band_4)/(band_3+par_a*(band_6-band_3))",
      checked = FALSE,
      a = ((665-560)/(740-560)) # standard reflectances
    ),
    "BDred" = data.frame(
      n_index = 311,
      longname = "Band Depth in the red",
      name = "BDred",
      link = "Panigada et al. 2019 (in press)",
      s2_formula = "1-(band_4)/(band_3+par_a*(band_6-band_3))",
      checked = TRUE
    ),
    "BDred_2A" = data.frame(
      n_index = 312,
      longname = "Band Depth in the red (Sentinel-2A reflectances)",
      name = "BDred_2A",
      link = "Panigada et al. 2019 (in press)",
      s2_formula = "1-(band_4)/(band_3+par_a*(band_6-band_3))",
      checked = FALSE,
      a = ((664.6-559.8)/(740.5-559.8)) # reflectances for S2A
    ),
    "BDred_2B" = data.frame(
      n_index = 313,
      longname = "Band Depth in the red (Sentinel-2B reflectances)",
      name = "BDred_2B",
      link = "Panigada et al. 2019 (in press)",
      s2_formula = "1-(band_4)/(band_3+par_a*(band_6-band_3))",
      checked = FALSE,
      a = ((664.9-559.0)/(739.1-559.0)) # reflectances for S2B 
    ),
    "BDred_0" = data.frame(
      n_index = 314,
      longname = "Band Depth in the red (standard reflectances)",
      name = "BDred_0",
      link = "Panigada et al. 2019 (in press)",
      s2_formula = "1-(band_4)/(band_3+par_a*(band_6-band_3))",
      checked = FALSE,
      a = ((665-560)/(740-560)) # standard reflectances
    ),
    "CRred2" = data.frame(
      n_index = 315,
      longname = "Continuum Removal in the red 2",
      name = "CRred2",
      link = "",
      s2_formula = "(((par_b-par_a)*(band_4+band_3)+(par_c-par_b)*(band_5+band_4)+(par_d-par_c)*(band_6+band_5))/((par_d-par_a)*(band_6+band_3)))",
      checked = FALSE
    ),
    "CRred2_2A" = data.frame(
      n_index = 316,
      longname = "Continuum Removal in the red 2 (Sentinel-2A reflectances)",
      name = "CRred2_2A",
      link = "",
      s2_formula = "(((par_b-par_a)*(band_4+band_3)+(par_c-par_b)*(band_5+band_4)+(par_d-par_c)*(band_6+band_5))/((par_d-par_a)*(band_6+band_3)))",
      checked = FALSE,
      a = 559.8, b = 664.6, c = 704.1, d = 740.5 # reflectances for S2A
    ),
    "CRred2_2B" = data.frame(
      n_index = 317,
      longname = "Continuum Removal in the red 2 (Sentinel-2B reflectances)",
      name = "CRred2_2B",
      link = "",
      s2_formula = "(((par_b-par_a)*(band_4+band_3)+(par_c-par_b)*(band_5+band_4)+(par_d-par_c)*(band_6+band_5))/((par_d-par_a)*(band_6+band_3)))",
      checked = FALSE,
      a = 559.0, b = 664.9, c = 703.8, d = 739.1 # reflectances for S2B
    ),
    "CRred2_0" = data.frame(
      n_index = 318,
      longname = "Continuum Removal in the red 2 (standard reflectances)",
      name = "CRred2_0",
      link = "",
      s2_formula = "(((par_b-par_a)*(band_4+band_3)+(par_c-par_b)*(band_5+band_4)+(par_d-par_c)*(band_6+band_5))/((par_d-par_a)*(band_6+band_3)))",
      checked = FALSE,
      a = 560, b = 665, c = 704, d = 740 # standard reflectances
    )
  ), fill=TRUE)
  
  s2_table_new[,n_index:=as.integer(n_index)]
  s2_table_new[,longname:=as.character(longname)]
  s2_table_new[,name:=as.character(name)]
  s2_table_new[,link:=as.character(link)]
  s2_table_new[,s2_formula:=as.character(s2_formula)]
  s2_table <- rbind(s2_table, s2_table_new, fill=TRUE)
  
  # add empty elements in MathML formulas
  for (i in length(s2_formula_mathml) + seq_len(nrow(s2_table) - length(s2_formula_mathml))) {
    s2_formula_mathml[[i]] <- NA
    s2_formula_mathml_general[[i]] <- NA
  }
  
  
  ## Convert in JSON
  # convert MathML to character
  s2_table$s2_formula_mathml <- sapply(
    s2_formula_mathml_general, # replaced from s2_formula_mathml in order not to show uncorrect band numbers
    function(x) {
      if (is(x, "XMLNode")) {
        paste(capture.output(print(x)), collapse="\n")
      } else {
        ""
      }
    }
  )
  
  json_table <- list(
    "indices" = s2_table,
    "pkg_version" = as.character(packageVersion("sen2r")),
    "creation_date" = as.character(Sys.time())
  )
  writeLines(jsonlite::toJSON(json_table, digits=NA, pretty=TRUE), json_path)
  return(invisible(NULL))
}
