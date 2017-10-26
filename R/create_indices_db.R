#' @title Create the indices database
#' @description The internal function checks if idices.json (the
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
#'  if missing or not updated; if TRUE, it is created in any case..
#' @return NULL
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom data.table data.table setnames ":="
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
    json_path <- file.path(system.file("extdata",package="fidolasen"),"indices.json")
  }
  if (system.file("extdata","indices.json", package="fidolasen") == json_path) {
    json_version <- jsonlite::fromJSON(json_path)$fidolasen_version %>%
      package_version()
    if (force == FALSE & json_version >= packageVersion("fidolasen")) {
      return(NULL)
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
    xslt_path <- file.path(system.file(package="fidolasen"),"extdata","xslt")
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
  s2_table[longname=="Enhanced Vegetation Index 2 -2",name:="EVI3"]
  # remove indices without name
  n_index_toremove <- c(n_index_toremove, s2_table[name=="",n_index])
  # replacing duplicated indices
  n_index_toremove <- c(
    n_index_toremove,
    s2_table[longname %in% c("RDVI","RDVI2","Normalized Difference NIR/Green Green NDVI"),n_index])
  # (duplicated_indices <- unique(s2_table$name[duplicated(s2_table$name)]))
  # removing indices with incorrect formulas
  n_index_toremove <- c(
    n_index_toremove,
    s2_table[name %in% c("CRI550","CRI700","GEMI","IR550","IR700","LWCI","mCRIG","mCRIRE","CCCI","Ctr6",
                         "ND800/680","NLI","RARSa1","RARSa2","RARSa3","RARSa4","RARSc3","RARSc4",
                         "mARI","NDVIc","RSR","SRSWIRI/NIR","SARVI","SQRT(IR/R)","TNDVI"),n_index]) # TODO some indices can be reintegrated
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

    n_iter <- 1
    while (length(grep("[{}]", tmp_latex))>0 & n_iter<=max_iter) {
      tmp_latex <- gsub(paste0("\\\\frac",parent_regex,parent_regex), "(\\1)/(\\2)", tmp_latex, perl=TRUE) # convert fractions
      tmp_latex <- gsub(paste0("\\\\sqrt",parent_regex), "sqrt(\\1)", tmp_latex, perl=TRUE) # convert sqrt
      tmp_latex <- gsub(paste0(parent_regex,"\\^",parent_regex),"\\1^\\2", tmp_latex, perl=TRUE) # square
      n_iter <- n_iter+1
    }

    s2_table[sel_row,"s2_formula"] <- tmp_latex
    # print(sel_row)

  }

  # last manual corrections on formulas
  s2_table[,s2_formula:=gsub("par\\_([0-9])", "band_\\1", s2_table$s2_formula)] # some bands were wrongly classified as parameters
  s2_table$s2_formula[s2_table$name=="TCI"] <- gsub("band\\_1\\.5","1.5",s2_table[name=="TCI",s2_formula]) # specific error

  # rename parameters (A, B, ...)
  s2_table[,s2_formula:=gsub("par\\_([aALyY]r?)", "a", s2_table$s2_formula)] # first parameters (a, A, ar, y, Y, L) -> "a"
  s2_table[,s2_formula:=gsub("par\\_([bB])", "b", s2_table$s2_formula)] # second parameters (b, B) -> "b"
  s2_table[,s2_formula:=gsub("par\\_([X])", "x", s2_table$s2_formula)] # third parameters ("X") -> "x"

  ## Test expressions
  # build a data.frame with random values
  test_df <- runif(16,0,1)
  names(test_df) <- c(paste0("band_",c(1:12,"8a")),"a","b","x")
  test_df <- as.data.frame(t(test_df))

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
  s2_table$x <- s2_table$b <- s2_table$a <- as.numeric(NA)

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
                       "BNDVI","GNDVI","NDII","TCI","MSAVI","OSAVI","MTVI2",
                       "MCARI/MTVI2","TCARI/OSAVI"),checked:=TRUE]

  # set default parameter values
  s2_table[name=="SAVI", a:=0.5] # default value for L (here "a") parameter

  # add missing indices
  s2_table_new <- data.table(
    n_index = 300,
    longname = c(
      "Normalized Difference Flood Index B1B7",
      "Normalized Difference Flood Index B1B6"
    ),
    name = c("NDFI","NDFI2"),
    link = c(
      "https://doi.org/10.1371/journal.pone.0088741",
      "https://doi.org/10.1371/journal.pone.0088741"
    ),
    s2_formula = c(
      "(band_4-band_12)/(band_4+band_12)",
      "(band_4-band_11)/(band_4+band_11)"
    ),
    checked = rep(TRUE,2),
    a = c(NA,NA),
    b = c(NA,NA),
    x = c(NA,NA)
  )
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
    "fidolasen_version" = as.character(packageVersion("fidolasen")),
    "creation_date" = as.character(Sys.time())
  )
  writeLines(jsonlite::toJSON(json_table, pretty=TRUE), json_path)
  return(NULL)

}
