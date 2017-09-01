

# WIP to read indices from indexdatabase.de and parse MathML to usable expressions






# Read HTML indices database from indexdatabase.de
library(XML)
library(magrittr)
library(data.table)
idb_url <- "http://www.indexdatabase.de"
idb_s2indices_url <- file.path(idb_url,"db/is.php?sensor_id=96")
download.file(idb_s2indices_url, s2_path <- tempfile(), method = "wget", quiet = TRUE)
s2_html <- htmlTreeParse(s2_path, useInternalNodes = FALSE) %>% xmlRoot()
s2_htmlinternal <- htmlTreeParse(s2_path, useInternalNodes = TRUE) %>% xmlRoot()
s2_html_table <- s2_html[["body"]][["div"]][["table"]]
s2_htmlinternal_table <- s2_htmlinternal[["body"]][["div"]][["table"]]
# doc <- htmlTreeParse(sub("s", "", fileURL), useInternal = TRUE)

s2_table <- data.table(readHTMLTable(s2_htmlinternal_table, header=TRUE, stringsAsFactors=FALSE)[,1:3])
setnames(s2_table, c( "Nr.\r\n      ", "Name\r\n      ","Abbrev.\r\n      "),
         c("n_index","longname","name"))

s2_table$link <- paste0(idb_url, sapply(seq_along(s2_html_table)[-1], function(x) {
  xmlAttrs(s2_html_table[[x]][[2]][[1]])["href"]
}))

s2_formula_mathml <- lapply(seq_along(s2_html_table)[-1], function(x) {
  s2_html_table[[x]][[5]][[1]]
})
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
s2_table[grep("MIR/NIR Normalized Difference",longname),name:="NDVI2"]
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
s2_table <- s2_table[!n_index %in% n_index_toremove,]


getwd() -> prec_wd
setwd(xlst_path)
parent_regex <- "\\{((?>[^{}]+)|(?R))*\\}"
max_iter = 7 # maximum numbero of iterations for nested fractions

for (sel_row in seq_len(nrow(s2_table))) {

  saveXML(s2_formula_mathml[[sel_row]],
          file.path(xlst_path, "ex_input.txt"))

  system("xsltproc mmltex.xsl ex_input.txt > ex_output.txt")

  # convert manually from latex to formula
  tmp_latex <- suppressWarnings(readLines(file.path(xlst_path,"ex_output.txt"))) %>%
    gsub("^\\$ *(.*) *\\$$","\\1",.) %>% # remove math symbols
    gsub("\\\\textcolor\\[rgb\\]\\{[0-9\\.\\,]+\\}", "\\\\var", .) %>% # RGB indications are variable names
    gsub(paste0("\\\\mathrm",parent_regex), "\\1", ., perl=TRUE) %>% # remove mathrm
    gsub("\\\\left\\|([^|]+)\\\\right\\|", "abs(\\1)", .) %>% # abs
    gsub("·", "*", .)  %>% # replace "per"
    gsub("\\\\times", "*", .)  %>% # replace "per"
    gsub("\\\\&InvisibleTimes;", "*", .)  %>% # remove invisibles "per"
    gsub("\u0096", "-band_", .)  %>% # strange special character
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
  print(sel_row)

}
setwd(prec_wd)

# other manual corrections
s2_table[,s2_formula:=gsub("par\\_([0-9])", "band_\\1", s2_table$s2_formula)] # some bands were wrongly classified as parameters
s2_table$s2_formula[s2_table$name=="TCI"] <- gsub("band\\_1\\.5","1.5",s2_table[name=="TCI",s2_formula]) # specific error

# rename parameters (A, B, ...)
s2_table[,s2_formula:=gsub("par\\_([aALyY]r?)", "a", s2_table$s2_formula)] # first parameters (a, A, ar, y, Y, L) -> "a"
s2_table[,s2_formula:=gsub("par\\_([bB])", "b", s2_table$s2_formula)] # second parameters (b, B) -> "b"
s2_table[,s2_formula:=gsub("par\\_([X])", "x", s2_table$s2_formula)] # third parameters ("X") -> "x"


# test expressions
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
test_out[type=="error",]

# for now, remove indices with errors.
# TODO check them and correct manually
# TODO2 check all the aoutmatic parsings (maybe expression do not provide errors but they are different from originals)
n_index_toremove <- test_out[type%in%c("error","warning"),n_index]
s2_formula_mathml <- s2_formula_mathml[!s2_table$n_index %in% n_index_toremove]
s2_table <- s2_table[!n_index %in% n_index_toremove,]


## Convert in JSON
library(jsonlite)
s2_table$s2_formula_mathml <- sapply(s2_formula_mathml, function(x) {
  paste(capture.output(print(x)), collapse="\n")
})

index_jsonpath <- tempfile()
writeLines(toJSON(s2_table, pretty=TRUE), index_jsonpath)










# table(stringr::str_extract_all(paste(s2_table$s2_formula[grep("par",s2_table$s2_formula)], collapse=" "), "par\\_[0-9a-zA-Z]*"))
# s2_table[grep("par",s2_table$s2_formula),list(n_index,name,s2_formula)]
# table(stringr::str_extract_all(paste(s2_table$s2_formula[grep("band",s2_table$s2_formula)], collapse=" "), "band\\_[0-9a-zA-Z]*"))


# tmp1 <- "altro{sqrt{sqrt{sqrt{x}}}}"
# tmp2 <- "altro{sqrt{sqrt{altro{x}}}}"
# tmp3 <- "{1, {2, 3}} {4, 5}"
#
# parent_regex <- "\\{((?>[^{}]+)|(?R))*\\}"
# gsub(paste0("sqrt",parent_regex),"radq(\\1)",tmp1, perl=TRUE)
# gsub(paste0("sqrt",parent_regex),"radq(\\1)",tmp2, perl=TRUE)
#
# parent_regex_2 <- "\\{((?>[^(?!\\{|\\})]+)|(?R))*\\}"
# gsub(paste0("sqrt",parent_regex_2),"radq(\\1)",tmp1, perl=TRUE)
# gsub(paste0("sqrt",parent_regex_2),"radq(\\1)",tmp2, perl=TRUE)
#
# parent_regex_3 <- "\\{((?>[^(?!sqrt\\{|\\})]+)|(?R))*\\}"
# gsub(paste0("sqrt",parent_regex_3),"radq(\\1)",tmp1, perl=TRUE)
# gsub(paste0("sqrt",parent_regex_3),"radq(\\1)",tmp2, perl=TRUE)
#
#
#
#
# parent_regex <- "\\{((?>[^{}]+)|(?R))*\\}"
# parent_regex_4 <- "\\{((?:[^{}]+|(?R))*)\\}"
# gsub(paste0("sqrt",parent_regex),"radq(\\1)",tmp1, perl=TRUE)
# gsub(paste0("sqrt",parent_regex_4),"radq(\\1)",tmp2, perl=TRUE)
#
#
# l2a_dir_existing_list = [f for f in os.listdir(l2a_dir) if re.search(r'(?:^S2A\_MSIL2A\_[0-9T]+\_[N0-9]+\_[R0-9]+\_[A-Z0-9T]+\_[0-9T]+|^S2A\_[A-Z]+\_PRD\_MSIL2A\_[A-Z]+\_[A-Z0-9T]+\_[R0-9]+\_[A-Z0-9VT]+\_[0-9T]+)\.SAFE$', f)]
# l2a_zip_existing_list = [f for f in os.listdir(l2a_dir) if re.search(r'(?:^S2A\_MSIL2A\_[0-9T]+\_[N0-9]+\_[R0-9]+\_[A-Z0-9T]+\_[0-9T]+|^S2A\_[A-Z]+\_PRD\_MSIL2A\_[A-Z]+\_[A-Z0-9T]+\_[R0-9]+\_[A-Z0-9VT]+\_[0-9T]+)\.SAFE\.zip$', f)]
#
#
#
# py_str <-
# "def recursive_bracket_parser(s, i):
#   while i < len(s):
#     if s[i] == '{':
#       i = recursive_bracket_parser(s, i+1)
#     elif s[i] == '}':
#       return i+1
#     else:
#       # process whatever is at s[i]
#       i += 1
#   return i"
# py_regex1 = reticulate::py_run_string(code = py_str, local = FALSE, convert = FALSE)
#
# py_regex1$recursive_bracket_parser(tmp2,as.integer(7))
#
# regex <- import("regex",convert=FALSE)
#
# py_str3 <- "
# import regex
# def bracket_parser(s,rgx1,rgx2):
#   out = regex.findall(rgx1+\"((?>[^\"+rgx2+rgx1+\"]+|(?R))*)\"+rgx2, s)
#   return out
# def sqrt_convert(s,rgx1,rgx2):
#   out = regex.findall(rgx1+\"((?>[^\"+rgx2+rgx1+\"]+|(?R))*)\"+rgx2, s)
#   return out"
# py_regex3 = reticulate::py_run_string(py_str3, convert=FALSE)
# py_regex3$bracket_parser(tmp3,"{","}")
#
#
#
#
# py_str2 <-
# "import re
# l1c_list = ['altro{sqrt{sqrt{altro{x}}}}']
# l1c_partnames = [re.compile('\\{((?>[^(?!sqrt\\{|\\})]+)|(?R))*\\}').match(f).group(1) for f in l1c_list]"
# py_regex2 = reticulate::py_run_string(code = py_str2, local = FALSE, convert = FALSE)


