

# WIP to read indices from indexdatabase.de and parse MathML to usable expressions

# Read HTML indices database from indexdatabase.de
library(XML)
library(magrittr)
library(data.table)
s2_url <- "http://www.indexdatabase.de/db/is.php?sensor_id=96"
download.file(s2_url, s2_path <- tempfile(), method = "wget", quiet = TRUE)
s2_html <- htmlTreeParse(s2_path, useInternalNodes = FALSE) %>% xmlRoot()
s2_htmlinternal <- htmlTreeParse(s2_path, useInternalNodes = TRUE) %>% xmlRoot()
s2_html_table <- s2_html[["body"]][["div"]][["table"]]
s2_htmlinternal_table <- s2_htmlinternal[["body"]][["div"]][["table"]]
# doc <- htmlTreeParse(sub("s", "", fileURL), useInternal = TRUE)

s2_table <- readHTMLTable(s2_htmlinternal_table, header=TRUE, stringsAsFactors=FALSE)[,-c(4,6,7)]
names(s2_table) <- c("n_index","longname","name","formula")
# names_products <- names(xmlChildren(rootNode)) # names of the single products
# names_products <- names_products[names_products != "comment"]


# use xsltproc to convert from MathML to LaTeX: http://fhoerni.free.fr/comp/xslt.html

dir.create(xlst_path <- "/home/lranghetti/tmp/xlst/")

download.file("https://netix.dl.sourceforge.net/project/xsltml/xsltml/v.2.1.2/xsltml_2.1.2.zip",
              file.path(xlst_path,"xsltml_2.1.2.zip"),
              method = "wget", quiet = TRUE)
unzip(file.path(xlst_path,"xsltml_2.1.2.zip"), exdir=xlst_path)


getwd() -> prec_wd
setwd(xlst_path)
parent_regex <- "\\{((?>[^{}]+)|(?R))*\\}"
max_iter = 6 # maximum numbero of iterations for nested fractions

for (sel_row in seq_len(nrow(s2_table))) {

  saveXML(s2_html_table[[sel_row+1]][[5]][[1]],
          file.path(xlst_path, "ex_input.txt"))

  system("xsltproc mmltex.xsl ex_input.txt > ex_output.txt")

  # convert manually from latex to formula
  tmp_latex <- suppressWarnings(readLines(file.path(xlst_path,"ex_output.txt"))) %>%
    gsub("^\\$ *(.*) *\\$$","\\1",.) %>% # remove math symbols
    gsub("\\\\textcolor\\[rgb\\]\\{[0-9\\.\\,]+\\}", "\\\\var", .) %>% # RGB indications are variable names
    gsub(paste0("\\\\mathrm",parent_regex), "\\1", ., perl=TRUE) %>% # remove mathrm
    gsub("\\\\left\\|([^|]+)\\\\right\\|", "abs(\\1)", .) %>% # abs
    gsub("·", "*", .)  %>% # replace "per"
    gsub("\\\\&InvisibleTimes;", "*", .)  %>% # remove invisibles "per"
    gsub("\\\\var\\{([0-9]+)\\}", "band\\_\\1", .)  %>% # recognise band names
    gsub("\\\\var\\{([^}]+)\\}", "par\\_\\1", .)  %>% # recognise other elements as parameters
    gsub("\\\\left\\(", "(", .)  %>% # parenthesis
    gsub("\\\\right\\)", ")", .) %>% # parenthesis
    gsub(paste0(parent_regex,"\\^",parent_regex),"\\1^\\2",., perl=TRUE) %>% # square
    gsub(paste0("\\\\sqrt",parent_regex), "sqrt(\\1)", ., perl=TRUE) #%>% # sqrt

  n_iter <- 1
  while (length(grep("\\\\frac", tmp_latex))>0 & n_iter<=max_iter) {
    tmp_latex <- gsub(paste0("\\\\frac",parent_regex,parent_regex), "(\\1)/(\\2)", tmp_latex, perl=TRUE) # convert fractions
    n_iter <- n_iter+1
  }

  s2_table[sel_row,"formula"] <- tmp_latex
  print(sel_row)

}
setwd(prec_wd)



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


