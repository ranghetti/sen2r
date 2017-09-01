
create_indices_db <- function(xslt_path=NA, json_path=NA) {

  # set XSLT path
  if (is.na(xslt_path)) {
    xslt_path <- file.path(system.file(package="fidolasen"),"xslt")
  }

  # if missing, download XSLT to convert from MathML to LaTeX: http://fhoerni.free.fr/comp/xslt.html
  if (any(!file.exists(file.path(xslt_path,c("cmarkup.xsl","entities.xsl","glayout.xsl","mmltex.xsl","scripts.xsl","tables.xsl","tokens.xsl"))))) {
    dir.create(xslt_path, recursive=FALSE, showWarnings=FALSE)
    download.file("https://netix.dl.sourceforge.net/project/xsltml/xsltml/v.2.1.2/xsltml_2.1.2.zip",
                  file.path(xslt_path,"xsltml_2.1.2.zip"),
                  quiet = TRUE)
    unzip(file.path(xslt_path,"xsltml_2.1.2.zip"), exdir=xslt_path)
    unlink(file.path(xslt_path,"xsltml_2.1.2.zip"))
  }

  # WIP


}
