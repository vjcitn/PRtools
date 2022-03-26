#' get maintainer email for a single package
#' @importFrom BiocPkgTools biocBuildEmail
#' @param x character(1) package name
#' @param \dots passed to biocBuildEmail, excluding textOnly, dry.run; version = "release" or "devel" will be important
#' @export
getDevEmail = 
   function(x, ...) suppressMessages({strsplit(biocBuildEmail(x, ..., textOnly=TRUE, dry.run=TRUE), "\\n")[[1]][1]})

#' make table of package names and email addresses for a set of packages
#' @importFrom BiocPkgTools biocPkgRanges
#' @param start character(1) package name beginning range
#' @param end character(1) package name end of range
#' @param condition character(1) "ERROR" or "WARNINGS",
#' @param phase character(1) "buildsrc", "checksrc", "install", "buildbin"
#' @param version character(1) "release" (default) or "devel"
#' @examples
#' getRangeTable("ReactomePA", "SLqPCR")
#' @export
getRangeTable = function(start, end, condition="ERROR", phase="buildsrc", version="release") {
  ps = biocPkgRanges(start=start, end=end, condition=condition, phase=phase, version=version)
  nms = names(ps)
  em = sapply( nms, getDevEmail )
  data.frame(pkg=names(em), email=unname(em))
}



#> biocPkgRanges("ReactomePA", "SLqPCR") -> bb
#> args(biocPkgRanges)
#function (start, end, condition = c("ERROR", "WARNINGS"), phase = "buildsrc", 
#    version = c("devel", "release")) 
