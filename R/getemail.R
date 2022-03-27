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
#' @param dump logical(1) if TRUE (default) produce pastable text
#' @examples
#' getRangeTable("ReactomePA", "SLqPCR")
#' @export
getRangeTable = function(start, end, condition="ERROR", phase="buildsrc", version="release",
    dump=TRUE) {
  ps = biocPkgRanges(start=start, end=end, condition=condition, phase=phase, version=version)
  nms = names(ps)
  em = sapply( nms, getDevEmail )
  lp = getLandingPage( nms, version=version )
  ans = data.frame(pkg=names(em), email=unname(em), url=lp)
  versionnum = "3.14"
  if (version == "devel") versionnum = "3.15"
  if (!dump) return(ans)
  cat("subject:\n----\n")
  cat(sprintf("Your Bioconductor package is in %s state in %s phase in version %s\n", condition, phase, versionnum))
  cat("\n----\n")
  writeLines(plzstr())
  cat("\n----\n")
  cat("package names\n----\n")
  writeLines(ans$pkg)
  cat("\n----\n")
  cat("email\n----\n")
  writeLines(ans$email)
  cat("\n----\n")
  cat("url\n----\n")
  writeLines(paste(ans$pkg, ans$url))
  cat("\n----\n")
  invisible(ans)
}

plzstr = function() "Please find your contributed package below, and check the given link to the
landing page.  Among the badges you will find some red ones.  Please correct the
errors.  Thanks!"


#> biocPkgRanges("ReactomePA", "SLqPCR") -> bb
#> args(biocPkgRanges)
#function (start, end, condition = c("ERROR", "WARNINGS"), phase = "buildsrc", 
#    version = c("devel", "release")) 

#' make URL for landing page
#' @param x character(1) package name
#' @param version character(1) "release" or "devel"
getLandingPage = function(x, version="release") {
  sprintf("https://bioconductor.org/packages/%s/bioc/html/%s.html", version, x)
}
