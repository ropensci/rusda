#' Downloads studies for input study IDs
#' 
#' Downloads studies for study IDs which are output of associations
#' @param id a single study id or a vector of study ids
#' 
#' @return an object of class \code{list} with studies
#' 
#' @author Franz-Sebastian Krah


getStudy_single <- function(id = 42264){
url <- paste0("https://nt.ars-grin.gov/fungaldatabases/fungushost/new_rptOneLit.cfm?fungRec=", id, "&thisError=")

pars <- rawToChar(GET(url)$content)
pars <- htmlTreeParse(pars, useInternalNodes = TRUE)
pars <- xpathApply(pars, "//p", xmlValue)
pars <- unlist(pars)
res <- pars[[1]]
return(res)
}

getStudy <- function(id = c(94, 42264, 6135)){
  res <- lapply(id, getStudy_single)
  return(res)
}
