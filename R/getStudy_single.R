
getStudy_single <- function(id = 42264){
  url <- paste0("https://nt.ars-grin.gov/fungaldatabases/fungushost/new_rptOneLit.cfm?fungRec=", id, "&thisError=")
  
  pars <- rawToChar(GET(url)$content)
  pars <- htmlTreeParse(pars, useInternalNodes = TRUE)
  pars <- xpathApply(pars, "//p", xmlValue)
  pars <- unlist(pars)
  res <- pars[[1]]
  return(res)
}