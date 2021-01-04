##                        getHF                       ##
##      This code is part of the rusda package        ##
##      F.-S. Krah 2015 (last update: 2020-12-06 by AS)     ##  

getHF <- function(x, process){
  if(process == TRUE){ message(paste(x, collapse= " "))}
  url <- paste("https://nt.ars-grin.gov/fungaldatabases/new_allViewGenBank.cfm?thisName=",
                     paste(x, collapse= "%20"),
                     "&organismtype=Fungus&CFID=28606i&",
                     "CFTOKEN=5f05e06d6d0caa92-993D3B37-A643-36F0-49FFCF751D1BE3B1", sep="")
  pars <- rawToChar(GET(url)$content)
  pars <- htmlTreeParse(pars, useInternalNodes = TRUE)
  pars <- xpathApply(pars, "//p", xmlValue)
  pars <- unlist(pars)
}
