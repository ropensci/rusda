#' Downloads studies for input study IDs
#' 
#' Downloads studies for study IDs which are output of associations
#' @param id a single study id or a vector of study ids
#' @param sep seperator how to collapse output literature references
#' @return an object of class \code{data.frame} with studies
#' 
#' @import stringr
#' @author Franz-Sebastian Krah
#' @export

getStudy <- function(id, sep = "; ") {
  
  if(any(str_detect(id, "BPI")))
    warning("References will only added for FH database records")
  
  getStudy_single <- function(id = 42264){
    url <- paste0("https://nt.ars-grin.gov/fungaldatabases/fungushost/new_rptOneLit.cfm?fungRec=", id, "&thisError=")
    
    pars <- rawToChar(GET(url)$content)
    pars <- htmlTreeParse(pars, useInternalNodes = TRUE)
    pars <- xpathApply(pars, "//p", xmlValue)
    pars <- unlist(pars)
    res <- pars[[1]]
    return(res)
  }
  
  extractRefs <- function(x, sep){
    x <- as.character(x)
    x <- str_extract_all(x, "\\d*")[[1]]
    x <- x[str_detect(x, "\\d+")]
    paste(unlist(lapply(x, getStudy_single)), collapse = sep)
  }
  
  ids <- unique(id)
  refs <- lapply(ids, extractRefs, sep = sep)
  names(refs) <- ids
  refs <- data.frame(ids, reference = unlist(refs))
  id <- data.frame(id, reference = refs[match(id, refs$ids),"reference"])
  
  return(id)
}