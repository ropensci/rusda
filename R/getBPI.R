#' Downloads specimens records for input study BPI accession number
#' 
#' @param BPI a single study BPI or a vector of study BPIs
#' @param sep seperator how to collapse output literature references
#' @return an object of class \code{data.frame} with studies
#' 
#' @import stringr
#' @author Franz-Sebastian Krah
#' @export
#' 
getBPI <- function(BPI, sep = "; ") {
  
  getBPI_single <- function(BPI = "BPI 633739"){
    BPI <- str_replace(BPI, " ", "%20")
    
    url <- paste0("https://nt.ars-grin.gov/fungaldatabases/specimens/new_rptSpecimenOneRec.cfm?thisrec=",
                  BPI)
    
    pars <- rawToChar(GET(url)$content)
    pars <- htmlTreeParse(pars, useInternalNodes = TRUE)
    pars <- xpathApply(pars, "//p", xmlValue)
    pars <- unlist(pars)
    pars <- pars[str_detect(pars, "")]
    return(pars)
  }
  
  extractBPIs <- function(x, sep){
    x <- as.character(x)
    paste(unlist(lapply(x, getBPI_single)), collapse = sep)
  }
  
  refs <- lapply(BPIs, extractBPIs, sep = sep)
  BPI <- data.frame(BPI, specimen = unlist(refs))
  
  return(BPI)
}

