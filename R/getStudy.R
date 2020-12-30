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

getStudy <- function(id) {
  
  #if(any(str_detect(id, "BPI")))
  #  warning("References will only added for FH database records")
  
  getStudy_single <- function(id){
    url <- paste0("https://nt.ars-grin.gov/fungaldatabases/fungushost/new_rptOneLit.cfm?fungRec=", id, "&thisError=")
    
    pars <- rawToChar(GET(url)$content)
    pars <- htmlTreeParse(pars, useInternalNodes = TRUE)
    pars <- xpathApply(pars, "//p", xmlValue)
    pars <- unlist(pars)
    res <- pars[[1]]
    return(res)
  }
  
  extractRefs <- function(x){
    x <- as.character(x)
    x <- str_extract_all(x, "\\d*")[[1]]
    x <- x[str_detect(x, "\\d+")]
    paste(unlist(lapply(x, getStudy_single)), collapse = ", ")
  }
  
  ids <- unique(id)
  refs <- lapply(ids, extractRefs)
  names(refs) <- ids
  refs <- data.frame(ids, reference = unlist(refs))
  id <- data.frame(id, reference = refs[match(id, refs$ids),"reference"], stringsAsFactors = F)
  id$reference <- as.character(id$reference)
  id$reference[which(grepl("BPI" ,id$id))] <- ""
  return(id)
}

#getStudy <- function(id) {
  
#  if(any(str_detect(id, "BPI")))
#    warning("References will only added for FH database records")
  
 
  
#  ids <- unique(id)
#  ids <- ids[which(!str_detect(ids, "BPI"))]   # filter out the BPI records
#  refs <- lapply(ids, function(x){
#    x <- as.character(x)
#    x <- str_extract_all(x, "\\d*")[[1]]
#    x <- x[str_detect(x, "\\d+")]
#    paste(unlist(lapply(x, function(study){
#      url <- paste0("https://nt.ars-grin.gov/fungaldatabases/fungushost/new_rptOneLit.cfm?fungRec=", study, "&thisError=")
#      
#      pars <- rawToChar(GET(url)$content)
#      pars <- htmlTreeParse(pars, useInternalNodes = TRUE)
#      pars <- xpathApply(pars, "//p", xmlValue)
#      pars <- unlist(pars)
#      res <- pars[[1]]
#      return(res)
#    })), collapse = "; ")
#  })
#  names(refs) <- ids
#  refs <- data.frame(ids, reference = unlist(refs))
#  id <- dplyr::left_join(data.frame(id = id), refs, by =c("id" = "ids"))
#  return(id)
#}


 