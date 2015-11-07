#' Downloads substrate data from SMML Nomenclature DB
#' 
#' Searches and downloads substrate data from SMML Nomenclature database
#' 
#' @param spec a vector of class \code{character} containing fungal or plant species names
#' @param process logical, if \code{TRUE} downloading and extraction process is displayed
#' 
#' @details Don't be disappointed. Not much data there. 
#' But depends on the study group, so give it try.
#' 
#' @return an object of mode \code{list} containing substrate for fungus species
#' 
#' @author Franz-Sebastian Krah
#' 
#' @examples
#' polyporus <- c("Polyporus_rhizophilus", "Polyporus_squamosus",
#'  "Polyporus_squamulosus", "Polyporus_varius")
#' subs.poly <- substrate(polyporus, process=TRUE)
#' subs.poly

substrate <- function(spec, process = TRUE)
{
  if(length(grep("\\sx\\s", spec)) > 0) stop(" no hybrids allowed as input ")
  ifelse(length(grep(" ", spec)) > 0,tax <- strsplit(spec, " "), tax <- strsplit(spec, "_"))
  
  ## I. PARSE DATA       ##
  #########################
  if(process == TRUE) { message("... retrieving data ... for:") }
  p <- foreach(i = seq_along(tax)) %do% getHF(tax[[i]], process = process, spec_type = "fungus")
  i <- NULL
  
  ## II. SUBSTRATE       ##
  ######################### 
  subst <- function(x)
  {
    subs.st <- grep("Substrate:", x)
    subs <- x[subs.st]
    subs <- gsub("Substrate: ", "", subs)
    ifelse(length(subs.st) > 0, subs, "nodata")
  }
  obj <- lapply(p, subst)
  
  ## III. Create Object ##
  ########################
  names(obj) <- spec
  return(obj)
}
