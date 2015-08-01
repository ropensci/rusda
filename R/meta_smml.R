#' Downloads and evaluate species presence in SMML DBs
#' 
#' Searches, downloads and evaluates presence/absence of data in the SMML databases
#' 
#' @param spec a vector of class \code{character} containing fungal or plant species names
#' @param spec_type a character string specifying the type of \code{spec}. 
#' Can be either \code{"plant"} or \code{"fungus"}
#' @param process logical, if \code{TRUE} downloading and extraction process is displayed
#' 
#' @details Use this function before deriving data from one of the databases in order to prune your
#' input species vector. With pruned species vectors the functions will run faster. This is important
#' if \code{spec} is some hundred species long. 
#' 
#' @return an object of class \code{data.frame}: presence/absence
#' 
#' @author Franz-Sebastian Krah
#' 
#' @examples \dontrun{
#' fungus.meta <- meta_smml(spec="Picea abies", process=TRUE, spec_type="plant")
#' fungus.meta
#' hosts.meta <- meta_smml(spec="Antrodiella citrinella", process=TRUE, spec_type="fungus")
#' hosts.meta
#' }

meta_smml <- function(spec, spec_type = c("plant", "fungus"), process = TRUE)
{
  expect_match(spec_type, ("fungus|plant"))
  if(length(grep("\\sx\\s", spec)) > 0) stop(" no hybrids allowed as input ")
  if(length(spec_type) == 2) stop(" 'spec_type' not specified. Please choose one of 'plant', 'fungus'")
  ifelse(length(grep(" ", spec)) > 0,tax <- strsplit(spec, " "), tax <- strsplit(spec, "_"))
  
  ## I. PARSE DATA       ##
  #########################
  if(process == TRUE) { message("... retrieving data ... for:") }
  p <- foreach(i = seq_along(tax)) %do% getHF(tax[[i]], process, spec_type = spec_type)
  i <- NULL
  
  # II. META Anylysis   ##
  ########################
  l <- lapply(p, getMETA)
  
  ## III. Create Object ##
  ########################
  names(l) <- spec
  obj = do.call(rbind,l)
  return(obj)
}
