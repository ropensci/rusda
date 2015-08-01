#' Downloads synonym data from SMML Nomenclature DB
#' 
#' Searches and downloads synonym data from SMML Nomenclature database
#' 
#' @param spec a vector of class \code{character} containing fungal or plant species names
#' @param spec_type a character string specifying the type of \code{spec}. 
#' Can be either \code{"plant"} or \code{"fungus"}
#' @param clean logical, if \code{TRUE} a cleaning step is run of the resulting associations list
#' @param process logical, if \code{TRUE} downloading and extraction process is displayed
#' 
#' @return an object of class \code{list} containing synonyms for \code{spec}
#' 
#' @author Franz-Sebastian Krah
#' 
#' @examples \dontrun{
#' spec <- c("Solanum tuberosum")
#' synonyms(spec, spec_type="plant", process=TRUE, clean=TRUE)
#' spec <- c("Phytophthora infestans", "Polyporus badius")
#' synonyms(spec, spec_type="fungus", process=TRUE, clean=TRUE)
#' }

synonyms <- function(spec, spec_type = c("plant", "fungus"), clean = TRUE, process = TRUE)
{
  expect_match(spec_type, ("fungus|plant"))
  if(length(grep("\\sx\\s", spec)) > 0) { stop(" no hybrids allowed as input") }
  if(length(spec_type) == 2) stop(" 'spec_type' not specified. Please choose one of 'plant', 'fungus'")
  ifelse(length(grep(" ", spec)) > 0,tax <- strsplit(spec, " "), tax <- strsplit(spec, "_"))
  i <- NULL
  
  ## I. PARSE DATA    ##
  ######################
  if(process == TRUE) { message("... retrieving data ... for:") }
  p <- foreach(i = seq_along(tax)) %do% getHF(tax[[i]], process = process, spec_type = spec_type)
  taxa <- lapply(tax, function(x) { paste(as.character(x[1]), as.character(x[2])) })
  
  ## III. SYNONYMS ##
  ###################
  if(process == TRUE) { message("... extracting Synonyms ...") }
  syns <- lapply(p, getSYNS, process = process, taxa = taxa)
  synos <- list(); for(i in seq_along(taxa))
  {
    st <- grep(paste("Nomenclature data for", as.character(taxa[[i]]), sep = " "), p[[i]])
    cond <- paste(c("Distribution", "Substrate", "Updated", "The Fungus-Host Distributions", "Notes"), collapse="|")
    sp <- grep(cond, p[[i]])
    if(length(st) == 0){ synos[[i]] <- "nodata" }
    else{ synos[[i]] <- p[[i]][(st + 1):(sp[1] - 1)] }
  }
  synos <- foreach(i = seq_along(taxa)) %do% c(syns[[i]], synos[[i]])
  
  if(clean == TRUE)
  {
    synos <- lapply(synos, clean_step, taxa = taxa, spec_type = spec_type, syns = syns, synonyms_incl = FALSE)
  }
  synos <- lapply(synos, unique)
  names(synos) <- taxa 
  return(synos)
}
