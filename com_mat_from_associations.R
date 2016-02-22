#' Community matrixes from associations output
#' 
#' Takes the output of the function \code{associations} and computes a set of community matrixes
#' 
#' @param associations is the second list element of the output from function \code{associations}
#' 
#' 
#' 
#' @details In this version the function produces four community matrixes. The resulting matrix will have input species as rows and the found associations as cols and presence/absence data. Basically there will always be two output for every "treatment". One with unchanged data that was processed to a community matrix and one with reduced data for a given threshold. The threshold specifies the minimum row and colSums of the community matrix. The default value is >= 1. Currently the rows stay species names, but the cols can either be the acctual output (mostly species names) or on the genus level.
#' 
#' 
#' @return an object of class \code{list} with currently 4 elments
#' @return First two are unreduced for species and genus. Second are reduced according to the thresholds. 
#' 
#' @author Franz-Sebastian Krah
#' 
#' @examples
#' hosts <- associations("Triticum", database="both", clean=TRUE, syn_include=TRUE,
#' spec_type="plant", process=TRUE)
#' 
#' comms <- com_mat_from_associations(hosts[[2]])

com_mat_from_associations <- function(associations, th.row = 1, th.col = 1){
  # prepare table for community matrix
  fu <- unlist(lapply(names(hosts$associations), function(x)  
  { rep(x, lapply(hosts$associations, length)[x])}), use.names = FALSE)
  ho <- unlist(hosts$associations, use.names = FALSE)
  ho <- data.frame(species = fu, hosts = ho)
  
  # related associations: split into genus and species names
  x <- strsplit(as.character(ho$hosts), " ")
  l <- do.call(rbind, lapply(x, length))
  x <- x[!l>4]
  ho <- ho[!l>4,]
  
  # remore "nodata" rows
  x <- x[-grep("nodata", ho$hosts)]
  ho <- ho[-grep("nodata", ho$hosts),]
  
  # combine data
  x <- rbind.fill(lapply(x,function(y) { as.data.frame(t(y), stringsAsFactors=FALSE) }))
  ifelse(dim(x)[2]==2, names(x) <- c("gen", "sp"), names(x) <- c("gen", "sp", "var", "ssp"))
  ho <- cbind(ho, x)
  
  comm_gen <- table(as.character(ho$species), as.character(ho$gen))
  comm_spec <- table(as.character(ho$species), as.character(ho$hosts))
  reduce_com_mat <- function(x, th.col, th.row){
    red <- x[rowSums(x) >= th.row,]
    red <- x[ , colSums(x) >= th.col]
    return(red)
  }
  comm_spec_red <- reduce_com_mat(comm_spec, th.col = th.col, th.row = th.row)
  comm_gen_red <- reduce_com_mat(comm_gen, th.col = th.col, th.row = th.row)
  res <- list(comm_spec, comm_gen, comm_spec_red, comm_gen_red)
  return(res)
}
