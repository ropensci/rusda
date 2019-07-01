#' Downloads studies for input study IDs
#' 
#' Downloads studies for study IDs which are output of associations
#' @param id a single study id or a vector of study ids
#' 
#' @return an object of class \code{list} with studies
#' 
#' @ import stringr
#' @author Franz-Sebastian Krah
#' @export

getStudy <- function(x, sep = "; ") {
  x <- as.character(x)
  x <- str_extract_all(x, "\\d*")[[1]]
  x <- x[str_detect(x, "\\d+")]
  res <- paste(unlist(lapply(x, getStudy_single)), collapse = sep)
  return(res)
}


