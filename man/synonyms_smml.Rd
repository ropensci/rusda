% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/synonyms_smml.R
\name{synonyms_smml}
\alias{synonyms_smml}
\title{Downloads synonym data from SMML Nomenclature DB}
\usage{
synonyms_smml(x, spec_type = c("plant", "fungus"), clean = TRUE,
  process = TRUE)
}
\arguments{
\item{x}{a vector of class \code{character} containing fungal or plant species or genus names}

\item{spec_type}{a character string specifying the type of \code{x}. 
Can be either \code{"plant"} or \code{"fungus"}}

\item{clean}{logical, if \code{TRUE} a cleaning step is run of the resulting associations list}

\item{process}{logical, if \code{TRUE} downloading and extraction process is displayed}
}
\value{
an object of class \code{list} containing synonyms for \code{x}
}
\description{
Searches and downloads synonym data from SMML Nomenclature database
}
\examples{
\dontrun{
x <- "Solanum tuberosum"
synonyms_usda(x, spec_type = "plant", process = TRUE, clean = TRUE)
x <- c("Phytophthora infestans", "Polyporus badius")
synonyms_usda(x, spec_type = "fungus", process = TRUE, clean = TRUE)
}
}
\author{
Franz-Sebastian Krah
}
