##                       associations                           ##
##      This code is part of the rusda package                  ##
##     FS-Krah (last update: 2020-01-04 by AS)                  ##

#' Downloads associations for input species from SMML Fungus-Host DB
#' 
#' Searches and downloads host associations from SMML Fungus-Hosts Distributions and Specimens database
#' for fungus input vector
#' @param x a vector of class \code{character} containing fungal species names or a genus name (see Details)
#' @param database a character string specifying the databases that should be queried. Valid are
#' \code{"FH"} (Fungus-Host Distributions), \code{"SP"} (Specimens) or \code{"both"} databases
#' @param syn_include logical, if \code{TRUE} associations for synonyms are searched and added. For a
#' complete synonyms list check \code{rusda::synonyms}
#' @param process logical, if \code{TRUE} downloading and extraction process is displayed
#' @param db if x is above species level, all species for the higher taxon are retrived using the function taxize::downstream. 
#' Here one of ITIS (itis), Catalogue of Life (col), GBIF (gbif), or NCBI (ncbi) has to be selected. NCBI is default.
#' 
#' @details The Fungus-Hosts distributions database 'FH' comprises data compiled from Literature. In
#' the uncleaned output all kinds of unspecified substrates are documented like "submerged wood".
#' Cleaned data displayes Linnean names only and species names with either "subsp.","f. sp." "f.",
#' "var.", "cv.". 
#' The Specimens database comprises entries from field collections. The output contains the Specimen ID as "study id". 
#' Some field collection entries cointain additional information (e.g. on collector etc.); this is stored in the column "notes".
#' 
#' 
#' If genera names are supplied, then species are derived from the NCBI taxonomy.
#' 
#' 
#' @return an object of class \code{list}. 
#' @return First is associations, a dataframe, second is no_data, a character vector with names from \code{x} for which no records where found.
#' 
#' @author Franz-Sebastian Krah, Anna Schertler
#' 
#' @examples
#' \dontrun{
#' ## Example for species name(s) as input
#' x <- "Rosellinia ligniaria"
#' hosts <- associations(x, database = "both", syn_include = TRUE,  process = TRUE)
#' is.element("Fagus sylvatica", hosts$association[["host_substrate"]])
#'
#' ## Example for genus/genera name(s) as input
#' x <- c("Cantharellus", "Phellodon")
#' hosts <- associations(x, database = "both", syn_include = TRUE, process = TRUE)
#' }
#' @import RCurl
#' @import httr
#' @import XML
#' @import stringr
#' @import testthat
#' @import taxize
#' @import dplyr
#' @import foreach
#' @export
#' 


associations <- function(x, 
                         database = c("FH", "SP", "both"),
                         process = TRUE, 
                         syn_include = TRUE, 
                         db = "ncbi")
{
  require(RCurl); require(httr); require(XML);require(stringr); require(testthat);require(taxize);require(dplyr);require(foreach)
  
  # test internet conectivity
  if(!url.exists("r-project.org") == TRUE) {stop( "Not connected to the internet. Please create a stable connection and try again." )}
  
  # test if arguments are given
  expect_match(database, ("FH|SP|both"))
  
  # clean the names
  x <- gsub("_", " ", x)
  x <- gsub("f\\.sp\\.", "f\\. sp\\.", x)
  x <- trimws(x)
  x <- x[!duplicated(x)]
  
  ## If a genus is supplied
  words <- vapply(strsplit(x, "\\W+"), length, integer(1))
  if (any(words == 1) & any(words == 2)) {
    stop(paste(" check if you specified ONLY genus names or ONLY species names \n",
    "AFAICS you provided:  \n", sum(words==1), "  genus name(s)  \n", sum(words==2), "  species name(s) ", sep=""))}
  if(all(words == 1)){
    x <- ncbiSpecies(x, clean = TRUE, sub = FALSE, db = db)
    x <- unlist(x)
  }
  
  # tests
  if(length(grep("\\sx\\s|\\s×\\s", x)) > 0) 
    stop(" no hybrids allowed as input ")
 
  # cleaned list of input taxa
  tax <- strsplit(x, "\\s")

  ## I. PARSE DATA    #####
  #########################.
  if(process == TRUE) { message("... retrieving data ... for:") }
  p <- foreach(i = seq_along(tax)) %do% {getHF(tax[[i]], process)}
  
  ## II. DATA CONDITIONS #####
  ############################.
  # check if data is available in the FH and in the Specimen Database
  co <- lapply(p, getCOND)
  
  # check for any taxa with 1) no data at all (then the co output is a string "no data" instead of a sublist) or 2) no records in HF and Specimens database
  tax_nodata <- ""
  if(any(unlist(lapply(co, is.list)) == FALSE) | 
     any(unlist(lapply(co, function(x) {if(is.list(x)){(x$sp == FALSE & x$hfu == FALSE)}}))))    {
    
      # find positions
          del_no_entry <- which(lapply(co, function(x) {
              if(is.list(x))
                {(x$sp == FALSE & x$hfu == FALSE)}
              else{x == "nodat"}}) == TRUE)
      
      # remove those
          p   <- p[-del_no_entry] 
          tax_nodata <- unlist(lapply(tax[del_no_entry], function(x) { paste(x, collapse = " ") })) 
          tax <- tax[-del_no_entry]
          co  <- co[-del_no_entry]
          
          
          if(length(p) == 0) {stop("all species were removed because no data available")}
          
          warning("Species with index ",   
                  gsub(",$","",paste0(del_no_entry, ",", collapse = "")), " removed because no data available.")

          rm(del_no_entry)
  }
  
  taxa <- lapply(tax, function(x) { paste(x, collapse = " ") }) 

  ## III. EXTRACTING DATA  #####
  ##############################.
  
  # __ extract the current scientific name used by usda (uppermost bold name) ####
  # note: his only works for names with a nomenclature db entry
  current_names <- unlist(lapply(p, function(x) { if(length(gsub("\\([^\\(]*$", "", x[grep("^Nomenclature data for", x)+1])) == 0)
  {"no nomenclature record" } 
    else {
      trimws(gsub("\\([^\\(]*$", "", x[grep("^Nomenclature data for", x)+1]))}}))

  # __ FH DB ####
  if(process == TRUE & database == "FH" | database == "both") { 
    message("... extracting Fungus-Hosts DB ...") }

  # extract the raw HF-DB strings
  hosts_hf <- foreach(i = seq_along(tax)) %do%  {
    if(!is.list(co[[i]])){ hf <- "nodata" 
    } else{
      if(length(co[[i]]$hf.st) > 0)
    {
      # check if there are entries in the literature database. Identify the beginning and the end of the Literature database chunk bzw. the note about no records.
      hf.sp <- ifelse(length(grep("The Literature database has", p[[i]])) > 0, 
                      grep("The Literature database has", p[[i]]), 
                      grep("No records were found in the Literature database", p[[i]]))
      if(length(hf.sp) == 0){
        hf.sp <- (grep(paste("There are no records for ",taxa[[i]], 
          " in the Literature database", sep=""), p[[i]]))
        }
      # extract
      p[[i]][(co[[i]]$hf.st + 1):(hf.sp - 1)] 
    } else{hf <- "nodata"}
    }
    }

  names(hosts_hf) <- unlist(taxa)
  # separate, so each database row (host + localities) forms one string
  hosts_hf <- hosts_hf[which(unlist(lapply(hosts_hf, function(x) {length(x) > 1})))]

  # convert into dataframe (input name, synonym used by the database, host, country, study id and notes)
  hosts_hf_clean <- sapply(names(hosts_hf) , simplify = F, USE.NAMES = T,  function(tax_name){
    
    x_hf <- hosts_hf[[tax_name]] # access list entry
    x_hf <- x_hf[which(x_hf != "")] # remove empty 
    x_hf <- gsub("\r\n\t\t\t\t|:", "",  x_hf) # remove nonsense
    synonyms_position <- which(!grepl("[0-9]+|card,",x_hf))  # get synonym positions
    record_position <- grep("[0-9]+|card,", x_hf) # get record positions 
    
    # assign synonyms to the records
    for (i in (1:length(x_hf))[-synonyms_position]) {
      used_synonym <- x_hf[max(synonyms_position[which(synonyms_position<i)])]
      x_hf[i] <- paste( used_synonym, "-----", x_hf[i])
    }
    
    x_hf <-  x_hf[record_position] # filter for records
    
    # split by all comas which are located before a capitalized character or by the chosen delimiter for the synonym name
    pieces <- strsplit(x_hf, "-----|,(?=([A-Z]))|\\*;\\s(?=([A-Z]))", perl = TRUE) 
    # the first string will always contain the the synonym of the fungus that was used 
    # the second string will contain the host name and the first locality - reference combination
    # the other strings will contain the other locality - reference combinations
    
    # get host name:
    # therefore separate second string in (1) species and (2) country plus reference using the following regex:
    # separate string at the last Capital letter
    spec <- lapply(pieces, function(x) {
        spec <- unlist(str_split(x, pattern = "(?<=.)(?=\\s[A-Z])"))[2]
        return(spec)
      })
    # assign the host species names
    names(pieces) <- spec
      
    # get countries and study_ids
    countries <- lapply(1:length(names(pieces)), function(i){
    df <- do.call(rbind.data.frame, str_split(pieces[[i]][2:length(pieces[[i]])], "\\s[-]+\\s"))  
        
    names(df) <- c("country", "study_id")
    df$input <- tax_name 
    df$synonym_used <- gsub(",|;|:|^\\s*|\\s*$", "", pieces[[i]][1])
    df$country <- gsub(names(pieces)[i], "", df$country, fixed = TRUE)
    df$country <- gsub("^\\s*|\\s*$", "", df$country)
    df$host_substrate <- trimws(gsub("\\s*\\(.*?\\)\\s*|\\-|card|,", "",names(pieces)[i]))
    
 
    # account for "card"-reference cases
      for (j in 1:nrow(df)){
      if(grepl("card", pieces)[i]){
        df$country[j] <- gsub(df$host_substrate[j], "", df$country[j])
        }
      }
      
      df$notes <- gsub("\\(|\\)", "" ,str_extract(names(pieces)[i], "\\(.*?\\)"))
      df$study_id <-gsub("^\\s*|\\s*$|^,|,$", "", df$study_id) 
        
    # account for cv. cases
      for (j in 1:nrow(df)){
      if(grepl(" cv\\.$", df$host_substrate[j])){
        df$host_substrate[j] <- gsub("^\\s*|\\s*$|^,|,$", "", paste(df$host_substrate[j], tolower(gsub("\\s.*?$", "",  df$country[j]))))
        df$country[j] <- gsub("^.*?\\s", "",  df$country[j])
        }
      }
        

    df <- df[,c("input" ,"synonym_used", "host_substrate", "country", "study_id","notes")]
    return(df)
    })
      
    # put in dataframe format and clean the columns
    df_fh <- do.call(rbind.data.frame, countries) 
    df_fh$scientific_name <- ifelse(length(which(unlist(taxa)  %in% tax_name)) >1,
                                    NA, current_names[which(unlist(taxa)  %in% tax_name)])
    df_fh$db <- "FH"
    df_fh$type <- ""
  df_fh <- df_fh[,c("input", "scientific_name", "synonym_used","host_substrate", "country", "db", "study_id", "type", "notes")]
  return(df_fh)
  })

  # __B) Specimens DB  #####
  if(process == TRUE & database == "SP" | database == "both") { message("... extracting Specimens DB ...")} 
 
  # extract the raw SP-DB strings and separate, so each database row (host + localities) forms one string
  hosts_sp <- foreach(i=seq_along(taxa)) %do% {
    if(length(co[[i]]$sp) == 0 | length(co[[i]]$spe.st) == 0){ specim <- "nodata" }
    if(length(co[[i]]$spe.st) > 0)
    {
      spe.sp <- max(which(nchar(p[[i]])==0))
      #spe.sp <- spe.sp[length(spe.sp)]  
      specim <- p[[i]][(co[[i]]$spe.st + 1):(spe.sp - 1)]
    }
  }   
  names(hosts_sp) <- unlist(taxa)

  # convert into dataframe (input name, current name, synonym used by the database, host, country, study id and notes)
  hosts_sp_clean <- sapply(names(hosts_sp),  simplify = F, USE.NAMES = T,  function(specim){
    
    x_sp <- hosts_sp[[specim]]
    
    # use the BPI ID to access the associated pages
    # extract country and locality data and additional notes
    x_sp <- gsub("^.*?\\s-\\s", "", x_sp[grepl("[0-9]+|card,",x_sp)]) # remove host name  and synonym
    bpi_id <- unlist(strsplit(x_sp, ", ")) # get all BPI IDs
    
    df_sp <- lapply(bpi_id, function(x) {
      # parse data
      x1 <- gsub("\\s", "%20", x)
      url <- (paste0("https://nt.ars-grin.gov/fungaldatabases/specimens/new_rptSpecimenOneRec.cfm?thisrec=", x1))
      pars <- rawToChar(GET(url)$content)
      pars <- htmlTreeParse(pars, useInternalNodes = TRUE)
      pars <- xpathApply(pars, "//p", xmlValue)
      pars <- unlist(pars)
      cont <- pars[str_detect(pars, "")] # remove empty strings
      
      # check for type specimen information 
      if(grepl("TYPE", cont[1])) {
        type <- trimws(gsub("^.*?[0-9]\\s", "", cont[1]))
      } else{type <- ""}
     
      # extract used name
      fungus_name_used <- gsub(x , "", cont[1]) # remove bpi id
      fungus_name_used <- gsub("\\s[A-Z].*?$", "", fungus_name_used)
    
      # get host/substrate
      if(length(unlist(str_split(cont[2], "\\s-\\s"))) > 1){
            species <- unlist(str_split(cont[2], "\\s-\\s"))[1]
            notes_more <- unlist(str_split(cont[2], "\\s-\\s"))[2:length(unlist(str_split(cont[2], "\\s-\\s")))]
          } else{
            species <- cont[2]
          }
      
      # get country
      country <-  unlist(strsplit(cont[3], "\\s-\\s|\\."))[1] # get country
      
      # get locality
      if(!is.na(unlist(strsplit( cont[3], "\\s-\\s"))[2])) {
      notes_locality <- paste("locality:", unlist(strsplit( cont[3], "\\s-\\s"))[2])}
      
      # notes: get other info which can be stored in notes
      notes_misc <- cont[4:length(cont)]
      notes <- paste(unlist(lapply(ls(pattern = "notes_"), function(x) {if(exists(x)) {return(get(x))}} )), collapse = "; ")
      
      # create output dataframe
      outdf <- data.frame(
        input = specim,
        scientific_name = ifelse(length(which(unlist(taxa)  %in% specim)) >1, 
                                   NA, current_names[which(unlist(taxa)  %in% specim)]),
        synonym_used = fungus_name_used,
        host_substrate = species, 
        country = country, 
        db = "Specimen",
        study_id = x,
        type = type,
        notes= notes, 
        stringsAsFactors = F)
      return(outdf)
      
    })
    df_sp <- do.call(rbind.data.frame, df_sp) 
    df_sp <- df_sp[which(!duplicated(df_sp)),]
    df_sp <- df_sp[,c("input", "scientific_name", "synonym_used","host_substrate", "country", "db", "study_id", "type", "notes")]
   
    return(df_sp)
  })
  


  ## IV. RESULTS OBJECT ####
  #######################.
  if (database == "FH") { res <-  hosts_hf_clean }
  
  if (database == "SP") { res <-  hosts_sp_clean }
  
  if (database == "both") {  res <-  foreach(i = seq_along(hosts_hf)) %do% rbind(hosts_hf_clean[[i]], hosts_sp_clean[[i]])
      names(res) <- unlist(taxa)
      res <- lapply(res, function(x)
        { if(length(grep("nodata",x)) == 2) { x <- "nodata" }
          if(!length(grep("nodata",x)) == 2) { x }})
  }
  
  res <- do.call(rbind.data.frame, res)
  
 
  ## V. CLEAN DATA   #####
  #########################.
  rownames(res) <- 1:nrow(res)
  res <- data.frame(apply(res, MARGIN = 2, function (x) 
    {x <- trimws(gsub("\\(\\(.*?\\)\\)|\\.([0-9])+$|\\s+$|^\\s+|:|;|,$|card|0000 000 00","",x))
    
    # some words miss the whitespace in between (typos in the database) - insert
     x <- gsub("(?<=[a-z])(?=[A-Z])" ," ", x , perl = T)
     
     # hybrid hosts 
     x <- gsub("ÃƒÂ—", "×", x)
     
     # check again for superfluous commas
     x <- trimws(gsub( "^,+|,$|,\\s+$", "",  x))
     return(as.character(x))
     }), stringsAsFactors = FALSE) 
  
  
  # current scientifc names without authors and authorship information in a separate column
  res$scientific_name_authorship <- res$scientific_name
  res$scientific_name <- trimws(gsub("\\s[A-Z].*?$|\\(.*?$|\\sde\\s.*?$", "", res$scientific_name))
  res$scientific_name_authorship <- trimws(mapply(gsub, res$scientific_name, "", res$scientific_name_authorship))
  
  #### exclude synonyms if syn_include == FALSE
  if(syn_include == FALSE){
    if(process == TRUE) { message("... excluding synonyms ...") }
    res <- res[which(res$scientific_name == res$synonym_used),]
  }
  
  #### collapse records that were downloaded via different synonyms (put all input names in one cell, separated by |)
  suppressMessages(res <- res %>%
    dplyr::group_by(scientific_name, scientific_name_authorship, synonym_used, host_substrate, country, db, study_id, type, notes) %>% 
     dplyr::summarise(input_names = paste(input, collapse = " | ")))
  res <- res[,c("input_names", "scientific_name", "scientific_name_authorship", "synonym_used","host_substrate", "country", "db", "study_id", "type", "notes")]
  
 # VI. RESULTS OBJECT OUT  #####
  ################################.
  return(list(association = as.data.frame(res),
                 no_data = tax_nodata))
}

