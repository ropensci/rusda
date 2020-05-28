#' Downloads associations for input species from SMML Fungus-Host DB
#' 
#' Searches and downloads associations from SMML Fungus-Hosts Distributions and Specimens database
#' for fungus or plant species input vector
#' @param x a vector of class \code{character} containing fungal or plant species names or a genus name (see Details)
#' @param database a character string specifying the databases that should be queried. Valid are
#' \code{"FH"} (Fungus-Host Distributions), \code{"SP"} (Specimens) or \code{"both"} databases
#' @param spec_type a character string specifying the type of \code{x}. 
#' Can be either \code{"plant"} or \code{"fungus"}
#' @param clean logical, if \code{TRUE} a cleaning step is run of the resulting associations list
#' @param syn_include logical, if \code{TRUE} associations for synonyms are searched and added. For a
#' complete synonyms list check \code{rusda::synonyms}
#' @param process logical, if \code{TRUE} downloading and extraction process is displayed
#' @param db if x is higher than species level, all species for the higher taxon are retrived using the function taxize::downstream. Here one of ITIS (itis), Catalogue of Life (col), GBIF (gbif), or NCBI (ncbi) has to be selected. NCBI is default.
#' 
#' @details The Fungus-Hosts distributions database 'FH' comprises data compiled from Literature. In
#' the uncleaned output all kinds of unspecified substrates are documented like "submerged wood".
#' Cleanded data displayes Linnean names only and species names with either "subsp.","f. sp." "f.",
#' "var.". The Specimens database comprises entries from field collections.
#' 
#' If genera names are supplied, then species are derived from the NCBI taxonomy.
#' 
#' 
#' @return an object of class \code{list}. 
#' @return First is synonyms, second is associations. Synonmys is a
#' vector of mode \code{list} with synonyms for \code{x}. Notice: This is not a
#' complete list of synonym data in the database. This is the list of synonyms that contain data for
#' the input \code{x}. For a complete synonyms list check \code{rusda::synonyms} or (if needed) for fungi R package rmycobank.
#' @return Associations is a vector of mode \code{list} of associations for \code{x}
#' 
#' 
#' @author Franz-Sebastian Krah
#' 
#' @examples
#' \dontrun{
#' ## Example for species name(s) as input
#' x <- "Fagus sylvatica"
#' pathogens <- associations(x, database = "both", clean = TRUE, syn_include = TRUE,
#' spec_type = "plant", process = TRUE)
#' x <- "Rosellinia ligniaria"
#' hosts <- associations(x, database = "both", clean = TRUE, syn_include = TRUE, 
#' spec_type = "fungus", process = TRUE)
#' is.element("Rosellinia ligniaria", pathogens$association[[1]])
#' is.element("Fagus sylvatica", hosts$association[[1]])
#' 
#' ## Example for genus/genera name(s) as input
#' x <- "Zehneria"
#' # or
#' x <- c("Zehneria", "Momordica")
#' hosts <- associations(x, database = "both", clean = TRUE, syn_include = TRUE, 
#' spec_type = "plant", process = TRUE)
#' }
#' @import foreach
#' @import RCurl
#' @import httr
#' @import XML
#' @import stringr
#' @import testthat
#' @import taxize
#' @import plyr
#' @export
# 
# x = "Protomyces inouyei"
# database = "FH"
# spec_type <- "fungus"
# process = TRUE
# library(foreach)
# library(XML)
# library(httr)

library(devtools)
install_github("ropensci/rusda", force = T)

database = "both"
spec_type <- "fungus"
process = TRUE


associations <- function(x, database = c("FH", "SP", "both"), 
  spec_type = c("plant", "fungus"), clean = TRUE, syn_include = TRUE, 
  process = TRUE, db = "ncbi")
{
  # test internet conectivity
  if(!url.exists("r-project.org") == TRUE) stop( "Not connected to the internet. Please create a stable connection and try again." )
  # if(!is.character(getURL("http://nt.ars-grin.gov/fungaldatabases/index.cfm")))
  # if(!GET("https://nt.ars-grin.gov/fungaldatabases/index.cfm")) stop(" Database is not available : http://nt.ars-grin.gov/fungaldatabases/index.cfm")
  # test if arguments are given
  expect_match(spec_type, ("fungus|plant"))
  expect_match(database, ("FH|SP|both"))
  
  # if underscore remove it
  x <- gsub("_", " ", x)
  
  ## If a genus is supplied
  words <- vapply(strsplit(x, "\\W+"), length, integer(1))
  if (any(words == 1) & any(words == 2)) 
  {stop(paste(" check if you specified ONLY genus names or ONLY species names \n",
    "AFAICS you provided:  \n", sum(words==1), "  genus name(s)  \n", sum(words==2), "  species name(s) ", sep=""))}
  if(all(words == 1)){
    # x <- lapply(x, ncbiSpecies, clean = TRUE, sub = FALSE)
    x <- ncbiSpecies(x, clean = TRUE, sub = FALSE, db = db)
    x <- unlist(x)
  }
  
  # tests
  if(length(grep("\\sx\\s", x)) > 0) 
    stop(" no hybrids allowed as input ")
 
  # cleaned list of input taxa
  tax <- strsplit(x, " ")
  
  
  ## I. PARSE DATA    #####
  #########################.
  if(process == TRUE) { message("... retrieving data ... for:") }
  p <- foreach(i = seq_along(tax)) %do% {
    getHF(tax[[i]], process, spec_type = spec_type)
  }
  

  ## II. DATA CONDITIONS #####
  ############################.
  
  if(any(unlist(lapply(p, is.null)))){
    del_no_entry <- which(lapply(p, is.null) == TRUE)
    p <- p[-del_no_entry]
    tax = tax[-del_no_entry]
    warning("Species with index ", del_no_entry, " was removed because no data available.")
  }
  if(length(p) == 0) {stop("all species were removed because no data available")}
  
  taxa <- lapply(tax, function(x) { paste(as.character(x[1]), as.character(x[2])) }) # ? why ?
  co <- lapply(p, getCOND)
  
  if(any(co == "nodat")){
    return("No data found")
  } # ? why ?
  
  
  ## III. SYNONYMS #####
  ######################.
  #if(process == TRUE) { message("... extracting Synonyms ...") }
  #syns <- lapply(p, getSYNS, process = process, taxa = taxa) 
  #names(syns) <- taxa
  
  ## IV. EXTRACTING DATA  #####
  #############################.
  # A) FH DB ####
  if(process == TRUE & database == "FH" | database == "both") { 
    message("... extracting Fungus-Hosts DB ...") }
  i <- NULL
  # extract the HF-DB strings
  hosts_hf <- foreach(i = seq_along(taxa)) %do%  {
    if(length(co[[i]]$hfu) == 0 | length(co[[i]]$hf.st) == 0){ hf <- "nodata" }
    if(length(co[[i]]$hf.st) > 0)
    {# Stop 
      # check if there are entries in the literature database. Locate the beginning and the end of the Literature database chunk bzw. the note about no records.
      hf.sp <- ifelse(length(grep("The Literature database has", p[[i]])) > 0, 
                      grep("The Literature database has", p[[i]]), 
                      grep("No records were found in the Literature database", p[[i]]))
      if(length(hf.sp) == 0){
        hf.sp <- (grep(paste("There are no records for ",taxa[[i]], 
          " in the Literature database", sep=""), p[[i]]))}
      # extract
      p[[i]][(co[[i]]$hf.st + 1):(hf.sp - 1)] 
    }
  }
  
  names(hosts_hf) <- unlist(taxa)
 
  hosts_hf_clean <- sapply(names(hosts_hf),  simplify = F, USE.NAMES = T,  function(tax_name){
    
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
      
      # --> get host name
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
        df$fungus_synonym_used <- gsub(",|;|:|^\\s+|\\s+$", "", pieces[[i]][1])
        df$country <- gsub(names(pieces)[i], "", df$country, fixed = TRUE)
        df$host <- gsub("\\s+\\(.*?\\)\\s{0,5}", "",names(pieces)[i])
        df$notes <- gsub("\\(|\\)", "" ,str_extract(names(pieces)[i], "\\(.*?\\)"))
        df$fungus <- tax_name 

        df <- df[,c("fungus" ,"fungus_synonym_used", "host", "country", "study_id","notes")]
        return(df)
      })
      # put in dataframe format and clean the columns
      df_fh <- do.call(rbind.data.frame, countries) 
      df_fh <- df_fh[,c("fungus","fungus_synonym_used","host", "country", "study_id", "notes")]
    return(df_fh)})
  
  
  
  # B) Specimens DB  #####
  if(process == TRUE & database == "SP" | database == "both") { message("... extracting Specimens DB ...") }
  i <- NULL
  hosts_sp <- foreach(i=seq_along(taxa)) %do% {
    if(length(co[[i]]$sp) == 0 | length(co[[i]]$spe.st) == 0){ specim <- "nodata" }
    if(length(co[[i]]$spe.st) > 0)
    {
      spe.sp <- max(which(nchar(p[[i]])==0))
      #spe.sp <- spe.sp[length(spe.sp)]  # ? why ?
      specim <- p[[i]][(co[[i]]$spe.st + 1):(spe.sp - 1)]
    }
  }   
  names(hosts_sp) <- unlist(taxa)

  hosts_sp_clean <- sapply(names(hosts_sp),  simplify = F, USE.NAMES = T,  function(specim){
    
    specim <- names(hosts_sp)[1]
    
    x_sp <- hosts_sp[[specim]]
    x_sp <- x_sp[grep("[0-9]+|card,", x_sp)]
    # new: 
    # extend the code by using the BPI ID and accessing the  associated page 
    # extract country and locality data.
 
    x_sp <- gsub("^.*?\\s-\\s", "", x_sp[grepl("[0-9]+|card,",x_sp)]) # remove host name  and synonym
    pieces <- strsplit(x_sp, ", ") # get all BPI IDs
    
    # download and add the information for the single specimens
    df_sp <- lapply(unlist(pieces), function(x) {
      x1 <- gsub("\\s", "%20", x)
      url <- (paste0("https://nt.ars-grin.gov/fungaldatabases/specimens/new_rptSpecimenOneRec.cfm?thisrec=", x1))
      pars <- rawToChar(GET(url)$content)
      pars <- htmlTreeParse(pars, useInternalNodes = TRUE)
      pars <- xpathApply(pars, "//p", xmlValue)
      pars <- unlist(pars)
      cont <- pars[str_detect(pars, "")]
      
      
      fungus <- gsub(x , "", cont[1])
      fungus <- gsub("\\(.*?$", "", fungus)
      
          if(length(unlist(str_split(cont[2], "\\s-\\s"))) > 1){
            species <- unlist(str_split(cont[2], "\\s-\\s"))[1]
            more_notes <- unlist(str_split(cont[2], "\\s-\\s"))[2:length(unlist(str_split(cont[2], "\\s-\\s")))]
          } else{
            species <- cont[2]
          }
      
      country <-  unlist(strsplit(cont[3], "\\s-\\s|\\."))[1] # get country
      locality <- paste("locality:", unlist(strsplit( cont[3], "\\s-\\s"))[2]) # get locality
          if(exists("more_notes")) {
            notes <- paste(c(cont[4:length(cont)],  locality, more_notes), collapse = "; ") 
          }
      notes <- paste(c(cont[4:length(cont)],  locality), collapse = "; ") 
      
      notes <- gsub(";\\s;|;\\s;\\s;|;\\s;\\s;\\s;" ,";",notes)
      notes <- gsub("; locality: NA", "", notes)
      outdf <- data.frame(
        fungus = specim,
        fungus_synonym_used = fungus,
        host = species, 
        country = country, 
        study_id = gsub("%20", " ", x), 
        notes= notes, 
        stringsAsFactors = F)
      return(outdf)
      
    })
    df_sp <- do.call(rbind.data.frame, df_sp) 
    df_sp <- df_sp[which(!duplicated(df_sp)),]
    df_sp <- df_sp[,c("fungus", "fungus_synonym_used","host", "country", "study_id", "notes")]
   
    return(df_sp)
  })

  # old:
  # the short version (without accessing the webpage again, but only for cleaning the specimen record section) is:
  # separate string at each - before "BPI" - this should always result in 2 strings (one with the species and one with the id)
  #pieces <- strsplit(x_sp, "-\\s+(?=(BPI))", perl = TRUE) 
  # df_sp <- lapply(pieces, FUN = function(x) {
  #  species <-  x[1]
  #  study_id <- x[2]
  # return(data.frame(species = species, country = NA, study_id = study_id))
  #})
  
  
  
  
 
  ## IV. SYNONYMS EXCLUDE  #####
  ##############################.
  ## Exclude results for synonyms if wanted:
  # find occurences for taxon for stop condition and 
  # extract until next synonym of input taxa
  # if(syn_include == FALSE){
  #  if(process == TRUE) { message("... excluding synonyms ...") }
  #  no_syns <- function(x){
  #    # search start and stops
  #    st <- foreach(i = seq_along(taxa)) %do% grep(taxa[[i]], x[[i]])
  #    sp <- foreach(i = seq_along(taxa)) %do% {
  #      sy <- paste(syns[[i]][!syns[[i]] == taxa[[i]]], collapse = "|")
  #      grep(sy, x[[i]], value = FALSE)}
  #    # choose next stop if there
  #    spp <- list(); for(i in seq_along(taxa)){
  #      if(is.integer(sp[[i]]) && length(sp[[i]]) == 0L){spp[[i]] <- integer(0)}
  #      if(length(st[[i]]) > 0 & length(sp[[i]]) > 0)    
  #        # choose the value next higher from starting (st) point, so two conditions 
  #        # must be matched: bigger and next integer, so the one with min distance
  #        spp[[i]] <- sp[[i]][(sp[[i]] > st[[i]][1]) & sp[[i]] == ((min(st[[i]][1] - sp[[i]]) * -1) + st[[i]][1])]
  #    }
  #    # choose only  
  #    res <- list(); for(i in seq_along(taxa)){
  #      # if there is no start and no stop
  #      if(length(st[[i]]) == 0 & is.integer(spp[[i]]) && length(spp[[i]]) == 0L){res[[i]] <- x}
  #      # if start but no stop: from stop to end (happens if input species occures at the bottom)
  #      if(length(st[[i]]) > 0 & length(spp[[i]]) == 0L)
  #        res[[i]] <- x[[i]][st[[i]][1] : length(x[[i]])]
  #      # if there is a start and stop condition
  #      if(length(st[[i]]) > 0 & length(spp[[i]]) > 0)
  #        if(length(st[[i]]) > 0)res[[i]] <- x[[i]][st[[i]][1]:(spp[[i]] - 1)]
  #        else
  #          res[[i]] <- x[[i]][st[[i]]:(spp[[i]] - 1)] }
  #    return(res)
  #  }
  #  res <- lapply(list(hosts_hf, hosts_sp), no_syns)
  #  hosts_hf <- res[[1]]
  #  hosts_sp <- res[[2]]
  #}
  


## V. RESULTS OBJECT ####
  #######################.
  if (database == "FH") { res <-  hosts_hf_clean }
  if (database == "SP") { res <-  hosts_sp_clean }
  if (database == "both") { 
    res <-  foreach(i = seq_along(hosts_hf)) %do% rbind(hosts_hf_clean[[i]], hosts_sp_clean[[i]])
    names(res) <- names(hosts_hf)
    res <- lapply(res, function(x)
    { if(length(grep("nodata",x)) == 2) { x <- "nodata" }
      if(!length(grep("nodata",x)) == 2) { x }})
  }
  
  res <- do.call(rbind.data.frame, res)
  
  
  
  ## VI. CLEAN DATA   #####
  #########################.
  
  #### clean the results ####
  rownames(res) <- 1:nrow(res)

  res <- data.frame(apply(res, MARGIN = 2, function (x) 
    {x <- gsub("\\.|\\(\\(.*?\\)\\)|\\.([0-9])+$|\\s+$|^\\s+|:|;|,$|- card","",x)}), stringsAsFactors = F)
  
  #### exclude synonyms if syn_include == FALSE
  if(syn_include == FALSE){
    if(process == TRUE) { message("... excluding synonyms ...") }
    
    res <- res[which(res$fungus == res$fungus_synonym_used),]
  }
  
  ## do not conduct clean step if wanted
  #res <- lapply(res, extract_info, spec_type = spec_type)
  
  #if(clean == TRUE){
  #  if(process == TRUE) { 
  #    message("... cleaning step ...")
  #  }
    #
    ### apply clean_step for each sublist (rapply not working, don't know why currently)
    #for(j in 1:length(res)){
    #  if(!(length(res[[j]])==1 & length(grep("nodata", res[[j]])) == 1)){
    #      res[[j]] <- clean_step(res[[j]], taxa = taxa,
    #        syns = syns, spec_type = spec_type, synonyms_incl = FALSE, subspec = TRUE)
    #  }
   # }
  #}
  
  #res <- lapply(res, function(x){
  #if(length(grep("nodata", x)) > 0 ){
  #  x[-grep("nodata", x)]
  #}else{x} })  
      
  
  #res <- data.frame(do.call(rbind, res))
  #res <- data.frame(input = rownames(res), res, row.names = NULL)
  #res$input <- str_replace(res$input, "\\.\\d+", "")
  #res$study_id <- as.character(res$study_id)
  
  #if(spec_type=="fungus"){
  #  names(res) <- c("fungus", "host", "country", "study_id")
  #}
  #
  #if(spec_type=="plant"){
  #  names(res) <- c("host", "fungus", "country", "study_id")
  #}
  
  # VII. RESULTS OBJECT OUT  #####
  ################################.
  #return(list(synonyms = syns, associations = res))
  return(associations = res)
}
