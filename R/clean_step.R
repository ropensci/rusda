##                       clean_step                  ##
##     This code is part of the rusda package        ##
##     F.-S. Krah 2015 (last update: 2015-07-11)     ##


extrac_info <- function(x, spec_type){
  
  if(is.null(x)) { 
    res_clean <- "nodata" 
  }else{
    # if(spec_type == "plant" & x[[1]] == "nodata" & length(x) == 1){
    #   res_clean <- x
    # }else{
      
      
      country <- c("Afghanistan", "Albania","Algeria","Andorra","Angola","Antigua & Deps","Argentina","Armenia","Australia","Austria","Azerbaijan","Bahamas","Bahrain","Bangladesh","Barbados","Belarus","Belgium","Belize","Benin","Bhutan","Bolivia","Bosnia Herzegovina","Botswana","Brazil","Brunei","Bulgaria","Burkina","Burundi","Cambodia","Cameroon","Canada","Cape Verde","Central African Rep","Chad","Chile","China","Colombia","Comoros","Congo","Congo","Costa Rica","Croatia","Cuba","Cyprus","Czech","Denmark","Djibouti","Dominica","Dominican","East Timor","Ecuador","Egypt","El Salvador","Equatorial Guinea","Eritrea","Estonia","Ethiopia","Fiji","Finland","France","Gabon","Gambia","Georgia","Germany","Ghana","Greece","Grenada","Guatemala","Guinea","Guinea-Bissau","Guyana","Haiti","Honduras","Hungary","Iceland","India","Indonesia","Iran","Iraq","Ireland","Israel","Italy","Ivory Coast","Jamaica","Japan","Jordan","Kazakhstan","Kenya","Kiribati","Korea North","Korea South","Kosovo","Kuwait","Kyrgyzstan","Laos","Latvia","Lebanon","Lesotho","Liberia","Libya","Liechtenstein","Lithuania","Luxembourg","Macedonia","Madagascar","Malawi","Malaysia","Maldives","Mali","Malta","Marshall Islands","Mauritania","Mauritius","Mexico","Micronesia","Moldova","Monaco","Mongolia","Montenegro","Morocco","Mozambique","Myanmar","Namibia","Nauru","Nepal","Netherlands","New Zealand","Nicaragua","Niger","Nigeria","Norway","Oman","Pakistan","Palau","Panama","Papua New Guinea","Paraguay","Peru","Philippines","Poland","Portugal","Qatar","Romania","Russian Federation","Rwanda","St Kitts & Nevis","St Lucia","Saint Vincent & the Grenadines","Samoa","San Marino","Sao Tome & Principe","Saudi Arabia","Senegal","Serbia","Seychelles","Sierra Leone","Singapore","Slovakia","Slovenia","Solomon Islands","Somalia","South Africa","South Sudan","Spain","Sri Lanka","Sudan","Suriname","Swaziland","Sweden","Switzerland","Syria","Taiwan","Tajikistan","Tanzania","Thailand","Togo","Tonga","Trinidad & Tobago","Tunisia","Turkey","Turkmenistan","Tuvalu","Uganda","Ukraine","United Arab Emirates","United Kingdom","United States","Uruguay","Uzbekistan","Vanuatu","Vatican City","Venezuela","Vietnam","Yemen","Zambia","Zimbabwe")
      
      
      search_spec <- word(x[grep("\r\n\t\t\t\t", x)[1]], 1,2)
      search_spec <- gsub("\\W", " ", search_spec)
      search_spec <- gsub("^\\s+|\\s+$", "", search_spec)
      
      # delete empty strings
      if(length(which(nchar(x) == 0)) > 0) 
        x <- x[-which(nchar(x) == 0)] 
      
      # flush left
      regex <- c(paste(intToUtf8(226),"\u0089", intToUtf8(161), sep=""),
        paste(intToUtf8(194),"\u0089",intToUtf8(161), sep=""), 
        paste(intToUtf8(194),"\\s*",sep=""),
        paste("=",intToUtf8(194),sep=""),"=", "Variant\\sspelling", 
        paste(intToUtf8(195),"\u0083\u0097",sep=""),":",
        paste(intToUtf8(195),"\u0083",sep=""), "\u0097",
        "BPI\\s[0-9]*", ",")
      del <- paste(regex, collapse = "|")
      if(length(grep(del, x)) > 0) x <- gsub(del, "", x)
      
      if(length(grep("^\\s+|\\s+$",x)) > 0) x <- gsub("^\\s+|\\s+$", "", x)
      if(length(grep("f. sp.", x)) > 0) x <- gsub("f. sp.", "f.sp.", x)
      
      # extract species name
      spec <- lapply(x, function(x){
        con <- grep(paste(c("f\\.","var\\.","subsp\\.","f.\\ssp."), collapse="|" ), x)
        if(length(con) > 0){ x <- word(x, 1, 4) }
        if(length(con) == 0 & sapply(gregexpr("[A-z]\\W+", x), length) >= 2){ x <- word(x, 1, 2) }
        return(x)
      })
      spec <- unlist(spec)
      
      # extract country
      state_found <- str_extract_all(x, paste(country, collapse = "|"))
      state_found <- lapply(state_found, data.frame)
      names(state_found) <- spec
      state_found2 <- data.frame(do.call(rbind, state_found))
      state_found <- data.frame(species = rep(spec, lapply(state_found, function(x) dim(x)[1])), state_found2, row.names = NULL)
      names(state_found) <- c("species", "country")
      
      
      # extract study ID
      ids <- str_extract_all(x, "[0-9]*")
      ids <- lapply(ids, function(x) x[grep(".", x)])
      ids <- unlist(lapply(ids, paste, collapse = ","))
      ids <- unlist(ids)
      names(ids) <- spec
      ids <- data.frame(species = spec, study_id = ids, row.names = NULL)
      
      ## create data.frame
      res_clean <- list(country = state_found, study_ids = ids)
    }
  return(res_clean)
}

clean_step <- function(x, syns, taxa, spec_type, synonyms_incl, subspec = TRUE){
  
  ## delete taxa sub species
  if(subspec == TRUE){
    regex <- c(":\\s*-", "sp\\.", "var\\.", "f\\.")
    del <- paste(regex, collapse = "|")
    if(length(grep(del, x$species)) > 0)   
      x <- x[-grep(del, x$species),]
  }
  
  # delete rows with taxa names or synonyms
  if(synonyms_incl==FALSE)
  {
    if(length(grep(taxa[[1]], x$species)) > 0) 
      x <- x[-grep(taxa[[1]], x$species),]
    syns <- paste(syns[[1]], collapse="|")
    if(length(grep(syns[[1]], x$species)) > 0) 
      x <- x[-grep(syns[[1]], x$species),]
  }
  
  # delete nonsense 
  regex <- c("NA", "litter,\\sdecayed", "Substrate\\sUndetermined",
    "leaves,\\sdecayed", "water,\\sfresh","foam\\sHong", "root,\\sdecayed", 
    "bark,\\sdecayed", "twig\\sVenezuela", "humus\\sMexico","-\\s,",
    "Japan\\s-", "dung\\sNorway", "wood\\sHong", "dung, herbivore","unknown Unknown",
    "air\\s[A-Z][a-z]*", "soil,\\s[A-Z][a-z]*", "unknown,\\s[a-z]*", "unknown,\\s[A-z][a-z]*",
    "wood,\\s[a-z]*", "soil\\s[A-Z][a-z]*", "ground\\s[A-Z][a-z]*",
    "wood\\s[A-Z][a-z]*", "unknown\\s[A-Z][a-z]*", "water\\s[A-Z][a-z]*",
    "stem\\s[A-Z][a-z]*", "straw\\s[A-Z][a-z]*","paper", "leaves,\\s[A-Z][a-z]*",
    "paper\\sChina", "paper\\s[A-Z][a-z]*","wall\\spod", "rotten", "soil\\-",
    "\\s-","nodata", ":", "unknown\\s", "root\\s", "wood\\s", " f.\\ssp.", "BPI",
    "\\(White\\sspongy", "England", "Germany", "Finlland", "Ukraine", "United\\sStates",
    "Canada", "China", "Denmark", "hardwood", "conifer","deciduous", "conifer" )
  del <- paste(regex, collapse="|")
  
  x <- x[!str_detect(x$species, del),]
  
  return(x)
}

