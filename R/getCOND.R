##                       getCOND                     ##
##      This code is part of the rusda package       ##
##     F.-S. Krah 2015 (last update: 2015-07-11)     ## 

getCOND <- function(x)
{
  dbs <- grep("This report contains data from the following databases:", x)
  hf.st <- grep("The Fungus-Host Distributions database has", x)
  spe.st <- grep("The Specimens database has", x)
  nodat <- grep("There are no records for", x)
  if(length(nodat)>0){
    return("nodat")
  }else{
  dbs <- x[dbs]
  sp <- grep("Specimens", dbs)
  hfu <- grep("Fungus-Host", dbs)
  list(sp=sp, hfu=hfu, hf.st=hf.st, spe.st=spe.st)
  }
}
