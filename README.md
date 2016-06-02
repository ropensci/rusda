rusda
=======



[![Build Status](https://api.travis-ci.org/ropensci/rusda.png)](https://travis-ci.org/ropensci/rusda)

## Interface to USDA databases

## Description

An interface to the web service methods provided by the United States Department of Agriculture (USDA). The Agricultural Research Service (ARS) provides a large set of databases. The current version of the package holds interfaces to the Systematic Mycology and Microbiology Laboratory (SMML), which consists of four databases: Fungus-Host Distributions, Specimens, Literature and the Nomenclature database. It provides functions for querying these databases. The main function is `associations()`, which allows searching for fungus-host combinations.

## Get rusda

From CRAN (not on CRAN yet)


```r
install.packages("rusda")
```

or from rOpenSci


```r
install.packages("devtools")
devtools::install_github("ropensci/rusda")
```

And load rusda


```r
library("rusda")
```

## Example 1
In the following case we want to search for the host of a fungus (Rosellinia ligniaria) and the fungal associations for a give host (Fagus sylvatica). From our expert knowledge we already know that these two species are associated. Let's see if the Fungus-Hosts Distributions database from the USDA confirms this hypothesis.
We first specify the input species vectors. In this case they they are only one element long (they are allowed to be longer, too).


```r
host <- "Fagus sylvatica"
fungus <- "Rosellinia aquila"
```

Then we search for the associations and look at the output. Since we are interested in "real" species names we choose the 'clean' output. We want the full possible list of associations so we choose 'synonyms=TRUE'.


```r
fungi <- associations(host, database = "both", clean = TRUE, syn_include = TRUE, spec_type = "plant", process = TRUE)
hosts <- associations(fungus, database = "both", clean = TRUE, syn_include = TRUE, spec_type = "fungus", process = TRUE)

head(fungi$association$`Fagus sylvatica`)
```

```
#> [1] "Absidia glauca"            "Acia stenodon"            
#> [3] "Acrogenospora megalospora" "Actinocladium rhodosporum"
#> [5] "Actinonema fagicola"       "Alternaria alternata"
```

```r
head(hosts$association$`Rosellinia aquila`)
```

```
#> [1] "Acer pseudoplatanus" "Acer rubrum"         "Acer sp."           
#> [4] "Alnus incana"        "Alnus rubra"         "Asclepias sp."
```

Now we want to check if our initial knowledge is correct:


```r
is.element("Rosellinia aquila", fungi$associations[[1]])
```

```
#> [1] TRUE
```

```r
is.element("Fagus sylvatica", hosts$association[[1]])
```

```
#> [1] TRUE
```

We were right. Now we can be happy and search for other associations ;-).

## Example 2
We want to know the mean number of associations for a group, e.g. the Polyporales. Lets create a species input vector with Linnean species names derived from GenBank. In a first step we might want to check how many species are deposited in the database.


```r
polyporus <- c("Polyporus_admirabilis", "Polyporus_alveoaris", "Polyporus_americanus", "Polyporus_arcularius", "Polyporus_brumalis", "Polyporus_chozeniae", "Polyporus_ciliatus", "Polyporus_corylinus", "Polyporus_craterellus", "Polyporus_dictyopus", "Polyporus_favescens", "Polyporus_fraxineus", "Polyporus_gayanus", "Polyporus_grammocephalus", "Polyporus_guianensis", "Polyporus_lepideus", "Polyporus_leprieurii", "Polyporus_leptocephalus", "Polyporus_longiporus", "Polyporus_melanopus", "Polyporus_meridionalis", "Polyporus_pinsitus", "Polyporus_pseudobetulinus", "Polyporus_radicatus", "Polyporus_rhizophilus", "Polyporus_squamosus", "Polyporus_squamulosus", "Polyporus_submelanopus", "Polyporus_subvarius", "Polyporus_tenuiculus", "Polyporus_tessellatus", "Polyporus_tricholoma", "Polyporus_tuberaster", "Polyporus_tubiformis", "Polyporus_udus", "Polyporus_umbellatus", "Polyporus_varius", "Polyporus_virgatus")

poly_meta <- meta_smml(polyporus, process = TRUE, spec_type = "fungus")
head(poly_meta)
```

```
#>                       Nomenclature Specimens Host_Fungus Literature
#> Polyporus_admirabilis            1         1           1          1
#> Polyporus_alveoaris              0         0           0          0
#> Polyporus_americanus             0         0           0          0
#> Polyporus_arcularius             1         1           1          1
#> Polyporus_brumalis               1         1           1          1
#> Polyporus_chozeniae              0         0           0          0
```

Do we need to query associations for all Polyporus species?

```
length(polyporus)                             # 38 all species
nrow(poly_meta[rowSums(poly_meta)>0,])        # 27 with data species
```

No, only 27 species are supported with data...


```r
polyporus <- rownames(poly_meta[rowSums(poly_meta) > 0, ])
polyporus_ass <- associations(polyporus, database = "both", clean = TRUE, syn_include = TRUE, spec_type = "fungus", process = TRUE)
mean(unlist(lapply(polyporus_ass[[2]], length)))
```

```
#> [1] 23.25926
```

So within the genus Polyporus the mean number of host associations is:

[![ropensci_footer](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
