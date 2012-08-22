# Get DOIs for articles in Journal of Ecology and get TotalImpact metrics
  # install.packages("devtools")
# require(devtools)
# install_github("RMendeley", "ropensci")
require(RMendeley)
# install_github("dryad", "ropensci")
require(dryad)

# Get Journal of Ecology DOIs
getdois <- function (input) {
  dois <- sapply(input, function(x) x$doi) # parse out DOIs
  dois <- if(any(sapply(dois, is.null)) == TRUE){
    dois[-(which(sapply(dois,is.null),arr.ind=TRUE))] } else
      {dois}
  dois[sapply(dois,str_length) > 0] # limit to DOIs > zero character length
}

  # Search Mendeley and parse out DOIs
je <- msearch("published_in:\"Journal of Ecology\"", numItems=999)
jedois <- getdois(je)

jae <- msearch("published_in:\"Journal of Animal Ecology\"", numItems=999)
jaedois <- getdois(jae)

fe <- msearch("published_in:\"Functional Ecology\"", numItems=999)
fedois <- getdois(fe)

jape <- msearch("published_in:\"Journal of Applied Ecology\"", numItems=999)
japedois <- getdois(jape)

mee <- msearch("published_in:\"Methods in Ecology and Evolution\"", numItems=999)
meedois <- getdois(mee)

# Get TotalImpact metrics for each DOI
dat <- laply(dois2[101:500], totimp, sleep=0.1, .progress='text')
dat2 <- laply(dois2[1:100], totimp, sleep=0.1, .progress='text')
dat3 <- laply(dois2[501:900], totimp, sleep=0.1, .progress='text')
dat4 <- laply(dois2[901:949], totimp, sleep=0.1, .progress='text')


# 
setwd("/Users/ScottMac/Dropbox/Unbuntu-RStudioServer")
load("totalimpact_dat.RData")
load("totalimpact_dat2.RData")
load("totalimpact_dat3.RData")
load("totalimpact_dat4.RData")
load("totalimpact_dat5.RData")

require(plyr); require(stringr)
x <- fedat[[1]]
alldat2 <- c(fedat, fedat2, jaedat, jaedat2, jaedat3, japedat, jedat, meedat)
df <- llply(alldat2, function(x) 
  cbind(rep(x[[1]][[1]]$id, length(x[[1]][[1]]$metrics)), 
        ldply(x[[1]][[1]]$metrics, function(x) c(x$id, x$value))), .inform=T)
names(df) <- c("DOI","metric","value") # assign column names
df <- cbind(df2,
  ldply(df2[,1], function(x) str_split(x, "[.]")[[1]][[4]]) ) # put in year
head(df) # looks good
 
# Attach meta-data about papers
# install.packages("rplos")
# require(rplos)
crossrefyear <- function (DOI) { # get year from crossref.org with error catching
  if(class(try(crossref(DOI)[[1]]$publication_date$year, silent=T)) == 'try-error'){
    NA} else
      {as.numeric(crossref(DOI)[[1]]$publication_date$year)}
}
crossrefyeartitle <- function (DOI) { # get year from crossref.org with error catching
  if(class(try(crossref(DOI)[[1]]$publication_date$year, silent=T)) == 'try-error'){
    NA} else
    { out <- crossref(DOI, TRUE)
      c(as.numeric(out[[2]][[1]]$publication_date$year), 
        out[[1]])
    }
}
# crossrefyeartitle(dois2[[2098]])

require(doMC); require(plyr)
registerDoMC(cores=4)
dois2 <- c(jedois, jaedois, fedois, japedois, meedois)
years <- llply(dois2, crossrefyeartitle, .parallel=TRUE) # get yrs
dat <- ldply(llply(years, function(x) if(any(is.na(x))){c("NA", "NA")} else{x}))
bb <- data.frame(DOI = t(data.frame(dois2)), row.names=NULL)
bbdat <- cbind(dat, bb)
names(bbdat) <- c("year","journal","DOI")
data_ <- merge(df, bbdat, by="DOI", sort=F) # combine yrs with totalimpact data
data_2 <- colClasses(data_, c("factor", "factor", "numeric", "numeric"))
data_2_ <- data_2[!is.na(data_2$year) == TRUE,] # remove records with no year data
out <- droplevels(data_2_[data_2_$journal %in%  # remove non BES journals
  c("Journal of Applied Ecology", "The Journal of Ecology", 
    "The Journal of Applied Ecology", "Journal of Animal Ecology", 
    "Methods in Ecology and Evolution", "Journal of Ecology", "Functional Ecology",
    "The Journal of Animal Ecology"),])
vec <- gsub("The Journal of Animal Ecology", "Journal of Animal Ecology", out$journal)
vec <- gsub("The Journal of Applied Ecology", "Journal of Applied Ecology", vec)
vec <- gsub("The Journal of Ecology", "Journal of Ecology", vec)
vec <- gsub("Journal of Animal Ecology", "J Anim Ecol", vec)
vec <- gsub("Journal of Applied Ecology", "J Appl Ecol", vec)
vec <- gsub("Functional Ecology", "Funct Ecology", vec)
vec <- gsub("Journal of Ecology", "J Ecology", vec)
vec <- gsub("Methods in Ecology and Evolution", "Meth Ecol Evol", vec)
out$journal2 <- as.factor(vec)
str(out)
write.csv(out, "finaldata.csv")