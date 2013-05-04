#!/usr/bin/env Rscript

library(plyr)
library(reshape2)

trayvon.keywords <- c(
"#trayvon",
"#trayvonmartin",
"#zimmerman",
"#georgezimmerman",
"#millionhoodiemarch",
"#riptrayvon",
"#iamtrayvon",
"trayvon",
"george zimmerman",
"#wearetrayvon")

fluke.keywords <- c(
"sandra fluke",
"#sandrafluke",
"#boycottrush",
"#flushrush",
"#stoprush",
"#standwithsandra",
"#standwithrush",
"#istandwithrush",
"#slutgate",
"rush limbaugh"
)

kony.keywords <- c(
"kony",
"#kony",
"#kony2012",
"#konysurrender",
"#stopkony",
"#lra",
"invisible children",
"jason russell")

dt <- read.delim("../Data/data-Rfactor/elex2012.handbookUserKeywords.csv", 
	sep = "\t", header = F,
	col.names = c("user.id", "keyword", "count"),
	stringsAsFactors = F)

dt.pca <- dcast(dt, user.id ~ keyword)
dt.pca[is.na(dt.pca)] <- 0

## split out by keyword
userid <- data.frame(dt.pca$user.id)
dt.kony    <- dt.pca[kony.keywords]
dt.fluke   <- dt.pca[fluke.keywords]
dt.trayvon <- dt.pca[trayvon.keywords]

## add user.id column to splited dataframe
dt.kony <- cbind(userid, dt.kony)
dt.fluke <- cbind(userid, dt.fluke)
dt.trayvon <- cbind(userid, dt.trayvon)

## add total number of hashtags
# kony
dt.kony <- cbind (dt.kony, data.frame(rowSums(dt.kony)))
dt.kony$rowSums.dt.kony. <- dt.kony$rowSums.dt.kony - dt.kony$dt.pca.user.id
names(dt.kony) <- c("user.id", "kony", "#kony", "#kony2012", "#konysurrender", "#stopkony", "#lra", "invisible children", "jason russell", "rowSums")  
# fluke
dt.fluke <- cbind (dt.fluke, data.frame(rowSums(dt.fluke)))
dt.fluke$rowSums.dt.fluke. <- dt.fluke$rowSums.dt.fluke - dt.fluke$dt.pca.user.id
names(dt.fluke) <- c("user.id", "sandra fluke", "#sandrafluke", "#boycottrush", "#flushrush", "#stoprush", "#standwithsandra", "#standwithrush", "#istandwithrush", "#slutgate", "rush limbaugh", "rowSums")
# trayvon
dt.trayvon <- cbind (dt.trayvon, data.frame(rowSums(dt.trayvon)))
dt.trayvon$rowSums.dt.trayvon. <- dt.trayvon$rowSums.dt.trayvon - dt.trayvon$dt.pca.user.id
names(dt.trayvon) <- c("user.id", "#trayvon", "#trayvonmartin", "#zimmerman", "#georgezimmerman", "#millionhoodiemarch", "#riptrayvon", "#iamtrayvon", "trayvon", "george zimmerman", "#wearetrayvon", "rowSums")
head(dt.trayvon)

## remove empty rows (maybe use different df names: dt.kony.noempty)
dt.kony <- dt.kony[dt.kony$rowSums > 0,]
dt.fluke <- dt.fluke[dt.fluke$rowSums > 0,]
dt.trayvon <- dt.trayvon[dt.trayvon$rowSums > 0,]

## get total of hashtags used by user
kony.total <- dt.kony$rowSums
fluke.total <- dt.fluke$rowSums
trayvon.total <- dt.trayvon$rowSums

## normalize by user totals
dt.kony.norm <- dt.kony / kony.total
dt.fluke.norm <- dt.fluke / fluke.total
dt.trayvon.norm <- dt.trayvon / trayvon.total

## convert data format for factor analysis
# count data
dt.kony$user.id <- NULL
dt.kony$rowSums <- NULL
dt.fluke$user.id <- NULL
dt.fluke$rowSums <- NULL
dt.trayvon$user.id <- NULL
dt.trayvon$rowSums <- NULL

# normalized data (not the best option)
dt.kony.norm$user.id <- NULL
dt.kony.norm$rowSums <- NULL
dt.fluke.norm$user.id <- NULL
dt.fluke.norm$rowSums <- NULL
dt.trayvon.norm$user.id <- NULL
dt.trayvon.norm$rowSums <- NULL


### PRINCIPAL COMPONENT ANALYSIS

## install packages
install.packages("corpcor"); install.packages("GPArotation"); install.packages("psych");install.packages("DSUR")
library(corpcor); library(GPArotation); library(psych); library(DSUR)

## make correlation matrix
cor.matrix.kony <- cor(dt.kony)
round(cor.matrix.kony, 2)
cor.matrix.fluke <- cor(dt.fluke)
round(cor.matrix.fluke, 2)
cor.matrix.trayvon <- cor(dt.trayvon)
round(cor.matrix.trayvon, 2)

## test assumptions
# Bartlett's test
cortest.bartlett(dt.kony)
cortest.bartlett(dt.fluke)
cortest.bartlett(dt.trayvon)
# KMO test (make function to do this test: https://github.com/decode/Rpkg/blob/master/test/kmo.R)
###----- BEGINNING OF FUNCTION -----###
# KMO Kaiser-Meyer-Olkin Measure of Sampling Adequacy
kmo = function( data ){
  
  library(MASS)
  X <- cor(as.matrix(data))
  iX <- ginv(X)
  S2 <- diag(diag((iX^-1)))
  AIS <- S2%*%iX%*%S2                      # anti-image covariance matrix
  IS <- X+AIS-2*S2                         # image covariance matrix
  Dai <- sqrt(diag(diag(AIS)))
  IR <- ginv(Dai)%*%IS%*%ginv(Dai)         # image correlation matrix
  AIR <- ginv(Dai)%*%AIS%*%ginv(Dai)       # anti-image correlation matrix
  a <- apply((AIR - diag(diag(AIR)))^2, 2, sum)
  AA <- sum(a)
  b <- apply((X - diag(nrow(X)))^2, 2, sum)
  BB <- sum(b)
  MSA <- b/(b+a)                        # indiv. measures of sampling adequacy
  
  AIR <- AIR-diag(nrow(AIR))+diag(MSA)  # Examine the anti-image of the
  # correlation matrix. That is the
  # negative of the partial correlations,
  # partialling out all other variables.
  
  kmo <- BB/(AA+BB)                     # overall KMO statistic
  
  # Reporting the conclusion
  if (kmo >= 0.00 && kmo < 0.50){
    test <- 'The KMO test yields a degree of common variance
    unacceptable for FA.'
    } else if (kmo >= 0.50 && kmo < 0.60){
      test <- 'The KMO test yields a degree of common variance miserable.'
    } else if (kmo >= 0.60 && kmo < 0.70){
      test <- 'The KMO test yields a degree of common variance mediocre.'
    } else if (kmo >= 0.70 && kmo < 0.80){
      test <- 'The KMO test yields a degree of common variance middling.'
    } else if (kmo >= 0.80 && kmo < 0.90){
      test <- 'The KMO test yields a degree of common variance meritorious.'
    } else {
      test <- 'The KMO test yields a degree of common variance marvelous.'
    }

    ans <- list(  overall = kmo,
                  report = test,
                  individual = MSA,
                  AIS = AIS,
                  AIR = AIR )
    return(ans)

}    # end of kmo()
###----- END OF FUNCTION -----###
kmo(dt.kony) # kmo = .733
kmo(dt.fluke) # kmo = .779
kmo(dt.trayvon) # kmo = .714
# determinant of the correlation matrix (cutoff value: greater than 0.00001)
det(cor(dt.kony)) # .017
det(cor(dt.fluke)) # .084
det(cor(dt.trayvon)) # .011

## PCA
pca.kony <- principal (dt.kony, nfactors=3, rotate="none")
plot(pca.kony$values, type="b")
pca.fluke <- principal (dt.fluke, nfactors=3, rotate="none")
plot(pca.fluke$values, type="b")
pca.trayvon <- principal (dt.trayvon, nfactors=3, rotate="none")
plot(pca.trayvon$values, type="b")

