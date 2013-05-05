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

## PCA (unrotated)
pca.kony <- principal (dt.kony, nfactors=2, rotate="none")
plot(pca.kony$values, type="b")
factor.model(pca.kony$loadings)
pca.fluke <- principal (dt.fluke, nfactors=2, rotate="none")
plot(pca.fluke$values, type="b")
factor.model(pca.fluke$loadings)
pca.trayvon <- principal (dt.trayvon, nfactors=2, rotate="none")
plot(pca.trayvon$values, type="b")
factor.model(pca.trayvon$loadings)

## PCA (orthogonal rotation: varimax)
pca.kony.v <- principal (dt.kony, nfactors=2, rotate="varimax")
pca.kony.v
plot(pca.kony.v$values, type="b")
factor.model(pca.kony.v$loadings)
print.psych(pca.kony.v, cut=0.3, sort=TRUE)
pca.fluke.v <- principal (dt.fluke, nfactors=3, rotate="varimax")
pca.fluke.v
plot(pca.fluke.v$values, type="b")
factor.model(pca.fluke.v$loadings)
print.psych(pca.fluke.v, cut=0.3, sort=TRUE)
pca.trayvon.v <- principal (dt.trayvon, nfactors=2, rotate="varimax")
pca.trayvon.v
plot(pca.trayvon.v$values, type="b")
factor.model(pca.trayvon.v$loadings)
print.psych(pca.trayvon.v, cut=0.3, sort=TRUE)

## PCA (oblique rotation: oblimin)
pca.kony.o <- principal (dt.kony, nfactors=2, rotate="oblimin")
pca.kony.o
plot(pca.kony.o$values, type="b")
factor.model(pca.kony.o$loadings)
print.psych(pca.kony.o, cut=0.3, sort=TRUE)
pca.fluke.o <- principal (dt.fluke, nfactors=3, rotate="oblimin")
pca.fluke.o
plot(pca.fluke.o$values, type="b")
factor.model(pca.fluke.o$loadings)
print.psych(pca.fluke.o, cut=0.3, sort=TRUE)
pca.trayvon.o <- principal (dt.trayvon, nfactors=3, rotate="oblimin")
pca.trayvon.o
plot(pca.trayvon.o$values, type="b")
factor.model(pca.trayvon.o$loadings)
print.psych(pca.trayvon.o, cut=0.3, sort=TRUE)

# PCA Variable Factor Map 
install.packages("FactoMineR"); library(FactoMineR)
result.pca.kony <- PCA(cov.kony) # graphs generated automatically
result.pca.fluke <- PCA(cov.kony) # graphs generated automatically
result.pca. <- PCA(cov.kony) # graphs generated automatically

###----- EXPLANATORY FACTOR ANALYSIS -----###
## make dataframe using hashtags only (no keywords)
dt.fa.kony <- dt.kony[ , 2:6]
dt.fa.fluke <- dt.fluke[ , 2:9]
dt.fa.trayvon <- dt.trayvon[ , c(1:7, 10)]

## make covariance matrix
cov.kony <- cov(dt.fa.kony)
cov.fluke <- cov(dt.fa.fluke)
cov.trayvon <- cov(dt.fa.trayvon)

## EFA: Maximum Likelihood Factor Analysis entering raw data and extracting N factors with varimax rotation 

# kony
fit.kony <- factanal(dt.fa.kony, 2, rotation="varimax")
print(fit.kony, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 
load.kony <- fit.kony$loadings[,1:2] 
plot(load.kony,type="n") # set up plot 
text(load.kony,labels=names(dt.fa.kony),cex=.7) # add variable names

#fluke
fit.fluke <- factanal(dt.fa.fluke, 3, rotation="varimax")
print(fit.fluke, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 
load.fluke <- fit.fluke$loadings[,1:2] 
plot(load.fluke,type="n") # set up plot 
text(load.fluke,labels=names(dt.fa.fluke),cex=.7) # add variable names

# trayvon
fit.trayvon <- factanal(dt.fa.trayvon, 3, rotation="varimax")
print(fit.trayvon, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 
load.trayvon <- fit.trayvon$loadings[,1:2] 
plot(load.trayvon,type="n") # set up plot 
text(load.trayvon,labels=names(dt.fa.trayvon),cex=.7) # add variable names

## Determine Number of Factors to Extract
install.packages("nFactors"); library(nFactors)

# kony
ev.kony <- eigen(cor(dt.fa.kony)) # get eigenvalues
ap.kony <- parallel(subject=nrow(dt.fa.kony),var=ncol(dt.fa.kony),
               rep=100,cent=.05)
nS.kony <- nScree(ev.kony$values, ap.kony$eigen$qevpea)
plotnScree(nS.kony)

# fluke
ev.fluke <- eigen(cor(dt.fa.fluke)) # get eigenvalues
ap.fluke <- parallel(subject=nrow(dt.fa.fluke),var=ncol(dt.fa.fluke),
                     rep=100,cent=.05)
nS.fluke <- nScree(ev.fluke$values, ap.fluke$eigen$qevpea)
plotnScree(nS.fluke)

# trayvon
ev.trayvon <- eigen(cor(dt.fa.trayvon)) # get eigenvalues
ap.trayvon <- parallel(subject=nrow(dt.fa.trayvon),var=ncol(dt.fa.trayvon),
                       rep=100,cent=.05)
nS.trayvon <- nScree(ev.trayvon$values, ap.trayvon$eigen$qevpea)
plotnScree(nS.trayvon)




##### additional information:
### http://www.statmethods.net/advstats/factor.html
### http://methodsconsultants.com/tutorial/9/Confirmatory-Factor-Analysis-Using-the-SEM-Package-in-R