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

# normalized data
dt.kony.norm$user.id <- NULL
dt.kony.norm$rowSums <- NULL
dt.fluke.norm$user.id <- NULL
dt.fluke.norm$rowSums <- NULL
dt.trayvon.norm$user.id <- NULL
dt.trayvon.norm$rowSums <- NULL
