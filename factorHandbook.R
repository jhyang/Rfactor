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

dt <- read.delim("../data/elex2012.handbookUserKeywords.csv", 
	sep = "\t", header = F,
	col.names = c("user.id", "keyword", "count"),
	stringsAsFactors = F)

dt.pca <- dcast(dt, user.id ~ keyword)
dt.pca[is.na(dt.pca)] <- 0

## split out by keyword
dt.kony    <- dt.pca[kony.keywords]
dt.fluke   <- dt.pca[fluke.keywords]
dt.trayvon <- dt.pca[trayvon.keywords]

## remove empty rows
dt.kony <- dt.kony[rowSums(dt.kony) > 0,]
dt.fluke <- dt.fluke[rowSums(dt.fluke) > 0,]
dt.trayvon <- dt.trayvon[rowSums(dt.trayvon) > 0,]

## get total of hashtags used by user
kony.total <- rowSums(dt.kony)
fluke.total <- rowSums(dt.fluke)
trayvon.total <- rowSums(dt.trayvon)

## normalize by user totals
dt.kony <- dt.kony / kony.total
dt.fluke <- dt.fluke / fluke.total
dt.trayvon <- dt.trayvon / trayvon.total
