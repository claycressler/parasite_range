# final data cleaning and calculations
# 8 May 2016

# use Stephen's cleaned host and parasite name lists
library(plyr)
library(dplyr)
library(tidyr)

dat <- read.csv("fishparas.csv",header=T)

# update parasite names
pnames <- read.csv("pnames_key.csv")
dat.p <- dat %>% unite(old_pname,P_Genus,P_species,sep=" ", remove=FALSE)
dat.p <- left_join(dat.p,pnames,by="old_pname")

# update host names
hnames <- read.csv("corrected_host_taxonomy.csv")
dat.ph <- dat.p %>% unite(orig_hname, hgenus, H_species,sep=" ",remove=FALSE)
dat.ph <- left_join(dat.ph,hnames,by="orig_hname")

dat.ph.drop <- select(dat.ph,-subspp,-hname,-H_species,-hgenus,-orig_hname,-H_F,-H_O,-P_Genus,-P_species,-old_pname,-P_Family)

# fix trait levels
levels(dat.ph.drop$P_Taxon) <- c("A","A","C","C","CR","M","M","N","N","T","T")
levels(dat.ph.drop$Host_stage) <- c("Both","Both","Definitive","Definitive","Intermediate",NA)
levels(dat.ph.drop$Endoparasite) <- c("Both","Ecto","Endo","Endo",NA)
levels(dat.ph.drop$Gender) <- c("Dioecious","Hermaphrodite","Hermaphrodite",NA)
levels(dat.ph.drop$Complex) <- c(NA,NA,"No","Yes","Yes_No")
levels(dat.ph.drop$Sexual) <- c(NA,"Both",NA,"Sexual","Sexual","Sexual")
levels(dat.ph.drop$Reproduction)[1:2] <- c(NA,NA)
levels(dat.ph.drop$Host_no)[c(1,8)] <- c(NA,NA)
levels(dat.ph.drop$Environment_LHS)[1:2] <- c(NA,NA)
levels(dat.ph.drop$Env_mode) <- c(NA,"napp",NA,"Water","Water")
levels(dat.ph.drop$Motile_LHS) <- c(NA,NA,"No","Yes","Yes")
levels(dat.ph.drop$Vector) <- c(NA,NA,"No","No","Yes")
levels(dat.ph.drop$Trophic) <- c(NA,NA,NA,NA,"No","Yes")
levels(dat.ph.drop$Horizontal) <- c(NA,"Yes",NA,"Yes")
levels(dat.ph.drop$GEO) <- c(NA,"AFR","ANT","AUS","IND",NA,NA,"NEA","NEO","PAL",NA,NA)

## How many unique intermediate and definitive hosts do the trophically transmitted parasites have?
merge(
    subset(dat.ph.drop, Trophic=="Yes" & Host_stage=="Definitive") %>%
        ddply(., .(new_pname), summarise, nDef=length(unique(new_hname))),
    subset(dat.ph.drop, Trophic=="Yes" & Host_stage=="Intermediate") %>%
        ddply(., .(new_pname), summarise, nInt=length(unique(new_hname))),
    by="new_pname",
    all=TRUE) -> hostCounts
hostCounts$nInt[is.na(hostCounts$nInt)] <- 0
hostCounts$nDef[is.na(hostCounts$nDef)] <- 0
hostCounts <- mutate(hostCounts, nHosts=nInt+nDef)

## host many parasites have no intermediate hosts recorded in the dataset?
sum(hostCounts$nInt==0)
## how many parasites have not definitive hosts recorded in the dataset?
sum(hostCounts$nDef==0)

## For parasites with more than one definitive and intermediate host, plot the relationship between these numbers
with(subset(hostCounts, nInt > 0 & nDef > 0), plot(nInt, nDef, xlab='No. of intermediate hosts', ylab='No. of definitive hosts'))
subset(hostCounts, nInt > 0 & nDef > 0 & nHosts < 5)

## of parasites with intermediate but not definitive hosts, what fraction  have different numbers of intermediate hosts?
sum(subset(hostCounts, nInt > 0 & nDef==0)$nInt<20)/nrow(subset(hostCounts, nInt > 0 & nDef==0))
sum(subset(hostCounts, nInt > 0 & nDef==0)$nInt<10)/nrow(subset(hostCounts, nInt > 0 & nDef==0))
sum(subset(hostCounts, nInt > 0 & nDef==0)$nInt<5)/nrow(subset(hostCounts, nInt > 0 & nDef==0))
sum(subset(hostCounts, nInt > 0 & nDef==0)$nInt==1)/nrow(subset(hostCounts, nInt > 0 & nDef==0))

## of parasites with intermediate but not definitive hosts, what fraction  have different numbers of intermediate hosts?
sum(subset(hostCounts, nDef > 0 & nInt==0)$nDef<20)/nrow(subset(hostCounts, nDef > 0 & nInt==0))
sum(subset(hostCounts, nDef > 0 & nInt==0)$nDef<10)/nrow(subset(hostCounts, nDef > 0 & nInt==0))
sum(subset(hostCounts, nDef > 0 & nInt==0)$nDef<5)/nrow(subset(hostCounts, nDef > 0 & nInt==0))
sum(subset(hostCounts, nDef > 0 & nInt==0)$nDef==1)/nrow(subset(hostCounts, nDef > 0 & nInt==0))

## Anisakis simplex has 222 intermediate hosts - who are they?
subset(dat.ph.drop, new_pname=="Anisakis simplex")$MaxL

## What if I break the analyses down by geographic region?
merge(
    subset(dat.ph.drop, Trophic=="Yes" & Host_stage=="Definitive") %>%
        ddply(., .(new_pname, GEO), summarise, nDef=length(unique(new_hname))),
    subset(dat.ph.drop, Trophic=="Yes" & Host_stage=="Intermediate") %>%
        ddply(., .(new_pname, GEO), summarise, nInt=length(unique(new_hname))),
    by="new_pname",
    all=TRUE) -> hostCountsByGeo
hostCountsByGeo$nInt[is.na(hostCountsByGeo$nInt)] <- 0
hostCountsByGeo$nDef[is.na(hostCountsByGeo$nDef)] <- 0
hostCountsByGeo <- mutate(hostCountsByGeo, nHosts=nInt+nDef)
subset(dat.ph.drop, Trophic=="Yes") %>%
    ddply(., .(new_pname, GEO), summarise, nDef=sum(Host_stage=="Definitive"), nInt=sum(Host_stage=="Intermediate")) -> hostCountbyGEO

## how many species come out as specialist if you look at each geographic region separately?
length(unique(subset(hostCountsByGeo, nDef==1)$new_pname))

