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

## screen through the dataset and remove duplicate rows.
## For example, there are 405 rows that are *exact* duplicates of other rows
duplicated(dat.ph.drop[,-1]) %>% sum

## I noticed that there are 3742 rows where GEO is NA, but LAT and LON are given. However, there are no rows were GEO is specified but LAT and LON are not.
## There are also rows where LAT and LON are identical, but GEO is not - for example, look at the data for Acanthobothrium americanum

duplicated(dat.ph.drop[,c("host_pname","host_hname","LAT","LON")]) %>% sum
## there are 4659 records where the host-parasite pair has identical latitude and longitude measurements


## first look for host-parasite pairs with the same geographic region



## this drops 1077 rows where the GEO variable is identical to other rows with the same host-parasite pair, or where the biogeographic region is NA



dat.ph.drop[-union(which(duplicated(dat.ph.drop[,c("new_pname","new_hname","GEO")])),
                   which(is.na(dat.ph.drop[duplicated(dat.ph.drop[,c("new_pname","new_hname")]),'GEO']))),] -> new.dat

merge(
    subset(new.dat, Trophic=="Yes" & Host_stage=="Definitive") %>%
        ddply(., .(new_pname, GEO), summarise, nDef=length(unique(new_hname))),
    subset(new.dat, Trophic=="Yes" & Host_stage=="Intermediate") %>%
        ddply(., .(new_pname, GEO), summarise, nInt=length(unique(new_hname))),
    by="new_pname",
    all=TRUE) -> hostCountsByGeo
hostCountsByGeo$nInt[is.na(hostCountsByGeo$nInt)] <- 0
hostCountsByGeo$nDef[is.na(hostCountsByGeo$nDef)] <- 0
hostCountsByGeo <- mutate(hostCountsByGeo, nHosts=nInt+nDef)

## how many species come out as specialist if you look at each geographic region separately?
length(unique(subset(hostCountsByGeo, nDef==1)$new_pname))

