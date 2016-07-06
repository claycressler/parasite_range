library(dplyr)
library(tidyr)
library(reshape2)

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

