########## UPDATE SYNTHESIS DATASET FOREST DIVERSITY 2021 ##########
# Changes from version with DataID: 24607 and 24608 to ??
# Script by: Caterina Penone, University of Bern
# March - November 2021
############################################################################

# This update applies the following changes:
# 1. Homogenize trophic level names as in grasslands dataset
# x. Check zeros and NAs issues
# x. Explore issue in Group_fine and Fun_group_fine (arthropods)
# x. Remove and add arthropods + add DataID (see point 4)
# x. Update bacteria dataset (new sequencing: 4868 (2011), 25067 (2014), 26569 (2017))
# x. Update soil fungi dataset (new sequencing: 26467 (2011), 26469 (2014), 26469 (2017),
#    and 26473 (species table))
# x. Add snails: dataset 24986
# x. Add earthworms: dataset 21687
# x. Add birds 2018: dataset 25306
# x. Add protists Cercozoa and Oomycota
# x. Add moth abundance from lighttrapping on all grassland and forest plots (26026)
# x. Update plant dataset (add more recent years: ID 30909)
# x. Add nematodes dataset (received directly from L.Ruess)
# x. Fix species names with multiple underscores or underscores at the end
# x. Change years to "2007_2008" lichens and mosses
# x. Add NAs for missing plots in micromammals

#TODO add AMF?
#TODO fungi deadwood they come from two datasets --> do something to differentiate?
#TODO: check if update in bats is needed

#TODO x. Homogenise trophic group names --> update at the end
#TODO x. Create a column with data versions --> update at the end


#TODO in GRASSLAND DATASET:
# add oomycota and change protist_oomycota, cercozoa (also in OTU name)

#TODO
### Add to metadata ###

#This dataset is mostly about understorey and belowground

# Add moth dataset (26026) --> two repetitions aggregated and if one missing -> NA

# Bacteria 2011, dataset 24868 HEW04 is missing and can not be recovered
# https://www.bexis.uni-jena.de/ddm/data/Showdata/24868

#soil fungi: mention that there is a probability column (we added all but species 
# could be filtered by their probability: "unassigned"      "Probable"        "Highly Probable" "Possible" )

#there is also data on earthworm biomass 21686 earthworm biomass

# Note HEW02 was replaced by HEW51 in 2017 
# 2008 -> 2016 HEW02 has data, then 2017 -> ... HEW51 #add a check on this
# HEW13 was harvested

#ants dataset 21906: ants were sampled monthly, samples are aggregated

#nematodes are at family level


#lichens dataset --> ok, data come from 2 different years
#mosses dataset --> ok, data come from 2 different years
#protists some groups have few OTUS
# cercozoa: mainly bacterivores (omnivores and eukarivores could be rmemoved)
# oomicota: mainly plant parasites (more reliable information), hemibiotroph are alternate parasites 
# and saprotrophs (less specialised): this info is in fun_group_fine

# these datasets are not in but they exists: 
#20034: Bark Beetle Antagonists sampled with PheromoneTraps in Forest EPs in 2010
#20031: Bark Beetles sampled with Pheromone Traps in Forest EPs in 2010 # this is in the dfunctions dataset
#20035_Bark Beetles pest control based on samples with Pheromone Traps in Forest EPs in 2010_1.1.0
#24106: Ambrosia beetles and antagonists sampled by Pheromone traps on all EPs in 2010 and on a subset in 2011
#22066: The soil macrofauna orderlevel from all forest EPs sampled in spring 2011
#21686: The earthworm biomass of all forest EPs from spring 2011
#22027: Window traps in tree crowns on forest VIPs and EPs, 2008, Coleoptera, Hemiptera, Orthoptera

# 27686: Arbuscular mycorrhizal fungi on all 300 EPs (from Soil Sampling Campains 2011, 2014 and 2017; Illumina MiSeq) - ASV taxonomic look-up table
# 27688: Arbuscular mycorrhizal fungi on all 150 forest EPs (from Soil Sampling Campain 2011; Illumina MiSeq) - ASV abundances
# 27690: Arbuscular mycorrhizal fungi on all 150 forest EPs (from Soil Sampling Campain 2014; Illumina MiSeq) - ASV abundances
# 27692: Arbuscular mycorrhizal fungi on all 150 forest EPs (from Soil Sampling Campain 2017; Illumina MiSeq) - ASV abundances
# 
# 22968: Fungal communities from fine roots collected from 150 forest plots in 2014 by Illumina sequencing  - normalized to 8400 reads per plot
# 30973: Abundant of root-associated fungi from the organic layer and mineral soil across 150 forest experimental plots (soil sampling campaign, 2017) - OTU taxonomic look-up table
# 30974: Abundant of root-associated fungi from organic layer and mineral soil across 150 forest experimental plots (soil sampling campaign, 2017)




require(data.table) #to manage large datasets
#setwd("N:/")
setwd("C:/Users/Caterina/Dropbox/")


####
source("R/SCRIPTS UTILES/BE_plots_zero.R") #will be added to synthesis functions in Git

#Read last version of forest dataset
frs <- fread("Exploratories/Data/FORESTS/190215_forestDiv.RAW_bexis.txt")
tr <- fread("Exploratories/Data/FORESTS/190215_forestSP_info_bexis.txt")

frs2 <- merge(frs,tr,by="Species")

#1. Homogenise trophic level names: first group then taxa ##########################
#Initial categories can become confusing, homogenise and specify names
unique(tr$Trophic_level)
# [1] "secondary.consumer"   "decomposer"           "herbivore"            "autotroph"           
# [5] "omnivore"             "tertiary.consumer"    "pollinator"           "vert.herb"           
# [9] "soilfungi.decomposer" "soilfungi.symbiont"   "soilfungi.pathogen"   "Bacteria.DNA"  

#arthropods
tr[Trophic_level=="secondary.consumer", Trophic_level:="secondary.consumer.arthropod"]
tr[Trophic_level=="decomposer", Trophic_level:="decomposer.arthropod"]
tr[Trophic_level=="herbivore", Trophic_level:="herbivore.arthropod"]
tr[Trophic_level=="omnivore", Trophic_level:="omnivore.arthropod"]
tr[Trophic_level=="pollinator", Trophic_level:="pollinator.arthropod"]

#separate bats+birds and micromamm
tr[Group_broad=="micromammmal", Group_broad:="micromammal"]
unique(tr[Group_broad=="micromammal"]$Trophic_level)
#[1] "omnivore"           "vert.herb"          "tertiary.consumer"  "secondary.consumer"
tr[(Group_broad=="micromammal" & Trophic_level=="omnivore"), Trophic_level:="omnivore.micromammal"]
tr[(Group_broad=="micromammal" & Trophic_level=="vert.herb"), Trophic_level:="herbivore.micromammal"]
tr[(Group_broad=="micromammal" & Trophic_level=="tertiary.consumer"), Trophic_level:="tertiary.consumer.micromammal"]
tr[(Group_broad=="micromammal" & Trophic_level=="secondary.consumer"), Trophic_level:="secondary.consumer.micromammal"]

unique(tr[Trophic_level=="tertiary.consumer"]$Group_broad)
tr[Trophic_level=="tertiary.consumer",Trophic_level:="tertiary.consumer.birdbat"]

unique(tr[Trophic_level=="vert.herb"]$Group_broad)
tr[Trophic_level=="vert.herb", Trophic_level:="herbivore.bird"]

#soil fungi
tr[Trophic_level=="soilfungi.decomposer", Trophic_level:="decomposer.soilfungi"]
tr[Trophic_level=="soilfungi.symbiont", Trophic_level:="symbiont.soilfungi"]
tr[Trophic_level=="soilfungi.pathogen", Trophic_level:="pathotroph.soilfungi"]

unique(tr$Trophic_level)
# [1] "secondary.consumer.arthropod"  "decomposer.arthropod"         
# [3] "herbivore.arthropod"           "autotroph"                    
# [5] "omnivore.arthropod"            "tertiary.consumer.birdbat"    
# [7] "pollinator.arthropod"          "herbivore.bird"               
# [9] "herbivore.micromammal"         "tertiary.consumer.micromammal"
# [11] "decomposer.soilfungi"          "symbiont.soilfungi"           
# [13] "pathotroph.soilfungi"          "Bacteria.DNA" 

sum(is.na(tr$Trophic_level))
sum(is.na(tr$Group_broad)) #all species have information

#merge again with data
frs2 <- merge(frs,tr,by="Species")
####################################################################################

#x. Check zeros or NA issues #######################################################
unique(frs$DataID)
for (i in unique(frs$DataID)){
  tt <- frs[DataID==i]
  print(paste(i, ":", (length(unique(tt$Plot))*length(unique(tt$Species)))*length(unique(tt$Year)),
              "/", (nrow(tt))))
}
#check datasets arth, 4460, 4141
tt <- frs2[DataID==4460] #lichens dataset --> ok, data come from 2 different years
tt <- frs2[DataID==4141] #mosses dataset --> ok, data come from 2 different years
rm(tt,i)


# # add zeros to arhtropod dataset --> dataset will be reloaded, do this later
# tt <- frs[DataID=="arth"]
# ttw <- dcast.data.table(tt, Plot+Plot_bexis+type+DataID+Year~Species, value.var = "value", fill = 0)
# ttw[1:10,1:10]
# tt2 <- melt.data.table(ttw, id.vars = 1:5,
#                       measure.vars = 6:ncol(ttw),
#                       variable.name = "Species")
# # in HEW34 pitfall removed so not comparable -> add NA
# tt2[Plot=="HEW34", value:=NA]

# # add back to main dataset
# nrow(frs)+366000-35485
# frs <- frs[DataID!="arth"]
# frs <- rbindlist(list(frs,tt2), use.names = T)
# 
# rm(tt,tt2,ttw); gc()

#now by group broad
unique(frs2$Group_broad)
for (i in unique(frs2$Group_broad)){
  tt <- frs2[Group_broad==i]
  print(paste(i, ":", (length(unique(tt$Plot))*length(unique(tt$Species)))*length(unique(tt$Year)),
              "/", (nrow(tt))))
}

#check fungi.deadw and micromammal
tt <- frs2[Group_broad=="fungi.deadw"] #they come from two datasets --> do something to differentiate?
tt <- frs2[Group_broad=="micromammal"] #126 plots --> need to add NAs

#merge again with data
frs2<-merge(frs,tr,by="Species")
####################################################################################

#x. Explore issue in Group_fine and Fun_group_fine  ################################
unique(frs2$DataID)
unique(frs2[DataID=="arth"]$Trophic_level)
# "secondary.consumer.arthropod" "decomposer.arthropod"         "herbivore.arthropod"         
# "omnivore.arthropod"           "pollinator.arthropod" 
unique(frs2[DataID=="arth"]$Fun_group_broad)
unique(frs2[DataID=="arth"]$Fun_group_fine)
unique(frs2[DataID=="arth"]$Group_broad) #"arthropod"
unique(frs2[DataID=="arth"]$Group_fine)
 # "extraintestinal"   "chewing.carnivore" "decomposer"        "chewing.herbivore"
 # "omnivore"          "sucking.herbivore" "Hemiptera"         "sucking.carnivore"
 # "Coleoptera"        "Hymenoptera"       "pollinator"        "carnivore"        
 # "Araneae"           "Orthoptera"        "Neuroptera"        "Dictyoptera"      
 # "Dermaptera"        "Opiliones" 
## issue here due to L.72 in script 190222_Upload_Bexis_FOR.R:
#"in Fun_group_fine, replace NA with Fun_group_broad info
#spinfo[is.na(Fun_group_fine),Group_fine:=Fun_group_broad]" 
# --> NEEDS TO BE FIXED

# where did the issue happen?
tr[Group_fine==Fun_group_broad]
unique(tr[Group_fine==Fun_group_broad]$Group_broad) #"arthropod"   "fungi.deadw" "plant"

#### arhtropods
# was this information really missing in the original arthropod dataset?
arthtr <- fread("Exploratories/Data/FORESTS/TEXTfiles/all_arthropods.txt") #nope..
# --> will remove and reload arthropods to be sure

#### plants
tr[Group_fine==Fun_group_broad & Group_broad=="plant"] 
# --> one species only (Cephalanthera_damasonium), will be anyway replaced by new plant dataset

#### fungi.deadw
tr[Group_fine==Fun_group_broad & Group_broad=="fungi.deadw"]
# --> this one is normal (all is fungi.deadw)

rm(arthtr)
####################################################################################


#x. Remove and add arthropods + add DataID #########################################
# read species abundances (data coming from script 190215_DataCleaningFOR.R)
arth <- fread("Exploratories/Data/FORESTS/TEXTfiles/all_arthropodsLYR2.txt")

# add zeros
length(unique(arth$species))*length(unique(arth$plotID))
arth <- dcast.data.table(arth, plotID+year~species, value.var = "abund", fill = 0) 
arth[1:10,1:10]
arth <- melt.data.table(arth, id.vars = 1:2,
                       measure.vars = 3:ncol(arth),
                       variable.name = "Species")
# in HEW34 pitfall removed so not comparable -> add NA
arth[plotID=="HEW34", value:=NA]

# homogenise column names and add columns as in main table
setdiff(names(arth), names(frs))
setnames(arth, c("plotID","year"), c("Plot","Year"))
setdiff(names(frs), names(arth))
arth$type <- "abundance" #add "type" column
arth <- data.table(BEplotNonZeros(arth, "Plot", plotnam = "Plot_bexis")) #add bexis plot names
# (DataID is based on Order, will be added later)


### read and prepare arthropod species info from Martin Gossner
atl <- fread("Exploratories/Data/FORESTS/Traits/Arthropod_trophic.csv")
atl$species<-gsub("\\.","_",atl$species) #underscore for species names
atl <- atl[species %in% unique(arth$Species)] #select only species that are in abundance table
length(unique(arth$Species))
atl$Suborder <- atl$Family <- atl$TrophicGroup <- NULL #unwanted columns
setnames(atl, names(atl), c("Species", "Group_fine", "Trophic_level", "Fun_group_broad"))
atl$Group_broad <- "arthropod"

#homogenise trophic level names
atl[Trophic_level=="secondary.consumer", Trophic_level:="secondary.consumer.arthropod"]
atl[Trophic_level=="decomposer", Trophic_level:="decomposer.arthropod"]
atl[Trophic_level=="herbivore", Trophic_level:="herbivore.arthropod"]
atl[Trophic_level=="omnivore", Trophic_level:="omnivore.arthropod"]
atl[Trophic_level=="pollinator", Trophic_level:="pollinator.arthropod"]
unique(atl$Trophic_level)

# any field missing?
setdiff(names(tr), names(atl))
atl$Fun_group_fine <- atl$Fun_group_broad #no further info --> group fine = group broad
setdiff(names(atl), names(tr))

# add DataID
unique(atl$Group_fine)
atl[Group_fine=="Araneae", DataID:=16868]
atl[Group_fine=="Coleoptera", DataID:=16866]
atl[Group_fine=="Hemiptera", DataID:=16867]
atl[Group_fine=="Hymenoptera", DataID:=16906]
atl[Group_fine %in% c("Neuroptera","Mecoptera","Raphidioptera"), DataID:=16869]
atl[Group_fine %in% c("Orthoptera","Dermaptera","Dictyoptera"), DataID:=16886]
atl[Group_fine %in% c("Opiliones","Pseudoscorpiones"), DataID:=16887]
unique(atl$DataID)


# add DataID in the abundance table
arth <- merge(arth, atl[,.(Species,DataID)], by="Species")
atl$DataID <- NULL #remove from trait table


## Remove arthropods from main tables and add new datasets
frs <- frs[!DataID=="arth"]
tr <- tr[!Group_broad=="arthropod"]

frs <- rbindlist(list(frs, arth), use.names = TRUE)
length(unique(frs$Species))*length(unique(frs$Plot))

tr <- rbindlist(list(tr, atl), use.names = TRUE)
length(unique(tr$Species))

rm(atl, arth); gc()

#merge again with data
frs2<-merge(frs, tr, by="Species")
####################################################################################

#x. Update bacteria datasets #######################################################
# Remove old datasets and replace by: 24868 (2011), 25067 (2014), 26569 (2017)
# This is RNA (not DNA as before)
# HEW04 is missing and can not be recovered --> add in metadata
# use taxonomy from 2014 and 2017 (the one from 2011 is outdated)

b11 <- fread("Exploratories/Data/FORESTS/Update2021/24868_2_data.txt")
b14 <- fread("Exploratories/Data/FORESTS/Update2021/25067_2_data.txt")
b17 <- fread("Exploratories/Data/FORESTS/Update2021/26569_2_data.txt")

length(unique(b11$Plot_ID))*length(unique(b11$Sequence_variant)) #zeros are missing

# add DataID and Year
b11$DataID <- 24868; b11$Year <- 2011
b14$DataID <- 25067; b14$Year <- 2014
b17$DataID <- 26569; b17$Year <- 2017

# split taxonomy and remove extra information
b11[, c("kingdom", "phylum", "class", "order", "family", "genus", "species") := tstrsplit(Taxonomy, ", ", fixed=TRUE)]
b11[,c("Taxonomy","phylum","class","order","family","genus","species"):=NULL]

b14[, c("kingdom", "phylum", "class", "order", "family", "genus", "species") := tstrsplit(Taxonomy, ", ", fixed=TRUE)]
b14[,c("Taxonomy","phylum","class","order","family","genus","species"):=NULL]

b17[, c("kingdom", "phylum", "class", "order", "family", "genus", "species") := tstrsplit(Taxonomy, ", ", fixed=TRUE)]
b17[,c("Taxonomy","phylum","class","order","family","genus","species"):=NULL]

# fix plot name in 2017 dataset (HEW2 --> HEW51) #checked with Johannes Sikorski
sort(unique(b17$Plot_ID))
b17[Plot_ID=="HEW2", Plot_ID:="HEW51"]

# 2014 and 2017 together
bac <- rbindlist(list(b14, b17))

# merge taxonomy from 2014/17 to 2011 dataset
setkey(b11,Sequence_variant)
setkey(bac,Sequence_variant)
b11[bac, kingdom := i.kingdom, by=.EACHI]

# 2014, 2017 and 2011 together
bac <- rbindlist(list(bac, b11))

# add plot zeros
plotIDs <- data.frame(Plot_ID=unique(bac$Plot_ID)) #this way is faster
plotIDs <- data.table(BEplotZeros(plotIDs,"Plot_ID", plotnam = "Plot"))
bac<-merge(bac, plotIDs, by="Plot_ID")
rm(plotIDs); gc()

# change column names
setnames(bac, c("Sequence_variant","Read_count","Plot_ID","kingdom"),
              c("Species","value","Plot_bexis","Group_fine"))

# add columns and info on trophic level etc.
bac$type <- "OTU_number"
bac$Group_broad <- bac$Trophic_level <- bac$Fun_group_broad <- bac$Fun_group_fine <- "bacteria.RNA"

# check if two OTUs have same "name" but different taxonomy
temp<-unique(bac, by=c("Species","Group_fine"))
length(unique(bac$Species)) #it matches!
rm(temp)

# prepare for merging: remove all old bacteria data from main dataset
unique(frs2$Group_broad)
frs2 <- frs2[!Group_broad %in% c("bacteria")]
setdiff(names(frs2), names(bac))
setdiff(names(bac), names(frs2))

# rbind new data with main dataset
frs2 <- rbindlist(list(frs2, bac), use.names=T)
rm(bac, b11, b14, b17); gc()

####################################################################################


#x. Update soil fungi datasets #####################################################
# Remove old datasets and replace by: 26467 (2011), 26468 (2014), 26469 (2017) and 
# 26473 (species table)

unique(frs2$Group_broad)
unique(frs2[Group_broad=="fungi.root.soil"]$DataID) #this was only soil (no roots)

# read data
f11<-fread("Exploratories/Data/FORESTS/Update2021/26467.txt")
length(unique(f11$Plotid))*length(unique(f11$OTU)) #miss zeros too, but too large to add
f14<-fread("Exploratories/Data/FORESTS/Update2021/26468.txt")
f17<-fread("Exploratories/Data/FORESTS/Update2021/26469.txt")
finfo<-fread("Exploratories/Data/FORESTS/Update2021/26473.txt")

# add year and dataID
f11$Year <- 2011; f11$DataID <- 26467
f14$Year <- 2014; f14$DataID <- 26468
f17$Year <- 2017; f17$DataID <- 26469

# check if 2017 has HEW51
sort(unique(f17$Plotid)) #all good

# all years together
soilf<-rbindlist(list(f11,f14,f17))
rm(f11,f14,f17)

# add plot names with zeros
soilf<-data.table(BEplotZeros(soilf, "Plotid", plotnam = "Plot"))

# rename columns
setnames(soilf,c("Plotid","OTU","Abundance"),c("Plot_bexis","Species","value"))
soilf$type<-"ASV_number"

## prepare OTU information data
finfo$Seq <- finfo$Kingdom<-NULL #remove unwanted info
summary(as.factor(finfo$Guild))
finfo <- finfo[OTU %in% unique(soilf$Species)] #remove grassland species
# create Trophic_level column and simplify categories
finfo$Trophic_level <- finfo$Guild
finfo[Trophic_level %in% c("AMF","EMF","Lichen","EricoidM","OrchidM"),Trophic_level:="symbiont.soilfungi"]
finfo[Trophic_level %in% c("Saprotroph"),Trophic_level:="decomposer.soilfungi"]
finfo[Trophic_level %in% c("Pathogen","Parasite","Epiphyte"),Trophic_level:="pathotroph.soilfungi"]
finfo[Trophic_level %in% c("unknown","Endophyte"),Trophic_level:="unknown.soilfungi"]
summary(as.factor(finfo$Trophic_level))

# create other columns and remove non-needed columns
finfo$Fun_group_broad<-finfo$Trophic_level #Fun_group_broad
setnames(finfo,"Guild","Fun_group_fine") #Fun_group_fine
setnames(finfo,"Phylum","Group_fine") #Group_fine
finfo$Group_broad<-"soilfungi" #Group_broad
finfo[,c("Class","Order","Family","Genus","Species","Probability","TrophicMode"):=NULL]

# Merge info and raw data
soilf<-merge(soilf, finfo, by.x="Species", by.y="OTU")

# add "soilf_" before otu number (to avoid confusion with bacteria or protists)
set(soilf, j="Species", value=paste("soilf_", soilf$Species, sep=""))

# Remove old fungi from main table and add new ones
frs2 <- frs2[!Group_broad %in% c("fungi.root.soil")]

frs2<-rbindlist(list(frs2, soilf), use.names=T)
rm(soilf,finfo); gc()

####################################################################################

#x. Add snail dataset ##############################################################
snail <- fread("Exploratories/Data/FORESTS/Update2021/24986.txt")
# select forests
snail <- snail[Habitat=="FOR"]
snail$Habitat <- snail$Exploratory <- NULL # not needed columns

# check HEw51
sort(unique(snail$Plot)) #all good

# check for missing plot X species combinations
length(unique(snail$Plot))*length(unique(snail$Species))*5 #146 plots and 63 species (and 5 replicates per plot)
# missing combinations are zeros or NAs? probably zeros
length(unique(snail$Plot))*5 #655
dim(unique(snail[,.(Plot,Subplot)])) #543, some combinations missing --> use the mean per plot
sum(is.na(snail$Abundance))
#Metadada states "On plots SEW8 and SEW24 no snail individuals have been found 
#(and therefore are not included in the data table), all other plots that are missing 
#have not been sampled for several reasons (e.g., grazing livestock).
setdiff(unique(frs2$Plot_bexis), unique(snail$Plot))

# add SEW8 and SEW24 that have 0 species
plotstoadd <- CJ(Plot = c("SEW8", "SEW24"),
                 Species = unique(snail$Species),
                 Subplot = unique(snail$Subplot),
                 Abundance = 0)
snail <- rbindlist(list(snail,plotstoadd), use.names = T)

# include missing combinations (zeros)
snail <- setDT(snail)[CJ(Species=Species, Plot=Plot, Subplot=Subplot, unique=T), 
                      on=.(Species, Plot,Subplot)]
snail[is.na(Abundance), Abundance := 0 ]

#Average per plot
snail[,value:=mean(Abundance), by=list(Plot,Species)]
snail$Abundance <- snail$Subplot <- NULL
snail<-unique(snail) #9324

# Add missing plots (=NAs)
setdiff(unique(frs2$Plot_bexis), unique(snail$Plot))
plotstoadd <- CJ(Plot = c("HEW2", "SEW12", "SEW44"),
                 Species = unique(snail$Species),
                 value = NA)

snail <- rbindlist(list(snail,plotstoadd), use.names = T)

# add columns to match main dataset
snail$DataID<-24986; snail$type<-"abundance"; snail$Year<-2017
setnames(snail,"Plot", "Plot_bexis") #rename Plot_bexis
snail<-data.table(BEplotZeros(snail,"Plot_bexis",plotnam = "Plot")) # add Plot
snail$Group_broad<-"Mollusca"; snail$Group_fine<-"Gastropoda" #Group_broad, Group_fine

# add unserscore in species names
snail$Species<-gsub(" ","_",snail$Species)

# add information on Trophic_level,  Fun_group_broad, Fun_group_fine
foods <- fread("Exploratories/Data/Traits/Snails_foodpreferences.csv")
snail <- merge(snail,foods)
snail$Fun_group_broad <- snail$Fun_group_fine <- snail$Trophic_level
length(unique(snail$Species))*length(unique(snail$Plot))

# add to main dataset
frs2<-rbindlist(list(frs2,snail), use.names = T)
rm(snail, foods, plotstoadd); gc()
###################################################################################


#x. Add earthworm dataset #########################################################
# Read dataset 21687_Earthworm community 2011 all forest EPs_1.1.7
ew <- fread("Exploratories/Data/FORESTS/Update2021/21687.txt")
length(unique(ew$EP)) * length(unique(ew$Species)) #dimension is correct 150 plots and 18 species

# add underscore in species names
ew$Species<-gsub(" ","_",ew$Species)
unique(ew$Species)

# homogeneise and add columns to match main dataset
names(frs2)
ew$Explo <- NULL #remove explo column
setnames(ew, names(ew), c("Plot_bexis", "Species", "value")) #rename columns
ew <- data.table(BEplotZeros(ew,"Plot_bexis","Plot")) # add plot zero
ew$type <- "abundance" #type
ew$DataID <- "21687" #DataID
ew$Year <- 2011 #year
ew$Group_broad <- "earthworm" #Group_broad
ew$Group_fine <- "Lumbricidae" #Group_fine
ew$Trophic_level <- "decomposer.earthworm" #Trophic_level
ew$Fun_group_broad <- ew$Fun_group_fine <- "decomposer" #Fun_group_broad and Fun_group_fine

# add to main dataset
frs2<-rbindlist(list(frs2,ew), use.names = T)
rm(ew); gc()

####################################################################################


#x. Add birds #####################################################################
bird <- fread("Exploratories/Data/FORESTS/Update2021/25306.txt")
bird <- bird[type=="forest"] #only forests

# Checks and data cleaning
length(unique(bird$plot_ID)) #150
bird <- bird[on_EP==1] #select only birds seen on EPs to match previous datasets

# Do all plots have all rounds?
unique(bird$round)
plround <- unique(bird[,.(plot_ID,round)]) #some rounds are missing --> which plots?
plround$Exists <- 1
plroundAll <- CJ(plot_ID = unique(plround$plot_ID),
                 round = unique(plround$round))
plroundDiff <- merge(plroundAll, plround, by=c("plot_ID", "round"), all=T)
unique(plroundDiff[is.na(Exists)]$plot_ID) #28 plots!
table(plroundDiff[is.na(Exists)]$round) #by removing rounds 1 and 3 we can keep 145 plots
rm(plround, plroundAll, plroundDiff)

# check if richness in different rounds is correlated
birdRi <- dcast.data.table(bird, plot_ID ~ round, value.var = "abundance", 
                           fun.aggregate = sum, fill = NA)
plot(birdRi[,2:6])
cor(birdRi[,2:6], use="complete.obs") #round1 is badly correlated to the rest but round 3 is ok
# round 1 has 22 missing plots, so can't stay, round 3 is "safe" to remove
rm(birdRi)

# keep most complete rounds
bird <- bird[!round %in% c(1,3)] #"AEW4"  "AEW45" "HEW32" "SEW10" "SEW15" have missing rounds

bird <- bird[,.(plot_ID,species_latin,abundance,diet)] #select only target columns

# Aggregate rounds and add zeros (for species X plots combinations)
birdc <- dcast.data.table(bird, species_latin + diet ~ plot_ID, value.var="abundance",
                          fill = 0, fun.aggregate = sum) #sum over all rounds

length(unique(bird$species_latin)) * length(unique(bird$plot_ID)) #8100 (54*150)

bird <- melt.data.table(birdc,measure.vars=3:ncol(birdc), variable.name="Plot")

length(unique(bird$Plot)) #150 plots
length(unique(bird$species_latin)) #54 species
rm(birdc)

# set "AEW4"  "AEW45" "HEW32" "SEW10" "SEW15" to NA because they have missing rounds
bird[Plot %in% c("AEW4", "AEW45", "HEW32", "SEW10", "SEW15")]
bird[Plot %in% c("AEW4", "AEW45", "HEW32", "SEW10", "SEW15"), value:=NA]

#rename and arrange
setnames(bird, names(bird), c("Species","Fun_group_fine","Plot_bexis","value")) #rename to match main table
bird <- data.table(BEplotZeros(bird,"Plot_bexis",plotnam = "Plot")) #add plotzero
bird$type <- "abundance"; bird$DataID <- 25306; bird$Year <- 2018 #add metadata
bird$Group_broad <- "bird"; bird$Group_fine <- "bird"

#Harmonise species names (keep the ones that are already in synthesis dataset)
sort(setdiff(unique(bird$Species), unique(frs2[Group_broad=="bird"]$Species))) # 5 "new" species
sort(setdiff(unique(frs2[Group_broad=="bird"]$Species), unique(bird$Species))) # 47 old species not present here

# The 5 new species are synonyms: fix
bird[Species=="Corvus_corone", Species:="Corvus_corone_corone"]
bird[Species=="Corvus_cornix", Species:="Corvus_corone_cornix"]
bird[Species=="Regulus_ignicapilla",Species:="Regulus_ignicapillus"]
bird[Species=="Parus_ater",Species:="Periparus_ater"]
bird[Species=="Parus_caeruleus",Species:="Cyanistes_caeruleus"]

#harmonize Fun_group_fine
unique(bird$Fun_group_fine)
unique(frs2[Group_broad=="bird"]$Fun_group_fine)
bird[Fun_group_fine=="Omnivore", Fun_group_fine:="omnivore"]
bird[Fun_group_fine=="Invertebrate", Fun_group_fine:="insectivore"]
bird[Fun_group_fine=="PlantSeed", Fun_group_fine:="granivore"] 
bird[Fun_group_fine=="VertFishScav", Fun_group_fine:="carnivore"]

# create Group_broad and Fun_group_broad
unique(frs2[Group_broad=="bird"]$Fun_group_broad)
bird$Fun_group_broad <- "vert.predator"
bird[Fun_group_fine %in% c("grazer","granivore"), Fun_group_broad:="vert.herb"]

unique(frs2[Group_broad=="bird"]$Trophic_level)
bird$Trophic_level<-"tertiary.consumer.birdbat"
bird[Fun_group_broad=="vert.herb",Trophic_level:="herbivore.bird"]

# create "zero dataset" for all species that are not in the 2018 dataset
oldbi <- sort(setdiff(unique(frs2[Group_broad=="bird"]$Species), unique(bird$Species))) #those need zeros in 2018 (47 species)
oldbi <- frs2[Species %in% oldbi & Year=="2008"] #select only one year
oldbi$Year <- 2018
oldbi$value <- 0
oldbi$DataID <- "25306"
150 * 47 #150 plots and 47 species = 7050 occurrences (dimension of oldbird dataset)
150 * 54 #dimension of bird dataset = 8100 occurrences (dimension of bird dataset)

# check hew51
sort(unique(bird$Plot))

# add to main dataset
setdiff(names(bird), names(frs2))
frs2 <- rbindlist(list(frs2, bird, oldbi), use.names=TRUE)

# backward checks
# checkbi <- frs22[Group_broad=="bird"] #90 900
# length(unique(checkbi$Species)) * length(unique(checkbi$Year)) * length(unique(checkbi$Plot)) #91 506
# length(unique(checkbi$Species)) * length(unique(checkbi$Year)) * 150 #ok, the difference is due to HEW51
# checkbi2 <- setDT(checkbi)[CJ(Species=Species, Plot_bexis=Plot_bexis, Year=Year, unique=T), 
#                            on=.(Species, Plot_bexis, Year)]
# checkbi2 <- checkbi2[is.na(Plot)]
# frs2[Species=="Accipiter_gentilis"]
# frs2[Species=="Accipiter_gentilis" & Plot_bexis=="HEW51"]
# sort(unique(checkbi[is.na(value)]$Species))
# sort(unique(checkbi$Species))
# sort(unique(bird$Species))

rm(allbirdtr,birdtr,bird,newsp,birdc,allpl,checkbi,newbioldy,oldbi,yearver,newbi); gc()
####################################################################################


#x. Update plant dataset 30909######################################################
# Remove old plant dataset and replace by new one: ID 30909
plants <- fread("Exploratories/Data/FORESTS/Update2021/30909_5_data.txt")

# keep only understory sp = remove B1 B2 = trees from 5 meters
plants <- plants[!Layer %in% c("B1","B2")]

# aggregate understory plants and remove layer column
plantagg <- plants[,value := sum(Cover), by=c("EP_PlotID","Species","Year")]
plantagg[,`:=`(Layer = NULL, Cover=NULL)]
plantagg <- unique(plantagg, by=c("EP_PlotID","Species","Year"))

# check dimension
length(unique(plants$EP_PlotID))* length(unique(plants$Species))* length(unique(plants$Year)) 
#151 plots (HEW51 replaces HEW02)
#does not correspond to plantagg --> missing NAs? yes, HEW51 misses all years before 2017
sum(is.na(plantagg$value))
# NAS <- plantagg[is.na(value)]; unique(NAS$EP_PlotID)

# create dataset with all combinations of plot year and species
allcomb <- CJ(EP_PlotID = unique(plants$EP_PlotID),
              Species = unique(plants$Species),
              Year = unique(plants$Year))

allcomb <- data.table(BEplotZeros(allcomb,"EP_PlotID","Useful_EPPlotID"))

# SEW04 misses data for 2010 --> solved in dataset version 5
# HEW51 should have before 2017
# plantagg[EP_PlotID=="HEW51"]

#what is missing from the dataset?
# plantagg$missing<-"no"
# allcomb <- merge(allcomb, plantagg, by=c("EP_PlotID","Species","Year"), all=T)
# allcomb <- allcomb[is.na(missing)]
# write.table(allcomb,"missingCombinations.txt",row.names = F) #file sent to Ralph on 29/04/21

# #add NAs for HEW02 in 2020
# plantagg[Useful_EPPlotID=="HEW02"]
# H2020 <- data.table(EP_PlotID="HEW2", Useful_EPPlotID="HEW02", Year=2020, 
#                     Species=unique(plantagg$Species), value=NA) #create NA table
# plantagg <- rbindlist(list(plantagg, H2020))
# 
# length(unique(plants$EP_PlotID))* length(unique(plants$Species))* length(unique(plants$Year)) - nrow(plantagg)
# 3591/399

# check if Cephalanthera_damasonium has information --> yes
#SEW04 misses 2010 --> solved in dataset version 5

# add missing combinations
plantagg <- merge(allcomb, plantagg, by=c("EP_PlotID", "Useful_EPPlotID", "Species", "Year"), all=T)

# homogenise column names
setnames(plantagg, names(plantagg), c("Plot_bexis", "Plot", "Species", "Year", "value"))

# add missing columns
plantagg$DataID <- 30909
plantagg$type <- "cover"

# create species info
# oldpl <- tr[Group_broad=="plant"]
# newpl <- data.table(Species = unique(plantagg$Species))
# newpl <- merge(newpl, oldpl, by="Species", all.x=T)
# # write table and fill manually
# write.table(newpl, "Exploratories/Data/FORESTS/Update2021/plant_species_update21.txt", row.names = F)
rm(oldpl)
newpl <- fread("Exploratories/Data/FORESTS/Update2021/plant_species_update21.txt", h=T)

# Merge
plantagg <- merge(plantagg, newpl, by="Species")

# Remove old plant dataset and add new one
frs2 <- frs2[!Group_broad == "plant"]
frs2 <- rbindlist(list(frs2, plantagg), use.names = T)
rm(plantagg, plants, newpl, allcomb); gc()

####################################################################################


#x. Add protists 2011 and 2017#####################################################

############CERCOZOA
# Read diversity data and check dimension
pro17 <- fread("Exploratories/Data/FORESTS/Update2021/24466.txt")
length(unique(pro17$variable)) * length(unique(pro17$EP_PlotID)) #every row is repeated twice!
pro17 <- unique(pro17)
pro11 <- fread("Exploratories/Data/FORESTS/Update2021/24426.txt")
length(unique(pro11$OTUs)) * length(unique(pro11$EP_PlotID))
pro11 <- unique(pro11)

# Remove columns not needed
pro17$MyPlotID <- NULL; pro11$MyPlotID <- NULL

# Remove grassland plots
length(unique(pro17$EP_PlotID))
pro17 <- pro17[!grepl("G", pro17$EP_PlotID)] #148 -> add NAs for missing plots HEW7, SEW18 (no amplification)

length(unique(pro11$EP_PlotID))
pro11 <- pro11[!grepl("G", pro11$EP_PlotID)] #144 -> add NAs for missing plots SEW31, SEW33, HEW7, HEW21, HEW23, HEW33 (no amplification)

# Add NAs for missing plots 2017
setdiff(unique(frs2$Plot_bexis), unique(pro17$EP_PlotID)) #HEW7, SEW18
toadd <- data.table(EP_PlotID = c("HEW7", "SEW18"), 
                    variable = rep(unique(pro17$variable),2),
                    raw_abund = NA)
length(unique(pro17$variable)) * 2

pro17 <- rbindlist(list(pro17,toadd))
rm(toadd)

# Add NAs for missing plots 2011
setdiff(unique(frs2$Plot_bexis), unique(pro11$EP_PlotID)) #"HEW7"  "HEW21" "HEW23" "HEW33" "SEW31" "SEW33"
toadd <- data.table(EP_PlotID = c("HEW7",  "HEW21", "HEW23", "HEW33", "SEW31", "SEW33"), 
                    OTUs = rep(unique(pro11$OTUs),6),
                    raw_abund = NA)
length(unique(pro11$OTUs)) * 6

pro11 <- rbindlist(list(pro11,toadd))
rm(toadd)

# Change hew2 to hew51 in 2017
sort(unique(pro17$EP_PlotID))
pro17[EP_PlotID=="HEW2", EP_PlotID:="HEW51"]

# Check species names matching
setdiff(unique(pro11$OTUs),unique(pro17$variable)) #perfect
setdiff(unique(pro17$variable),unique(pro11$OTUs)) #perfect

# Read species information / traits
proinf17 <- fread("Exploratories/Data/FORESTS/Update2021/24468.txt")
proinf11 <- fread("Exploratories/Data/FORESTS/Update2021/24467.txt")
proinf11 <-unique(proinf11)

# Add year and DataID
pro17$DataID <- 24466; pro11$DataID <- 24426
pro17$Year <- 2017; pro11$Year <- 2011
setnames(pro17,"variable","OTUs")

#use OTUs or species? look at correlation
# pro11s<-merge(pro11,proinf11,by="OTUs")
# pro11s[raw_abund!=0,raw_abund:=1]
# pro11s[,otuRich:=sum(raw_abund),by=EP_PlotID]
# pro11s[,spRich:=sum(raw_abund),by=list(Species,EP_PlotID)]
# 
# rich<-unique(pro11s[,.(EP_PlotID,otuRich,spRich)])
# rich<-unique(rich,by="EP_PlotID")
# plot(rich$otuRich,rich$spRich)
# cor.test(rich$otuRich,rich$spRich) #0.86 correlated, just keep OTUs in the table (so people can do rarefaction)
# rm(pro11s,rich)

setnames(pro11,"raw_abund","value"); setnames(pro17,"raw_abund","value")

# Merge species information
pro11 <- merge(pro11,proinf11,by="OTUs")
pro17 <- merge(pro17,proinf17,by="OTUs")

# Add group name to otu ID (to avoid confusions with fungi or bacteria)
pro11[,OTUs:=paste(OTUs,"_protist_CERCOZOA",sep="")] #2011 and 2017 are compatible, use same OTUID
pro17[,OTUs:=paste(OTUs,"_protist_CERCOZOA",sep="")] #2011 and 2017 are compatible, use same OTUID

# 2011 and 2017 together
prot <- rbind(pro11,pro17)
rm(pro11,pro17,proinf11,proinf17)

# Select only focus columns
prot<-prot[,.(OTUs,EP_PlotID,value,DataID,Year,
              Phylum,Class,nutrition_bacterivore, nutrition_omnivore, nutrition_eukaryvore, nutrition_plant_parasite,
              nutrition_parasite_not_plant, nutrition_unknown)]

# Add trophic level
prot$Trophic_level <- "protist.unknown"
## are there species with multiple nutrition?
prot[rowSums(prot[,8:13,with=F])>1]
prot[rowSums(prot[,8:13,with=F])==0]
dim(prot[rowSums(prot[,8:13,with=F])==1])

prot[nutrition_bacterivore==1,Trophic_level:="bacterivore.protist"]
prot[nutrition_omnivore==1,Trophic_level:="omnivore.protist"]
prot[nutrition_eukaryvore==1,Trophic_level:="eukaryvore.protist"]
prot[nutrition_plant_parasite==1,Trophic_level:="plantparasite.protist"]
prot[nutrition_parasite_not_plant==1,Trophic_level:="nonplantparasite.protist"]
nrow(prot[Trophic_level=="unknown.protist"])
sum(prot$nutrition_unknown)
# remove nutrition columns
prot[,(8:13):=NULL]

# Add "Group_broad"     "Group_fine"      "Fun_group_broad" "Fun_group_fine" 
unique(frs2$Group_broad)
prot$Group_broad <- "Protists"
unique(frs2$Group_fine)

setnames(prot,"Phylum","Group_fine")
unique(frs2$Fun_group_broad); unique(frs2$Fun_group_fine)
prot$Fun_group_broad<-prot$Fun_group_fine<-prot$Trophic_level
prot$Class <- NULL
prot$type <- "OTU_number"
setnames(prot,"OTUs","Species")


# Add Plot
prot<-data.table(BEplotZeros(prot,"EP_PlotID",plotnam="Plot"))
setnames(prot,"EP_PlotID","Plot_bexis")


#############OOMYCOTA
# Read diversity data and check dimension
pro17 <- fread("Exploratories/Data/FORESTS/Update2021/25767_2_data.txt")
length(unique(pro17$OTU)) * length(unique(pro17$EP_PlotID))

pro11 <- fread("Exploratories/Data/FORESTS/Update2021/25766_2_data.txt")
length(unique(pro11$OTU)) * length(unique(pro11$EP_PlotID))

# Read species information / traits
proinf <- fread("Exploratories/Data/FORESTS/Update2021/25768_2_data.txt")

# Change hew2 to hew51 in 2017
sort(unique(pro17$EP_PlotID))
pro17[EP_PlotID=="HEW2", EP_PlotID:="HEW51"]

# Add year, DataID and merge
pro17$My_PlotID <- NULL; pro11$MyPlotID <- NULL
pro17$DataID <- 25767; pro11$DataID <- 25766
pro17$Year <- 2017; pro11$Year <- 2011

protoo <- rbindlist((list(pro11,pro17)))
rm(pro11, pro17)

# Remove grassland plots
length(unique(protoo$EP_PlotID))
protoo <- protoo[!grepl("G", protoo$EP_PlotID)] 
length(unique(protoo[Year=="2011"]$Plot_bexis)) #150
length(unique(protoo[Year=="2017"]$Plot_bexis)) #150

#### Prepare species info table
# Add Group broad and Group fine
proinf$Group_broad <- "Protists_oomycota"
unique(proinf$Order)
setnames(proinf, "Order", "Group_fine")

# Create trophic level information
table(proinf[,.(Lifestyle, Substrate)])
proinf$Trophic_level <- "plantparasite.protist"
proinf[Lifestyle=="saprotroph", Trophic_level:="decomposer.protist"]
proinf[(!Lifestyle %in% "saprotroph" & Substrate == "Metazoa"),
       Trophic_level:="animalparasite.protist"]
proinf[(!Lifestyle %in% "saprotroph" & Substrate == "substrate_undetermined"),
       Trophic_level:="unknown.protist"]

table(proinf[,.(Lifestyle, Trophic_level)]) #check ok
table(proinf[,.(Substrate, Trophic_level)]) #check ok

# Functional group broad and fine
proinf$Fun_group_broad <- proinf$Fun_group_fine <- proinf$Trophic_level
proinf[(Lifestyle == "obligate_biotroph" & Trophic_level == "plantparasite.protist"),
       Fun_group_fine:="plant.obligate.biotroph.protist"]

proinf[(Lifestyle == "hemibiotroph" & Trophic_level == "plantparasite.protist"),
       Fun_group_fine:="plant.hemibiotroph.protist"]

proinf[(Lifestyle == "obligate_biotroph" & Trophic_level == "animalparasite.protist"),
       Fun_group_fine:="animal.obligate.biotroph.protist"]

proinf[(Lifestyle == "hemibiotroph" & Trophic_level == "animalparasite.protist"),
       Fun_group_fine:="animal.hemibiotroph.protist"]

proinf[(Trophic_level == "decomposer.protist"),
       Fun_group_fine:="saprotroph.protist"]

table(proinf[,.(Fun_group_fine, Trophic_level)]) #check ok

# Remove columns not needed
proinf$Genus <- proinf$Lifestyle <- proinf$Substrate <- NULL

## Change names
setnames(proinf, "OTUs", "Species")
setnames(protoo, c("EP_PlotID", "OTU", "abundance"), c("Plot_bexis", "Species", "value"))

# Merge diversity and species info
protoo <- merge(protoo, proinf, by = "Species")

# Add group name to otu ID (to avoid confusions with fungi or bacteria)
protoo[,Species:=paste(Species,"_protist_OOMYCOTA",sep="")]

# Add new columns: type, plot zero
setdiff(names(frs2), names(protoo))
setdiff(names(protoo), names(frs2))
protoo$type <- "OTU_number"
protoo <- data.table(BEplotZeros(protoo, "Plot_bexis", plotnam = "Plot"))

# Checks
apply(prot, 2, function(x)sum(is.na(x)))
apply(protoo, 2, function(x)sum(is.na(x)))

length(unique(prot$Plot)) #151
length(unique(protoo$Plot)) #151

length(unique(prot$Species)) #2101
length(unique(protoo$Species)) #1148

150 * 2101 * 2 #630300
150 * 1148 * 2 #344400

# Checks passed, merge with main dataset
frs2 <- rbindlist(list(frs2, prot, protoo),use.names = T)
rm(prot, protoo, proinf); gc()
####################################################################################


#x. Add moth dataset (26026) #######################################################
moth <- fread("Exploratories/Data/FORESTS/Update2021/26026_2_data.txt")
summary(moth) #zeros are missing

# Keep only forest plots
moth <- moth[Habitat=="W"]

# Remove unwanted columns
moth[,c("Exploratory", "Habitat", "Familie", "Subfamily", "Genus", 
        "Author", "Comments", "CollectionDate"):=NULL]

# Check species overlap with main dataset
length(unique(moth$Species)) #397 species
length(setdiff(unique(moth$Species), unique(frs2$Species))) #not a single species overlaps!

# Check if all plots have two repetitions ("CollectionRun")
moth[is.na(NumberIndividuals)] #missing collection runs are NAs
length(unique(moth[is.na(NumberIndividuals)]$PlotID)) #14 plots with missing info --> maybe use one run?

# Explore first and second repetition
cr1 <- moth[CollectionRun==1]
cr2 <- moth[CollectionRun==2]
# How many species overlap?
length(intersect(unique(cr1$Species), unique(cr2$Species))) #199 species overlap
# Is species richness in the two repetitions correlated?
sr1 <- dcast.data.table(cr1, PlotID~Trophic_level, fun.aggregate = sum, value.var= "NumberIndividuals")
sr2 <- dcast.data.table(cr2, PlotID~Trophic_level, fun.aggregate = sum, value.var= "NumberIndividuals")
plot(sr1$detritivore ~ sr2$detritivore)
cor(sr1$detritivore, sr2$detritivore) #0.34
plot(sr1$herbivore ~ sr2$herbivore)
cor(sr1$herbivore, sr2$herbivore) #0.29
# --> repetitions not correlated, need to use both and loose plots
rm(sr1,sr2,cr1,cr2)

# Aggregate both repetitions
moth[,value := sum(NumberIndividuals), by=c("PlotID", "Species")]
moth[PlotID=="SEW38" & CollectionRun==2] #check if NAs are kept -> yes
moth <- unique(moth, by=c("PlotID","Species"))

# Check dimension
length(unique(moth$Species)) * length(unique(moth$PlotID))#397 species and 150 plots --> missing combinations are zeros

# Add zeros
moth <- dcast.data.table(moth, Species + Trophic_level + Fun_group_broad + Fun_group_fine ~ PlotID, 
                          value.var = "value", fill = 0)
moth[1:10,1:10]
#remove Species==NA
moth <- moth[!is.na(Species)]
#melt
moth <- melt.data.table(moth, id.vars = 1:4,
                        measure.vars = 5:ncol(moth),
                        variable.name = "Plot")
# Check dimension again
length(unique(moth$Species)) * length(unique(moth$Plot))#396 species (one was NA) and 150 plots --> missing combinations are zeros

# Add missing columns: plot Bexis, type, DataID, Year, Group_broad, Group_fine
moth <- BEplotNonZeros(moth, "Plot", "Plot_bexis")
moth$type <- "abundance"
moth$DataID <- 26026
moth$Year <- 2018
moth$Group_broad <- "arthropod"
moth$Group_fine <- "Lepidoptera"

# check hew51
sort(unique(moth$Plot)) #ok

# Add to main dataset
frs2 <- rbindlist(list(frs2, moth), use.names = T)
####################################################################################


#x. Add nematodes dataset ##########################################################
# This dataset is not in Bexis, the author is not available at the moment but agreed to
# add it to the synthesis dataset. We will update the code once the dataset is in Bexis
nem <- fread("Exploratories/Data/FORESTS/Update2021/nematodes.csv")

# Long format
nem <- melt.data.table(nem, variable.name = "Species")
summary(nem)

# Merge species info
neminfo <- fread("Exploratories/Data/FORESTS/Update2021/nematode_species_info.csv")
nem <- merge(nem, neminfo, by="Species")

# Add missing columns
nem$type <- "presence_absence"
nem$DataID <- NA
nem$Year <- 2014
nem <- BEplotNonZeros(nem,"Plot","Plot_bexis")

# Add to main dataset
frs2 <- rbindlist(list(frs2, nem), use.names = T)
rm(nem, neminfo)
####################################################################################


#x. Fix underscores in species names ###############################################
# Do some species have multiple underscores or underscores at the end?
frs2[grep("__",frs2$Species),] #yes, replace with single underscores
frs2$Species <- gsub("__","_",frs2$Species)
frs2[grep("__",frs2$Species),] #none left

# Any double underscores at the end of species names?
frs2[grep("_$",frs2$Species),] #yes, remove
frs2$Species <- sub("_$","",frs2$Species)
####################################################################################


#x. Change years for lichens and mosses ############################################
# Both datasets have a mix of data collected in 2007 and 2008
unique(frs2[DataID==4460]$Year) #lichens
unique(frs2[DataID==4141]$Year) #mosses

# Change year to 2007_2008 to avoid confusions
# (this information will be added in the metadata)
frs2 <- frs2[, Year:=as.character(Year)] #year is numeric, change this
frs2[DataID %in% c(4460,4141), Year:="2007_2008"]
frs2[is.na(Year)]
####################################################################################


#x. Add NAs for missing plots in micromammals ######################################
# Check number of plots
sort(unique(frs2[Group_broad == "micromammal"]$Plot)) #126 plots
mm <- frs2[Group_broad == "micromammal"] #subset
length(unique(mm$Plot)) #126
length(unique(mm$Species)) #12
length(unique(mm$Year)) #2
126*12*2

# Create missing combinations
mm2 <- CJ(Species=unique(mm$Species), Plot=unique(mm$Plot), Year=unique(mm$Year))

# Fill the rest of the columns
mm2 <- merge(mm2, unique(mm[,.(Plot, Plot_bexis, type)]), by="Plot", all.x = T)
mm2 <- merge(mm2, unique(mm[,.(Year, DataID)]), by="Year", all.x = T)
mm2 <- merge(mm2, unique(mm[,.(Species, Group_broad, Group_fine, Trophic_level,
                               Fun_group_broad, Fun_group_fine)]), by="Species", all.x = T)
mm2 <- merge(mm2, mm[,.(Plot, Species, Year, value)], by = c("Plot", "Species", "Year"), all.x = T)

sum(is.na(mm2$value)) #1051
nrow(mm2) - nrow(mm) #1051 --> fine

# Remove old micromamm dataset and add new one
frs2 <- frs2[!Group_broad == "micromammal"]
frs2 <- rbindlist(list(frs2, mm2), use.names = T)
rm(mm, mm2); gc()
####################################################################################




#TODO: check if update in bats is needed
#TODO add AMF?
#TODO: did I put hew2 and 51 everywhere for plants?


#x. Example header #################################################################
####################################################################################



#x. Homogenise trophic group names #################################################
# This will match better with the grassland dataset
trG <- fread("Exploratories/Data/GRASSLANDS/190218_EP_species_info_GRL.txt")
unique(trG$Group_broad)
unique(tr$Group_broad)
tr[Group_broad == "arthropod", Group_broad:= "Arthropod"]
tr[Group_broad == "plant", Group_broad:= "Plant"]
tr[Group_broad == "fungi.deadw", Group_broad:= "Fungi.deadw"]
tr[Group_broad == "lichen", Group_broad:= "Lichen"]
tr[Group_broad == "bryophyte", Group_broad:= "Bryophyte"] #the grl dataset has "Moss" but bryo is better
tr[Group_broad == "micromammal", Group_broad:= "Micromammal"]
tr[Group_broad == "bat", Group_broad:= "Bat"] #the grl dataset has Bats but this is more conisitent
tr[Group_broad == "fungi.root.soil", Group_broad:= "SoilFungi.roots"]
tr[Group_broad == "bacteria", Group_broad:= "Bacteria"]

#TODO: same with other groups
#TODO fungi deadwood they come from two datasets --> do something to differentiate?
unique(frs2$Trophic_level)
unique(frs2$Group_broad)

rm(trG)
####################################################################################







#x. Create a version column ##########################
sort(as.numeric(unique(frs2$DataID)))
# 4141  4460 10300 10301 13146 13526 15187 15188 15189 15190 16866 16867 16868 16869 16886
# 16887 16906 17186 18547 20366 21047 24690 24868 25067 26569

frs2$Dataversion<-NA
frs2[DataID==4141, Dataversion:="1.6.8"]; frs2[DataID==4460, Dataversion:="1.11.14"]
frs2[DataID==10300, Dataversion:="1.2.4"]; frs2[DataID==10301, Dataversion:="1.1.5"]
frs2[DataID==13146, Dataversion:="1.1.7"]; frs2[DataID==13526, Dataversion:="1.2.3"]
frs2[DataID==15187, Dataversion:="2.1.3"]; frs2[DataID==15188, Dataversion:="2.1.2"]
frs2[DataID==15189, Dataversion:="2.1.3"]; frs2[DataID==15190, Dataversion:="3.1.4"]
frs2[DataID==16866, Dataversion:="1.1.3"]; frs2[DataID==16867, Dataversion:="1.1.4"]
frs2[DataID==16868, Dataversion:="1.1.3"]; frs2[DataID==16869, Dataversion:="1.1.4"]
frs2[DataID==16886, Dataversion:="1.1.2"]; frs2[DataID==16887, Dataversion:="1.1.2"]
frs2[DataID==16906, Dataversion:="1.1.1"]; frs2[DataID==17186, Dataversion:="2.1.0"]
frs2[DataID==18547, Dataversion:="1.1.0"]; 
 
frs2[DataID==24690, Dataversion:="4.1.2"] #bird dataset
frs2[DataID==24868, Dataversion:="2"]; frs2[DataID==25067, Dataversion:="2"] #bacteria
frs2[DataID==26569, Dataversion:="2"];
frs2[DataID==24986, Dataversion:="1.3.9"] #snails
frs2[DataID==21906, Dataversion:="2"] #ants
frs2[DataID==25306, Dataversion:="1.0.0"] #birds


#continue here:
frs2[DataID==, Dataversion:=""]; frs2[DataID==, Dataversion:=""]
frs2[DataID==, Dataversion:=""]; frs2[DataID==, Dataversion:=""]
#will be added
#21687_Earthworm community 2011 all forest EPs_1.1.7
#26467_Abundant soil fungi on all 150 forest EPs (from Soil Sampling Campain 2011; Illumina MiSeq) - ASV abundances (zero-radius OTUs)_1.1.2
#26468_Abundant soil fungi on all 150 forest EPs (from Soil Sampling Campain 2014; Illumina MiSeq) - ASV abundances (zero-radius OTUs)_1.1.2
#26469_Abundant soil fungi on all 150 forest EPs (from Soil Sampling Campain 2017; Illumina MiSeq) - ASV abundances (zero-radius OTUs)_1.1.1
#plants: 30909_5_Dataset version 5
#moth: version 2
#nematodes: NA


####################################################################################





############## Datasets not included and why #####################
# Soil macrofauna (ID: 22066) -> not added because order level and overlap with arthropods
# --> add to metadata
# soilmf <- fread("Exploratories/Data/FORESTS/Update2021/22066_2_data.txt")
# unique(soilmf$Orderlevel) #many overlaps with arthropod dataset
# unique(frs2[Group_broad=="arthropod"]$Group_fine)
# setdiff(unique(soilmf$Orderlevel), unique(frs2[Group_broad=="arthropod"]$Group_fine))
# setdiff(unique(soilmf$Orderlevel), unique(frs2$Group_fine))

#Bark beetles --> overlap with arthropods + too specific: do not add but put info in metadata
#20034: Bark Beetle Antagonists sampled with PheromoneTraps in Forest EPs in 2010
#20031: Bark Beetles sampled with Pheromone Traps in Forest EPs in 2010 # this is in the dfunctions dataset
#20035_Bark Beetles pest control based on samples with Pheromone Traps in Forest EPs in 2010_1.1.0
#24106: Ambrosia beetles and antagonists sampled by Pheromone traps on all EPs in 2010 and on a subset in 2011

#21906: Pitfall traps on forest EPs in 2008 subset Formicidae Species Abundances
#Issues in data that cannot be solved for the moment, but the code is below, ready to be updated

#x. Add ants dataset 21906 #########################################################
ant <- fread("Exploratories/Data/FORESTS/Update2021/21906_2_data.txt")

# Explore data
unique(ant$Traptype) #only BF
unique(ant$CollectionYear) #2008
ant[is.na(Species)] #some all NAs with abundance = NA --> these are plots with no ants
ant[is.na(Abundance)] #none
length(unique(ant$Plot_ID)) * #150 plots
  length(unique(ant$CollectionMonth))* #5 months
  length(unique(ant$Trapnumber)) * #4 trap numbers, corresponding to cardinal points
  length(unique(ant$Species)) #30 species

# Issue: the number of sampling months per plot varies between 1 and 5 --> these are zeros (after checking with Heike)
# the number of trap locations per plot varies between 1 and 3 --> remove the ones with 1 and 
# Match the traps with the Core arthropod dataset then
# randomly select 2 for each month as in Grevé et al 2018, Ecosphere


# Transform "Oct2010" into "Oct"
ant <- ant[CollectionMonth=="Oct2010", CollectionMonth:="Oct"]

# Recover which traps were destroyed (from arhtropod Core dataset)
arthro <- fread("Exploratories/Data/FORESTS/Update2021/17016_2_data.txt")
arthro <- arthro[CollectionYear==2008] #only 2008
arthro <- arthro[Traptype=="BF"] #only BF traps
unique(arthro$CollectionMonth)
arthro <- arthro[!CollectionMonth=="Apr"] #remove April
arthro[CollectionMonth=="Okt", CollectionMonth:="Oct"] #use same name of ant dataset
summary(arthro) #no NAs
length(unique(arthro$PlotID)) * #150
  length(unique(arthro$Subplot)) * #4 (but only 3 traps installed!)
  length(unique(arthro$CollectionMonth)) #5
#total should be 3000 but dimension is 2250 so 750 traps lost (ok because 3 repetitions per plot)
#--> by merging we can recover the zeros!
#adapt the arthro dataset to match the ant one
names(arthro)
names(ant)
arthro[,c("TrapID", "Exploratory", "Traptype", "CollectionYear", "CollectionDate"):=NULL] #remove redundant columns
setnames(arthro,c("PlotID", "Subplot"), c("Plot_ID", "Trapnumber")) #match names to ant dataset
#ant dataset with only plot, trap, month
zeroant <- unique(ant[,.(Plot_ID, Trapnumber, CollectionMonth)], 
                  by=c("Plot_ID", "Trapnumber", "CollectionMonth")) #734
zeroant$id <- 1
zeroant <- merge(arthro, zeroant, by=c("Plot_ID","Trapnumber","CollectionMonth"), all.x = T)
zeroant[is.na(id), id:=0]

#Merge to add zeros
ant2 <- merge(ant, zeroant, by=c("Plot_ID","Trapnumber","CollectionMonth")) #--> issue here

# check combinations in ant and not in arthro dataset
strangecombi <- ant[!zeroant, on=.(Plot_ID, Trapnumber, CollectionMonth)]
# --> 4 samples do not exist in the arthropod dataset...! I officially hate this dataset
# - Plot_ID: HEW2, CollectionMonth: June, Trapnumber: NO
# In HEW2, in June there was NW, SO, SW (but no NO traps).
# The ant dataset reports information for: NO and SO
# 
# - Plot_ID: AEW36, CollectionMonth: June, Trapnumber: SW
# In AEW36, in June there was NO, NW, SO (but no SW traps).
# The ant dataset reports information for: NW, SW
# 
# - Plot_ID: AEW44, CollectionMonth: June, Trapnumber: SO
# In AEW44, in June there was NO, NW, SW (but no SO traps).
# The ant dataset reports information for: NO and SO.


# 30 plots have zero ants (in any of the traps) -- > add them back later after aggregating
plotstoadd <- unique(strangecombi[is.na(Species)]$Plot_ID)


# Randomly select two traps per month?
set.seed(16)
ant <- ant[,.SD[sample(.N, min(2,.N))], by = c("Plot_ID", "CollectionMonth")] #745

# Add zeros for missing months

# Final number should be: 150 plots x 2 traps x 5 month x 30 species = 45000

# Average?

#remove pitfall HEW34
arthro<-arthro[!(Traptype=="BF" & PlotID=="HEW34")] #######rather add NAs

# Same traps as for arthropods so remove HEW34

# Aggregate information across the whole sampling period (monthly sampling)
ant[,value:=sum(Abundance), by=c("Plot_ID", "Species")]

plantagg <- plants[,value := sum(Cover), by=c("EP_PlotID","Species","Year")]
ant <- ant[,.(Plot_ID, Species, Abundance)] #only target columns

ant$Species<-gsub(" ","_",ant$Species)
ant$DataID<-23986
ant$Dataversion<-"2.1.6"
ant$Year<-"2014_2015"
ant<-data.table(BEplotNonZeros(ant,"Plot",plotnam = "Plot_bexis"))
setnames(ant,"Presence_absence","value")
ant$type<-"presenceabsence"
ant$Trophic_level<-ant$Fun_group_broad<-ant$Fun_group_fine<-"omnivore.ant"
ant$Group_broad<-ant$Group_fine<-"Formicidae"
#ant[, c("Group_fine", "sp") := tstrsplit(Species, "_", fixed=TRUE)]
#ant$sp<-NULL
length(unique(ant$Species))*length(unique(ant$Plot)) #31 species and 110 plots..

#overlap with existing species?
intersect(unique(ant$Species),unique(grl2$Species)) #11 overlapping species!

#remove from ant dataset (because pollinator dataset is more complete)
ant<-ant[!Species %in% intersect(unique(ant$Species),unique(grl2$Species))]

grl2<-rbind(grl2,ant)
rm(ant); gc()


####################################################################################

