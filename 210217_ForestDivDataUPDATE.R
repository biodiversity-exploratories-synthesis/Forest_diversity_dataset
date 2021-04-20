####UPDATE SYNTHESIS DATASET FOREST DIVERSITY - version ??####
#Changes from version with DataID: ?? to ??
#Script by: Caterina Penone


# 1. Homogeneise trophic level names as in grasslands dataset
# 2. Check zeros and NAs issues
# 3. Explore issue in Group_fine and Fun_group_fine (arthropods)
# 4. Remove and add arthropods + add DataID (see point 4)
# x. Update bacteria dataset (new sequencing: 4868 (2011), 25067 (2014), 26569 (2017))

# x. Update plant dataset (add more recent years: ID 30909)
# x. Update soil fungi dataset (new sequencing: 26467 (2011), 26469 (2014), 26469 (2017),  and 
# 26473 (species table))
# x. Create a column with data versions

# add nematodes from liliane ruess (not in Bexis)
# add AMF?

# Note HEW02 was replaced by HEW51 in 2017 
# 2008 -> 2016 HEW02 has data, then 2017 -> ... HEW51 #add a check on this
# HEW13 was harvested

# METADATA: # Bacteria 2011, dataset 24868 HEW04 is missing and can not be recovered --> add in metadata
# https://www.bexis.uni-jena.de/ddm/data/Showdata/24868

require(data.table)
setwd("N:/")
setwd("C:/Users/Caterina/Dropbox/")

source("R/SCRIPTS UTILES/BE_plots_zero.R")

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

#2. Check zeros or NA issues #######################################################
unique(frs$DataID)
for (i in unique(frs$DataID)){
  tt <- frs[DataID==i]
  print(paste(i, ":", (length(unique(tt$Plot))*length(unique(tt$Species))),
              "/", (nrow(tt))))
}
#check datasets arth and 20366
tt <- frs2[DataID==20366] #plant dataset, will be replaced by new one (30909) -> ok
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

#merge again with data
frs2<-merge(frs,tr,by="Species")
####################################################################################

#3. Explore issue in Group_fine and Fun_group_fine  ################################
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


#4. Remove and add arthropods + add DataID #########################################
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
bac$type<-"OTU_number"
bac$Group_broad<-bac$Trophic_level<-bac$Fun_group_broad<-bac$Fun_group_fine<-"bacteria.RNA"

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
frs2 <- rbindlist(list(frs2, bac, use.names=T))
rm(bac, b11, b14, b17); gc()

####################################################################################


#x. Update soil fungi datasets #####################################################
# Remove old datasets and replace by: 26467 (2011), 26469 (2014), 26469 (2017) and 
# 26473 (species table)

####################################################################################



















#x. Update plant dataset ###########################################################
# Remove old plant dataset and replace by new one: ID 30909
plants <- fread("Exploratories/Data/FORESTS/Update2021/30909_3_data.txt")

#keep only understory sp = remove B1 B2 = trees from 5 meters
plants <- plants[!Layer %in% c("B1","B2")]

#aggregate understory plants and remove layer column
plantagg <- plants[,value := sum(Cover), by=c("EP_PlotID","Species","Year")]
plantagg[,`:=`(Layer = NULL, Cover=NULL)]
plantagg <- unique(plantagg, by=c("EP_PlotID","Species","Year"))

#check dimension
length(unique(plants$EP_PlotID))* length(unique(plants$Species))* length(unique(plants$Year)) 
#151 plots (HEW51 replaces HEW02)
#does not correspond to plantagg --> missing NAs? yes, HEW02 misses NAs for 2020
sum(is.na(plantagg$value))

# create dataset with all combinations of plot year and species
allcomb <- CJ(EP_PlotID=unique(plants$EP_PlotID),
              Species=unique(plants$Species),
              Year=unique(plants$Year))


#SEW04 misses data for 2010
#HEW51 should have before 2017
plantagg[EP_PlotID=="HEW51"]

#what is missing from the dataset?
plantagg$missing<-"no"
allcomb <- merge(allcomb, plantagg, by=c("EP_PlotID","Species","Year"), all=T)
allcomb <- allcomb[is.na(missing)]
write.table(allcomb,"missingCombinations.txt",row.names = F) #file sent to Ralph on 29/04/21

# #add NAs for HEW02 in 2020
# plantagg[Useful_EPPlotID=="HEW02"]
# H2020 <- data.table(EP_PlotID="HEW2", Useful_EPPlotID="HEW02", Year=2020, 
#                     Species=unique(plantagg$Species), value=NA) #create NA table
# plantagg <- rbindlist(list(plantagg, H2020))
# 
# length(unique(plants$EP_PlotID))* length(unique(plants$Species))* length(unique(plants$Year)) - nrow(plantagg)
# 3591/399





setnames(plantagg,"abund2","cover")
plantagg$dataID<-20366

#homogenise column names
setnames(plants, names(plants), c("Plot_bexis", "Plot", "Year", "Layer","Species","value"))

# check if Cephalanthera_damasonium has information --> yes

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

frs2[DataID==24690, Dataversion:="???"] #bird dataset - look for zipfile in O?
frs2[DataID==24868, Dataversion:="2"]; frs2[DataID==25067, Dataversion:="2"] #bacteria
frs2[DataID==26569, Dataversion:="2"]; frs2[DataID==, Dataversion:=""]

#continue here:
frs2[DataID==, Dataversion:=""]; frs2[DataID==, Dataversion:=""]
frs2[DataID==, Dataversion:=""]; frs2[DataID==, Dataversion:=""]
#will be added
#26467_Abundant soil fungi on all 150 forest EPs (from Soil Sampling Campain 2011; Illumina MiSeq) - ASV abundances (zero-radius OTUs)_1.1.2
#26468_Abundant soil fungi on all 150 forest EPs (from Soil Sampling Campain 2014; Illumina MiSeq) - ASV abundances (zero-radius OTUs)_1.1.2
#26469_Abundant soil fungi on all 150 forest EPs (from Soil Sampling Campain 2017; Illumina MiSeq) - ASV abundances (zero-radius OTUs)_1.1.1
#plants: 30909_3_Dataset version 3
#contact Bexis to know about the new version numbers

####################################################################################
