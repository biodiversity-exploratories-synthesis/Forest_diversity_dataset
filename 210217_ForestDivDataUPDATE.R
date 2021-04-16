####UPDATE SYNTHESIS DATASET FOREST DIVERSITY - version ??####
#Changes from version with DataID: ?? to ??
#Script by: Caterina Penone

# 1. Homogeneise trophic level names as in grasslands dataset
# 2. Add zeros for arhtropod dataset and add NA for one plot
# 3. Add Data IDs for arthropods
# 4. Create a column with data versions
# x. Update bacteria dataset (new sequencing)
# x. Update soil fungi dataset (new sequencing)
# x. Update plant dataset (add more recent years)

# add nematodes from liliane ruess (not in Bexis)
# add AMF?


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

#2. Fill zeros or NAs ##############################################################
unique(frs$DataID)
for (i in unique(frs$DataID)){
  tt <- frs[DataID==i]
  print(paste(i, ":", (length(unique(tt$Plot))*length(unique(tt$Species))),
              "/", (nrow(tt))))
}
#check datasets arth and 20366
tt <- frs2[DataID==20366] #plant dataset, will be replaced by new one (30909) -> ok
rm(tt,i)

# add zeros to arhtropod dataset
tt <- frs[DataID=="arth"]
ttw <- dcast.data.table(tt, Plot+Plot_bexis+type+DataID+Year~Species, value.var = "value", fill = 0)
ttw[1:10,1:10]
tt2 <- melt.data.table(ttw, id.vars = 1:5,
                      measure.vars = 6:ncol(ttw),
                      variable.name = "Species")
# in HEW34 pitfall removed so not comparable -> add NA
tt2[Plot=="HEW34", value:=NA]

# add back to main dataset
nrow(frs)+366000-35485
frs <- frs[DataID!="arth"]
frs <- rbindlist(list(frs,tt2), use.names = T)

rm(tt,tt2,ttw); gc()

#merge again with data
frs2<-merge(frs,tr,by="Species")
####################################################################################


#3. Add DataID for arthropods ######################################################
unique(frs2$DataID)
unique(frs2[DataID=="arth"]$Trophic_level)
# "secondary.consumer.arthropod" "decomposer.arthropod"         "herbivore.arthropod"         
# "omnivore.arthropod"           "pollinator.arthropod" 
unique(frs2[DataID=="arth"]$Group_broad) #"arthropod"
unique(frs2[DataID=="arth"]$Group_fine)
 # "extraintestinal"   "chewing.carnivore" "decomposer"        "chewing.herbivore"
 # "omnivore"          "sucking.herbivore" "Hemiptera"         "sucking.carnivore"
 # "Coleoptera"        "Hymenoptera"       "pollinator"        "carnivore"        
 # "Araneae"           "Orthoptera"        "Neuroptera"        "Dictyoptera"      
 # "Dermaptera"        "Opiliones" 
#TODO: issue here!!!!
unique(frs2[DataID=="arth"]$Fun_group_broad)
unique(frs2[DataID=="arth"]$Fun_group_fine)


####################################################################################


#4. Create a version column and add DataID for arthropods ##########################
unique(frs2$DataID)
frs2$Dataversion<-NA
grl2[DataID==4140,Dataversion:="1.2.5"]; grl2[DataID==5522,Dataversion:="1.8.10"]

####################################################################################



#x. Update soil fungi datasets #####################################################
# Remove old datasets and replace by: 26467 (2011), 26469 (2014), 26469 (2017) and 
# 26473 (species table)

####################################################################################




#x. Update bacteria datasets #######################################################
# Remove old datasets and replace by: 24868 (2011), 25067 (2014), 26569 (2017)
# This is RNA (not DNA as before)
# HEW04 is missing and can not be recovered

####################################################################################
