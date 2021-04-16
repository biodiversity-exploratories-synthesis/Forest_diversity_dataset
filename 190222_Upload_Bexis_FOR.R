###Script to prepare the dataset for bexis upload (homogenise with grassland one)###
setwd("N:/")
require(data.table)
source("R/SCRIPTS UTILES/BE_plots_zero.R")


#read grl datasets to see how they look like
grl<-fread("Exploratories/Data/GRASSLANDS/170724_EP_species_diversity_GRL.txt")
spinfoGRL<-fread("Exploratories/Data/GRASSLANDS/170724_EP_species_info_GRL.txt")

PAdat<-fread("Exploratories/Data/FORESTS/180607_forestDiv.RAW.txt") #160524_forestDivPA.unique
setkey(PAdat,species)
PAdat$species<-gsub("\\.","_",PAdat$species)


setnames(PAdat,names(PAdat),c("Plot","Species","DataID","Year","value","type"))
setcolorder(PAdat,c("Plot","Species","value","type","DataID","Year"))

#add plotID bexis column
PAdat<-BEplotNonZeros(PAdat,"Plot",plotnam = "Plot_bexis")
setcolorder(PAdat,c("Plot_bexis","Plot","Species","value","type","DataID","Year"))

#change "type" names
PAdat[type=="abund",type:="abundance"]
PAdat[type=="prAb",type:="presence_absence"]
PAdat[type=="relabund",type:="relative_abundance"]

spinfo<-fread("Exploratories/Data/FORESTS/170313_forestSP_info.txt")
spinfo$species<-gsub("\\.","_",spinfo$species)
names(spinfoGRL)
names(spinfo)
unique(spinfo$dataID)
spinfo$Well.dispersed<-spinfo$Body.size<-spinfo$dataID<-spinfo$Clade.fine<-spinfo$FG.simpl<-NULL
setnames(spinfo,names(spinfo),c("Fun_group_broad","Group_broad","Species","Group_fine","Trophic_level","Fun_group_fine"))
setcolorder(spinfo,names(spinfoGRL))


#homogeneize trophic.level names (capital letters, etc..)
spinfo[Trophic_level=="bacteriaDNA",Trophic_level:="Bacteria.DNA"]
spinfo[Trophic_level=="Pathotroph",Trophic_level:="soilfungi.pathogen"]
spinfo[Trophic_level=="Saprotroph",Trophic_level:="soilfungi.decomposer"]
spinfo[Trophic_level=="Symbiotroph",Trophic_level:="soilfungi.symbiont"]

#homogeneize Fun_group_broad names (capital letters, etc..)
spinfo[Fun_group_broad=="Omnivore",Fun_group_broad:="omnivore"]
spinfo[Fun_group_broad=="Symbiotroph",Fun_group_broad:="soilfungi.symbiont"]
spinfo[Fun_group_broad=="bacteriaDNA",Fun_group_broad:="Bacteria.DNA"]
spinfo[Fun_group_broad=="Saprotroph",Fun_group_broad:="soilfungi.decomposer"]
spinfo[Fun_group_broad=="Pathotroph",Trophic_level:="soilfungi.pathogen"]

#homogeneize Fun_group_fine names (capital letters, etc..)
spinfo[Fun_group_fine=="bacteriaDNA",Fun_group_fine:="Bacteria.DNA"]

#homogeneize Group_broad names (capital letters, etc..)
spinfo[Group_broad=="fungi.root.soil",Fun_group_fine:="fungi.soil"]

#in Group_fine, replace NA with Group_broad info
spinfo[is.na(Group_fine),Group_fine:=Group_broad]

#in Fun_group_fine, replace NA with Fun_group_broad info
spinfo[is.na(Fun_group_fine),Group_fine:=Fun_group_broad]

setkey(spinfo,Species)
setkey(PAdat,Species)
PAdat2<-merge(PAdat,spinfo,all.y=F)

aa <- union(setdiff(unique(PAdat$Species),unique(spinfo$Species)),setdiff(unique(spinfo$Species),unique(PAdat$Species)))
bb <- setdiff(unique(PAdat$Species),unique(spinfo$Species))

#remove species that are in Spinfo but not in raw data
spinfo<-spinfo[Species %in% unique(PAdat$Species)]
rm(aa,bb)


#any duplicates?
spinfo[duplicated(spinfo)]
PAdat[duplicated(PAdat)]


#all good, save files
write.table(PAdat,"Exploratories/Data/FORESTS/190215_forestDiv.RAW_bexis.txt",row.names = F)
write.table(spinfo,"Exploratories/Data/FORESTS/190215_forestSP_info_bexis.txt",row.names = F)
