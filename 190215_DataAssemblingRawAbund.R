### Biodiversity exploratories - Synthesis dataset FORESTS ###
### Author: Caterina Penone

### This script assembles single datasets to create the forest dataset 
### (raw data and species info) ###

## It should be used after running the script 190215_DataCleaningFOR.R and 
## is then followed by the script 190222_Upload_Bexis_FOR.R for bexis upload

## Warning: This was my first script doing this kind of dataset operations so it's far 
# from being perfect (and is a bit too "home-made"). I could re-write it but given the 
# limited time I prefer to focus on updating the dataset with better scripts!


#assemble and continue species table: add genus for bacteria,mycorrhiza,fungi.soil

require(data.table)
setwd("N:/")
setwd("c:/Users/Caterina/Dropbox/")

dirpath<-"Exploratories/Data/FORESTS/TEXTfiles/TO_MERGE"

#open all files
all.files<-list.files(dirpath,full.names=T,pattern="txt")
all.filesPA<-list.files(dirpath,full.names=T,pattern="PA.txt") #only file names with P/A data
all.filesAB<-list.files(dirpath,full.names=T,pattern="AB.txt") #only file names with abundance data
all.filesRelab<-list.files(dirpath,full.names=T,pattern="relab.txt") #only file names with  relative abundance data
all.filesCover<-list.files(dirpath,full.names=T,pattern = "plant|lichen|bryophytes")
mylist<- lapply(all.files, function(i) fread(i))
names(mylist)<-all.files
#check if some datasets have NAs
sum(is.na(mylist)) #0 :)

####################create raw file#############################
rbindatRAW<-rbindlist(mylist[c(all.files)],use.names=T,fill=T)
rbindatRAW[ ,`:=`(order = NULL)]
rbindatRAW$PrAb<-as.numeric(rbindatRAW$PrAb)
rbindatRAW$value<-rbindatRAW$PrAb
rbindatRAW$type<-"prAb"

rbindatRAW[!is.na(abund), type := "abund"]
rbindatRAW[!is.na(abund), value := abund]

rbindatRAW[!is.na(relabund), type := "relabund"]
rbindatRAW[!is.na(relabund), value := relabund]

rbindatRAW[!is.na(cover), type := "cover"]
rbindatRAW[!is.na(cover), value := cover]

rbindatRAW[ ,`:=`(abund = NULL, relabund=NULL, PrAb = NULL, cover = NULL)]

#homogeneize the EP names, e.g. SEW07/SEW7
plotinfo<-fread("Exploratories/Data/GeneralData/PlotIDzeros_all.csv")
plotinfo$Explo<-NULL
setkey(rbindatRAW,plotID)
setnames(plotinfo,"All_EP_PlotID_ALL","plotID")
rbindatRAW<-merge(rbindatRAW,plotinfo,by="plotID")
rbindatRAW[ ,`:=`(plotID = NULL)]
setnames(rbindatRAW,"EP_PlotID0","plotID")
length(unique(rbindatRAW$plotID)) #150 :)
dim(rbindatRAW[duplicated(rbindatRAW)]) #check if there are duplicate rows
setcolorder(rbindatRAW,c("plotID","species","dataID","year","value","type"))

write.table(rbindatRAW,"Exploratories/Data/FORESTS/190215_forestDiv.RAW.txt",row.names=F)
rm(rbindatRAW,mylist)

#####################rbind and transform into presence/absence######################
mylist<- lapply(all.files, function(i) fread(i))
names(mylist)<-all.files
for (i in all.filesRelab){setnames(getElement(mylist, i),"relabund","abund")} #change relabund into abund
for (i in all.filesCover){setnames(getElement(mylist, i),"cover","abund")} #change cover into abund 
rbindatPA<-rbindlist(mylist[c(all.filesAB,all.filesRelab)],use.names=T,fill=T) #only abundance and relative abundance files
rbindatPA[ ,`:=`(order = NULL)]
set(rbindatPA,j="abund",i=which(rbindatPA$abund!=0),value=1)
setnames(rbindatPA,"abund","PrAb")
rbindatPA<-rbind(rbindatPA,rbindlist(mylist[all.filesPA],use.names=T,fill=T),use.names=T, fill=T) ##add files with has P/A data only
sum(is.na(rbindatPA))

#homogeneize the EP names, e.g. SEW07/SEW7
plotinfo<-fread("Exploratories/Data/GeneralData/PlotIDzeros_all.csv")
plotinfo$Explo<-NULL
setkey(rbindatPA,plotID)
setnames(plotinfo,"All_EP_PlotID_ALL","plotID")
rbindatPA<-merge(rbindatPA,plotinfo,by="plotID")
rbindatPA[ ,`:=`(plotID = NULL)]
setnames(rbindatPA,"EP_PlotID0","plotID")
length(unique(rbindatPA$plotID)) #150 :)
length(unique(rbindatPA$dataID)) #19
length(unique(rbindatPA$species)) #26 208! =  combinations
unique(rbindatPA[,c("year","dataID"),with=F]) #combinations ID,year 4141, 4460 are mixed
dim(rbindatPA[duplicated(rbindatPA)]) #check if there are duplicate rows

#for the groups with less than 150 plots, add NAs for the plots with no info.. necessary??
####for the moment I did not do that (lack of memory and code not optimized)
#setkey(rbindatPA,plotID,dataID,year,species)
#temp<-rbindatPA[CJ(unique(plotID),unique(dataID),unique(year),unique(species))] #not enough memory..
#system.time(temp<-as.data.table(with(rbindatPA, expand.grid(plotID = unique(plotID), species = unique(species), dataID=unique(dataID))),key=c("plotID","species")))
#system.time(temp<-rbindatPA[CJ(unique(plotID),unique(species))])
#or cast and melt again, but same problem of memory..
#castemp<-dcast.data.table(rbindatPA,dataID+plotID+year~species,value.var="PrAb",drop=F)

setcolorder(rbindatPA,c("plotID","species","PrAb","year","dataID"))

write.table(rbindatPA,"N:/Exploratories/Data/FORESTS/190215_forestDivPA.all.txt",row.names=F)
rm(rbindatPA,mylist)
####################################################################################

#####################rbind and calculate relative abundances#########################
mylist<- lapply(all.files, function(i) fread(i))
names(mylist)<-all.files
for (i in all.filesCover){setnames(getElement(mylist, i),"cover","abund")} #change cover into abund 

rbindat<-rbindlist(mylist[all.filesAB],use.names=T,fill=T) 
setkey(rbindat,dataID,plotID)

sum(is.na(rbindat))

#create a column with the aggregated sum
rbindat[,abundAll := sum(abund),by=c("dataID","year")]
rbindat[,relabund := abund/abundAll]
#replace NaN due to division by zero into zeros
setkey(rbindat,relabund)
set(x=rbindat,i=which(is.na(rbindat$relabund)),j="relabund",value=0) #data.table is sooo cool :)
#remove non needed columns
rbindat[ ,`:=`(abund = NULL, abundAll=NULL)]

##add files with relative abundance data only
relab<-rbindlist(mylist[all.filesRelab],use.names=T,fill=T)
rbindat<-rbind(rbindat,relab,use.names=T, fill=T) 

#homogeneize the stupid EP names, e.g. SEW07/SEW7
plotinfo<-fread("Exploratories/Data/GeneralData/PlotIDzeros_all.csv")
plotinfo$Explo<-NULL
setkey(rbindat,plotID)
setnames(plotinfo,"All_EP_PlotID_ALL","plotID")
rbindat<-merge(rbindat,plotinfo,by="plotID")
rbindat[ ,`:=`(plotID = NULL)]
setnames(rbindat,"EP_PlotID0","plotID")
length(unique(rbindat$plotID)) #150 :)
length(unique(rbindat$species)) #25 734!
length(unique(rbindat$dataID)) #14 (3 datasets have P/A only)
unique(rbindat[,c("year","dataID"),with=F]) #combinations ID,year 4141, 4460 are mixed
dim(rbindat[duplicated(rbindat)]) #check if there are duplicate rows

#for the groups that have less than 150 plots, add NAs for the plots with no info.. necessary?? see above

setcolorder(rbindat,c("plotID","species","year","relabund","dataID"))

write.table(rbindat,"Exploratories/Data/FORESTS/190215_forestDivRAB.all.txt",row.names=F)
rm(list = ls())
gc()

#####################remove duplicates from presence/absence data######################
padat<-fread("N:/Exploratories/Data/FORESTS/190215_forestDivPA.all.txt")
##remove year to avoid misuse of the data
padat[ ,`:=`(year = NULL)]
setkey(padat,species,plotID)
padat<-padat[,PrAb:=max(PrAb),by=c("plotID","species")]
setkey(padat,plotID,species)
padat<-unique(padat,by=key(padat))

write.table(padat,"N:/Exploratories/Data/FORESTS/190215_forestDivPA.unique.txt",row.names=F)


#####################Make table with tax and trophic info for each species####################
padat<-fread("N:/Exploratories/Data/FORESTS/180322_forestDivPA.unique.txt") 
setkey(padat,species)
padat<-unique(padat,by=key(padat))
padat[ ,`:=`(plotID = NULL,PrAb=NULL)]
#create a new column by replacing dataID by info on tax group
padat$group<-padat$dataID
padat[group == "arth", group := "arthropod"]
padat[group == 19526, group := "bacteria"]
padat[group == 20366, group := "plant"]
padat[group == 18547 | group == 17186, group := "fungi.deadw"]
padat[group == 4460, group := "lichen"]
padat[group == 4141, group := "bryophyte"]
padat[group == 15187, group := "bird"]
padat[group == 13146, group := "bat"]
padat[group == 10301 | group == 10300, group := "micromammmal"]
#padat[group == 19186, group := "mycorrhiza"]
#padat[group == 19026, group := "fungi.soil"]
padat[group == "21047", group := "fungi.root.soil"] #check this in future


##add info from grassland dataset
grasp<-fread("N:/Exploratories/Data/GRASSLANDS/EP species list FGs 19.06.15.txt")
setkey(setnames(grasp,"Species","species"),species)
padat$species<-gsub("_",".",padat$species)
setkey(padat,species)
padat2<-merge(padat,grasp,all.x=T) #613 sp in common
padat2[group == "mycorrhiza", `:=`(Trophic.level = "symbiont",FG.fine= "symbiont", FG.broad= "symbiont")]
padat2[group == "plant"| group == "bryophyte"| group == "lichen", `:=`(Trophic.level = "autotroph")]
padat2[group == "fungi.deadw", `:=`(Trophic.level = "decomposer")]
padat2[group == "bacteria", `:=`(Trophic.level = "bacteriaDNA",FG.fine = "bacteriaDNA",FG.broad = "bacteriaDNA")]
#padat2[group == "fungi.soil", `:=`(Trophic.level = "fungiDNA")]
padat2[group == "bacteria", `:=`(Trophic.level = "bacteriaDNA",FG.fine = "bacteriaDNA",FG.broad = "bacteriaDNA")]
padat2[group == "bryophyte", `:=`(Clade = "Bryophyte",FG.fine = "moss",FG.broad = "moss")]
padat2[group == "lichen", `:=`(Clade = "Lichen",FG.fine = "lichen",FG.broad = "lichen")]

#homogeneize in "Clade" and populate
padat2[group == "bird", `:=`(Clade = "Bird")]
padat2[group == "bat", `:=`(Clade = "Bat")]
padat2[group == "micromammmal", `:=`(Clade = "Micromam")]
padat2[group == "plant", `:=`(Clade = NA)]
rm(grasp)

bb<-fread("N:/Exploratories/Data/FORESTS/Traits/BirdBat.diet.troph.csv")
setkey(bb,species)
setkey(padat2,species)
padat2[bb, `:=` (Trophic.level=i.Trophic.level,FG.fine=i.FG.fine,FG.broad=i.FG.broad)]
rm(bb)

#merge with info on arthropod taxonomy
arth<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/all_arthropods.txt")
setkey(arth,species)
arth<-unique(arth,by=key(arth))
arth$species<-gsub("_",".",arth$species)
setnames(arth,"Order","Clade")
setkey(arth,species)

sum(is.na(padat2$Clade))
padat2[arth, Clade := i.Clade] #very cool data.table way to update a field!!
rm(arth)

#add arthropod trophic level from Martin Gossner
atl<-fread("N:/Exploratories/Data/FORESTS/Traits/Arthropod_trophic.csv")
setkey(atl,species)
setkey(padat2,species)
padat2[atl, `:=` (Trophic.level=i.Trophic.level,FG.broad=i.FG.broad)]
rm(atl)



##add clade & traits information for plants
spt<-fread("N:/Exploratories/Data/FORESTS/Traits/plantFOREST_taxtrait.txt")
setkey(spt,species)
setkey(padat2,species)
padat2[spt, `:=` (Clade=i.Clade, Trophic.level=i.Trophic.level,FG.broad=i.FG.broad,FG.fine=i.FG.fine,
                  Well.dispersed=i.Well.dispersed,Body.size=i.Body.size)]


sum(is.na(padat2$Clade))
unique(padat2[is.na(Clade)]$group)
rm(spt)


########fungi soil and roots
funsr<-fread("N:/Exploratories/Data/FORESTS/Traits/soil_root_fungi.txt")
setkey(funsr,species)

#homogenize guild types
funsr[Guild=="Ectomycorrhizal",Guild:="EcM"]
funsr[Guild=="Arbuscular Mycorrhizal",Guild:="AM"]
funsr[Guild=="Plant Pathogen",Guild:="Plant_pathogen"]
#could be further simplified/recoded in future

#use same names as padat2
setnames(funsr,c("Guild","ecology"),c("FG.fine","FG.broad"))
setnames(funsr,c("pyhlum","taxclass"),c("Clade","Clade.fine"))
funsr$FG.simpl<-funsr$FG.broad

funsr$group<-"fungi.root.soil"
funsr$year<-NULL;funsr$OTU<-NULL
funsr$dataID<-"21047_19168"
funsr$Trophic.level<-funsr$FG.simpl

funsr<-funsr[,names(funsr) %in% names(padat2),with=F]


  
setkey(funsr,species)
setkey(padat2,species)
padat2[funsr,`:=` (Clade = i.Clade, Trophic.level=i.Trophic.level, FG.broad=i.FG.broad,
                 Clade.fine=i.Clade.fine, FG.fine=i.FG.fine, FG.simpl=i.FG.simpl, group=i.group,
                 )] #to be completed

#change FG.simpl names
padat2[FG.simpl=="Saprotroph",FG.simpl:="soil.saprotroph"]
padat2[FG.simpl=="Symbiotroph",FG.simpl:="soil.symbiont"]
padat2[FG.simpl=="Pathotroph",FG.simpl:="soil.pathogen"]



#add fungi.soil clade
#fun<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/21047_soilfung.txt")
#setkey(fun,OTU)
#fun<-unique(fun)
#dim(fun[pyhlum=="unclassified"]); unique(fun$pyhlum) #2724 and 6
#dim(fun[class=="unclassified"]) ; unique(fun$class) #3638 and 27
#dim(fun[order=="unclassified"]) ; unique(fun$order) #4028 and 88#

#replace "unclassified" into NA
#fun[pyhlum=="unclassified",pyhlum:=NA]

#add phylum information
#set(fun,j="OTU",value=paste("soilf.",fun$OTU,sep=""))#add "soilf." 
#setnames(fun,"function","ecology")
#fun<-fun[,c("OTU","kingdom","pyhlum","class","order","family","genus","ecology"),with=F]
#setnames(fun,c("OTU","pyhlum","class"),c("species","Clade","Clade.fine"))
#setkey(fun,species)
#setkey(padat2,species)


#fun[ecology=="",ecology:="Unknown"]

#fun[ecology %in% c("AM","EcM","Lichenized","ericoidMyco"),Trophic.level:="soilfungiRNA.symbiont"]
#fun[ecology %in% c("Animal parasite","Mycoparasite","Parasite","Pathogen","Plant pathogen"),Trophic.level:="soilfungiRNA.pathogenRNA"]
#fun[ecology %in% c("Saprotroph"),Trophic.level:="soilfungiRNA.decomposerRNA"]
#fun[ecology %in% c("Unknown"),Trophic.level:="soilfungiRNA.unknown"]

#fun$FG.simpl<-fun$Trophic.level
#fun$FG.broad<-fun$Trophic.level
#setnames(fun,"ecology","FG.fine")
#fun$group<-"soil.fungi"
#fun$dataID<-21047

#setkey(fun,species)
#setkey(padat2,species)
#padat2[fun,`:=` (Clade = i.Clade, Trophic.level=i.Trophic.level, FG.broad=i.FG.broad,
#                 Clade.fine=i.Clade.fine, FG.fine=i.FG.fine, FG.simpl=i.FG.simpl, group=i.group)]

#asked to Stifu for functional group: decomposers, symbionts or parasites
#a<-fun[,colnames(fun) %in% c("class","order"),with=F]
#a<-unique(a)
#write.table(a,"N:/Exploratories/Data/FORESTS/Traits/fungi.csv")
#add Stifu data
#st<-fread("N:/Exploratories/Data/FORESTS/Traits/fungiExploForest_StifuCat.csv")
#st<-st[,-1,with=F]
#setkey(fun,order)
#setkey(st,order)
#fun<-merge(fun,st)
#replace underscores in sp names
#fun$species<-gsub("_",".",fun$species)
#setkey(fun,species)
#setkey(padat2,species)
#sum(is.na(padat2$Trophic.level))
#padat2[fun,`:=` (Trophic.level=i.Trophic.level,FG.broad=i.FG.broad)]
#rm(fun)
#rm(st)

####add mychorizae clade
#mic<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/19186_mychorriz.txt")
#setkey(mic,OTU_ID)
#mic<-unique(mic)
#unique(mic$phylum) #  4
#unique(mic$class) #  19
#unique(mic$order) #  62

#replace "unclassified" into NA
#mic[phylum=="p__unidentified",phylum:=NA]

#add phylum information
#set(mic,j="OTU_ID",value=paste("micz.",mic$OTU_ID,sep=""))#add "micz."
#setkey(mic,OTU_ID)

#mic<-mic[,c("OTU_ID","kingdom","phylum","class","order","family","genus","species"),with=F]
#setnames(mic,c("OTU_ID","phylum"),c("species","Clade"))
#mic$Clade<-gsub("p__","",mic$Clade)
#setkey(mic,species)
#setkey(padat2,species)
#sum(is.na(padat2$Clade))
#padat2[mic,Clade := i.Clade]
#rm(mic)


#plants<-padat2[group=="plant"]
#write.table(plants,"N:/Exploratories/Data/FORESTS/plantFOREST22_info.txt",row.names=F)
#lichen<-padat2[group=="lichen"]

#arth<-spInfo[dataID=="arth"]
#length(unique(arth$species))
#arth<-arth[,c("Order","Suborder","Family","species"),with=F]
#write.table(arth,"N:/Exploratories/Data/FORESTS/Traits/Arthro_tax.csv",row.names=F)
#to be completed! wait for new Martin's data



###add bacteria clade
bac<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/19526_bacteria.relab.txt")
setkey(bac,OTUcode)
bac<-unique(bac,by"OTUcode")
unique(bac$Phylum) #  51
unique(bac$Class) #  182
unique(bac$Order) #  318

##too many classes, so create a Clade.fine to include this information
padat2$Clade.fine<-padat2$Clade

bac$Phylum<-gsub(" ",".",bac$Phylum)
bac$OTUcode<-gsub("_",".",bac$OTUcode)


#add phylum information to padat2

bac<-bac[,c("OTUcode","Phylum","Class","Order","Family","Genus","Species"),with=F]
setnames(bac,c("OTUcode","Phylum"),c("species","Clade.fine"))
setkey(bac,species)
setkey(padat2,species)
sum(is.na(padat2$Clade.fine))
padat2[bac,Clade.fine := i.Clade.fine]

#just put bacteria in the Clade column
sum(is.na(padat2$Clade))
padat2[group == "bacteria", `:=`(Clade = "bacteria")]
rm(bac)


#add trophic level for micromammals
mm<-fread("N:/Exploratories/Data/FORESTS/Traits/micromamm.csv")
setkey(mm,species)
setkey(padat2,species)
padat2[mm, `:=` (Trophic.level=i.Trophic.level,FG.fine=i.FG.fine,FG.broad=i.FG.broad)]
rm(mm)

#for the moment put mycorrhiza and soil.fung in the Clade column of these 2
padat2[group == "mycorrhiza", `:=`(Clade = "mycorrhiza")]
padat2[group == "fungi.soil", `:=`(Clade = "fungi.soil")]

#add fungi.deadwood clade (do we have it?) #ask Stifu, for the moment just put fungi.deadw
padat2[group == "fungi.deadw", `:=`(Clade = "fungi.deadw")]
padat2[group == "fungi.soil", `:=`(Trophic.level = "fungi.soil")]

##create a table with "layer" trait for lichens and bryophytes
#bryo<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/4141_bryophytes_layer.txt")
#bryo<-unique(bryo[,3:4,with=F])
#replace NA by B (=terricolous)
#bryo[which(is.na(bryo$layer)), layer := "B"]
#aggregate layer info per species
#bryo2<-data.table(aggregate(layer~sp_name,paste,collapse="_",data=bryo))
#save table
#write.table(bryo2,"N:/Exploratories/Data/FORESTS/Traits/bryo_layer.txt",row.names=F)
#modfied the txt to add a FG.simpl with 3 categories wood/soil/mixed

#remove NA's in FG.broad
padat2[group == "fungi.soil" & is.na(FG.broad), `:=`(FG.broad = "fungi.soil")]
padat2[group == "fungi.deadw" & is.na(FG.broad), `:=`(FG.broad = "fungi.deadw")]


##add a column with simplified FG #only 3 vert.omni -> vert.carn
unique((padat2[,c("FG.broad","group"),with=F]))
FG.s<-fread("N:/Exploratories/Data/FORESTS/TRAITS/FG_simpl.csv")
setkey(FG.s,FG.broad,group)
setkey(padat2,FG.broad,group)
padat2<-merge(padat2,FG.s)

apply(padat2,2,function(x)sum(is.na(x)))


bryo2<-fread("N:/Exploratories/Data/FORESTS/Traits/bryo_layer.txt")
#update trait table
setkey(bryo2,species)
setkey(padat2,species)
padat2[bryo2, `:=` (FG.simpl=i.FG.simpl)]
rm(bryo2)

##lichens
#lich<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/4460_lichMelt.txt")
#lich<-unique(lich[,4:5,with=F])

#simplify the bryicolous information (ME,MG,MT into M)
#lich[Substrate %in% c("ME","MG","MT"), Substrate := "M"]
#lich<-unique(droplevels(lich))

#aggregate layer info per species
#lich2<-data.table(aggregate(Substrate~Sp_name,paste,collapse="_",data=lich))

#save table
#write.table(lich2,"N:/Exploratories/Data/FORESTS/Traits/lich_layer.txt",row.names=F)
#modfied the txt to add a FG.simpl with 3 categories wood/soil/mixed (lichens on mosses are included in "wood")
lich2<-fread("N:/Exploratories/Data/FORESTS/Traits/lich_layer.txt")
#update trait table
setkey(lich2,species)
setkey(padat2,species)
padat2[lich2, `:=` (FG.simpl=i.FG.simpl)]


########################read again################# HEEEEEEEEEEEEEERRRRREEEEEEEEEEEEEE
padat2<-fread("N:/Exploratories/Data/FORESTS/170313_forestSP_info.txt")
setkey(padat2,species)

temp<-padat2[is.na(padat2$Clade)] #fungi soil and mycorrhiza is ok to have NA's
unique(temp$group)

temp<-padat2[is.na(padat2$Trophic.level)] #fungi soil is ok to have NA's
unique(temp$group)



write.table(padat2,"N:/Exploratories/Data/FORESTS/170313_forestSP_info.txt",row.names=F)

##to do:
#add liverworth info in taxa column (same colum as rosids.. etc)
#########add insect layer/mosses/lichens









#get genus information from species column
padat2[,genus := unlist(strsplit(species, "\\."))[1],by=species]
#then change it for OTUs





grep('_', "species.b")
unlist(strsplit("species.b", "\\."))[1]
