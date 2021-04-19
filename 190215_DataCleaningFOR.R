### Biodiversity exploratories - Synthesis dataset FORESTS ###
### Author: Caterina Penone

####This script registers all the operations that were made on the downloaded datasets###

## The dataset is then aggregated within the script 190215_DataAssemblingRawAbund.R
## and prepared for Bexis upload in the script 190222_Upload_Bexis_FOR.R

## Warning: This was my first script doing this kind of dataset operations so it's far 
# from being perfect (and is a bit too "home-made"). I could re-write it but given the 
# limited time I prefer to focus on updating the dataset with better scripts!


#add?
#bark beetles:
#20034: Bark Beetle Antagonists sampled with PheromoneTraps in Forest EPs in 2010
#20031: Bark Beetles sampled with Pheromone Traps in Forest EPs in 2010
#bacteria RNA from sikorski
# AEW-DatasetId=22449.htm - downloaded
# HEW-DatasetId=22451.htm - downloaded
# SEW-DatasetId=22450.htm - downloaded
# 21687: earthworm community and 21686 earthworm biomass -downloaded in NewDataToAdd --> comvert from density to abund? see myriapods in grl
#change soil fungi when ready
#how to deal with same sp of deadwood and soil fungi?

# 21906: Pifall traps on forest EPs in 2008 subset Formicidae Species Abundances(https://www.bexis.uni-jena.de/Data/ShowXml.aspx?DatasetId=21906)



require(data.table)
require(reshape2)
source("R/SCRIPTS UTILES/BE_plots_zero.R")

###########bryophytes##########
bryo<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/CrossedTables/4141_bryophytes.txt")
bryoMelt<-melt(bryo,"Plotid",variable.name="Sp_name",value.name="Abund")
write.table(bryoMelt,"N:/Exploratories/Data/FORESTS/TEXTfiles/4141_bryophytesMelt.txt",row.names=F)
rm(bryo)
rm(bryoMelt)
bryo<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/4141_bryophytes_layer.txt")
bryo$data_id<-4141
#add year data
brynfo<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/4141.txt")
setkey(bryo,plotID)
setkey(brynfo,Plotid)
bryo<-merge(bryo,setnames(brynfo[,c(1,5),with=F],c("plotID","year")))
write.table(bryo,"N:/Exploratories/Data/FORESTS/TEXTfiles/4141_bryophytes_layer.txt",row.names=F)

#after removing layer info, aggregate abundances by site and sp
bryo<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/4141_bryophytes_layer.txt")
bragg<-bryo[,abundAll := sum(abund),by=c("plotID","sp_name","year")]
bragg[ ,`:=`(Sp_name_layer = NULL, layer = NULL, abund=NULL)]
bragg<-unique(bragg,by=key(bragg))
##now just keep EP sites and put the EP plot names
plotinfo<-fread("N:/Exploratories/Data/GeneralData/10580.txt")
setkey(plotinfo,PlotID)
setkey(bragg,plotID)
bragg2<-merge(bragg,setnames(plotinfo[,c(1,4),with=F],c("plotID","EP_PlotID")))
#bragg2<-bragg2[plotType=="EP"] #some plots are both GP and EP
setnames(bragg2,"abundAll","cover")
bragg2$plotID<-NULL
write.table(bragg2,"N:/Exploratories/Data/FORESTS/TEXTfiles/TO_MERGE/4141_bryophytes.AB.txt",row.names=F)


##############lichens#############
#remove species with unsure identification
sp_to_rem<-c("Cf._absconditella_delutula","Cf._bacidia","Cf._bacidia_spec.","Cf._bacidina","Cf._biatora_spec.",
"Cf._leproloma_voauxii","Cf._leproloma_vouauxii","Cf._leptorhaphis_maggiana","Cf._micarea_viridileprosa",
"Cf._mycobilimbia_epixanthoides","Cf._ropalospora_viridis","Cf._thelidium_spec.","Cf._verrucaria_spec.",
"Cf_eurhynchium","Cf_platygyrium_repens","Cf_pohlia","Cf_ptychodium")

lich<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/CrossedTables/4460_lichen.txt")
lich$Sp_name_substrate<-paste(lich$Sp_name,lich$Substrate,lich$Substrate_sp,sep="_")
divtab<-lich[,4:ncol(lich),with=F]
lichMelt<-melt(divtab,"Sp_name_substrate",variable.name="PlotID",value.name="Abund")
setkey(lichMelt,Sp_name_substrate)
spinfo<-lich[,c(1:3,ncol(lich)),with=F]
setkey(spinfo,Sp_name_substrate)
lichmerge<-merge(lichMelt,spinfo)
lichmerge<-lichmerge[!Sp_name %in% sp_to_rem]

write.table(lichmerge,"N:/Exploratories/Data/FORESTS/TEXTfiles/4460_lichMelt.txt",row.names=F)
##add year, dataID and aggregate all substrates, keep only EP's
lich<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/4460_lichMelt.txt")
lich$dataID<-4460
lich[ ,`:=`(Sp_name_substrate = NULL, Substrate = NULL, Substrate_sp=NULL)]
#add year
lichinfo<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/4460.txt")
setkey(lich,PlotID)
setkey(lichinfo,Plot_ID)
lich<-merge(lich,setnames(lichinfo[,c(1,4),with=F],c("PlotID","year")))
#Aggregate
lichagg<-lich[,abundAll := sum(Abund),by=c("PlotID","Sp_name","year")]
setkey(lichagg,PlotID,Sp_name,year)
lichagg<-unique(lichagg,by=key(lichagg))
##now just keep EP sites and put the EP plot names
plotinfo<-fread("N:/Exploratories/Data/GeneralData/10580.txt")
setkey(plotinfo,PlotID)
setkey(lichagg,PlotID)
lichagg2<-merge(lichagg,plotinfo[,c(1,4),with=F])
#bragg2<-bragg2[plotType=="EP"] #some plots are both GP and EP
lichagg2[ ,`:=`(PlotID = NULL, Abund=NULL)]
setnames(lichagg2,c("abundAll","Sp_name","EP_PlotID"),c("cover","species","plotID"))
write.table(lichagg2,"N:/Exploratories/Data/FORESTS/TEXTfiles/TO_MERGE/4460_lichen.AB.txt",row.names=F)


#############ARTHROPODS#####################
##combine all arthropods --> finally I used the table provided by Martin
#dirpath<-"N:/Exploratories/Data/FORESTS/TEXTfiles/arthropods"
#all.files<-list.files(dirpath)
#mylist <- lapply(all.files, function(i) read.table(paste(dirpath,"/",i,sep=""),h=T) )
#mydf <- do.call('rbind', mylist)
#write.table(mydf,"N:/Exploratories/Data/FORESTS/TEXTfiles/all_arthropods.txt",row.names=F)
##aggregate by plot all trapping techniques
arthro<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/arthropods/all_arthropodsM.txt")
#remove pitfall HEW34
arthro<-arthro[!(Traptype=="BF" & PlotID=="HEW34")]
#remove diptera
arthro<-arthro[!Order=="Diptera"]

#--------------
##calculate relabundances per plot per trap! is that correct? #how many sp overlap btw the different traps?
#t1<-unique(arthro[Traptype=="BF"]$SpeciesID)
#t2<-unique(arthro[Traptype=="FFB"]$SpeciesID)
#t3<-unique(arthro[Traptype=="FFK"]$SpeciesID)
#length(Reduce(intersect, list(t1,t2,t3))) #537
#length(unique(arthro$SpeciesID)) #2541
#sp that are trapped in different layers will appear more abundant than the ones captured in one layer only..
#---------------

artagg<-arthro[,abund := sum(Ind),by=c("PlotID","SpeciesID")] #abundance per sp per plot
artagg[ ,`:=`(Traptype = NULL, Ind = NULL, VIP_EP = NULL,Plot = NULL)]
artagg<-unique(artagg)
setnames(artagg,c("PlotID","SpeciesID"),c("plotID","species"))
artagg$year<-2008
artagg$dataID<-"arth"
write.table(artagg,"N:/Exploratories/Data/FORESTS/TEXTfiles/all_arthropodsLYR.txt",row.names=F)
artagg[ ,`:=`(Order = NULL, Suborder = NULL,Family = NULL)]
write.table(artagg,"N:/Exploratories/Data/FORESTS/TEXTfiles/TO_MERGE/all_arthropods.AB.txt",row.names=F)

#create a "trait" table with information on layer where they were captured
arthro<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/arthropods/all_arthropodsM.txt")
#remove pitfall HEW34
arthro<-arthro[!(Traptype=="BF" & PlotID=="HEW34")]
arthro<-unique(arthro[,c("SpeciesID","Traptype"),with=F],,by=key(arthro))
padat$Traptype<-gsub("_",".",padat$species)
arthro[which(Traptype=="BF"), Traptype := "low"]
arthro[which(Traptype=="FFB"), Traptype := "mid"]
arthro[which(Traptype=="FFK"), Traptype := "high"]
setkey(arthro,Traptype)
arthro2<-data.table(aggregate(Traptype~SpeciesID,paste,collapse="_",data=arthro))
write.table(arthro2,"N:/Exploratories/Data/FORESTS/Traits/arthro_layer.txt",row.names=F)


#########klebsorbidium###########
klebs<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/18626_Klebs.txt")
names(klebs)
#how many in forests?
plotinfo<-fread("N:/Exploratories/Data/GeneralData/10580.txt")
setkey(plotinfo,EP_PlotID)
setkey(klebs,PlotID)
klebs<-merge(klebs,setnames(plotinfo[,3:4,with=F],c("type","PlotID")))
#klebs<-klebs[Klebsormidium1yn=="y"]
#klclade<-unique(klebs$Clade1)
#klebs[, ID := .GRP, by = Sequence1]
#klebs[, ID := paste("Seq_",ID,sep="")]
##should we keep the species or use the clade too?
klebs[ ,`:=`(Sequence1 = NULL, Klebsormidium1yn = NULL)]
klebs<-klebs[type=="Wald"]
setkey(klebs,NULL)
klebs<-unique(klebs,by=key(klebs))#94 observations
klebs<-na.omit(klebs) ###there are data for 58 forest plots only 
#(did not successfully culture Klebsormidium from every plot)
#abundances based on clades
klebs<-klebs[,abund := .N, by = list(PlotID,Clade1)]
klebs[ ,`:=`(NameIsolate1 = NULL, GeneBankAccessionNumber1 = NULL)]
klebs<-unique(klebs,by=key(klebs))
setnames(klebs,"Clade1","clade")
klebs$dataID<-18626
klebs$year<-2011
write.table(klebs,"N:/Exploratories/Data/FORESTS/TEXTfiles/TO_MERGE/klebs.txt",row.names=F)
#not enough plots.. removed :(

#########small mamm############
smAB1<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/NotUsed/3901_SmMammTrap2008.txt") #abundance data depends on surevey week, removed
smAB2<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/NotUsed/5840_SmMammTrap2009.txt") #abundance data depends on surevey week, removed
#see DataAssemblyINFO.docx to learn more about these two datasets
sm1<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/micromamm/10300_SmMammals2008.txt") 
sm2<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/micromamm/10301_SmMammals2009.txt")

#select only forest plots and check if richness vary along the season
smAB1<-unique(smAB1[,c("Plot","Habitat","KW"),with=F],by=key(smAB1))
setkey(smAB1,Plot)
smAB1<-unique(smAB1,by=key(smAB1))
setkey(sm1,Plot)
sm1b<-merge(sm1,smAB1)
sm1b<-sm1b[Habitat=="forest"]
plot(rowSums(sm1b[,3:13,with=F])~factor(sm1b$KW)) #ok no impact of week of survey on species richness
sm1b$year<-2008
sm1b$dataID<-10300
sm1b[ ,`:=`(KW = NULL, Habitat = NULL, Exploratory=NULL)]
melt1<-melt(sm1b,id.vars=c("Plot","dataID","year"),measure.vars=2:12,variable.name="species",value.name="PrAb")

#select only forest plots and check if richness vary along the season
smAB2<-unique(smAB2[,c("Plot","Habitat","KW"),with=F],,by=key(smAB2))
setkey(smAB2,Plot)
smAB2<-unique(smAB2,by=key(smAB2))
setkey(sm2,Plot)
sm2b<-merge(sm2,smAB2)
sm2b<-sm2b[Habitat=="forest"]
plot(rowSums(sm2b[,3:12,with=F])~factor(sm2b$KW)) #ok no impact of week of survey on species richness
sm2b$year<-2009
sm2b$dataID<-10301
sm2b[ ,`:=`(KW = NULL, Habitat = NULL, Exploratory=NULL)]
melt2<-melt(sm2b,id.vars=c("Plot","dataID","year"),measure.vars=2:11,variable.name="species",value.name="PrAb")

#melt
meltmam<-rbind(melt1,melt2)

write.table(meltmam,"N:/Exploratories/Data/FORESTS/TEXTfiles/all_micromamm.PA.txt",row.names=F)
write.table(meltmam,"N:/Exploratories/Data/FORESTS/TEXTfiles/TO_MERGE/all_micromamm.PA.txt",row.names=F)

########bats###########
#assemble the two years and inform about the date
b1<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/bats/13146_Bats2009.txt")
b2<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/bats/13526_Bats2010.txt")
b1$year<-2009
b2$year<-2010
b1$dataID<-13146
b2$dataID<-13526
b3<-rbind(b1,b2)
plotinfo<-fread("N:/Exploratories/Data/GeneralData/11603.txt")
setkey(plotinfo,id)
setkey(b3,Plotid)
b3<-merge(b3,setnames(plotinfo[,1:2,with=F],c("Plotid","type")))
b3<-b3[type=="Forest"]
b3[,type:=NULL]
b3[,Plotid:=NULL]
b3[,activity_total:=NULL]
#melt data
allMelt<-melt(b3,id.vars=c("year","dataID","EP_Plotid"),measure.vars=2:12,variable.name="species",value.name="abund")
rm(plotinfo)
write.table(allMelt,"N:/Exploratories/Data/FORESTS/TEXTfiles/all_bats.AB.txt",row.names=F)

#######################birds##########################
#assemble the two years and inform about the date
bi1<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/birds/4901.txt") #aggregated measures, not included
bi2<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/birds/6100.txt") #species names in German
bi3<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/birds/11422.txt") ##species names in German
bi4<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/birds/15187.txt")
bi5<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/birds/15188.txt")
bi6<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/birds/15189.txt")
bi7<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/birds/15190.txt")
#bi8<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/birds/15191.txt") #this dataset contained errors!
bi8<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/birds/24690.txt")

bi4[,Date:=NULL]
bi5[,Date:=NULL]
bi6[,Date:=NULL]
bi7[,Date:=NULL]
bi8[,Date:=NULL]
bi7$Dendrocopos_syriacus<-0

#arrange 2012 data (b8) like others
bi8$GermanName<-bi8$Explo<-NULL
bi8<-unique(bi8) #remove two species that are duplicated
bi8<-bi8[!is.na(SpeciesCode)] #remove species =NA
bi8<-dcast.data.table(bi8, Plot_ID+Round~SpeciesCode)
bi8<-data.table(BEaddHabitat(bi8,column="Plot_ID",habitatname = "type"))
setnames(bi8,c("Plot_ID","Round"),c("EP_Pl0tid","round"))
bi8$year<-2012

#merge datasets
all<-rbind(bi4,bi5,bi6,bi7,bi8,use.names=T,fill=T)
#remove unwanted columns
all[ ,`:=`(sequence = NULL, round = NULL, month=NULL,day=NULL,expl=NULL)]

#select only forests
setkey(all,type)
all<-all[c("fors","Wald","forest")]
all[,type:=NULL]

#replace NA's with zeros
sp<-all[,3:ncol(all),with=F]
sp[is.na(sp)]<-0
all[,3:ncol(all)]<-sp

#first aggregate by summing all round within a year
allAgg<-all[,lapply(.SD,sum),by=c("year","EP_Pl0tid")]
#melt the dataset
allMelt<-melt(allAgg,id.vars=c("year","EP_Pl0tid"),measure.vars=3:103,variable.name="species",value.name="abund")
dataID<-c(15187:15190,24690)
allMelt$dataID<-allMelt$year
allMelt$dataID <- factor(allMelt$dataID, labels=dataID)
#remove duplicates!
allMelt<-unique(allMelt,by=key(allMelt))
# or melt the dataset before
#allMelt<-melt(all,id.vars=c("year","EP_Pl0tid"),measure.vars=8:108,variable.name="species",value.name="abund")
# and aggregate later
#allMeltA<-allMelt[, sum(Abund), by=c("year","EP_Pl0tid","variable")] 
#change column names
setnames(allMelt,"EP_Pl0tid","plotID")

write.table(allMelt,"N:/Exploratories/Data/FORESTS/TEXTfiles/TO_MERGE/all_birds.AB.txt",row.names=F)


##########COMPARE FUNGI DATASETS############
##18547_fungdeadwBern Bern #559 sp
##17186_fungdeadw Freibourg #211 sp
##soil fungi #855 sp
fungBern<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/18547_fungdeadwBern.txt")
fungFr<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/17186_fungdeadw.txt")
soilf<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/19026_soilfung.txt")
spDNA<-unique(soilf$species)
spBern<-unique(fungBern$Sp_name)
spFrei<-unique(fungFr$species)
#diff1<-sort(setdiff(spBern,spFrei))
#diff2<-sort(setdiff(spFrei,spBern))
commonBerFri<-sort(intersect(spBern,spFrei))#75 common species

#overlap with fungi on  deadwood
commonbern<-sort(intersect(spBern,DNAfung))#93 common sp
commonfrei<-sort(intersect(spFrei17,DNAfung))#33 common sp

allsp<-unique(c(spDNA,spBern,spFrei)) #1453 unique sp (1625 fungi entries and 172 overlapping between the 3 datasets)

###and the mychorrizae dataset also have overlaps!!
mic2<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/19186_mychorriz.txt")
soilf<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/19026_soilfung.txt")
mic2$order<-gsub("o__","",mic2$order)
mic2$genus<-gsub("g__","",mic2$genus)
mic2$species<-gsub("s__","",mic2$species)
mic2$species<-gsub(" ","_",mic2$species)
gmic<-unique(mic2$genus) #270
gfun<-unique(soilf$genus) #433
smic<-unique(mic2$species) #857
sfun<-unique(soilf$species) #855
temp<-intersect(gmic,gfun) #180
temp<-intersect(smic,sfun) #184


###########18547_fungdeadw Bern #559 sp#############
fung<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/CrossedTables/18547_fungdeadw.txt")
fungMelt<-melt(fung,"Sp_name",variable.name="PlotID",value.name="PrAb")
write.table(fungMelt,"N:/Exploratories/Data/FORESTS/TEXTfiles/18547_fungdeadwMelt.txt",row.names=F)
rm(fung)
rm(fungMelt)
####in excel I changed the site names from HEG to HEW and from SEG to SEW (asked stefan blaser)
###add year and ID and select only forest data
fung<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/18547_fungdeadwBern.txt")
fung$dataID<-18547
fung$year<-2010
#keep only EP sites
#plotinfo<-fread("N:/Exploratories/Data/GeneralData/10580.txt")
#setkey(plotinfo,EP_PlotID)
#setkey(fung,PlotID)
#fung<-merge(fung,setnames(plotinfo[,3:4,with=F],c("type","PlotID"))) ####only 50 plots in grasslands..
#fung<-fung[type=="Wald"]
write.table(fung,"N:/Exploratories/Data/FORESTS/TEXTfiles/TO_MERGE/18547_fungdeadwBern.PA.txt",row.names=F)

###########17186_fungdeadw Freib #211 sp#############
fungFr<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/17186_fungdeadw.txt")
fungFr<-fungFr[,.(plot_ID,species,coverage)]
fungFr<-na.omit(fungFr)
#cast and melt again to have all species in all sites ("absences")
fungFrc<-dcast.data.table(fungFr,plot_ID~species, value.var="coverage")
fungFrm<-melt(fungFrc,id="plot_ID",variable.name="Sp_name",value.name="abund",na.rm=F)
fungFrm$year<-2011
fungFrm$dataID<-17186
write.table(fungFrm,"N:/Exploratories/Data/FORESTS/TEXTfiles/TO_MERGE/17186_fungdeadw.AB.txt",row.names=F)



##########PROTOZOA#########
pro<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/14426_protozoa.txt")
#removed, only 52 plots with data

##########MICHORIZAE 19168#######
#mic<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/19168_mychorriz.txt")
#mic<-mic[,.(OTU_ID,EP_Plot,Abundance_OTU,order)] #150 plots, 4544 OTUs
#mic$year<-2011
#mic$dataID<-19168
#mic$order<-gsub("o__","",mic$order)
#mic$order<-gsub(" ","_",mic$order)
#if data are used at the order level, remove order=="unidentified" and "Incertae sedis"
#write.table(mic,"N:/Exploratories/Data/FORESTS/TEXTfiles/TO_MERGE/19168_mychorriz.AB.txt",row.names=F)
##I would rather use the following dataset that has 111 plots but is described as quality control="valildated"

##########MICHORIZAE 19186_mychorriz#######
#mic2<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/19186_mychorriz.txt")
#mic2<-mic2[,.(OTU_ID,EP_Plot,Abundance_OTU,order)] #111 plots, 4359 OTUs
#mic2$year<-2011
#mic2$dataID<-19186
#mic2$order<-gsub("o__","",mic2$order)
#mic2$order<-gsub(" ","_",mic2$order)
#set(mic2,j="OTU_ID",value=paste("micz_",mic2$OTU_ID,sep=""))#add "micrz_" before otu number 
#if data are used at the order level, remove order=="unidentified" and "Incertae sedis"
#write.table(mic2,"N:/Exploratories/Data/FORESTS/TEXTfiles/TO_MERGE/19186_mychorriz.AB.txt",row.names=F)




########prepare soil fungi 21047 dataset############
soilf<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/21047_soilfung.txt")
setnames(soilf,"function","Guild")
soilf[Guild=="", Guild:="Unknown"]
soilf<-soilf[Guild!="Unknown"]
setnames(soilf,"class","taxclass")
soilf$Acc_No<-NULL; soilf$rep_Seq<-NULL
setnames(soilf,"Plotid","plotID")
#simplify categories
soilf[Guild %in% c("AM","EcM","Lichenized","ericoidMyco"),ecology:="Symbiotroph"]
soilf[Guild %in% c("Animal parasite","Mycoparasite","Parasite","Pathogen","Plant pathogen"),ecology:="Pathotroph"]
soilf[Guild %in% c("Saprotroph"),ecology:="Saprotroph"]
#put genus name in species name when species==unclassified
soilf[species=="unclassified",species:=paste("unclassified",genus,sep="_")]

#add useful EP name
plotname<-fread("N:/Exploratories/Data/GeneralData/PlotIDzeros.csv")
setnames(plotname,names(plotname),c("plotID","Plot","Explo"))
setkey(plotname,plotID)
plotname$Explo<-NULL
setkey(soilf,plotID)
soilf<-merge(soilf,plotname)
soilf$plotID<-NULL
setnames(soilf,"Plot","plotID")

#add "OTU" before species name (beacuse some species are shared with deadwood fungi)
set(soilf,j="species",value=paste("OTU_",soilf$species,sep=""))

#remove spaces in table
soilf[,(names(soilf)) := lapply(.SD, gsub, pat="[: ]", rep="_")]
soilf[,(names(soilf)) := lapply(.SD, gsub, pat="-", rep="_")]

#remove mychorrizae from soilfungi dataset (we want to be sure that they interact - see email 
#with Rodica Pena on 08.03.2017)
#soilf<-soilf[ecology %in% c("Pathotroph","Saprotroph")]
#this does not change the results

#add year and data ID
soilf$year<-2011
soilf$dataID<-"21047"

#table of "ecology"
ecol<-soilf[,.(OTU,ecology,Guild,pyhlum,taxclass,order,family,genus,species,year,dataID)]
ecol<-unique(ecol,by="species")

write.table(ecol,"N:/Exploratories/Data/FORESTS/Traits/soil_fungi.txt",row.names=F)

#keep only trophic mode
#mic$Guild<-NULL

#aggregate per species and remove duplicates
soilf$Data <- as.numeric(soilf$Data)
soilf[,abund:=sum(Data),by=c("species","plotID")]
soilf<-unique(soilf,by=c("species","plotID"))

#prepare table for big merge
soilf<-soilf[,.(species,plotID,abund,year,dataID)]

write.table(soilf,"N:/Exploratories/Data/FORESTS/TEXTfiles/TO_MERGE/2104719168_soilfungOTU.AB.txt",row.names=F)


###########soil fungi #211 sp#############just use OTU, see if use species in future...
#soilf<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/21047_soilfung.txt")
#setnames(soilf,"function","ecology")
#soilf[ecology=="", ecology:="Unknown"]

#dim(soilf[pyhlum == "unclassified"]);length(unique(soilf$pyhlum))
#setnames(soilf,"class","taxclass")
#dim(soilf[taxclass == "unclassified"]);length(unique(soilf$taxclass))
#dim(soilf[order == "unclassified"]);length(unique(soilf$order))
#dim(soilf[family == "unclassified"]);length(unique(soilf$family))
#dim(soilf[genus == "unclassified"]);length(unique(soilf$genus))
#dim(soilf[species == "unclassified"]);length(unique(soilf$species))
#here I need to replace species==unclassified by OTU name
#soilf2<-soilf[,.(OTU,Plotid,Data)]
#setkey(soilf2,OTU)

#can 2 OTU belong to 1 species?
#temp<-soilf2[,.(OTU,species)]
#temp<-temp[species!="unclassified"]
#setkey(temp,NULL)
#temp<-unique(temp)
#length(unique(temp$species)) #470
#length(unique(temp$OTU)) #858

#add year
soilf2$year<-2011
soilf2$dataID<-21047

#add useful EP name
#plotname<-fread("N:/Exploratories/Data/GeneralData/PlotIDzeros.csv")
#setnames(plotname,names(plotname),c("Plotid","Plot"))
#setkey(plotname,Plotid)
#setkey(soilf2,Plotid)
#soilf2<-merge(soilf2,plotname)

#soilf2[,OTU:=lapply(OTU, function(x)(paste0("soilf_",x)))] #add "soilf_" before otu number 
#set(soilf2,j="OTU",value=paste("soilf_",soilf2$OTU,sep=""))#add "soilf_" before otu number FASTER!
#soilf2$Plotid<-NULL
#setnames(soilf2,c("OTU","Plot","Data"),c("species","plotID","abund"))
#write.table(soilf2,"N:/Exploratories/Data/FORESTS/TEXTfiles/TO_MERGE/21047_soilfungOTU.AB.txt",row.names=F)



##########BACTERIA#######
#bac<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/19526_bacteria.txt")#,drop=c("Order","Family","Genus","Species"))
#keep only forest data
#plotinfo<-fread("N:/Exploratories/Data/GeneralData/10580.txt")
#setkey(plotinfo,PlotID)
#setkey(bac,plotid)
#bac2<-merge(bac,setnames(plotinfo[,c(4,3),with=F],c("plotid","type")))
#bac2<-bac2[type=="Wald"]
#for (j in names(bac2))
#  set(bac2,which(bac2[[j]]==""),j,NA) #this is super mega fast :))) #############NA or zero???????

#for (j in names(bac2))
#  set(bac2,i=NULL,j=j,value=gsub(" ","_",bac2[[j]],perl=T))

#length(unique(bac2$plotid)) #150
#length(sort(unique(bac2$Class))) #phylum: 50 #order: 291 #family:535 #species: 2681 #####genus: 954 ##class:143

#write.csv(data.frame(unique(bac2$Genus)),"N:/Exploratories/Data/FORESTS/TEXTfiles/Bact_genus.csv",row.names=F)

######see how the grassland data are for those!
#grl.dat<-fread("N:/Exploratories/Data/GRASSLANDS/All species EPG 19.06.15.txt")
#unique(grl.dat$Clade)
#unique(grl.dat[Clade=="Bacteria"]$Species)
#unique(grl.dat[Clade=="Protists"]$Species)
#(unique(bac2$Family))[1:10]


#open the version with relative abundances and OTU checked and cleaned by Kristin Kaiser
bac<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/Bacteria/L7_tax.txt")
#melt table
setkey(bac,OTUcode)
bacmelt<-melt(bac,id.vars=c("Phylum","Class","Order","Family","Genus","Species","OTU","OTUcode"),measure.vars=9:308,variable.name="plotID",value.name="relabund")
#select only forests
plotinfo<-fread("N:/Exploratories/Data/GeneralData/10580.txt")
setkey(plotinfo,PlotID)
setkey(bacmelt,plotID)
bacmelt<-merge(bacmelt,setnames(plotinfo[,3:4,with=F],c("type","plotID")))
bacmelt<-bacmelt[type=="Wald"]
#remove species that were in grasslands but not in forests
su<-function(x)sum(x,na.rm=T)
bacmelt[, FreqF := su(relabund), by=Species]
bacmelt<-bacmelt[FreqF>0]
bacmelt[ ,`:=`(type = NULL,FreqF=NULL)]
#year and dataID
bacmelt$year<-2011
bacmelt$dataID<-19526
#save with tax info
write.table(bacmelt,"N:/Exploratories/Data/FORESTS/TEXTfiles/19526_bacteria.relab.txt",row.names=F)
#remove tax info and save again for big merge
bacmelt[ ,`:=`(Phylum=NULL, Class=NULL, Order=NULL, Family=NULL, Genus=NULL, Species=NULL, OTU=NULL)]
setnames(bacmelt,"OTUcode","species")
write.table(bacmelt,"N:/Exploratories/Data/FORESTS/TEXTfiles/TO_MERGE/19526_bacteria.relab.txt",row.names=F)


##########plants############
#use the multi-years tab now##
plants<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/20366_all_plants09_15.txt")
setnames(plants,names(plants),c("plotID","year","layer","species","abund"))
#keep only understory sp = remove B1 B2 = trees from 5 meters
plants<-plants[!layer%in%c("B1","B2")]
#aggregate understory plants and remove layer column
plantagg<-plants[,abund2 := sum(abund),by=c("plotID","species","year")]
plantagg[,`:=`(layer = NULL,abund=NULL)]
plantagg<-unique(plantagg,by=key(plantagg))
setnames(plantagg,"abund2","cover")
plantagg$dataID<-20366
write.table(plantagg,"N:/Exploratories/Data/FORESTS/TEXTfiles/TO_MERGE/20366_all_plants.AB.txt",row.names=F)

#merge new sp list from 09-15 assembled data to old list (09-10 only) for trait information
#plantsp<-data.table(species=unique(plants$Species))
#plantsp$species<-gsub("_",".",plantsp$species)
#setkey(plantsp,species)

#plantTrait<-fread("N:/Exploratories/Data/FORESTS/Traits/plantFOREST_taxtrait.txt")
#setkey(plantTrait,species)

#difplant<-data.table(species=setdiff(plantsp$species,plantTrait$species))
#setkey(difplant,species)

#plantTrait2<-merge(plantsp,plantTrait,all=T)
#write.csv(plantTrait2,"N:/Exploratories/Data/FORESTS/Traits/plantFOREST_taxtraitNEW.csv")
#write.csv(difplant,"N:/Exploratories/Data/FORESTS/Traits/newPLantForest.csv",row.names = F)


##datasets
lich<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/TO_MERGE/4460_lichen.txt")
bryo<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/TO_MERGE/4141_bryophytes.txt")


###dungwebs #not included
dung<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/19867_dungwebs.txt")
arthro<-fread("N:/Exploratories/Data/FORESTS/TEXTfiles/all_arthropods.txt")
length(unique(arthro$species)) #2541
length(unique(dung$species)) #31
setdiff(dung$species,arthro$species) #11 species not Martin's dataset... better not to include it since the methods are different :(

