library(plyr)

### AMPHIBIANS

wd_list <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Amphibians/'
wd_occurrences <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Amphibians/Occurrences/'
wd_range <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Amphibians/Range_maps'

#load species list
setwd(wd_list)
checklist <- read.csv("Amphibia_checklist.csv")
sps_list_all <- unique(as.character(checklist$Species))

#check which species have been introduced to at least 10 regions
sps_list2 <- ddply(checklist, .(Species), nrow)
sel_sps <- sps_list2$Species[which(sps_list2$V1 >= 10)]
checklist2 <- checklist[which(checklist$Species %in% sel_sps),]

#check which species have occurrences
setwd(wd_occurrences)
occ <- list.files()

missing_occ <- sel_sps[-which(sel_sps %in% occ)] #make list of sps that do not have 
                                           #occ

missing_occ #all sps have occ!

#check which species have range maps
setwd(wd_range)
range <- gsub(".shp","",list.files(pattern = ".shp$"))

missing_range <- sel_sps[-which(sel_sps %in% range)] #make list of sps that do not 
                                                #have range maps

missing_range #all sps have range maps!

#save lists
setwd(wd_list)
saveRDS(sel_sps, 'Selected_species')


### MAMMALS

wd_list <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Mammals'
wd_occurrences <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Mammals/Occurrences'
wd_range <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Mammals/Range_maps'

#load species list
setwd(wd_list)
checklist <- read.csv("Mammal_checklist.csv")
sps_list_all <- as.character(checklist$Species)

#check which species have been introduced to at least 10 regions
sps_list2 <- ddply(checklist, .(Species), nrow)
sel_sps <- sps_list2$Species[which(sps_list2$V1 >= 10)]

#check which species have occurrences
setwd(wd_occurrences)
occ <- list.files()

missing_occ <- sel_sps[-which(sel_sps %in% occ)] #make list of sps that do not have 
#occ

missing_occ

sel_sps2 <- sel_sps[-which(sel_sps %in% missing_occ)] #eliminate sps without occ from the sps list
checklist2 <- checklist[which(checklist$Species %in% sel_sps2),]

#check which species have range maps
setwd(wd_range)
range <- gsub(".shp","",list.files(pattern = ".shp$"))

missing_range <- sel_sps2[-which(sel_sps2 %in% range)] #make list of sps that do not 
#have range maps

missing_range 

sel_sps3 <- sel_sps2[-which(sel_sps2 %in% missing_range)] #eliminate sps without occ from the sps list
checklist2 <- checklist[which(checklist$Species %in% sel_sps3),]

#save lists
setwd(wd_list)
saveRDS(sel_sps3, 'Selected_species')





### BIRDS

wd_list <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Birds'
wd_occurrences <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Birds/Occurrences'
wd_range <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Birds/Range_maps'


#load species list
setwd(wd_list)
checklist <- read.csv("Birds_checklist.csv")
sps_list_all <- as.character(checklist$gbifDarwinCore)

#check which species have been introduced to at least 10 regions
sps_list2 <- ddply(checklist, .(gbifDarwinCore), nrow)
sel_sps <- sps_list2$gbifDarwinCore[which(sps_list2$V1 >= 10)]

#check which species have occurrences
setwd(wd_occurrences)
occ <- list.files()

missing_occ <- sel_sps[-which(sel_sps %in% occ)] #make list of sps that do not have 
#occ

missing_occ #all sps have occ!

#check which species have range maps
setwd(wd_range)
range <- gsub(".shp","",list.files(pattern = ".shp$"))

missing_range <- sel_sps[-which(sel_sps %in% range)] #make list of sps that do not 
#have range maps

missing_range 

sel_sps2 <- sel_sps[-which(sel_sps %in% missing_range)] #eliminate sps without occ from the sps list
checklist2 <- checklist[which(checklist$gbifDarwinCore %in% sel_sps2),]

#save lists
setwd(wd_list)
saveRDS(sel_sps2, 'Selected_species')
