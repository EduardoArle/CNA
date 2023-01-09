###### AMPHIBIANS ######


#list working directories
wd_data <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles"
wd_prepared_data <- "C:/Users/ca13kute/Documents/3rd_Chapter/Amphibians"
  
#read checklist
setwd(wd_data)
table <- read.csv("Checklist_amphibians_reptiles.csv")

#select rows with amphibians
table_amph <- table[which(table$Group == "Amphibia"),]

#make an amphibia species list
sps_list <- unique(table_amph$Species)

#save prepared data
setwd(wd_prepared_data)

write.csv(table_amph,"Amphibia_checklist.csv",row.names = F)
write.csv(sps_list,"Amphibia_species_list.csv",row.names = F)


###### MAMMALS ######


#list working directories
wd_data <- "/Users/carloseduardoaribeiro/Documents/CNA/Data/Mammals"
wd_prepared_data <- "/Users/carloseduardoaribeiro/Documents/CNA/Data/Mammals"

#read checklist
setwd(wd_data)
table <- read.csv("Mammal_checklist.csv")

#make an mammal species list
sps_list <- unique(table$Species)

#save prepared data
setwd(wd_prepared_data)

write.csv(table,"Amphibia_checklist.csv",row.names = F)
write.csv(sps_list,"Amphibia_species_list.csv",row.names = F)



