#load necessary packages
library(raster); library(rgdal); library(rgeos); library(rworldmap)
#set wds
wd_variables <- '/Users/carloseduardoaribeiro/Documents/Post-doc/Teaching/Teaching/wc10'
wd_map_stuff <- '/Users/carloseduardoaribeiro/Documents/Soup/Map stuff'
wd_range_map <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Mammals/Range_maps'
wd_alien_range <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Mammals/Alien_ranges/DataS1'
wd_standardised_var <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Variables/Standardised_variables'
wd_mammal_cl <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Mammals/Regions_shapefile'

#### PANEL A ####

#load one environmental variable
setwd(wd_variables)
var_1 <- raster('bio1.bil')
var_2 <- raster('bio12.bil')
var_3 <- raster('bio19.bil')

#load map stuff
setwd(wd_map_stuff)
world <- readRDS("wrld.rds")
worldmapframe <- readRDS("Worldmapframe.rds")

#reproject maps to Eckert
worldmapframe <- spTransform(worldmapframe, CRS(proj4string(world)))
var_1 <- projectRaster(var_1, crs = proj4string(world))
var_2 <- projectRaster(var_2, crs = proj4string(world))
var_3 <- projectRaster(var_3, crs = proj4string(world))

#make a buffer of the frame to solve the weird things out it
frame2 <- gBuffer(worldmapframe, width = 6000000)
frame3 <- gDifference(frame2, worldmapframe)

#plot variable map
par(mar = c(2,2,2,2), bg = 'blue')

plot(worldmapframe, lwd = 4, col = 'white')
plot(var_1, legend = F, bty = "n", box = F, axes = F, add = T,
     col = grey.colors(10), colNA = 'white')
plot(frame3, col = "blue", add = T, border = NA)
plot(worldmapframe, add = T, lwd = 4) #save width 1000

plot(worldmapframe, lwd = 4, col = 'white')
plot(var_2, legend = F, bty = "n", box = F, axes = F, add = T,
     col = grey.colors(10), colNA = 'white')
plot(frame3, col = "blue", add = T, border = NA)
plot(worldmapframe, add = T, lwd = 4) #save width 1000

plot(worldmapframe, lwd = 4, col = 'white')
plot(var_3, legend = F, bty = "n", box = F, axes = F, add = T,
     col = grey.colors(10), colNA = 'white')
plot(frame3, col = "blue", add = T, border = NA)
plot(worldmapframe, add = T, lwd = 4) #save width 1000

#load expert range map
range <-readOGR('Phalanger orientalis', dsn = wd_range_map)

#separate regions by biogeographical status
nat_map <- range[which(range$legend == 'Extant (resident)'),]
nat_map2 <- gBuffer(nat_map, width = 0.5)
nat_map2 <- gSimplify(nat_map2, tol = 0.1)
alien_map <- range[which(range$legend == 'Extant & Introduced (resident)'),]
alien_map2 <- gBuffer(alien_map, width = 0.5)
alien_map2 <- gSimplify(alien_map2, tol = 0.1)
unknown_map <- range[which(range$legend == 'Extant & Origin Uncertain (resident)'),]
unknown_map2 <- gBuffer(unknown_map, width = 0.5)
unknown_map2 <- gSimplify(unknown_map2, tol = 0.1)

#plot range map
par(mar = c(2,2,2,2), bg = 'blue')
plot(nat_map2, lwd = 4, col = 'white') #save width 500

#plot gbif occurrences

#load world map
world_map <-  getMap(resolution = 'low')
world_map <-  world_map[-which(world_map$SOVEREIGNT == 'Antarctica'),]
world_map <- gBuffer(world_map, width = -1)

#create random points on land
occ <- spsample(world_map, n = 200, type = "random")
occ <- spTransform(occ, CRS(proj4string(world)))

par(mar = c(2,2,2,2), bg = 'blue')

plot(worldmapframe, lwd = 4, col = 'white')
plot(var_1, legend = F, bty = "n", box = F, axes = F, add = T,
     col = 'grey70', colNA = 'white')
plot(frame3, col = "blue", add = T, border = NA)
plot(worldmapframe, add = T, lwd = 4) #save width 1000

plot(occ, pch = 21, bg = "darkorange", cex = 1, add = T) #save width 1000


#### PANEL B ####

#plot original and standardised variable map
setwd(wd_standardised_var)
var_1_stand <-  raster('bio1.asc')
var_1_stand <- projectRaster(var_1_stand, crs = proj4string(world))

par(mar = c(2,2,2,2), bg = 'blue')

#standardised
plot(worldmapframe, lwd = 4, col = 'white')
plot(var_1_stand, legend = F, bty = "n", box = F, axes = F, add = T,
     col = grey.colors(10), colNA = 'white')
plot(frame3, col = "blue", add = T, border = NA)
plot(worldmapframe, add = T, lwd = 4) 

val_range_stand <- c(round(min(var_1_stand[], na.rm = T), 2),
                     round(max(var_1_stand[], na.rm = T), 2)) #get min and max values

#install function myGradientLegend
myGradientLegend(valRange = val_range_stand ,
                 pos = c(0.25, 0.29, 0.75, .305),
                 color = grey.colors(10),
                 side = 1,
                 dec = 2,
                 n.seg = 0,
                 label.col = 'darkorange3',
                 cex = 3)    #save width 1000


#original
plot(worldmapframe, lwd = 4, col = 'white')
plot(var_1, legend = F, bty = "n", box = F, axes = F, add = T,
     col = grey.colors(10), colNA = 'white')
plot(frame3, col = "blue", add = T, border = NA)
plot(worldmapframe, add = T, lwd = 4)

val_range <- c(round(min(var_1[], na.rm = T), 2),
               round(max(var_1[], na.rm = T), 2)) #get min and max values

myGradientLegend(valRange = val_range,
                 pos = c(0.25, 0.29, 0.75, .305),
                 color = grey.colors(10),
                 side = 1,
                 dec = 0,
                 n.seg = 0,
                 label.col = 'darkorange3',
                 cex = 3)   #save width 1000

#### PANEL C ####

#plot range by biogeo status
par(mar = c(2,2,2,2), bg = 'white')
plot(alien_map2, col = '#e18c00', lwd = 4)
plot(nat_map2, col = '#4a794a', lwd = 4, add = T)
plot(unknown_map2, col = '#b3b3b3', lwd = 4, add = T) #save width 500


#### PANEL D ####

#overlap mammal range maps to checklist
setwd(wd_mammal_cl)
checklist <- readOGR('Bentity2_shapefile_fullres', dsn = wd_mammal_cl,
               use_iconv = TRUE, encoding = 'UTF-8')

#crop the shapefile to show only the region where the range map is
south_america <-  crop(checklist, extent(-85.35,-32,-57,13.76))
brasil <- south_america[c(1,2,3,7,17,27,31,41,42,43,46,53,55,56,57,59,62,
                          63,65,67,68,73,78,79,84,94),]

#simplify the shp
brasil2 <- gSimplify(brasil, tol = 0.2, topologyPreserve = T)  

#load a good range map to show the overlap
alien_ex <- readOGR('Callithrix jacchus', dsn = wd_alien_range)
alien_ex2 <- gSimplify(alien_ex, tol = 0.2, topologyPreserve = T) 

#plot checklist regions with transparent range map on top
par(mar = c(2,2,2,2), bg = 'blue')
plot(brasil2, col = 'white', lwd = 4)
plot(alien_ex2, add = T, col = '#e18c0095', border = '#e18c00',  lwd = 4) #save width 500

#create bg points in the alien range of the sps and identify overlapping regs
pts_regs <- spsample(alien_ex2, n=1000, type="stratified", cellsize = 0.25)
sel_regs <- over(pts_regs, brasil)
sel_regs2 <- brasil[which(brasil$BENTITY2_N %in% sel_regs$BENTITY2_N),]

#plot select shp regions
sel_regs3 <- gSimplify(sel_regs2, tol = 0.2, topologyPreserve = T) 
plot(brasil2, col = 'blue', border = NA)
plot(sel_regs3, col = 'white', lwd = 4, add = T) #save width 500


head(checklist@data)

plot(checklist)



############### scrap ##################


for(i in 1:length(alien_mamm))
{
  setwd('/Users/carloseduardoaribeiro/Documents/CNA/Figure/Methods figure/Visualisation_overlap')
  
  alien_ex <- readOGR(alien_mamm[i], dsn = wd_alien_range)
  
  
  jpeg(file=alien_mamm[i],width = 1000, height = 1000)
  
  
  plot(checklist3)
  plot(alien_ex, add = T, col = 'red')
  
  dev.off()
  print(i)
}

