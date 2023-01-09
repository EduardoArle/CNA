#load necessary packages
library(raster); library(rgdal); library(rgeos); library(rworldmap)
#set wds
wd_variables <- '/Users/carloseduardoaribeiro/Documents/Post-doc/Teaching/Teaching/wc10'
wd_map_stuff <- '/Users/carloseduardoaribeiro/Documents/Soup/Map stuff'
wd_range_map <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Mammals/Range_maps'
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
checklist2 <- gSimplify(checklist, tol = 0.2, topologyPreserve = T)  #simplify the shp


#load a good range map to show the overlap


#get centroid of each region in the shapefile
lon <- numeric()
lat <- numeric()
for(i in 1:nrow(checklist))
{
  centroid <- gCentroid(checklist[i,])
  lon[i] <-  coordinates(centroid)[,1]
  lat[i] <-  coordinates(centroid)[,2]
}

#select only regions in the area of the species range
cl_regional <- checklist[which(checklist$lon > (xmin(alien_map2) - 5) &
                               checklist$lon < (xmax(alien_map2) + 5) &
                               checklist$lat > (ymin(alien_map2) - 5) &
                               checklist$lat < (ymax(alien_map2) + 5)),]

cl_regional2 <- gBuffer(cl_regional, width = 0.2, byid = T)
cl_regional3 <- gSimplify(cl_regional2, topologyPreserve = T, tol = 0.1)
cl_regional3 <- SpatialPolygonsDataFrame(cl_regional3, cl_regional2@data)


#create bg points in the alien range of the sps and identify overlapping regs
pts_regs <- spsample(alien_map, n=1000, type="stratified", cellsize = 0.25)
sel_regs <- over(pts_regs, cl_regional)
sel_regs2 <- cl_regional3[which(cl_regional3$BENTITY2_N %in% sel_regs$BENTITY2_N),]

plot(cl_regional3)
plot(sel_regs2, add=T, col='red')

plot(alien_map,add=T,col='red')

alien_map2

plot(bg_alien ,add = T, col = 'red')

cl_regional 



head(checklist@data)

plot(checklist)
