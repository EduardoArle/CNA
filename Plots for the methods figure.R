#load necessary packages
library(raster); library(rgdal); library(rgeos); library(rworldmap); library(data.table)

#set wds
wd_variables <- '/Users/carloseduardoaribeiro/Documents/Post-doc/Teaching/Teaching/wc10'
wd_map_stuff <- '/Users/carloseduardoaribeiro/Documents/Soup/Map stuff'
wd_range_map <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Mammals/Range_maps'
wd_alien_range <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Mammals/Alien_ranges/DataS1'
wd_standardised_var <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Variables/Standardised_variables'
wd_mammal_cl <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Mammals/Regions_shapefile'

options(bitmapType='cairo') #otherwise plots are not working...

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

plot(worldmapframe, lwd = 4, col = '#efead7')
plot(var_1, legend = F, bty = "n", box = F, axes = F, add = T,
     col = grey.colors(10), colNA = '#efead7')
plot(frame3, col = "blue", add = T, border = NA)
plot(worldmapframe, add = T, lwd = 4) #save width 1000

plot(worldmapframe, lwd = 4, col = '#efead7')
plot(var_2, legend = F, bty = "n", box = F, axes = F, add = T,
     col = grey.colors(10), colNA = '#efead7')
plot(frame3, col = "blue", add = T, border = NA)
plot(worldmapframe, add = T, lwd = 4) #save width 1000

plot(worldmapframe, lwd = 4, col = '#efead7')
plot(var_3, legend = F, bty = "n", box = F, axes = F, add = T,
     col = grey.colors(10), colNA = '#efead7')
plot(frame3, col = "blue", add = T, border = NA)
plot(worldmapframe, add = T, lwd = 4) #save width 1000

#load expert range map
range_0 <-readOGR('Phalanger orientalis', dsn = wd_range_map)

#select some of the features to make the map look good for the figure
range <- range_0[-c(1,3,4,5,6,7,10,12,14,15,16,18,33,34,37,38),]

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
plot(nat_map2, lwd = 4, col = '#efead7') #save width 500

#plot gbif occurrences

#load world map
world_map <-  getMap(resolution = 'low')
world_map <-  world_map[-which(world_map$SOVEREIGNT == 'Antarctica'),]
world_map <- gBuffer(world_map, width = -1)

#create random points on land
occ <- spsample(world_map, n = 500, type = "random")
occ <- spTransform(occ, CRS(proj4string(world)))

par(mar = c(2,2,2,2), bg = 'blue')

plot(worldmapframe, lwd = 4, col = '#efead7')
plot(var_1, legend = F, bty = "n", box = F, axes = F, add = T,
     col = 'grey70', colNA = '#efead7')
plot(frame3, col = "blue", add = T, border = NA)
plot(worldmapframe, add = T, lwd = 4) 
plot(occ, pch = 21, bg = "gold", cex = 1, add = T) #save width 1000


#### PANEL B ####

#plot original and standardised variable map
setwd(wd_standardised_var)
var_1_stand <-  raster('bio1.asc')
var_1_stand <- projectRaster(var_1_stand, crs = proj4string(world))

par(mar = c(2,2,2,2), bg = 'blue')

#standardised
plot(worldmapframe, lwd = 4, col = '#efead7')
plot(var_1_stand, legend = F, bty = "n", box = F, axes = F, add = T,
     col = grey.colors(10), colNA = '#efead7')
plot(frame3, col = "blue", add = T, border = NA)
plot(worldmapframe, add = T, lwd = 4) 

val_range_stand <- c(round(min(var_1_stand[], na.rm = T), 2),
                     round(max(var_1_stand[], na.rm = T), 2)) #get min and max values

vals <- c(min(val_range_stand), max(val_range_stand))

#install function myGradientLegend
myGradientLegend(valRange = val_range_stand ,
                 pos = c(0.25, 0.29, 0.75, .305),
                 color = grey.colors(10),
                 side = 1,
                 dec = 2,
                 n.seg = 0,
                 values = c(min(vals), max(vals)),
                 label.col = 'darkorange3',
                 font =2,
                 cex = 3)    #save width 1000


#original
plot(worldmapframe, lwd = 4, col = '#efead7')
plot(var_1, legend = F, bty = "n", box = F, axes = F, add = T,
     col = grey.colors(10), colNA = '#efead7')
plot(frame3, col = "blue", add = T, border = NA)
plot(worldmapframe, add = T, lwd = 4)

val_range <- c(round(min(var_1[], na.rm = T), 2),
               round(max(var_1[], na.rm = T), 2)) #get min and max values

vals <- c(min(val_range), max(val_range))

myGradientLegend(valRange = val_range_stand ,
                 pos = c(0.25, 0.29, 0.75, .305),
                 color = grey.colors(10),
                 side = 1,
                 dec = 2,
                 n.seg = 0,
                 values = c(round(min(vals)), round(max(vals))),
                 label.col = 'darkorange3',
                 font =2,
                 cex = 3)    #save width 1000

#### PANEL C ####

#plot range by biogeo status
par(mar = c(2,2,2,2), bg = 'white')
plot(range, col = 'white')
plot(alien_map2, col = '#e18c00', lwd = 4, add = T)
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
plot(brasil2, col = '#efead7', lwd = 4)
plot(alien_ex2, add = T, col = '#e18c0095', border = '#e18c00',  lwd = 4) #save width 500

#create bg points in the alien range of the sps and identify overlapping regs
pts_regs <- spsample(alien_ex2, n=1000, type="stratified", cellsize = 0.25)
sel_regs <- over(pts_regs, brasil)
sel_regs2 <- brasil[which(brasil$BENTITY2_N %in% sel_regs$BENTITY2_N),]

#plot select shp regions
sel_regs3 <- gSimplify(sel_regs2, tol = 0.2, topologyPreserve = T) 
plot(brasil2, col = 'blue', border = NA)
plot(sel_regs3, col = '#efead7', lwd = 4, add = T) #save width 500


#### PANEL E ####

#make a shapefile showing pixels
#make a raster
rast <- raster(extent(matrix(c(25, 0, 26.333334, 1.333334), nrow=2)), res = 0.1666667,  
               crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") 

rast[] <- c(1:length(rast))

grid_shp <- rasterToPolygons(rast) #transform into shapefile

#make a smaller area so that the points look more inside
shp_union <- gUnaryUnion(grid_shp)
shp_smaller <- gBuffer(shp_union, width = -0.04166668)

#generate points
occ <- spsample(shp_smaller, n = 200, type = "random")

#select only one point per cell
vals_occ <- extract(rast, occ)
vals_occ <- data.frame(ID = c(1:200), grid = vals_occ)

#include values in the shp
occ2 <- SpatialPointsDataFrame(occ, vals_occ)

#get unique points per grid
dat <-  as.data.table(occ2@data)
unique_pts <- unique(dat, by = 'grid')

#select unique points per cell
occ3 <- occ2[unique_pts$ID,]

#crop variable by extent of the shapefile
var_crop <-  crop(var_1, extent(grid_shp))

#plot var_crop
par(mar = c(2,2,2,2), bg = 'blue')
plot(shp_union, lwd = 4)
plot(var_crop, legend = F, bty = "n", box = F, axes = F, colNA = 'white', add = T)
plot(shp_union, add = T, lwd = 4) #save width default width

#plot all points on grid
par(mar = c(2,2,2,2), bg = 'blue')
plot(grid_shp, lwd = 4, col = '#efead7')
plot(occ, pch = 21, bg = "gold", cex = 1.5, add = T) #save width default width

#plot one point per cell on grid. WRONG
par(mar = c(2,2,2,2), bg = 'blue')
plot(grid_shp, lwd = 4, col = '#efead7')
plot(occ3, pch = 21, bg = "gold", cex = 1.5, add = T) #save width default width


#### PANEL F ####

#make a polygon to represent the study area
p <- Polygon(cbind(c(120,120,149,149,120), c(-13,2,2,-13,-13)))

ps <- Polygons(list(p), 1)
shp <- SpatialPolygons(list(ps))
shp2 <- gBuffer(shp, width = 1)

#make a smaller area so that the points look more inside
shp_smaller <- gBuffer(shp2, width = -0.5)

#generate points
occ <- spsample(shp_smaller, n = 400, type = "random")

#plot range by biogeo status and all pts over study region
plot(shp2, lwd = 4, col = '#efead7')
plot(alien_map2, col = '#e18c00', lwd = 4, add = T)
plot(nat_map2, col = '#4a794a', lwd = 4, add = T)
plot(unknown_map2, col = '#b3b3b3', lwd = 4, add = T) 
plot(occ, pch = 21, bg = "gold", cex = 1.5, add = T) #save width 750

#select and classify native and alien points
proj4string(occ) <- crs(nat_map2) #inform coordinate system

nat_pts1 <-  over(occ, nat_map2)
nat_pts2 <-  occ[which(!is.na(nat_pts1)),]

al_pts1 <-  over(occ, alien_map2)
al_pts2 <-  occ[which(!is.na(al_pts1)),]

#plot points by biogeo status over study region with borders of ranges
plot(shp2, lwd = 4, col = '#efead7')
plot(occ, pch = 21, bg = "white", col = '#e5e5e5', cex = 1.5, add = T)
plot(nat_map2, col = '#efead7', border = '#4a794a', lwd = 4, add = T)
plot(alien_map2, col = '#efead7', border = '#e18c00', lwd = 4, add = T)
plot(nat_pts2, pch = 21, bg = "#4a794a", cex = 1.5, add = T)
plot(al_pts2, pch = 21, bg = "#e18c00", cex = 1.5, add = T) #save width 750


#### PANEL H ####

#make a polygon to represent the native range

p <- Polygon(cbind(c(10,08,09,12,15,16,14,12,14,20,25,32,34,35,31,29,29,26,23,20,15,12,10),
                   c(10,11,13,13,15,15,17,18,23,26,29,30,26,23,14,12,10,09,08,05,07,08,10)))

ps <- Polygons(list(p), 1)
sps <- SpatialPolygons(list(ps))
sps2 <- gBuffer(sps, width = 1)

#seed random points within the polygon to represent native occ

occ <- spsample(sps, n = 100, type = "random")

#sample all points from the native occ to randomise

occ_sample <- occ[sample(c(1:length(occ)),length(occ)),]

#plot fig
plot(sps2, lwd = 4, col = '#efead7')
plot(occ, pch = 21, bg = "white", cex = 1.5, add = T)
plot(occ_sample[c(1:60)], pch = 21, bg = "#ccece6", cex = 1.5, add = T) 
plot(occ_sample[c(21:60)], pch = 21, bg = "#41ae76", cex = 1.5, add = T) 
plot(occ_sample[c(41:60)], pch = 21, bg = "#00441b", cex = 1.5, add = T) #save fig default


#plot example for the accumulation curve in the native range
par(mar=c(6,4,4,4), bg = 'white')
par(pty="s")

plot(c(0,103), c(0,1.03), type= "n", xlab = "", ylab = "", cex.axis = 2.5, 
     cex.lab = 1.8, xaxs = "i", yaxs = "i")
rect(0,0,103,1.03, col = "#efead7")

mtext("Niche breadth", side=2, line=4, cex = 4)

mtext("Occurrences", side=1, line=4, cex = 4)

lines(seq(0,100,10),c(0,0.4,0.55,0.8,0.87,0.91,0.95,0.963,0.97,0.99,0.997),  
      type="l",ylab = NA, xlab = NA,
      ylim = c(0,1), lwd = 8, 
      col = "#4a794a")

points(10, 0.4, pch = 21, cex = 5, bg = "#ccece6")
points(20, 0.55, pch = 21, cex = 5, bg = "#41ae76")
points(30, 0.8, pch = 21, cex = 5, bg = "#00441b")
points(100, 0.997, pch = 21, cex = 5, bg = "black")

text(15.5, 0.365, '1', font = 2, cex = 3)
text(25.5, 0.515, '2', font = 2, cex = 3)
text(35.5, 0.765, '3', font = 2, cex = 3)
text(100, 0.93, 'n', font = 2, cex = 3)  #save width 1000


#### PANEL I ####

#make polygons to represent the reference regions

p1 <- Polygon(cbind(c(10,08,09,34,35,26,23,10),
                    c(10,11,13,26,23,09,08,10)))
ps1 <- Polygons(list(p1), 1)

p2 <- Polygon(cbind(c(80,90,103,105,110,100,88,80),
                    c(60,75,80,57,50,55,57,60)))
ps2 <- Polygons(list(p2), 2)

p3 <- Polygon(cbind(c(05,12,25,10,12,08,05),
                    c(63,75,87,99,85,87,63)))
ps3 <- Polygons(list(p3), 3)

p4 <- Polygon(cbind(c(27,40,34,20,27),
                    c(53,35,66,59,53)))
ps4 <- Polygons(list(p4), 4)

p5 <- Polygon(cbind(c(67,74,84,60,48,67),
                    c(13,35,46,59,33,13)))
ps5 <- Polygons(list(p5), 5)

p6 <- Polygon(cbind(c(40,50,50,88,60,40),
                    c(90,95,99,87,70,90)))
ps6 <- Polygons(list(p6), 6)

sps <- SpatialPolygons(list(ps1, ps2, ps3, ps4, ps5, ps6))

#neg buffer to plot the points more beautifully
sps2 <- gBuffer(sps, width = -1, byid = T)

#seed random points within each polygon to represent alien occ
occ1 <- spsample(sps2[1,], n = 12, type = "random")
occ2 <- spsample(sps2[2,], n = 7, type = "random")
occ3 <- spsample(sps2[3,], n = 5, type = "random")
occ4 <- spsample(sps2[4,], n = 3, type = "random")
occ5 <- spsample(sps2[5,], n = 15, type = "random")
occ6 <- spsample(sps2[6,], n = 18, type = "random")

#plot fig
par(mar = c(2,2,2,2), bg = 'blue')
plot(sps, lwd = 4, col = '#efead7')
plot(occ1, pch = 21, bg = "white", cex = 1.5, add = T)
plot(occ2, pch = 21, bg = "white", cex = 1.5, add = T)
plot(occ3, pch = 21, bg = "white", cex = 1.5, add = T)
plot(occ4, pch = 21, bg = "white", cex = 1.5, add = T)
plot(occ5, pch = 21, bg = "white", cex = 1.5, add = T)
plot(occ6, pch = 21, bg = "white", cex = 1.5, add = T)

plot(occ1, pch = 21, bg = "#fee391", cex = 1.5, add = T)
plot(occ2, pch = 21, bg = "#fe9929", cex = 1.5, add = T)
plot(occ3, pch = 21, bg = "#993404", cex = 1.5, add = T) #save fig default


#plot example for the accumulation curve in the alien range
par(mar=c(6,4,4,4), bg = 'white')
par(pty="s")

plot(c(0,6.2), c(1,6.2), type= "n", xlab = "", ylab = "", cex.axis = 2.5, 
     cex.lab = 1.8, xaxs = "i", yaxs = "i")
rect(0,1,6.2,6.2, col = "#efead7")

lines(seq(0,6,1),c(1,3.6,4.8,5.6,5.78,5.9,5.95),  
     type="l",ylab = NA, xlab = NA,
     ylim = c(0,6), lwd = 8,
     col = "#e18c00")

mtext("Niche breadth", side=2, line=4, cex = 4)

mtext("Alien regions", side=1, line=4, cex = 4)

points(1, 3.6, pch = 21, cex = 5, bg = "#fee391")
points(2, 4.8, pch = 21, cex = 5, bg = "#fe9929")
points(3, 5.6, pch = 21, cex = 5, bg = "#993404")
points(6, 5.95, pch = 21, cex = 5, bg = "black")

text(1.3, 3.4, '1', font = 2, cex = 3)
text(2.3, 4.6, '2', font = 2, cex = 3)
text(3.3, 5.35, '3', font = 2, cex = 3)
text(6, 5.6, 'n', font = 2, cex = 3)  #save width 1000


#### PANEL J ####

#seed stratified points within each polygon to represent bg points
bg1 <- spsample(sps2[1,], type = "regular", cellsize = 3)
bg2 <- spsample(sps2[2,], type = "regular", cellsize = 3)
bg3 <- spsample(sps2[3,], type = "regular", cellsize = 3)
bg4 <- spsample(sps2[4,], type = "regular", cellsize = 3)
bg5 <- spsample(sps2[5,], type = "regular", cellsize = 3)
bg6 <- spsample(sps2[6,], type = "regular", cellsize = 3)

#plot fig
par(mar = c(2,2,2,2), bg = 'blue')
plot(sps, lwd = 4, col = '#efead7')
plot(bg1, pch = 21, bg = "white", cex = 1.5, add = T)
plot(bg2, pch = 21, bg = "white", cex = 1.5, add = T)
plot(bg3, pch = 21, bg = "white", cex = 1.5, add = T)
plot(bg4, pch = 21, bg = "white", cex = 1.5, add = T)
plot(bg5, pch = 21, bg = "white", cex = 1.5, add = T)
plot(bg6, pch = 21, bg = "white", cex = 1.5, add = T)

plot(bg1, pch = 21, bg = "#bcbddc", cex = 1.5, add = T)
plot(bg2, pch = 21, bg = "#6a51a3", cex = 1.5, add = T)
plot(bg3, pch = 21, bg = "#3f007d", cex = 1.5, add = T) #save fig default


#plot examples for the curve showing conditions offered by the regions and the alien accumulation curve

par(mar=c(6,4,4,4), bg = 'white')
par(pty="s")

plot(c(0,6.2), c(1,17.5), type= "n", xlab = "", ylab = "", cex.axis = 2.5, 
     cex.lab = 1.8, xaxs = "i", yaxs = "i")
rect(0,1,6.2,17.5, col = "#efead7")

lines(seq(0,6,1),c(1,6,6,8,10,11,17),  
      type="l",ylab = NA, xlab = NA,
      lwd = 8,
      col = "#807dba")

mtext("Niche breadth", side=2, line=4, cex = 4)

mtext("Alien regions", side=1, line=4, cex = 4)

points(1, 6, pch = 21, cex = 5, bg = "#bcbddc")
points(2, 6, pch = 21, cex = 5, bg = "#6a51a3")
points(3, 8, pch = 21, cex = 5, bg = "#3f007d")
points(6, 17, pch = 21, cex = 5, bg = "black")

text(1, 7.2, '1', font = 2, cex = 3)
text(2, 7.2, '2', font = 2, cex = 3)
text(3, 9.2, '3', font = 2, cex = 3)
text(5.6, 17, 'n', font = 2, cex = 3) 

#add ANAC
lines(seq(0,6,1),c(1,3.6,4.8,5.6,5.78,5.9,5.95),  
      type="l",ylab = NA, xlab = NA,
      ylim = c(0,6), lwd = 8,
      col = "#e18c00")

points(1, 3.6, pch = 21, cex = 5, bg = "#fee391")
points(2, 4.8, pch = 21, cex = 5, bg = "#fe9929")
points(3, 5.6, pch = 21, cex = 5, bg = "#993404")
points(6, 5.95, pch = 21, cex = 5, bg = "black")

text(1, 2.4, '1', font = 2, cex = 3)
text(2, 3.6, '2', font = 2, cex = 3)
text(3, 4.4, '3', font = 2, cex = 3)
text(6, 4.75, 'n', font = 2, cex = 3)  #save width 1000

#add legend lines
lines(c(0.8,1.5),c(15,15),  
      type="l", lwd = 8,
      col = "#807dba")

lines(c(0.8,1.5),c(13.5,13.5),  
      type="l", lwd = 8,
      col = "#e18c00")

text(2.4, 15, 'BGAC', font = 2, cex = 2)
text(2.4, 13.5, 'ANAC', font = 2, cex = 2)


