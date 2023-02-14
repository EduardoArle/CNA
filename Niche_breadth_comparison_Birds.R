#packages
library(nicheRealisation);library(bRacatus);library(rgdal);library(rgeos)
library(raster);library(data.table)

options(bitmapType='cairo') #otherwise plots are not working...

#directories
wd_birds <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Birds'
wd_sps_occ <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Birds/Occurrences'
wd_sps_range <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Birds/Range_maps'
wd_shp <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Birds/Regions_shapefile'
wd_fig_points <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Birds/Figures/Points'
wd_fig_regs <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Birds/Figures/Reference_regions'
wd_fig_graphs_nat <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Birds/Figures/Graphs_nat'
wd_fig_graphs_alien <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Birds/Figures/Graphs_alien'
wd_stats_nat <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Birds/Stats/Native'
wd_stats_alien <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Birds/Stats/Alien'
wd_variables <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Variables/wc2-5'
wd_stand_var <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Variables/Standardised_variables'
wd_var_cont <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Birds/Variable_contribution'
wd_mapping <- '/Users/carloseduardoaribeiro/Documents/Soup/Map stuff'


#load variables layer to clean points
setwd(wd_stand_var)
layer <- raster('bio1.asc')
layer_ID <- layer
layer_ID[c(1:length(layer))] <- c(1:length(layer)) #for some weird reason, produces an error the first time I run, but works fine the second...

#load alien bird list
setwd(wd_birds)
birds <- read.csv('Birds_checklist.csv')

#load selected species list
list <- readRDS('Selected_species')

#load birds shapefile
shp <- readOGR('Shapefile_birds', dsn = wd_shp,
               use_iconv = TRUE, encoding = 'UTF-8')

#load world map and frame
setwd(wd_mapping)
world <- readRDS('wrld.rds')
frame <- readRDS('worldmapframe.rds')
frame <- spTransform(frame, CRS(proj4string(world)))


############## loop through species ################
i=3

for(i in 3:length(list))
{
  #load and visulalise the data
  setwd(wd_sps_occ)
  occ <- try(readRDS(list[i]),silent = T)
  
  if(class(occ) != 'try-error'){
    occ <- occ[complete.cases(occ$decimalLatitude),]
    
    #clean the points (max one per cell in var layer)
    occ_sp <- occSpatialPoints(occ)
    cell_ID <- extract(layer_ID,occ_sp)
    occ$cellID <- cell_ID
    occ <- as.data.table(occ)
    occ <- unique(occ,by = 'cellID')
    occ <- as.data.frame(occ)
    
    #select checklist regions where the species is listed
    sps_cl <- birds[which(birds$gbifDarwinCore== list[i]),]
    sps_alien_regs <- shp[which(shp$GAVIARg %in%
                                  sps_cl$Region),]
    
    #load species range map
    sps_range <- readOGR(list[i], dsn = wd_sps_range)
    sps_range_ext <- sps_range[which(sps_range$PRESENCE == 1),]
    
    #prepare ref_reg data to classify points as native or alien
    nat_al <- list(native=sps_range_ext,alien=sps_alien_regs)
    
    #classify points as native or alien
    nat_al_pts <- natAlPoints(occ,nat_al)
    
    #check if there are native and alien points available
    if(!FALSE %in% sapply(nat_al_pts,nrow) > 0){
      #save figure with the species native range and alien ref_regs
      setwd(wd_fig_regs)
      
      jpeg(file=paste0(list[i],"_reference_regions.jpeg"),width = 1000, height = 1000,
           type = 'cairo')
      
      par(mar = c(2,2,2,2))
      
      plot(frame,add=F)
      plot(world,border=NA,col="gray70",add=T)
      sps_range2 <- spTransform(sps_range_ext,CRS(proj4string(world)))
      plot(sps_range2,add=T,col="darkgreen",border=NA)
      sps_alien_regs2 <- spTransform(sps_alien_regs,CRS(proj4string(world)))
      plot(sps_alien_regs2,add=T,col="orange",border=NA)
      text(0,8900000,list[i],cex=2,font=3)
      points(-13000000,-1600000,pch = 15, col = "darkgreen",cex=2.5)
      text(-11000000,-1600000,"Native range")
      points(-13000000,-2200000,pch = 15, col = "orange",cex=2.5)
      text(-11000000,-2200000,"Alien regions")
      
      dev.off()
      
      #save figure with the species native and alien points
      setwd(wd_fig_points)
      
      jpeg(file=paste0(list[i],"_points.jpeg"),width = 1000, height = 1000,
           type = 'cairo')
      
      par(mar = c(2,2,2,2))
      
      plot(frame,add=F)
      plot(world,border=NA,col="gray70",add=T)
      native_points1 <- occSpatialPoints(nat_al_pts$native)
      native_points2 <- spTransform(native_points1,CRS(proj4string(world)))
      plot(native_points2,add=T,col="darkgreen",pch=19,cex=.4)
      alien_points1 <- occSpatialPoints(nat_al_pts$alien)
      alien_points2 <- spTransform(alien_points1,CRS(proj4string(world)))
      plot(alien_points2,add=T,col="orange",pch=19,cex=.4)
      text(0,8900000,list[i],cex=2,font=3)
      points(-13000000,-1600000,pch = 19, col = "darkgreen",cex=1)
      text(-11000000,-1600000,"Native points")
      points(-13000000,-2200000,pch = 19, col = "orange",cex=1)
      text(-11100000,-2200000,"Alien points")
      
      dev.off()
      
      #clean points that don't overlap a native or an alien region
      occ_clean <- rbind(nat_al_pts$native, nat_al_pts$alien)
      
      #extract the values for variables in the occurrence locations
      vars <- extractValues(wd_variables, occ_clean, original_format = '.bil$')
      
      #select only variables less correlated than 0.7 using the usdm vifcor approach
      corel <- variableCorrelation(occ_clean, wd_variables, th = 0.7,
                                   original_format = '.bil$')
      
      #run a maxent and calculate the contribution of each variable to the model
      var_cont <- variableContribution(occ_clean, wd_variables, corel,
                                       original_format = '.bil$')
      
      #save variable cont
      setwd(wd_var_cont)
      write.csv(var_cont, paste0(list[i], "_var_cont.csv"))
      
      #prepare variables that will be used in the nicheBreadth calculation
      vars2 <- variablePreparation(wd_stand_var, var_cont, th=5)
      
      #calculate niche breadth using all native points
      nat_breadth <- nicheBreadth(nat_al_pts$native, vars2)
      
      #calculate niche breadth in the native region using a random sample of the
      #points, including 10 per 10 records in order to see how the accumulation 
      #will look like
      
      #make ten repetitions per species
      
      rep_tol <- ifelse(nrow(nat_al_pts$native) < 1000,10,100)
      rep_tol <- ifelse(nrow(nat_al_pts$native) < 30,1,rep_tol)
      nat_breath_var <- matrix(nrow = ceiling(nrow(nat_al_pts$native)/rep_tol),
                               ncol = 10)
      
      for(j in 1:10){
        #sample all points from the native samples to randomise
        occ_sample <- nat_al_pts$native[sample(c(1:nrow(nat_al_pts$native)),
                                               nrow(nat_al_pts$native)),]
        
        nat_breadth2 <- numeric()
        for(k in 1:ceiling(nrow(occ_sample)/rep_tol))
        {
          occ_sample2 <- occ_sample[c(1:(k*rep_tol)),]
          occ_sample2 <- occ_sample2[complete.cases(occ_sample2$decimalLatitude),]
          
          nat_breadth2[k] <- nicheBreadth(occ_sample2,vars2)/nat_breadth
          print(paste0(j,"_",k))
        }
        
        nat_breath_var[,j] <- nat_breadth2
      }
      
      nat_breath_var2 <- as.data.frame(nat_breath_var)
      names(nat_breath_var2) <- c(1:10)
      
      #save calculations for native range saturation
      setwd(wd_stats_nat)
      write.csv(nat_breath_var2,paste0(list[i],".csv"),row.names = F)
      
      #save graphs for native grain
      setwd(wd_fig_graphs_nat)
      
      jpeg(file=paste0(list[i],"_graph_nat.jpeg"),width = 1000, height = 1000)
      
      par(mar = c(12,12,12,12))
      
      for(j in 1:10){
        if(j == 1){
          plot(c(1:nrow(nat_breath_var2))*rep_tol,nat_breath_var2[,j],
               type="l", lwd = 2, axes = F,
               ylim = c(0,1), ylab = NA, xlab = NA,
               col = "gray80")
          axis(1, cex.axis = 3, mgp = c(0,2,0))
          axis(2, cex.axis = 3)
          box()
          title(ylab = 'Niche breadth', cex.lab = 3,
                line = 5)
          title(xlab = 'Occurrences', cex.lab = 3,
                line = 5)
        }else{
          lines(c(1:nrow(nat_breath_var2))*rep_tol,nat_breath_var[,j], 
                col= "gray80", lwd = 2)
        }
      }
      
      m_nat_breath <- apply(nat_breath_var2,1,mean)
      lines(c(1:length(m_nat_breath))*rep_tol, m_nat_breath,
            col = "darkgreen", lwd = 5)
      
      dev.off()
      
      ### ALIEN regions calculation
      
      ###  find a way to have "geo_entity" working for any data 
      
      #get the index for each point's region identity
      
      alien_pts_sp <- occSpatialPoints(nat_al_pts$alien)
      alien_pts_sp_regs <- over(alien_pts_sp,sps_alien_regs)
      alien_pts_sp2 <- cbind(nat_al_pts$alien,alien_pts_sp_regs)
      
      #indicate the column name of region ID
      reg_ID <- "GAVIARg"
      reg_index <- which(names(alien_pts_sp2) == reg_ID)
      
      #list unique regions where the species is alien
      regs <- unique(alien_pts_sp2[,reg_index])
      
      #seed points in alien regions to get all offered environmental conditions
      regs_shp <- shp[which(shp$GAVIARg %in% regs),]
      bg_pts <- list()
      
      for(j in 1:length(regs))  
      {
        bg_pts[[j]] <- try(spsample(regs_shp[j,],
                                    n=1000,type="stratified",
                                    cellsize = 0.25),
                           silent=T)
        
        if(class(bg_pts[[j]]) == "try-error"){
          
          bg_pts[[j]] <- spsample(gBuffer(regs_shp[j,],
                                          width = 1),
                                  n=1000,type="stratified",
                                  cellsize = 0.25)
        }
        print(j)
      }
      
      names(bg_pts) <- regs_shp$GAVIARg
      
      #make a matrix to input the niche offered by each region
      alien_regions_var <- matrix(nrow = length(regs_shp),
                                  ncol = 10)
      
      #make a matrix to input the niche realised when including each region
      alien_breadth_var <- matrix(nrow = length(regs_shp),
                                  ncol = 10)
      
      for(j in 1:10){
        #sample all regions to randomise
        regs_sample <- regs[sample(c(1:length(regs)),
                                   length(regs))]
        
        reg_breadth <- numeric()
        al_breadth <- numeric()
        
        for(k in 1:length(regs_sample))
        {
          #select bg points from all alien regions of the loop 
          bg_alien <- bg_pts[which(names(bg_pts) %in% regs_sample[c(1:k)])]
          
          #harmonise bg points with the data
          
          coords1 <- lapply(bg_alien,coordinates)
          coords2 <- lapply(coords1,as.data.frame)
          coords3 <- rbindlist(coords2)
          names(coords3) <- c("decimalLongitude","decimalLatitude")
          l_pts <- list(nat_al_pts$native,coords3)
          
          occ_bg <- rbindlist(l_pts,use.names = T, fill = T)
          
          reg_breadth[k] <- nicheBreadth(occ_bg, vars2) / nat_breadth
          
          #select occ points from the all alien regions of the loop
          occ_alien <- alien_pts_sp2[which(alien_pts_sp2[,reg_index] %in% 
                                             regs_sample[c(1:k)]),]
          
          #eliminate shp derived columns to rbind selceted alien to all ant points
          occ_alien2 <- occ_alien[,-c((ncol(occ_alien)-ncol(alien_pts_sp_regs)+1):
                                        ncol(occ_alien))]
          
          occ2 <- rbind(nat_al_pts$native, occ_alien2)
          
          
          al_breadth[k] <- nicheBreadth(occ2,vars2) / nat_breadth
          
          print(paste0(j,"_",k))
        }
        
        alien_regions_var[,j] <- reg_breadth
        alien_breadth_var[,j] <- al_breadth
      }
      
      alien_regions_var <- as.data.frame(alien_regions_var)
      alien_breadth_var <- as.data.frame(alien_breadth_var)
      names(alien_regions_var) <- c(1:10)
      names(alien_breadth_var) <- c(1:10)
      
      #save calculations for alienrange saturation
      setwd(wd_stats_alien)
      write.csv(alien_regions_var,paste0(list[i],"_available.csv"),row.names = F)
      write.csv(alien_breadth_var,paste0(list[i],"_realised.csv"),row.names = F)
      
      #save graphs for alien range
      setwd(wd_fig_graphs_alien)
      
      jpeg(file=paste0(list[i],"_graph_alien.jpeg"),width = 1000, height = 1000)
      
      par(mar = c(12,12,12,12))
      
      for(j in 1:10){
        if(j == 1){
          plot(alien_regions_var[,j],
               type="l", lwd = 2, axes = F,
               ylim = c(1,max(alien_regions_var[,j])), 
               ylab = NA, xlab = NA,
               col = "gray80")
          axis(1, cex.axis = 3, mgp = c(0,2,0))
          axis(2, cex.axis = 3)
          box()
          title(ylab = 'Niche breadth', cex.lab = 3,
                line = 5)
          title(xlab = 'Alien regions', cex.lab = 3,
                line = 5)
          
          lines(c(1,alien_breadth_var[,j]),col="gray80", lwd = 2, lty = 2,)
          
        }else{
          lines(alien_regions_var[,j], lwd = 2, col = "gray80")
          lines(alien_breadth_var[,j], lwd = 2, col = "gray80", lty = 2)
        }
      }
      
      m_alien_regions_var <- apply(alien_regions_var,1,mean)
      lines(m_alien_regions_var,col = "blue", lwd=5)
      
      m_alien_breadth_var <- apply(alien_breadth_var,1,mean)
      lines(m_alien_breadth_var,col = "red", lwd=5)
      
      dev.off()
    }else{
      setwd(wd_amphibia)
      missing <- readRDS("Missing_species")
      missing[length(missing)+1] <- list[i]
      saveRDS(missing,"Missing_species")
    }
    
  }else{
    setwd(wd_amphibia)
    missing <- readRDS("Missing_species")
    missing[length(missing)+1] <- list[i]
    saveRDS(missing,"Missing_species")
  }
}


