#set wds
wd_fig <- '/Users/carloseduardoaribeiro/Documents/CNA/Figure/Conceptual figure/Parts'
  
options(bitmapType='cairo') #otherwise plots are not working...


#make vector of the asymptote in the native range
asym <- 0.45
for(i in 1:10){asym[i+1] <- asym[i] + 0.3/(i ^ 1.8)}

#plot native range with asymptote
setwd(wd_fig)

jpeg(file = "Nat_asymp.jpeg", width = 1000, height = 1000)

par(mar = c(12,12,12,12))

plot(c(0,100), c(0,1), type= "n", xlab = "", ylab = "",
     yaxt = "n", xaxt = 'n', xaxs = "i", yaxs = "i")
rect(0,0,100,17.5, col = "#efead7")

#plot line
lines(seq(0,100,10),asym,  
      type="l",ylab = NA, xlab = NA,
      ylim = c(0,6), lwd = 8,
      col = "darkgreen")

title(ylab = 'Niche breadth', cex.lab = 3,
      line = 5)
title(xlab = 'Occurrences', cex.lab = 3,
      line = 5)

dev.off()

#make vector of no asymptote in the native range
n_asym <- seq(0.3,1,0.07)

#plot native range without asymptote
setwd(wd_fig)

jpeg(file = "Nat_non_asymp.jpeg", width = 1000, height = 1000)

par(mar = c(12,12,12,12))

plot(c(0,100), c(0,1), type= "n", xlab = "", ylab = "",
     yaxt = "n", xaxt = 'n', xaxs = "i", yaxs = "i")
rect(0,0,100,17.5, col = "#efead7")

#plot line
lines(seq(0,100,10),n_asym,  
      type="l",ylab = NA, xlab = NA,
      ylim = c(0,6), lwd = 8,
      col = "darkgreen")

title(ylab = 'Niche breadth', cex.lab = 3,
      line = 5)
title(xlab = 'Occurrences', cex.lab = 3,
      line = 5)

dev.off()


#make vector of the bg in the alien range
bg <- seq(2.2,19,1.62)
  
#make vector of the asymptote in the alien range
asym <- 1.1
for(i in 1:10){asym[i+1] <- asym[i] + 2.2/(i ^ 1.5)}

#plot alien range with asymptote and BG
setwd(wd_fig)

jpeg(file = "Alien_asymp.jpeg", width = 1000, height = 1000)

par(mar = c(12,12,12,12))

plot(c(0,100), c(1,20), type= "n", xlab = "", ylab = "",
     yaxt = "n", xaxt = 'n', xaxs = "i", yaxs = "i")
rect(0,0,100,20, col = "#efead7")

#plot line
lines(seq(0,100,10),bg,  
      type="l",ylab = NA, xlab = NA,
      ylim = c(0,6), lwd = 8,
      col = "blue")

lines(seq(0,100,10),asym,  
      type="l",ylab = NA, xlab = NA,
      ylim = c(0,6), lwd = 8,
      col = "red")

title(ylab = 'Niche breadth', cex.lab = 3,
      line = 5)
title(xlab = 'Alien regions', cex.lab = 3,
      line = 5)

dev.off()


#make vector of the bg in the alien range
bg <- seq(2.2,19,1.62)

#make vector of the asymptote in the alien range
n_asym <- seq(1.1,9,0.73)

#plot alien range with asymptote and BG
setwd(wd_fig)

jpeg(file = "Alien_non_asymp.jpeg", width = 1000, height = 1000)

par(mar = c(12,12,12,12))

plot(c(0,100), c(1,20), type= "n", xlab = "", ylab = "",
     yaxt = "n", xaxt = 'n', xaxs = "i", yaxs = "i")
rect(0,0,100,20, col = "#efead7")

#plot line
lines(seq(0,100,10),bg,  
      type="l",ylab = NA, xlab = NA,
      ylim = c(0,6), lwd = 8,
      col = "blue")

lines(seq(0,100,10),n_asym,  
      type="l",ylab = NA, xlab = NA,
      ylim = c(0,6), lwd = 8,
      col = "red")

title(ylab = 'Niche breadth', cex.lab = 3,
      line = 5)
title(xlab = 'Alien regions', cex.lab = 3,
      line = 5)

dev.off()
