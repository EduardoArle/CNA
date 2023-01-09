library(plotfunctions)

myGradientLegend <- function (valRange, color = "terrain", nCol = 30, pos = 0.875, values = NULL,
                              side = 4, dec = NULL, length = 0.25, depth = 0.05, inside = FALSE, 
                              coords = FALSE, pos.num = NULL, n.seg = 1, border.col = "black",
                              tick.col = NULL, label.col = "black", fit.margin = TRUE, ...) 
{
  loc <- c(0, 0, 0, 0)
  ticks <- c()
  labels <- c()
  if (side %in% c(1, 3)) {
    if (length(pos) == 1) {
      pos.other <- ifelse(side > 2, 1, 0)
      switch <- ifelse(inside, 0, 1)
      switch <- ifelse(side > 2, 1 - switch, switch)
      loc <- getCoords(c(pos - 0.5 * length, pos.other - 
                           switch * depth, pos + 0.5 * length, pos.other + 
                           (1 - switch) * depth), side = c(side, 2, side, 
                                                           2))
    }
    else if (length(pos) == 4) {
      if (coords) {
        loc <- pos
      }
      else {
        loc <- getCoords(pos, side = c(1, 2, 1, 2))
      }
    }
    if (length(n.seg) == 1) {
      ticks <- seq(loc[1], loc[3], length = n.seg + 2)
      labels <- seq(min(valRange), max(valRange), length = n.seg + 
                      2)
    }
    else {
      labels <- c(min(valRange), sort(n.seg[n.seg > min(valRange) & 
                                              n.seg < max(valRange)], decreasing = FALSE), 
                  max(valRange))
      b <- diff(loc[c(1, 3)])/diff(range(labels))
      ticks <- (labels - min(labels)) * b + loc[1]
    }
  }
  else if (side %in% c(2, 4)) {
    if (length(pos) == 1) {
      pos.other <- ifelse(side > 2, 1, 0)
      switch <- ifelse(inside, 0, 1)
      switch <- ifelse(side > 2, 1 - switch, switch)
      loc <- getCoords(c(pos.other - switch * depth, pos - 
                           0.5 * length, pos.other + (1 - switch) * depth, 
                         pos + 0.5 * length), side = c(1, side, 1, side))
    }
    else if (length(pos) == 4) {
      if (coords) {
        loc <- pos
      }
      else {
        loc <- getCoords(pos, side = c(1, 2, 1, 2))
      }
    }
    if (length(n.seg) == 1) {
      ticks <- seq(loc[2], loc[4], length = n.seg + 2)
      labels <- seq(min(valRange), max(valRange), length = n.seg + 
                      2)
    }
    else {
      labels <- c(min(valRange), sort(n.seg[n.seg > min(valRange) & 
                                              n.seg < max(valRange)], decreasing = FALSE), 
                  max(valRange))
      b <- diff(loc[c(2, 4)])/diff(range(labels))
      ticks <- (labels - min(labels)) * b + loc[2]
    }
  }
  if (is.null(pos.num)) {
    if (side %in% c(1, 3)) {
      if (inside) {
        pos.num <- ifelse(side == 1, 3, 1)
      }
      else {
        pos.num <- side
      }
    }
    else {
      if (inside) {
        pos.num <- ifelse(side == 2, 4, 2)
      }
      else {
        pos.num <- side
      }
    }
  }
  getcol <- get_palette(color, nCol = nCol)
  mycolors <- getcol[["color"]]
  if (is.null(tick.col)) {
    tick.col = border.col
  }
  vals <- seq(min(valRange), max(valRange), length = length(mycolors))
  im <- as.raster(mycolors[matrix(1:length(mycolors), ncol = 1)])
  n <- max(c(length(ticks) - 2, 0))
  if (length(n.seg) > 1){
    n.seg <- length(n.seg)
  }
  if (side%%2 == 1) {
    im <- t(im)
    rasterImage(im, loc[1], loc[2], loc[3], loc[4], col = mycolors, 
                xpd = T)
    if(n.seg != 0){
      segments(x0 = ticks, x1 = ticks, y0 = rep(loc[2], n), 
               y1 = rep(loc[4], n), col = tick.col, xpd = TRUE)
    }
    rect(loc[1], loc[2], loc[3], loc[4], border = border.col, 
         xpd = T)
  }
  else {
    im <- rev(im)
    rasterImage(im, loc[1], loc[2], loc[3], loc[4], col = mycolors, 
                xpd = T)
    if(n.seg != 0){
      segments(x0 = rep(loc[1], n), x1 = rep(loc[3], n), y0 = ticks, 
               y1 = ticks, col = tick.col, xpd = TRUE)
    }
    rect(loc[1], loc[2], loc[3], loc[4], border = border.col, 
         xpd = T)
  }
  lab.loc.x <- lab.loc.y <- c()
  if (side %in% c(1, 3)) {
    lab.loc.x <- ticks
    if (pos.num == 1) {
      lab.loc.y <- rep(loc[2], length(lab.loc.x))
    }
    else if (pos.num == 3) {
      lab.loc.y <- rep(loc[4], length(lab.loc.x))
    }
    else {
      lab.loc.y <- rep((loc[2] + loc[4])/2, length(lab.loc.x))
    }
  }
  else if (side %in% c(2, 4)) {
    lab.loc.y <- ticks
    if (pos.num == 2) {
      lab.loc.x <- rep(loc[1], length(lab.loc.y))
    }
    else if (pos.num == 4) {
      lab.loc.x <- rep(loc[3], length(lab.loc.y))
    }
    else {
      lab.loc.x <- rep((loc[1] + loc[3])/2, length(lab.loc.y))
    }
  }
  determineDec <- function(x) {
    out = max(unlist(lapply(strsplit(as.character(x), split = "\\."), 
                            function(y) {
                              return(ifelse(length(y) > 1, nchar(gsub("^([^0]*)([0]+)$", 
                                                                      "\\1", as.character(y[2]))), 0))
                            })))
    return(out)
  }
  if (is.null(dec)) {
    dec <- min(c(6, determineDec(labels)))
  }
  eval(parse(text = sprintf("labels = sprintf('%s', round(labels, dec) )", 
                            paste("%.", dec, "f", sep = ""))))
  labels <- gsub("^(\\-)(0)([\\.0]*)$", "\\2\\3", 
                 labels)
  if (!is.null(values)) {
    labels <- values
  }
  if (fit.margin == TRUE & inside == FALSE) {
    lab.height <- max(strheight(labels))
    lab.width <- max(strwidth(labels))
    lab.cor <- strheight("0") * 0.5
    max.pos <- getFigCoords("f")
    cex.f <- NA
    change <- NA
    if (pos.num == 1) {
      max.height = lab.loc.y[1] - max.pos[3]
      cex.f = max.height/(lab.height + lab.cor)
      change <- ifelse(cex.f < 0.8, (lab.height + lab.cor) * 
                         0.8 - max.height, NA)
    }
    else if (pos.num == 2) {
      max.width = lab.loc.x[1] - max.pos[1]
      cex.f = max.width/(lab.width + lab.cor)
      change <- ifelse(cex.f < 0.8, (lab.width + lab.cor) * 
                         0.8 - max.width, NA)
    }
    else if (pos.num == 3) {
      max.height = max.pos[4] - lab.loc.y[1]
      cex.f = max.height/(lab.height + lab.cor)
      change <- ifelse(cex.f < 0.8, (lab.height + lab.cor) * 
                         0.8 - max.height, NA)
    }
    else if (pos.num == 4) {
      max.width = max.pos[2] - lab.loc.x[1]
      cex.f = max.width/(lab.width + lab.cor)
      change <- ifelse(cex.f < 0.8, (lab.width + lab.cor) * 
                         0.8 - max.width, NA)
    }
    if (cex.f < 0.8) {
      margin <- c("bottom", "left", "top", 
                  "right")
      warning(sprintf("Increase %s margin to fit labels or decrease the number of decimals, see help(gradientLegend).", 
                      margin[pos.num]))
    }
    par <- list(...)
    if ("cex" %in% names(par)) {
      text(x = lab.loc.x, y = lab.loc.y, labels = labels, 
           col = label.col, pos = pos.num, xpd = T, ...)
    }
    else {
      text(x = lab.loc.x, y = lab.loc.y, labels = labels, 
           col = label.col, pos = pos.num, cex = min(c(0.8, 
                                                      cex.f)), xpd = T)
    }
  }
  else {
    par <- list(...)
    if ("cex" %in% names(par)) {
      text(x = lab.loc.x, y = lab.loc.y, labels = labels, 
           col = tick.col, pos = pos.num, xpd = T, ...)
    }
    else {
      text(x = lab.loc.x, y = lab.loc.y, labels = labels, 
           col = tick.col, pos = pos.num, cex = 0.8, xpd = T)
    }
  }
  invisible(list(loc = loc, ticks = ticks, labels = labels, 
                 im = im))
}


