aout.kernel <- function(data, alpha, plot = TRUE, plottitle = "", kernel = "gaussian", nkernel = 1024,
                        kern.bw = "SJ", kern.adj = 1, xlim = NA, ylim = NA, outints = FALSE, w = NA, ...){
  if(nkernel %in% c(512, 1024, 2048, 4096)) nd <- length(data) else stop("nkernel must be one
                                                                         of 512, 1024, 2048, 4096.")
  
  if(is.na(min(xlim))) xlim <- range(data)
  if(is.na(min(w))) w <- rep(1, length(data))/length(data)
  densobj <- density(data, kernel = kernel, n = nkernel, bw = kern.bw, adjust = kern.adj, weights = w, ...)
  if(is.na(min(ylim))) ylim <- range(densobj$y)
  # helper function 1: pcafund
  pcafund <- function(a, dscx, dscy){
    z <- numeric(length(a))
    for (i in 1:length(a)){
      if (a[i] < min(dscx)) z[i] <- 0
      else if (a[i] > max(dscx)) z[i] <- 0
      else z[i] <- dscy[max(which(dscx <= a[i]))]
    }
    return(z)
  }
  # helper function 2: varyBalpha
  varyBalpha <- function(varalpha, fixedalpha, subdivisions = 1000, dscx, dscy){
    x <- dscx
    y <- dscy
    
    we <- which(y >= quantile(y, probs = varalpha))
    wnull <- (1:length(y))[-we]
    y[wnull] <- 0
    currentAlpha <- 1 - integrate(pcafund, lower = x[min(we)], upper = x[max(we)],
                                  subdivisions = subdivisions, dscx = x, dscy = y)$value
    fixedalpha - currentAlpha
  }
  
  # the actual function aout.kernel starts here
  x <- densobj$x
  y <- densobj$y
  # compute the value of B(alpha)
  temp <- uniroot(varyBalpha, interval = c(0.00001,0.99999), subdivisions = 10000, 
                  dscx = x, dscy = y, fixedalpha = alpha)$root

  ytemp <- y*(y > quantile(y, probs = temp))
  xnew <- numeric(nd)
  for(i in 1:nd) xnew[i] <- which.min(abs(data[i] - x))
  result <- data.frame(data = data, is.outlier = (y[xnew] < quantile(y, probs = temp)), 
                       fhat.of.x = y[xnew], B.of.alpha = rep(quantile(y, probs = temp), nd))

  
  if(plot == TRUE){
    plot(densobj, main = plottitle, xlim = xlim, ylim = ylim, ...)
    yind <- (y > quantile(y, probs = temp))
    ychange <- which(yind[-1] - yind[-nkernel] != 0)
    xintervals <- findInterval(x, x[ychange]) + 1
    
    if (mean(y[xintervals == 1]) < mean(y[xintervals == 2])) interval1 <- 1 else interval1 <- 2
    alloutints <- seq(interval1, max(xintervals), by = 2)
    for(i in 1:length(alloutints)){
      polygon(c(min(x[xintervals == alloutints[i]]), x[xintervals == alloutints[i]], 
                max(x[xintervals == alloutints[i]])), c(0, y[xintervals == alloutints[i]], 0), 
              density = 20, angle = 45)
    }
    points(result[,1], rep(0, nd), pch = 16 + result[,2], col = result[,2] + 1)
  }
  if(outints == TRUE) result <- list(Results = result, Bounds.of.Inlier.Regions = x[ychange], 
                                     KDE.Chosen.Bandwidth = densobj$bw)
  result
}
