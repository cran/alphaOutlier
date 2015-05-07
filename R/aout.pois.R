aout.pois <-
function(data, param, alpha = 0.1, hide.outliers = FALSE){
  # check arguments
  if (!is.numeric(param) | !is.vector(param) | length(param) != 1) 
    stop("param must be a numeric vector of length 1.")
  if (!is.numeric(data) | !is.vector(data)) 
    stop("data must be a numeric vector.")
  if (length(alpha) != 1 | alpha <= 0 | alpha >= 1) 
    stop("alpha must be a real number between 0 and 1, but it is ", alpha, ".")
  # end check arguments
  # determine the outlier region 
  tu <- qpois(alpha, param)
  to <- qpois(1-alpha, param)
  
  P.now <- sum(dpois((tu+1):(to-1), param))
  
  while(P.now <= 1-alpha){ # loop uses the ascending-descending character of the p.d.f.
    if (dpois(tu, param) > dpois(to, param)) {
      next.P <- dpois(tu, param)
      tu <- tu - 1
    }
    else {
      next.P <- dpois(to, param)
      to <- to + 1
    }     
    P.now <- P.now + next.P
  }
  temp.region <- (tu+1):(to-1) # is the inlier region
  # give the results of the analysis
  temp <- data.frame(data = data, is.outlier = !(data %in% temp.region))
  if (hide.outliers == FALSE) temp
  else temp[temp[,2] == FALSE, 1]
}
