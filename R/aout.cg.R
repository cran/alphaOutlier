aout.cg <- function(data, param, alpha = 0.1, hide.outliers = FALSE){
  # check arguments
  if (!is.list(param) | length(param) != 3) 
    stop("param must be a list of length 3.")
  if (!is.numeric(data) | !is.matrix(data) | ncol(data) != 2) 
    stop("data must be a numeric matrix with two columns.")
  if (length(alpha) != 1 | alpha <= 0 | alpha >= 1) 
    stop("alpha must be a real number between 0 and 1, but it is ", alpha, ".")
  if (length(param[[1]]) != length(param[[2]]) | length(param[[2]]) != length(param[[3]])) 
    stop("The vectors contained by param must have the same length.")
  if (sum(param[[1]]) != 1) 
    stop("The sum of the probabilities in the first element of param must be 1.")
  
  # end check arguments
  # determine the outlier region
  # helper functions:
  uppbound <- function(K.alpha){
    x <- -2 * sigma^2 * log(K.alpha * sqrt(2 * pi) * sigma / p)
    sqrt(ifelse(x >= 0, x, 0)) + mu
  }
  lowbound <- function(K.alpha){
    x <- -2 * sigma^2 * log(K.alpha * sqrt(2 * pi) * sigma / p)
    -sqrt(ifelse(x >= 0, x, 0)) + mu
  }
  findroot <- function(K.alpha){
    1 - t(pnorm(uppbound(K.alpha), mu, sigma) 
          - pnorm(lowbound(K.alpha), mu, sigma)) %*% p - alpha
  }
  
  p <- param[[1]]
  mu <- param[[2]]
  sigma <- param[[3]]
  K <- uniroot(findroot, interval=c(0, 0.3989423/min(sigma)), tol = 1e-10)$root 
  # K is between 0 and the maximum of the Gaussian curves by definition
  
#  Inlierwahrscheinlichkeit <- pnorm(uppbound(K), mu, sigma) - pnorm(lowbound(K), mu, sigma)
#  adj.Inlierwahrscheinlichkeit <- Inlierwahrscheinlichkeit * p
#  kum.Inlierwahrscheinlichkeit <- cumsum(adj.Inlierwahrscheinlichkeit)
  temp.region <- cbind(lowbound(K), uppbound(K))
  is.out <- logical(nrow(data))
  for (i in 1:nrow(data)){
    is.out[i] <- (data[i,2] < temp.region[data[i,1], 1] | 
                    data[i,2] > temp.region[data[i,1], 2])
  }
  # give the results of the analysis
  temp <- data.frame(data = data, is.outlier = is.out)
  if (hide.outliers == FALSE) temp
  else temp[temp[,3] == FALSE, -3]
}
aout.cg(cbind(Class = c(1,2,1,2,3), Value = c(4,23,1,33,2)), 
        list(p = c(0.3,0.6,0.1), mu = c(1,32,3), sigma = c(3,2.3,1)), 
        hide.outliers = FALSE)
