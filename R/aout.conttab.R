aout.conttab <-
function(data, param, alpha = 0.1, hide.outliers = FALSE, 
                         show.estimates = FALSE){
  # check arguments
  if (is.data.frame(data)) data <- as.matrix(data)
  if (!is.matrix(data)) stop("data must be a matrix or data.frame.")
  N <- length(data)
  if (param[1] == "ML" | param[1] == "L1" | param[1] == "MP") param.temp <- TRUE
  else { if (!is.numeric(param) | !is.vector(param) | length(param) != N) 
    stop("param must be a numeric vector of length ", N, " or a character string 
         specifying the estimator to be used.")
    else param.temp <- FALSE
  }
  if (length(alpha) != 1 | alpha <= 0 | alpha >= 1) 
    stop("alpha must be a real number between 0 and 1, but it is ", alpha, ".")
  # end check arguments
  
  # internal functions 
  # creates design matrices for contingency tables with n = #rows, p = #columns
  create.X <- function(n, p) {
    X0 <- rep(1, n*p) # first row 
    X11 <- kronecker(diag(1,n-1), t(rep(1,p)))
    X12 <- matrix(rep(-1, p*(n-1)), nrow=n-1, ncol=p)
    
    X1 <- cbind(X11, X12) # first block
    
    X21 <- cbind(diag(1, p-1), rep(-1, p-1))
    X22 <- X21
    
    for(i in 1:(n-1)) X21 <- cbind(X21, X22) # second block
    
    rbind(X0, X1, X21)
  }
  
  # preprocessing
  i1 <- min(dim(data))
  i2 <- max(dim(data)) 
  if (dim(data)[1] > dim(data)[2]) data <- t(data)
  # then data is defined as in Kuhnt et al. (2012)
  
  XDesi <- create.X(i1, i2) # creates a design matrix with i1+i2-1 rows, i1*i2 columns
  
  if (param[1] == "ML"){ # Compute the ML estimator
    temp <- glm.fit(t(XDesi), c(t(data)), family=poisson(log))
    STmator <- t(temp$coefficients)
    param.temp <- t(temp$fitted.values)
  }
  
  if (param[1] == "L1"){  # Compute the L1 estimator 
    STmator <- t(rq.fit.fnc(t(XDesi), c(t(log(data))), R=t(XDesi), r=rep(1,dim(XDesi)[2]),
                            eps=1e-05)$coefficients)
    param.temp <- exp(t(t(XDesi) %*% t(STmator)))
  }
  
  if (param[1] == "MP") param.temp <- data - medpolish(data)$residuals
  # Compute the median polish estimator 
  
  if (is.numeric(param.temp)) param <- param.temp
  temp.results <- matrix(ncol = 2, nrow = N)
  for (i in 1:N) temp.results[i,] <- unlist(aout.pois(data[i], param[i], alpha, 
                                                      hide.outliers = FALSE))
  temp <- data.frame(data = c(data), is.outlier = (temp.results[,2] == 1),
                     param = c(param))
  if (hide.outliers == FALSE & show.estimates == TRUE) return(temp)
  if (hide.outliers == FALSE & show.estimates == FALSE) return(temp[,c(1,2)])
  if (hide.outliers == TRUE & show.estimates == TRUE) return(temp[temp[,2] == FALSE, 
                                                                  c(1,3)])
  if (hide.outliers == TRUE & show.estimates == FALSE) return(temp[temp[,2] == FALSE, 1])
}
