aout.pareto <-
function(data, param, alpha = 0.1, hide.outliers = FALSE){
  # check arguments
  if (!is.numeric(param) | !is.vector(param) | length(param) != 2) 
    stop("param must be a numeric vector of length 2.")
  if (!is.numeric(data) | !is.vector(data)) 
    stop("data must be a numeric vector.")
  if (length(alpha) != 1 | alpha <= 0 | alpha >= 1) 
    stop("alpha must be a real number between 0 and 1, but it is ", alpha, ".")
  # end check arguments
  # determine the outlier region
  temp.region <- c(param[2], param[2]*alpha^(-1/param[1]))
  # give the results of the analysis
  temp <- data.frame(data = data, is.outlier = (data < temp.region[1] | 
                                                  data > temp.region[2]))
  if (hide.outliers == FALSE) temp
  else temp[temp[,2] == FALSE, 1]
}
