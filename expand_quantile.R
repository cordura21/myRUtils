expand_quantile <- function(x, p, skip.first,exclude.last = TRUE,...){

  x.length <- length(x)
  if(x.length < skip.first){
    return(rep(NA,x.length))
  }
  exp_quantile_vector <- vector(length = x.length)
  exp_quantile_vector[1:x.length] <- NA
  
  for(iLoop in skip.first:length(x)){
    exp_quantile_vector[iLoop] <- quantile(x[1:iLoop],p = p[1])
  }
  if(exclude.last){
    return(shift(exp_quantile_vector))    
  } else {
    return(exp_quantile_vector)  
  }
} 