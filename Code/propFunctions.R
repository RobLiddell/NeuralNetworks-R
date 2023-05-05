ReLu <- function(x){
  y <- (x+abs(x))/2
  return(y)
}

softMax <- function(x){
  y <- exp(x)/sum(exp(x))
  return(y)
}

tanh(c(-10:10))
