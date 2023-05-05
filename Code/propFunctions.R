ReLu <- function(x){
  y <- pmax(0,x)
  return(y)
}

ReLuDeriv <-  function(x){
  deriv <- (x>0) |> as.numeric()
  return(deriv)
}

softMax <- function(x){
  y <- exp(x)/sum(exp(x))
  return(y)
}

oneHot <- function(x){
  oneHotLab <- matrix(0,nrow=length(x),ncol=10)
  oneHotLab[cbind(1:length(x),x+1)] <- 1
  oneHotLab <- t(oneHotLab)
  return(oneHotLab)
}

forwardProp <- function(W1,b1,W2,b2,x){
  Z1=W1%*%X+b1
  A1=ReLu(Z1)
  Z2=W2%*%A1+b2
  A2=softMax(Z2)
  return(list(Z1=Z1,A1=A1,Z2=Z2,A2=A2))
}

backProp <- function(Z1,A1,Z2,A2,W2,X,Y){
  m=length(Y)
  
  oneHotLab <- oneHot(Y)
  dZ2 <- A2 - oneHotLab
  dW2 <- 1/m * dZ2 %*% t(A1)
  db2 <- 1/m * sum(dZ2,2)
  dZ1 <- t(W2) %*% dZ2 * ReLuDeriv(Z1)
  dW1 <- 1/m * dZ1 %*% t(X)
  db1 <- 1/m * sum(dZ1,2)
  
  return(list(dW1=dW1,db1=db1,dW2=dW2,db2=db2))
}

updateParams<- function(W1,b1,W2,b2,dW1,db1,dW2,db2,alpha){
  W1=W1-alpha*dW1
  b1=b1-alpha*db1
  W2=W2-alpha*dW2
  b2=b2-alpha*db2
  return(list(W1=W1,b1=b1,W2=W2,b2=b2))
}


