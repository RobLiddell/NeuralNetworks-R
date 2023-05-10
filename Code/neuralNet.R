ReLu <- function(x){
  y <- pmax(x,0)
  return(y)
}
ReLu_Der <-  function(x){
  deriv <- (x>0) |> as.numeric()
  return(deriv)
}
softMax <- function(x){
  y <- t(t(exp(x))/colSums(exp(x)))
  return(y)
}
softMax_Der <- function(x){
  return(1)
}
oneHot <- function(x){
  oneHotLab <- matrix(0,nrow=10,ncol=length(x))
  oneHotLab[cbind(x+1,1:length(x))] <- 1
  return(oneHotLab)
}
forwardProp <- function(NN,X){
  Z <- list()
  A <- list()

  Z[[1]] <- NN$weights[[1]]%*%X|>
    sweep(1,(NN$biases[[1]]),'+')
  A[[1]] <- ReLu(Z[[1]])
  
  for(i in 2:NN$nLayers){
    
    Z[[i]] <- NN$weights[[i]]%*%A[[i-1]]|>
      sweep(1,(NN$biases[[i]]),'+')
    
    if(i<NN$nLayers){
        A[[i]] <- ReLu(Z[[i]])
    }
  }
  
  A[[NN$nLayers]] <- softMax(Z[[NN$nLayers]])

  return(list(Z=Z,A=A))
  
}
backProp <- function(NNout,NN,X,Y){
  dZ <- list()
  dW <- list()
  db <- list()
  
  m=length(Y)
  oneHotLab <- oneHot(Y)
  
  dZ[[NN$nLayers]] <- (NNout$A[[NN$nLayers]] - oneHotLab)
  dW[[NN$nLayers]] <- (dZ[[NN$nLayers]]%*%t(NNout$A[[NN$nLayers-1]]))/m
  db[[NN$nLayers]] <- rowSums(dZ[[NN$nLayers]])/m
  
  
  if(NN$nLayers>2){
    for(i in (NN$nLayers-1):2){
      dZ[[i]] <- (t(NN$weights[[i+1]]) %*% dZ[[i+1]]) * ReLu_Der(NNout$Z[[i]])
      dW[[i]] <- (dZ[[i]]%*%t(NNout$A[[i-1]]))/m
      db[[i]] <- rowSums(dZ[[i]])/m
    }
  }
  
  
  dZ[[1]] <- (t(NN$weights[[2]]) %*% dZ[[2]]) * ReLu_Der(NNout$Z[[1]])
  dW[[1]] <- (dZ[[1]]%*%X)/m
  db[[1]] <- rowSums(dZ[[1]])/m
  
  # browser()
  
  return(list(dW=dW,db=db))
}
updateParams<- function(NN,wbErr,alpha){
  
  for(i in 1:NN$nLayers){
    NN$weights[[i]] <- NN$weights[[i]]-alpha*wbErr$dW[[i]]
    NN$biases[[i]] <- NN$biases[[i]]-alpha*wbErr$db[[i]]
  }
  
  return(NN)
}
getPredictions <- function(output){
  preds <- output|>
    apply(2, which.max)-1
  return(preds)
}
getAccuracy <- function(pred,ans){
  accuracy <- (pred==ans)|>
    sum()/length(ans)
  return(accuracy)
}
getCost <- function(NNout,labels){
  nLayers <- length(NNout$A)
  
  cost <- sum(-log(colSums(NNout$A[[nLayers]]*oneHot(labels))))/length(labels)
  
  # cost <- sum((NNout$A[[nLayers]]-oneHot(labels))^2)/length(labels)
  return(cost)
}
makePredictions <- function(NN,images){
  output <- forwardProp(NN,t(images))
  pred <- getPredictions(output$A[[NN$nLayers]])
  return(pred)
}
initNN <- function(sizes=c(784,36,10)){
  
  biases <- list()
  weights <- list()
  
  for(i in 2:length(sizes)){
    biases[[i-1]] <- matrix(runif(sizes[i])-0.5,ncol=1)
    weights[[i-1]] <- matrix(runif(sizes[i]*sizes[i-1])-0.5,nrow=sizes[i])
  }
  
  NN <- list(nLayers=length(sizes)-1,
             weights=weights,
             biases=biases)
  
  return(NN)
}
gradientDescent <- function(NN, trainingData, iterations, alpha,silent=F){
  for(i in 1:iterations){
    NNout <- forwardProp(NN,t(trainingData$Images))
    wbErr <- backProp(NNout,NN,trainingData$Images,trainingData$Labels)
    NN <- updateParams(NN,wbErr,alpha)
    if(i%%50==0&!silent){
      cat('Iteration: ',i,';')
      cat('Accuracy: ', getAccuracy(getPredictions(NNout$A[[NN$nLayers]]),trainingData$Label),'; ')
      cat('Cost: ', getCost(NNout,trainingData$Label),'......\n')
    }
  }
  return(NN)
}

testAccuracy <- function(NN,data){
  pred <- makePredictions(NN,data$Images)
  acc <- getAccuracy(pred,data$Labels)
  return(acc)
}
