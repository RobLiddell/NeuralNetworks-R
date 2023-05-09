source('Code\\dataLoading.R')
source('Code\\neuralNet.R')

imgData <- getNextNLabeledImages(imgCon,lblCon, N=500)


NN <- initNN(c(784,36,10,10))|>
  gradientDescent(imgData,500,0.1)

NN <- NN|>
  gradientDescent(imgData,2000,0.1)



makePredictions(NN,imgData$Images[1:12,])
plotXNumbers(imgData,1:12,3,4)


SGD <- function(NN,imgCon,lblCon,iterations=1000,nTraining=60000,step=500){
  
  for(i in 1:iterations){
    trainingDat <- getNextNLabeledImages(imgCon,lblCon,step)
    NN <- NN|>
      gradientDescent(trainingDat,nTraining/step,0.1)
  }
}


close(imgCon)
close(lblCon)



