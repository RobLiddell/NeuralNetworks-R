source('Code\\dataLoading.R')
source('Code\\neuralNet.R')




SGD <- function(NN,trainingCon,testingCon,iterations=5,nTraining=60000,step=100){
  resetCons(trainingCon)
  resetCons(testingCon)
  testingData <- getNextNLabeledImages(testingCon,10000)
  
  testAcc <- c()
  trainAcc <- c()
  
  for(i in 1:iterations){
    cat('Epoch ', i,':')
    for(j in 1:(nTraining/step)){
      trainingData <- getNextNLabeledImages(trainingCon,step)
      if(j%%10==0){
        cat(j)
        trainAcc <- append(trainAcc,testAccuracy(NN, trainingData))
        testAcc <- append(testAcc,testAccuracy(NN, testingData))
      }else{
        cat('.')
      }
      NN <- NN|>
        gradientDescent(trainingData,5,0.1,silent=T)
    }
    cat('\n\tAccuracy:',testAccuracy(NN, trainingData),'\n')
    trainAcc <- append(trainAcc,testAccuracy(NN, trainingData))
    testAcc <- append(testAcc,testAccuracy(NN, testingData))
    resetCons(trainingCon) 
  }
  resetCons(testingCon) 
  plot(trainAcc)
  lines(testAcc)
  return(NN)
}

NN <- initNN(c(784,36,10,10))|>
  SGD(trainingCon,testingCon,iterations=3,nTraining = 60000,step=500)









testData <- getNextNLabeledImages(testingCon,12)

makePredictions(NN,testData$Image)
plotXNumbers(testData,1:12,3,4)

