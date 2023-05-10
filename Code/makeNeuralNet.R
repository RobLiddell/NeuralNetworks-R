source('Code\\dataLoading.R')
source('Code\\neuralNet.R')


NN <- initNN(c(784,36,10,10))|>
  SGD(trainingCon,testingCon,iterations=3,nTraining = 60000,step=500)

testData <- getNextNLabeledImages(testingCon,12)

testAccuracy(NN, testData)

makePredictions(NN,testData$Image)

plotXNumbers(testData,1:12,3,4)