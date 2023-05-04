library(stringr)


fileLoc <- "C:\\Users\\user\\Documents\\Databases\\MNIST\\"
imgFiles <- c('train-images.idx3-ubyte')
lableFiles <- c('train-labels.idx1-ubyte')

mnistCon <- file(str_glue('{fileLoc}{imgFiles}'),'rb')

readBin(mnistCon,raw(),n=2)
dataType <- readBin(mnistCon,raw(),n=1)
nDim <- readBin(mnistCon,raw(),n=1)
nImg <- readBin(mnistCon,integer(),n=1,endian='big')
nRow <- readBin(mnistCon,integer(),n=1,endian='big')
nCol <- readBin(mnistCon,integer(),n=1,endian='big')

imgRaw <- readBin(mnistCon,raw(),n=500*nRow*nCol)

imgRaw|>
  matrix()


as.integer()|>
  as.raster(max=255L,min)|>
  plot()


imgs <- img4|>
  as.integer()|>
  matrix(nrow = 4,byrow = T)

imgs[4,]|>
  matrix(nrow = 28,byrow = T)|>
  as.raster(max=255L,min)|>
  plot()
