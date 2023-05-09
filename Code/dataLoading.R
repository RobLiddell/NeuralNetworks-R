library(stringr)

fileLoc <- "C:\\Users\\user\\Documents\\Databases\\MNIST\\"
imgFiles <- c('train-images.idx3-ubyte')
labelFiles <- c('train-labels.idx1-ubyte')

imgCon <- file(str_glue('{fileLoc}{imgFiles}'),open='rb')
lblCon <- file(str_glue('{fileLoc}{labelFiles}'),open='rb')


getIDXFileInfo <- function(con){
  currLoc <- seek(con,where=2)
  dataType <- readBin(con,raw(),n=1)
  nDim <- readBin(con,raw(),n=1)|>
    as.integer()
  
  dims <- readBin(con,integer(),n=1,endian='big')
  if (nDim>1){
    for(i in 2:nDim){
      dims <- append(dims,readBin(con,integer(),n=1,endian='big'))
    }
  }
  
  conInfo <- list(dataType=dataType,nDim=nDim,dims=dims)
  
  if(currLoc>seek(con)){
    seek(con,where=currLoc)
  }
  
  return(conInfo)
}
getNextNLabeledImages <- function(imgCon,lblCon,N=1,nRow=28,nCol=28){
  currLoc=((seek(imgCon)-16)/(nRow*nCol))+8
  
  seek(lblCon, where=currLoc)
  lbls <- readBin(lblCon,raw(),n=N)|>
    as.integer()
  
  img <- readBin(imgCon,raw(),n=N*nRow*nCol)|>
    as.integer()|>
    matrix(ncol=nRow*nCol,byrow = TRUE)/255
  
  
  
  return(list(Labels=lbls,Images=img))
}
plotNumber <- function(imgData,figure=1){
  imgData$Images[figure,]|>
    matrix(nrow=28,byrow = T)|>
    as.raster()|>
    plot()
}
plotXNumbers <- function(imgData,figs,nrow,ncol){
  par(mfrow=c(nrow,ncol))
  for(i in figs){
    plotNumber(imgData,i)
  }
}

imgInfo <- getIDXFileInfo(imgCon)
lblInfo <- getIDXFileInfo(lblCon)

nImg <- imgInfo$dims[1]
nRow <- imgInfo$dims[2]
nCol <- imgInfo$dims[3]



if(FALSE){

  imageDat <- getNextNLabeledImages(imgCon,lblCon,N=5000)
  
  
  
  imageDat <- getNextNLabeledImages(imgCon,lblCon,N=4)
  
  par(mfrow=c(2,2))
  
  for(i in 1:4){
    imageDat$Images[i,]|>
      matrix(nrow = 28,byrow = T)|>
      as.raster(max=255L)|>
      plot()
  }
  
  
  close(imgCon)
  close(lblCon)
  
  
  imageDat|>
    tibble::as.tibble()|>
    dplyr::select(Images)|>
    dplyr::slice(1)%>%
    dplyr::pull()|>
    matrix(nrow=28,byrow = T)|>
    as.raster(max=255L)|>
    plot()
  
  
}

