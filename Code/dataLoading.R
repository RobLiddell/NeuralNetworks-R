library(stringr)

fileLoc <- "C:\\Users\\user\\Documents\\Databases\\MNIST\\"

trainingFiles <- list(img='train-images.idx3-ubyte',lbl='train-labels.idx1-ubyte')
testingFiles <- list(img='t10k-images.idx3-ubyte',lbl='t10k-labels.idx1-ubyte')

createCons <- function(files,fileLoc){
  connections <- list(img=file(str_glue('{fileLoc}{files$img}'),open='rb'),
                   lbl=file(str_glue('{fileLoc}{files$lbl}'),open='rb'))
  return(connections)
}

closeCons <- function(connections){
  close(connections$img)
  close(connections$lbl)
}

resetCons <- function(connections){
  seek(connections$img,where=16)
  seek(connections$lbl,where=8)
}

trainingCon <- createCons(trainingFiles,fileLoc)
testingCon <- createCons(testingFiles,fileLoc)


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
getNextNLabeledImages <- function(dataCon,N=1,nRow=28,nCol=28){
  currLoc=((seek(dataCon$img)-16)/(nRow*nCol))+8 #Calculate current image index
  
  seek(dataCon$lbl, where=currLoc)
  lbls <- readBin(dataCon$lbl,raw(),n=N)|>
    as.integer()
  
  img <- readBin(dataCon$img,raw(),n=N*nRow*nCol)|>
    as.integer()|>
    matrix(ncol=nRow*nCol,byrow = TRUE)/255 #Scale values 0-1
  
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

imgInfo <- getIDXFileInfo(trainingCon$img)
lblInfo <- getIDXFileInfo(trainingCon$lbl)
getIDXFileInfo(testingCon$img)
getIDXFileInfo(testingCon$lbl)


nImg <- imgInfo$dims[1]
nRow <- imgInfo$dims[2]
nCol <- imgInfo$dims[3]

