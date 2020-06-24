# helper function to generate an index matrix, or return unique elements of a matrix
indexMatrix<-function(dimension,symmetrical=FALSE,upper=FALSE,lowerTriangular=FALSE, sep=NULL,starttext=NULL,endtext=NULL,
  unique=FALSE,rowoffset=0,coloffset=0,indices=FALSE,diagonal=TRUE,namesvector=NULL,shortdiag=FALSE){
  if(is.null(namesvector)) namesvector=1:9999
  if(indices==TRUE) sep<-c(",")
  tempmatrix<-matrix(paste0(starttext,rep(namesvector[1:dimension+coloffset],dimension)),nrow=dimension,ncol=dimension)
  for(i in 1:nrow(tempmatrix)){ #append step by step for shortdiag
    for(j in 1:ncol(tempmatrix)){
      # tempmatrix[i,j] <- paste0(tempmatrix[i,j],sep,namesvector[1:dimension+rowoffset][i])
      # print(tempmatrix[i,j])
      # print(namesvector[1:dimension+rowoffset][i])
      if(i!=j || !shortdiag) tempmatrix[i,j] <- paste0(tempmatrix[i,j],sep,namesvector[1:dimension+rowoffset][j])
    }
  }
  tempmatrix[,]<-paste0(tempmatrix[,],endtext)
  
  if(upper==TRUE) tempmatrix<-t(tempmatrix)
  if(symmetrical==TRUE) tempmatrix[col(tempmatrix)>row(tempmatrix)] <-t(tempmatrix)[col(tempmatrix)>row(tempmatrix)]
  if(unique==TRUE && symmetrical==TRUE) tempmatrix<-tempmatrix[lower.tri(tempmatrix,diag=diagonal)]
  if(lowerTriangular==TRUE) tempmatrix[col(tempmatrix) > row(tempmatrix)] <- 0
  if(indices==TRUE){
    tempmatrix<-matrix(c(unlist(strsplit(tempmatrix,","))[seq(1,length(tempmatrix)*2,2)],
      unlist(strsplit(tempmatrix,","))[seq(2,length(tempmatrix)*2,2)]),ncol=2)
  }
  return(tempmatrix)
}

rl<-function(x) { #robust logical - wrap checks likely to return NA's in this
  x[is.na(x)] <- FALSE
  return(x)
}

# generates more complex sequences than seq
cseq <- function(from, to, by){
  temp<-c()
  for(i in from){
    temp<-c(temp,seq(i,to,by))
  }
  temp<-sort(temp)
  return(temp)
}
