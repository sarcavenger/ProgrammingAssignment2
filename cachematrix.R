#function that is used to cache inverse of the given matrix
makeCacheMatrix <- function(Matrix=matrix()){ #function that is used to cache inverse of the given matrix
  invMatrix<-NULL           #this holds the value of inverse matrix and is initialised to null
  setMatrix<-function(m){    #define the value of given matrix
    Matrix<<-m
    invMatrix<<-NULL
                        }
  getMatrix<-function()Matrix    #return the value of given matrix               
  setinvMatrix<-function(inverse)invMatrix<<-inverse  #assign value of inverse of matrix
  getinvMatrix<-function()invMatrix                   #get the value of inverse of matrix  
  list(setMatrix=setMatrix,getMatrix=getMatrix,setinvMatrix=setinvMatrix,getinvMatrix=getinvMatrix) #list to access various functions
}

#function to calculate inverse of matrix created by above function
cacheSolve <- function(Matrix, ...){ #function to calculate inverse of matrix created by above function
  invMatrix<-Matrix$getinvMatrix()
  if(!is.null(invMatrix)){
    message("getting cached data as inverse calculated already") #retrieve inverse from cache 
    return(invMatrix)                                            #if inverse already been calculated
                         }
  x<-Matrix$getMatrix()
  invMatrix<-solve(x, ...)
  Matrix$setinvMatrix(invMatrix)
  invMatrix
}
