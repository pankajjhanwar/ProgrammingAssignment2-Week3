## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Creates a special matrix object that can cache its inverse
##<<- operator is used to assign a value to an object in an environment that is different 
##from the current environment 

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL         ##initializing the variable 
  
  #Set the value of the matrix
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  
  getMatrix <- function() x                              #get the value of the Matrix
  setInverse <- function(inverse) invMatrix <<- inverse  #set the value of the invertible matrix
  getInverse <- function() invMatrix                     #get the value of the invertible matrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function

##Takes the output of the previous matrix and finds the inverse of the matrix
## using the solve function 
##checks if the inverse matrix from the makeCacheMatrix(matrix) has any value 
##If empty, it gets the original data and sets the invertible matrix using the 
##solve function
##If makeCacheMatrix(matrix) has some value in it, it returns a message and retrieves the 
##inverse from cache 

cacheSolve <- function(x, ...) {

  
  invMatrix <- x$getInverse()   ##get the value of the invertible matrix from the makeCacheMatrix function 
  if(!is.null(invMatrix)) {    ##Considering the inverse matrix is not NULL                     
    message("Getting Cached Invertible Matrix")   ##Prints after first occurence 
    return(invMatrix)                             
  }
  
  
  MatrixData <- x$getMatrix()                  ##get the original data   
  invMatrix <- solve(MatrixData, ...)          ##use the function to find the inverse of the matrix    
  x$setInverse(invMatrix)                         #set the invertible matrix 
  return(invMatrix)                               ##return the matrix 
}
