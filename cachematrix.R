## Put comments here that give an overall description of what your
## functions do



## Write a short comment describing this function





## Two functions are included in this file. 
## Function Descriptions
## 1. makeCacheMatrix
## Input: X, a square, invertible matrix. 
## Output: A list of functions: 
##   . set: Input is X, a square invertible matrix.
##   . get: Output is the square invertible matrix X. 
##   . setInverse: Input is the inverse of matrix X.
##   . getInverse: Output is the inverse of matrix X
## 2. cacheSolve
## Input: square, invertable matrix X.
## Output: The inverse of the matrix X. 
## NOTE: If the inverse of the matrix X was already computed, 
## the returned inverse matrix is what was saved to the cache.
## Example of use: 
##  t<-matrix(c(3,2,4, 6,1,4, 6,4,5),nrow=3, ncol = 3,byrow = TRUE) 
##  v<-makeCacheMatrix()
##  v$set(t)
##  cacheSolve(v)  
## [,1]       [,2]       [,3]
## [1,] -0.4074074  0.2222222  0.1481481
## [2,] -0.2222222 -0.3333333  0.4444444
##
###########################################################

makeCacheMatrix<- function(x = matrix())
{
  xInverse <-NULL  #Initialize the Inverse Matrix
  
  setX<- function(y) { #When the vector is set
                       #reset the inverse to NULL
    x <<-y  
    xInverse <<- NULL
  }  
  getX<-function() x  
  setInverseS<-function(Inverse) xInverse <<- Inverse  
  getInverseS<-function() xInverse   
  list(set = setX , get = getX ,setInverse = setInverseS, getInverse = getInverseS) 
}
  

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'   
    xInverse <- x$getInverse()  
    if(!is.null(xInverse)) {
      message("getting cached data.")
      return(xInverse)
    }
    data<- x$get()
    xInverse <- solve(data, ...)
    x$setInverse(xInverse)
    xInverse       

}

