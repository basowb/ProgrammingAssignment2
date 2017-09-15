## The goal these functions is to first 
## 1: create a matrix 
## 2: cache the inverse matrix in an environemnt 

##############################################################################


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
       inverseMatrix <- NULL
       
       set <- function(y) {
         
              x <<- y
              
              inverse <<- NULL
              
       }
  
       get <- function() x
       
       setInverse <- function(inverse) inverseMatrix <<- inverse
       
       getInverse <- function() inverseMatrix
       
       list(set = set,
            
            get = get,
            
            setInverse = setInverse,
            
            getInverse = getInverse)
            
}


## This function actually computes the inverse of the matrix created by
## the above function.  Hopfully if the inverse has already been calculated
## this function will use the cached solution instead of recalculating 
## the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        inverseMatrix <- x$getInverse()
        
        if (!is.null(inverseMatrix)) {
                  
                  message("getting cached data")
          
                  return(inverseMatrix)
        }
        
        mat <- x$get()
        
        inverseMatrix <- solve(mat, ...)
        
        x$setInverse(inverseMatrix)
        
        inverseMatrix
        
}
