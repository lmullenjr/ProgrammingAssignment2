## makeCachMatrix will create a list that will set the value of the input matrix,
## get the value of the input matrix, set the inverse of the input matrix, and
## get the inverse of the input matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function (solve) m <<- solve
  getinverse <- function() m
  
  list(set = set, get= get, setinverse = setinverse, getinverse=getinverse)
}


## cacheSolve will check to see if the inverse of the input matrix has already
## been cached, if it has, then it will return cached value, otherwise
## it will solve and return value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
 
    m <- x$getinverse()
    
    ## Check to see if the inverse has been cached
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    ## if the inverse was not cached, solve for it
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
#   }


   
  
}


