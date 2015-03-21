## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {  

    m <- NULL                                     ## creates a variable m and sets it to NA 
                                                      ## in case m had a previous value
    set <- function(y) {                          ## creates a function called set in  makeCacheMatrix
        x <<- y                                   ## sets x in global environment
        m <<- NULL                                ## clears m variable in global environment
      
    } 
      
        get <- function() x                       ## creates a function called get in  makeChacheMatrix
        setinvers <- function(solve) m <<- solve  ## creates a function called setinverse in  makeCacheMatrix,
                                                  ## solves the inverse of the matrix, and sends it to the 
                                                  ## global environment
        getinvers <- function() m                 ## creates a function called getinverse in  makeCacheMatrix and
                                                  ## retrieves the inverse of the matrix in local environment
        list(set = set, get = get,                ## makes a list of all elements to be returned from the function
            setinvers = setinvers,
            getinvers = getinvers)
    
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' from a global environment or calculates it
  
  m <- x$getinvers()              ## gets the inverted matrix from the global environment and assigns 
                                     ## it to the m var
  if(!is.null(m)) {               ## if the m var has something in it, then it will return that solution
    message("getting cached data")
    return(m)
  }                               ## if the m var is null (nothing found in the global environment)
                                      ## then this will calculate the solution
       data <- x$get()            ## gets local data
       m <- solve(data, ...)      ## calculates solution on local data
       x$setinvers(m)             ## sends the solution to the global environment and
       m                          ## returns solution
  
}
