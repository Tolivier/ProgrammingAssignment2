## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
    ## cache (im) is initialized to NULL
    im <- NULL
    
    ## set the matrix and initialize the cache (im) to NULL
    set <- function(y) 
    {
        x <<- y
        im <<- NULL
    }
    
    ## return the matrix to inverted
    get <- function() x
    
    ## set the inverted maxtrix to cache (im)
    setsolve <- function(solve) im <<- solve
    
    ## get the inverted matrix (im)
    getsolve <- function() im
    
    
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above (argument x) 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## If the inverse has not already been calculated, cacheSolve calculated it
## by the "solve" function, with the optional arguments (argument ...)

cacheSolve <- function(x, ...) 
{
    im <- x$getsolve()
    if(!is.null(im)) 
    {
        ## getting cached data
        return(im)
    }
    
    ## the inverse matrix is not in the cache
    data <- x$get()
    ## calculate the inverted matrix
    im <- solve(data, ...)
    ## the result is stored in the cache
    x$setsolve(im)
    ## the inverted matrix is returned
    im
}
