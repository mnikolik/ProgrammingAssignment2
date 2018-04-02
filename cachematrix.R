## The function makeCacheMatrix does a "special" matrix which is actually a wrapper of the normal matrix
## with additional cache vvariable thta holds the value of Matrix::solve function
## The function cacheSolve takes the "special" matrix and calculates the value of the Matrix:solve function
## if the function was not previously executed, the solve function is called and the value is stored in the wrapper variable 
## if the function was solved previously, the function returns the previous result

## This fuction creates a wrapper for the matrix with additional:
## variable to cache the solve function result
## functions to set and get the matrix
## functions to set and get the solve function result into and from the cache
makeCacheMatrix <- function(x = matrix()) {
        # define the matrix
        m <- NULL
        # create a setter
        set <- function(y){
                # set the matrix to value y 
                x <<- y
                # reset cached inverse
                m <<- NULL
        }
        # create a getter
        get <- function() x
        # setter for the inverse, set the value of m to provided parameter solve 
        setSolve <- function(solve) m <<- solve
        # getter for the inverse, returns the value of m
        getSolve <- function() m
        
        # generate a list with all of the functions contained
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## The function cacheSolve takes the "special" matrix and calculates the value of the Matrix:solve function
## if the function was not previously executed, the solve function is called and the value is stored in the wrapper variable 
## if the function was solved previously, the function returns the previous result
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # call the "special matrix" and call function getSolve from it
        # the value of getSolve can be NULL or a value of inverted matrix if one is cached
        m <- x$getSolve()
        # if the value is not null present a message on screen to show this is from cache and return the value of the matrix 
        if(!is.null(m)) {
                message("getting cached data")
                # return value and stop execution of further commands
                return(m)
        }
        # since the value of the cache is null, we got to this point
        # get the matrix data
        data <- x$get()
        # solve the matrix
        m <- Matrix::solve(data, ...)
        # fill the cache with the solved value
        x$setSolve(m)
        # return the solved value
        m
}
