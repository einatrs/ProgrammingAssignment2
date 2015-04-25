## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix gives a special matrix that stores the inverse of a given matrix 'x'
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
m = NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- inv
        getinv <- function() m
        list(set = set, get = get, setinv = setinv, getinv = getinv )

}


## Write a short comment describing this function
## this function calculates looks for the inverse of 'x' and returns it, or calculates it if it was not calculated in the makeCachMatrix above
cacheSolve <- function(x, ...) {
m <- x$getinv()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinv(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
