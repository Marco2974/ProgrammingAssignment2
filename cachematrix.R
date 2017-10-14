## Cache the inverse of a matrix and check the cache
## before calculating the inverse of an identical matrix.

## makeCacheMatrix create a special "matrix", a list of methods:
## set = set the matrix in cache, get = get yhe value of matrix from cache
## set_inv = set the inverse in cache, get_inv = get the inverse from cache

makeCacheMatrix <- function(x = matrix()) {
       inv<-NULL
       set <- function(y) {
                 x <<- y
                 inv <<- NULL
       }
       get <- function() x
       set_inv <- function(solve) inv <<- solve
       get_inv <- function() inv
       list(set = set, get = get,
            set_inv = set_inv,
            get_inv = get_inv)
}


## cacheSolve checks if the matrix is already in the cache:
## if the matrix is stored, return the inverse from the cache,
## otherwise the inverse is calculated and the cache updated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inv()
        if(!is.null(inv)) {
                 message("getting cached data")
                 return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$set_inv(inv)
        inv
}
