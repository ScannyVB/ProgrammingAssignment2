## Computing inverse of matrix is computationally costly so we are caching the inverse here for easier access
## makeCacheMatrix - This function is used to create the special matrix object that caches its inverse
## cacheSolve - This calculates the inverserequired for makeCacheMatrix

## Gets the inverse if its already created/computed

makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL
        set <- function(y) {
                x <<- y
                inv_mat <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv_mat <<- inverse
        getinverse <- function() inv_mat
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## computes the inverse using the "solve" function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_mat <- x$getinverse()
        if(!is.null(inv_mat)) {
                message("getting cached data")
                return(inv_mat)
        }
        data <- x$get()
        inv_mat <- solve(data, ...)
        x$setinverse(inv_mat)
        inv_mat
}