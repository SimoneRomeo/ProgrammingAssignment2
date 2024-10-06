## Put comments here that give an overall description of what your
## functions do

## Calculates the inverse matrix and stores it in a cache.
makeCacheMatrix <- function(x = matrix()) {
    #Initialize the inverse matrix as NULL
    inv_matrix <- NULL
    set <- function(y) {
        x <<- y
        inv_matrix <<- NULL
    }
    get <- function() x
    setInvMatrix <- function(solve) inv_matrix <<- solve
    getInvMatrix <- function() inv_matrix
    list( set = set, get = get,
          setInvMatrix = setInvMatrix,
          getInvMatrix = getInvMatrix)
}


## If the inversematrix already exists, send it.  Otherwise compute it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_matrix <- x$getInvMatrix()
    if(!is.null(inv_matrix)) {
        message("getting cached data")
        return(inv_matrix)
    }
    data <- x$get()
    inv_matrix <- solve(data)
    print(inv_matrix)
    x$setInvMatrix(inv_matrix)
    inv_matrix
}
