## Cache Matrix by Erika Hernandez

# This function stores a matrix and its inverse in 
# cache and declares the methods get (matrix), set (matrix), 
# getInverse and setInverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    setInverse <- function (inv = matrix()) inverse <<- inv
    getInverse <- function() inverse
    
    list(set = set, get = get, 
         setInverse = setInverse, getInverse = getInverse )
}


# This function return the inverse of a matrix 
# First, obtains the inverse of the matrix from cache, 
# if its NULL, then calculate the inverse, and stores it in cache

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    
    if (!is.null(inv)){
        message("getting cache data")
        return (inv)
    }
    
    m <- x$get()
    inv <- solve(m)
    x$setInverse(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
}