## R Programming Coursera course Peer Assignment
## Work of Balazs Bosze
## Coursera profile link: https://www.coursera.org/user/i/4497541f854f48f33c5bb947c83580ab


## Inverting matrices can be quite time and resource consuming computations.
## It has benefits to compute and cache the matrix with its inverse instead of calculating
## the inverse repeatedly.
## makeCacheMatrix() function creates a special matrix that can cache its inverse.
## cacheSolve() function returns the inverse of that matrix, and caches it.


## makeCacheMatrix() creates a list that contains the matrix and can cache its inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        ## At first, the inverse is not cached, inv is NULL
        inv <- NULL
        
        ## Setting the value of the matrix to be cached
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## Get the value of the matrix
        get <- function() x

        ## Set the inverse matrix
        setinverse <- function(solve) inv <<- solve
        
        ## Get the value of the inverse matrix
        getinverse <- function() inv
        
        ## The result of the function is a list
        ## Setting the value of the matrix: m <- makeCacheMatrix(some.matrix)
        ## Setting the value of the inverse matrix: m$setinverse(inverse.matrix)
        ## Getting the value of the matrix: m$get()
        ## Getting the value of the inverse matrix: m$getinverse()

        ## Return value for the makeCacheMatrix() function: the list that can contain the matrix
        ## with its inverse matrix
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}

## cacheSolve() computes the inverse matrix of the special matrix created by makeCacheMatrix()
## and caches it
cacheSolve <- function(x, ...) {
        ## Checking if the matrix already has its inverse calculated
        inv <- x$getinverse()
        
        ## If yes, then we can get it from the cache
        if(!is.null(inv)) {
                ## Write out a message if we get the cached inverse matrix
               	message("getting cached data")
                return(inv)
        } else {
                ## If the inverse matrix is not yet created, we have to calculate it
                ## At first, get the matrix
                data <- x$get()
                
                ## Then calculate its inverse matrix
                inv <- solve(data, ...)
                
                ## Finally put the inverse matrix into the cache
                x$setinverse(inv)
        }
        
        ## Return value for the cacheSolve() function
        inv
}
