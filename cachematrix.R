## Two functions are used get the inverse of a square matrix
## Step 1:  Prepare a special vector using makeCacheMatrix function
##        ma <- makeCacheMatrix(B)  B is a invertible square matrix

## Step 2: call cacheSolve with input being special vector created in step 1
##                   cacheSolve(ma)

## On calling cachSolve(ma) again, the result is got from cache and displayed


## makeCacheMatrix function creates a special vector for the specified invertible square matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Top level function which takes a special vector created by makeCacheMatrix function as input
## output will be the inverse 
## usage is:  First create a makeCacheMatrix object for a square matrix
##   example: ma <- makeCacheMatrix(B), where B is a square matrix
## call CacheResolve(ma)
## on calling the cacheResolve again with ma again, a message will be printed
## indicating that inverse is retrieved from cached data
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## check if the inverse for the matrix is already stored in cache
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## computing the inverse of the matrix for first time
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
