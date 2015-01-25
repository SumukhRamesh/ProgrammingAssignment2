##cacheSolve and makeCacheMatrix are 2 functions written to compute the inverse
## of a matrix and store the computed inverse in cache. This allows for quick 
## computations of matrix inverse when dealing with large matrices

## Function to store the inverse of a matrix in cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) ##Used to set values of x to input matrix.
        {
                x <<- y
                m <<- NULL        ##Both m & x are set in the context of the function's parent environment 
        }
        
        get <- function() ## Fetches the value of x in the functions parent environment
        {
                x
        }
        
        getInverse <- function() ##Fetches the value of inverse of matrix x
        {
                m
        }
        
        setInverse <- function(solve) ## Stores the value of inverse of matrix x in the parent environment
        {
                m <<- solve
        }
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        
}

## Function to check for matrix inverse in cache. 
## If matrix inverse not present then compute the inverse of a matrix and store in cache
cacheSolve <- function(x, ...) {
        
        ## Below statement fetches the existing value of matrix inverse from cache
        m <- x$getInverse()
        
        ## If block to check m is Not Null and print the cached value
        if(!is.null(m))
        {
                message("getting cached data")
                return(m) ## Return a matrix that is the inverse of 'x'
        }
        
        ##In the below section we calculate the matrix inverse and store in cache
        data <- x$get()
        m <- solve(data, ...)
        
        x$setInverse(m) ##Set the matrix inverse to cache
        
        m  ## Return a matrix that is the inverse of 'x'
        
}