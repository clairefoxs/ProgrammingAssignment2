## the makeCacheMatrix function stores a matrix and cache's its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y ## set the value of the matrix
                i <<- NULL ## set inverse equal to NULL
        }
        get <- function() x # get the value of the matrix
        setinv <- function(inv) i <<- inv ## set the value of the inverse
        getinv <- function() i ## get the value of the inverse
        list(set = set, get = get, 
             setinv = setinv,
             getinv = getinv) #return this list
}


## the following calculates the inverse of the matrix created with the above
## function

cacheSolve <- function(x, ...) {
        # set i equal to cached inverse
        i <- x$getinv()
        # if inverse has been cached, return cached data instead of calculating
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get() ## get matrix, x
        i <- solve(data) ## solve inverse of x
        x$setinv(i) ## cache inverse
        i ## Return a matrix that is the inverse of 'x'
}
