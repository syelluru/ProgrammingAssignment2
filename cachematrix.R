makeCacheMatrix <- function(x = matrix()) {
inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        getfunc <- function() x
        setInv <- function(inverse) inver <<- inverse
        getInv <- function() inver
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
       
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getInv()
        if (!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        mat <- x$get()
        inver <- solve(mat, ...)
        x$setInv(inver)
        inver
}
