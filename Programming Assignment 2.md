# makeCacheMatrix(): creates a special “matrix” object that can cache its inverse.
# cacheSolve(): computes the inverse of the “matrix” returned by makeCacheMatrix(). If the inverse has already been calculated and the matrix has not changed, it’ll retrieves the inverse from the cache directly.


makeCacheMatrix <- function(x = matrix()) {
       
        inv = NULL
        set = function(y) { 
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
        
        inv = x$getinv()
        
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        x$setinv(inv)
        
        return(inv)
}
