## My functions compute the inverse of a matrix. If the inverse of a matrix
## is previously computed, the cached result will be returned. 
## If not, the functions will do the computation and return the result.

## The first function creats and stores 4 functions. "get" returns the matrix x 
## stored in the main function. "set" changes the matrix stored in the main
## function. "setinv" sets the inverse matrix. "getinv" returns
## the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        get<-function() x
        set<-function(y){
                x<<-y
                inv<-NULL
        }
        setinv<-function(inverse) inv<<-inverse
        getinv<-function() inv
        ## The following line stores the 4 functions in the main function.
        list(get=get, set=set, setinv=setinv, getinv=getinv)
}




## The second function computes the inverse of a matrix. However, it first checks
## to see if the inverse has already been computed. If so, it gets the result 
## from the cache and skips the computation. Otherwise, it computes the inverse
## and cache the result via the "setinv" function.

cacheSolve <- function(x, ...) {
## If the inverse has already been cached, the cached result will be returned
## along with a message.
        inv<-x$getinv()
        if (!is.null(inv)) {
                message("getting cached data")
                return (inv)
        }
        
## If there is no result in the cache, the following lines will compute the 
## inverse of the matrix, cache the result, and return the result.  
        data<-x$get()
        inv<-solve(data,...)
        x$setinv(inv)
        inv     ## Return a matrix that is the inverse of 'x'
}
