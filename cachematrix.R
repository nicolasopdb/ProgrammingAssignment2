## These two functions complement each other. For a given matrix, the first one
## allows the user to store the matrix inverse in a cache. The second function
## looks to see if this cached value can be retrieved, and if not it calculates
## it.

## makeCacheMatrix() features the set function, which will replace the old 
## matrix with a new one if necessary and will reset the cached inverse value.
## It also features the setinv() function which will replace the old inverse
## value with the newly computed one. Finally, it features the get() and
## getinv() functions which will retrieve the values of the matrix and its
## inverse, respectively.

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function() x
    setinv<-function(inversenew) inv<<-inversenew
    getinv<-function() inv
    list(set=set, get=get,
    setinv=setinv,
    getinv=getinv)
}


## cacheSolve() tries to retrieve an existing inverse value for a specified
## matrix, stored in the function above. If there is no inverse, or the original
## matrix has been changed, it computes a new inverse and stores it in the
## special matrix's cache.

cacheSolve <- function(x, ...) {
    inv<-x$getinv()
    if(!is.null(inv)){
        message("getting cached data - matrix inverse")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data,...)
    x$setinv(inv)
    inv
}
