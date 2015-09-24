## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL #m is used to store the result matrix
    set <- function(y){
        x <<- y
        m << NULL
    }
    get<-function() x #return the input matrix
    setreverse<- function(solve) m <<- solve #store the result matrix
    getreverse<- function() m #return the result matrix
    list(set=set,get=get, #output the list
         setreverse=setreverse,
         getreverse=getreverse)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m<-x$getreserve() #get the result matrix from x
    if(!is.null(m)){ #to judge if the input matrix is null or not
        message("getting cached reversed matrix")
        return(m)
    }
    data <-x$get() #using index, to get the matrix
    m<-solve(data)
    x$setreverse(m) #set it to the object
    m
}
