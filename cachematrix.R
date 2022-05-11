## Put comments here that give an
overall description of what your
## functions do

## Write a short comment describing this function
##This function creats a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function()x
        setInverse<-function(Inverse) inv<<-Inverse
        getInverse<-function()inv
        list(set=set, get=get,setInverse=setInverse,getInverse=getInverse)             
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        inv<-x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(inv)
        }
        mat<-inv$get()
        inv<-solve(mat,...)
        x$setInverse(inv)
        inv
}
