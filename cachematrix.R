## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #the following will store the reversed matrix
        matrixInverse<-NULL
        
        #a setter
        set<-function(y){
                x<<-y
                matrixInverse<<-NULL
        }
        
        #a getter
        get<-function() x
        
        #a setter for the inverse
        setInverse<-function(solve) matrixInverse <<- solve
        
        #a getter for the inverse
        getInverse<-function() matrixInverse
        
        #putting everything is a list
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrixInverse<-x$getInverse()
        
        #if no inverse created then print the message and retunr the inverse 
        if(!is.null(matrixInverse)){
                message("getting cached reversed matrix")
                return(matrixInverse)
        }
        
        #if no inverse has been yet generated then generate one
        data<-x$get()
        matrixInverse<-solve(data, ...)
        
        #store the just calculated inverse
        x$setInverse(matrixInverse)
        
        #return the inverse
        matrixInverse
}


