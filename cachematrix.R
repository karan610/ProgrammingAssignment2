## Created by Karan Chadha as a part of the 2nd assignement for Cousera
## - R Progaramming module

## Creating function for getting a super list
## using super asignment operator

makeCacheMatrix <- function(x = matrix()) {
    ##Assigning Null to Inverse
    inverse <- NULL

    ##Defining Set Function
    ##Also assigning Null to inverse because the matrix is getting set again
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }

    ##Defining functions
    get<-function() x
    setinverse <- function(inverse_arg) inverse <<- inverse_arg
    getinverse <- function() inverse

    ##Creating a list of functions to be returned
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


##Creating a function to get the inversed value of a matrix
##In case the value has been revived earlier, retrieving it from cache

cacheSolve <- function(x,...){

    ##Getting inverse, if it already exists in cache
    inverse <- x$getinverse()

    ##If a vallue has been returned from cache then returning that value
    ## and also printing a message on colsole
    if(!is.null(inverse)){
        message("data from cache")
        return(inverse)
    }

    ##If no value has been returned from cache then getting the matrix
    ##and then getting an inverse of it using score
    ##and then returning the inverse using setinverse function
    data<-x$get()
    inverse<-solve(data)
    x$setinverse(inverse)
    inverse
}
