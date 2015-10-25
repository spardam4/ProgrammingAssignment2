## Date: 10/25/2015 
## Created by Marlon Martinez
## In this exercice I created a list of functions to calculate de inverse of a matrix
## and validate if the calculation is cached

## the first function called makeCacheMatrix creates a list of functions including
## set= set the value of the matrix
## get= get the value of the matrix
## setinv= set the value of the inverse matrix
## getinv= get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set <- function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setinv<-function(solve){
                m<<-solve
        }
        getinv<-function(){
                m
        }
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function inverse the content of a matrix
## It returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        m<-x$getinv()
        if(!is.null(m)){
                message("getting cached data.")
                return (m)
        }
        data<-x$get()
        m<-solve(data)
        x$setinv(m)
        m
}

##example
##w<-matrix(11:14,2,2)
##m<-makeCacheMatrix(w)
##m$get()
##     [,1] [,2]
##[1,]   11   13
##[2,]   12   14
##cacheSolve(m)
##     [,1] [,2]
##[1,]   -7  6.5
##[2,]    6 -5.5
