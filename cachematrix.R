## makeCacheMatrix function creates a special "matrix", 
##which is really a list containing a function to, 
##1.set the value of the matrix.2.get the value of the matrix
##3.set the value of the inverse of the matrix.4.get the value of the inverse of the mean.


makeCacheMatrix <- function(x = matrix()) {
    m<- NULL
    set<- function(y){
        x<<-y
        m<<-NULL
    }
    get<-function()x
    setcachesolve<-function(solve) m<<-solve
    getcachesolve<-function()m
    list(set=set,get=get,
         setcachesolve=setcachesolve,
         getcachesolve=getcachesolve)

}


## The following function calculates the inverse of the special 
##"matrix" created with the above function.But first it checks if the inverse already exists.
## if it does, returns "getting chached data" and skips the computation. if does not, then it
##calculates the inverse of the data and sets the inverse in the cache via
##the setcachesolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getcachesolve()
    if(!is.null(m)){
        message("getting chached data")
        return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setcachesolve(m)
    m
}
