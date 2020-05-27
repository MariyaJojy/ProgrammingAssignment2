## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      inv<-NULL                                                  #setting inv (inverse)  to null
      set<-function(y){                                          #sets x to a y and resets inv to null  
        x<<-y
        inv<<-NULL
      }
      get<-function() x                                          #returns matrix x  
      setinv<-function(inverse) inv<<-inverse                    #sets inv to inverse
      getinv<-function() inv                                     #returns inv
      list(set=set,get=get,setinv=setinv,getinv=getinv)          #this is the special matrix returned by the makeCacheMatrix and contains these list parameters
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {                                #x is the special matrix
      inv<-x$getinv()                                           #puts the value of inv as the inverse matrix
      if(!is.null(inv)){                                        #if inv contains a cached matrix then that matrix is returned by the cacheSolve function 
        message("getting cached data")
        return(inv)
      }
      data<-x$get()                                             #otherwise the inverse is calculated and nad the new inverse matrix is also cached
      inv<-solve(data,...)
      x$setinv(inv)
      inv                                                       #returns the inverted matrix (not cached previously)
}
