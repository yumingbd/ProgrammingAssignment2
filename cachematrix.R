## Caching the inverse of a matrix

## This function creates a list of four functions in a vector
## form. Those functions were designed to set the value of 
## a matrix, get the value of it, set the inverse of it, and
## get the inverse of it

makeCacheMatrix <- function(x = matrix()) 
{
   inv <- matrix()
   
   set <- function(y)
   {
      x <<- y
      inv <<- matrix()
   }
   
   get <- function() x
   
   setinverse <- function(inverse) inv <<- inverse
   
   getinverse <- function() inv
   
   list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function is to retrieve the inverse of a matrix by
## first checking if it's already calculated; if not, it 
## will return the inverse

cacheSolve <- function(x, ...) 
{
   inv <- x$getinverse()
   if(!is.null(inv))
   {
      message("getting cached data")
      return(inv)
   }
   
   data <- x$get()
   inv <- solve(data,...)
   x$setinverse(inv)
   inv
}