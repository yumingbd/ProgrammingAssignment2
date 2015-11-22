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