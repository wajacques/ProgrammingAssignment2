## R Programming Course - JHU/Coursera
## Programming Assignment 02 - William A. Jacques
## Date: August 23, 2014


## The function makeCacheMatrix simply defines methods for storing and recovering
## a matrix and it's inverse. 


makeCacheMatrix <- function(x = matrix()) 
{
    
  cached_matrix <- NULL
  cached_inverse <- NULL
  
  get <- function()
  {     
    
    cached_matrix
  
  }
  
  getinverse <- function()
  {
        
    cached_inverse
  
  }
  
  set <- function(y = matrix())
  {
    cached_matrix <<- y
    cached_inverse <<- NULL
  }
  
  setinverse <- function(z = matrix())
  {
    cached_inverse <<- z
  }

  list(get=get,getinverse=getinverse,set=set,setinverse=setinverse)

}


## The global variable methods is used to call the listed functions/methods
## defined by the makeCacheMatrix function

methods <- makeCacheMatrix()


## The function cacheSolve returns the inverse of an invertible matrix. 
## If the inverse is already cached on the system, the function simply returns it.  
## If not, the function computes the inverse, for the first time, and then store it on cache. 


cacheSolve <- function(x = matrix(), ...) 
{
         
  stored_matrix <- methods$get()  
  
  if(identical(stored_matrix,x))
  {
    
    message("Getting Cached Inverse")
    
    inverse <- methods$getinverse()
    
  }
  else
  {

    message("Computing Inverse and Storing Matrices")
      
    inverse <- solve(x)
    methods$set(x)
    methods$setinverse(inverse)
    
  }
  
  inverse

}
