## These functions worked together to create a matrix, solve it's inverse, and return the result
## either from stored cache or from performing the solve() function. 

## makeCacheMatrix creates a list of matrix methods that can cache store it's inverse. 
makeCacheMatrix <- function(x = matrix()) {  #default is an empty matrix
  inverse <- NULL                           #Initially its inverse is set to NULL  
  set <-function(new_one){
    x <<- new_one   #superassignment makes the outer environment x and inverse equal to NULL
    inverse <<-NULL
  }
  get <-function() x    #basically returns itself if called
  set_inverse <- function(inversed) inverse <<-inversed     #cacheSolve will use this to set the inverse equal to solved inverse matrix
  get_inverse <- function () inverse     #returns what is stored in inverse, either NULL or actual inverse
  list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse) #list of matrix methods
}
  

## cacheSolve is a function that solves and returns the inverse of a matrix. 
##If it has already performed the inverse, it returns the stored answer via the get_inverse function in makeCacheMatrix function
cacheSolve <- function(x, ...) {
  inverse <-x$get_inverse()  #fetches the inverse
  if (!is.null(inverse)){    #if it has the inverse, it returns the stored inverse answer
    message("cached data retrieved")
    return (inverse)
  }
  answer <- x$get()           #gets itself
  inverse <- solve(answer)    #computes the inverse
  x$set_inverse(inverse)      #sets it own inverse to the answer
  inverse                      #returns the inverse
}
