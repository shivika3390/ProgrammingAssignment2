##Repeated computations like matrix inversion are usually costly.
##Caching the inverse of a matrix provides some benefit.
##makeCacheMatrix and cacheSolve are two functions used to cache the inverse of a matrix.

##The below function will create a special "matrix" object and that will be able to cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv_m <- NULL
  set <- function(y)
    {
      x <<- y
      inv_m <<- NULL
    }
  get <- function() x
  setinverse <- function(solve) inv_m <<- solve
  getinverse <- function() inv_m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The below function will compute the inverse of the matrix using solve().
##It will first check if the inverse is already present in the cache based on previous run.
##If so then the same output would be returned after displaying the message and computation is skipped.

cacheSolve <- function(x, ...) {
        inv_m <- x$getinverse()
  if(!is.null(inv_m))
    {
    message("getting cached data")
    return(inv_m)
    }
  matrix <- x$get()
  inv_m <- solve(matrix)
  x$setinverse(inv_m)
  inv_m
}
