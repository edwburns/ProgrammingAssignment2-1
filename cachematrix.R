## cachematrix has two funtions.
##
## The first makeCacheMatrix creates a listing that has functions to:
##  1. set the value of the matrix,
##  2. get the value of the matrix,
##  3. set the value of the inverse,
##  4. get the value of the inverse.
## It uses the function solve.
## 
##
makeCacheMatrix <- function(xx=matrix()){
# m <- NULL
  ne<-new.env()
  ne$x<-xx
  getnex<-function() xx <<- ne$x
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solved) m <<- solved
  getinverse <- function() m 
      
  list(getnex= getnex,set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

test_identity<-function(mtx1,mtx2){
   flag<-TRUE
   if(!is.matrix(mtx1)){flag<-FALSE} 
   if(!is.matrix(mtx2)){flag<-FALSE}
   if(flag==TRUE){
     if((nrow(mtx1)==nrow(mtx2))&(ncol(mtx1)==ncol(mtx2)))
     { r<- (mtx1==mtx2)
       l<-as.list(r)
       if(sum(as.integer(l))==length(l))
       {flag<-TRUE}else{flag<-FALSE}
     }
     else {flag<-FALSE}
   }
   return(flag)
}

## cacheSolve returns the value of the inverse matrix, the matrix
## which was the argument of makeCacheMatrix.

cacheSolve <- function(fnc, ...) {
## Return a matrix that is the inverse of 'x'
   if(test_identity(fnc$getnex(),fnc$get()))
   {
      im <- fnc$getinverse()
   }
   else
   {
      newm<-fnc$getnex()
      fnc$set(newm)
      im <- fnc$getinverse()
   } 
  
   if(!is.null(im))
   {
      message("getting cached data")
      return(im)
   } 
   else
   {
    data <- fnc$get()
    tryCatch(im <- solve(data, ...),
             error = function(e){message("an error\n",e)},
             warning = function(w){message("a warning\n",w)},
             finally = {
               fnc$setinverse(im)
               message("done")
               return(im)}
             )
  }
}

