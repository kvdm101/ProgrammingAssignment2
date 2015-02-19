## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    ##This is just to test the code    
    # mat1 <- matrix(data = c(9,12,1,6), nrow = 2, ncol = 2)
    # mat2 <- makeCacheMatrix(mat1)
    # cacheSolve(mat2)
    
    m<-NULL
    set<-function(y){#set function
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) m<<- solve   #set function call (solve())
    getmatrix<-function() m                 #Call function 
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    #check if function ran before
    if(!is.null(m)){ 
        message("getting cached data")
        return(m) #displays cached result
    }
    #no cached data, run function
    matrix <- x$get() 
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m #display function result
}
