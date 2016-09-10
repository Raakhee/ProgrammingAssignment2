## Program to get the inverse of a matrix(square matrix or not); store it in cache; retrieve it later if needed 

## makeCacheMatrix functions returns a set of functions that cane be used to set up the matrix (passed as argument) and its inverse in memory and retrieve them when necessary 

makeCacheMatrix <- function(x = matrix()) {
     m_inv <- NULL
     setmat <- function(y) {
     	x <<- y
     	m_inv <<- NULL
     }
     stored_mat <<- x
     #print("stored mat is") 
     #print(stored_mat)
      
     getmat <- function() x
     setinv <- function(mat_inv) m_inv <<- mat_inv
     getinv <- function() m_inv
     print("inside makeCacheMatrix")
     list(setmat = setmat,  setinv =setinv, getmat=getmat, getinv = getinv)
  
}


## cacheSolve accepts the functions defined in makeCacheMatrix as a list argument to do the actual calculation to get the inverse of the matrix; 
## for square matrices, the solve() function, and for other matrices the ginv() function is used. The ginv() function needs to have the MASS library loaded 
## Use the library(MASS) command in R commandline before executing cacheSolve to get access to ginv() function .
## this function does not check for non-inversible matrices.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inv <- x$getinv()
        dat_mat <- x$getmat()        

        if ((!is.null(m_inv)) && (dat_mat == stored_mat)){
        	print("getting cached info")
        	return(m_inv)
        }
        
        if (ncol(dat_mat) == nrow(dat_mat)) {
       			 m_inv <- solve(dat_mat) 	
        }
        else{
        		m_inv <- ginv(dat_mat)
        }
        
        x$setinv(m_inv)
        stored_mat <<- dat_mat
        #print(m_inv)
        m_inv 
}

