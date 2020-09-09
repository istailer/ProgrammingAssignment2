## My Assignment Week3: Caching the Inverse of a Matrix
## github 'istailer'. Thank's four your time

### Sorry, some comments are in Portuguese so I can understand in the future.

## Function to store the inverse of a matrix in the cache

makeCacheMatrix <- function(x = matrix()) {                     # valor padrao de x = matrix
        invM <- NULL                                            # invM com valor NULO
        set <- function(y) {                                    # funcao para 'setar' novo valor
                x <<- y
                invM <<- NULL
        }
        get <- function() x                                     # funcao para retornar o valor da matriz
        setinv <- function(invArg) invM <<- invArg              # funcao para atribui valor inverso
        getinv <- function() invM                               # funcao para obter o valor inverso
        list(set = set,                                         # criar lista para usar 'names' $
             get = get,
             setinv = setinv,
             getinv = getinv)

}


## Calculates the inverse of the Matrix (if necessary) of makeCacheMatrix

cacheSolve <- function(x, ...) {
        invM <- x$getinv()                                      # obtem o valor de x inverso
        if(!is.null(invM)){                                     # funcao para verificar se x não é NULO e retorno x inverso
                message("getting cached data")
                return(invM)
        }
        data <- x$get()                                         # 'data' obtem o valor de x
        invM <- solve(data,...)                                 # 'invM' atribui o valor da funcao 'solve' - estudar essa funcao
        x$setinv(invM)                                          # novo valor e "setado" para x
        invM                                                    # mostra o valor de x (matriz)
}
