#' @name inversion
#' @rdname inversion
#' @aliases det determinant solve
#' @title Determinant and matrix inversion for dual matrices
#'
#' @param x a dual matrix
#' @param logarithm if 'TRUE', get logarithm of modulus of determinant
#' @param a,b dual or numerical arguments for `solve`
#' @param ... extra parameters (ignored)
#'
#' @description Methods extending to dual matrices the corresponding methods for numeric matrices.
#'
#' @return 'det' returns a dual scalar, 'determinant' a list with components 'modulus' (which is a dual object)
#' and 'sign', and 'solve' returns a dual object (vector or matrix).
#'
#' @examples x <- dual( matrix(c(1,2,1,3), 2, 2) )
#' det(x)
#' d(det(x), "x1.1")
#' solve(x)
#' d(solve(x), "x1.1")
#'
#' @exportS3Method det dual
det.dual <- function(x, ...) { 
  V <- det(x@x)
  # using sum(t(X)*Y) = trace(X %*% Y)
  if(V != 0) {
    A <- V*t(solve(x@x))
  } else {
    A <- tadjugate(x@x)
  }
  D <- unclass(x@d)
  for(i in seq_along(D))
    D[[i]] <- sum(A * D[[i]])
  class(D) <- "differential"
  fastNewDual(V, D)
}
#' @rdname inversion
#' @export
setMethod("det", "dual", det.dual)

#' @rdname inversion
#' @exportS3Method determinant dual
determinant.dual <- function(x, logarithm = TRUE, ...) { 
  detx <- determinant(x@x, logarithm, ...)
  if(logarithm) {
    if(!is.infinite(detx$modulus)) {
      A <- t(solve(x@x))
    } else {  
      A <- x@x
      A[] <- NA_real_
    }
  } else {
    V <- detx$modulus # ! ne pas mettre le signe, c'est bien la dérivée de modulus qu'on veut
    if(V != 0) {
      A <- V*t(solve(x@x))
    } else {
      A <- tadjugate(x@x)
    }
  }
  
  D <- unclass(x@d)
  for(i in seq_along(D))
    D[[i]] <- sum(A * D[[i]])
  class(D) <- "differential"

  detx$modulus <- fastNewDual(detx$modulus, D)
  detx
}

#' @rdname inversion
#' @export
setMethod("solve", c(a = "dual", b = "dual"), function(a, b, ...) {
  Ai <- solve(a@x, ...)
  dA <- a@d
  dB <- b@d
  fastNewDual(Ai %*% b@x, substract_diff(matrixProdNuDi(Ai, dB) , matrixProdDiNu( matrixProdNuDi(Ai, dA), Ai %*% b@x )))
})

#' @rdname inversion
#' @export
setMethod("solve", c(a = "dual", b = "missing"), function(a, b, ...) {
  Ai <- solve(a@x, ...)
  dA <- a@d
  fastNewDual(Ai, matrixProdDiNu(matrixProdNuDi(-Ai, dA), Ai))
})

#' @rdname inversion
#' @export
setMethod("solve", c(a = "numericOrArray", b = "dual"), function(a, b, ...) {
  Ai <- solve(a, ...)
  dB <- b@d
  fastNewDual(Ai %*% b@x, matrixProdNuDi(Ai, dB))
})

#' @rdname inversion
#' @export
setMethod("solve", c(a = "dual", b = "numericOrArray"), function(a, b, ...) {
  Ai <- solve(a@x, ...)
  dA <- a@d
  fastNewDual(Ai %*% b, matrixProdDiNu( matrixProdNuDi(-Ai, dA), Ai %*% b ))
})


