# ------------------- subset methods ---------------------- 
# semble un poil moins moins rapide -- retester, avec dim.dual peut-être ?
# `[.dual` <- function(x, i, j, ..., drop = TRUE) {
#   mi <- missing(i)
#   mj <- missing(j)
#   if(mi) {
#     if(mj) { # i, j manquants
#       V <- x@x[,,..., drop = drop]
#       D <- sub_diff_mm(x@d, ..., drop = drop)
#     } else { # i manquant, j présent
#       V <- x@x[,,..., drop = drop]
#       D <- sub_diff_mj(x@d, j, ..., drop = drop)
#     }
#   } else {
#     if(mj) {
#       # i présent, j missing
#       if(is.null(dim(x@x))) { # vecteur
#         V <- x@x[i]
#         D <- sub_diff_i(x@d, i)
#       } else { # matrice
#         if(nargs() == 2L) { # appel x[i]
#           V <- x@x[i]
#           D <- sub_diff_i(x@d, i)
#         } else { # appel x[i,]
#           V <- x@x[i,, ..., drop = drop]
#           D <- sub_diff_im(x@d, i, ..., drop = drop)
#         }
#       }
#     } else { # i et j presents
#       V <- x@x[i, j, ..., drop = drop]
#       D <- sub_diff_ij(x@d, i, j, ..., drop = drop) 
#     }
#   }
#   fastNewDual(V,D)
# }       
#                                 
 
#' @rdname extract 
setMethod("[", c(x = "dual", i = "index", j = "index", drop = "ANY"),
    function(x, i, j, ..., drop) {
      V <- x@x[i, j, ..., drop = drop]
      D <- sub_diff_ij(x@d, i, j, ..., drop = drop)
      fastNewDual(V,D)
    })

#' @rdname extract 
setMethod("[", c(x = "dual", i = "missing", j = "index", drop = "ANY"),
    function(x, i, j, ..., drop) {
      V <- x@x[, j, ..., drop = drop]
      D <- sub_diff_mj(x@d, j, ..., drop = drop)
      fastNewDual(V,D)
    })

#' @rdname extract 
setMethod("[", c(x = "dual", i = "index", j = "missing", drop = "ANY"),
    function(x, i, j, ..., drop) {
      if(is.null(dim(x@x))) { # vecteur
        V <- x@x[i]
        D <- sub_diff_i(x@d, i)
      } else { # matrice
        if(nargs() == 2L) {  # appel x[i]
          V <- x@x[i]
          D <- sub_diff_i(x@d, i)
        } else { # appel x[i,]
          V <- x@x[i,, ..., drop = drop]
          D <- sub_diff_im(x@d, i, ..., drop = drop)
        }
      }
      fastNewDual(V,D)
    })

#' @rdname extract 
setMethod("[", c(x = "dual", i = "missing", j = "missing", drop = "ANY"),
    function(x, i, j, ..., drop) {
      V <- x@x[,, ..., drop = drop]
      D <- sub_diff_mm(x@d, ..., drop = drop)
      fastNewDual(V,D)
    })

