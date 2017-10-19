#' Compute Jaccard indices for matrix of booleans
#'
#' @param m logical matrix. If something else is supplied, coercion will be
#'          attempted
#' @param method method to use for overlap computation.
#'   Default is 'jaccard' for Jaccard matrix, alternatively use 'corrob'
#'   for corroboration matrix (FALSE)
overlap_analysis <- function(m, method = 'jaccard') {
  library(Matrix)
  method <- match.arg(method, c('jaccard', 'corroboration'))

  ## total counts for each column (filter)
  m <- as.matrix(m)
  CS <- colSums(m, na.rm = T)

  ## Jaccard formula: #common / (#i + #j - #common)
  if (method == 'jaccard') {
    ## count pairwise common entries
    A <- crossprod(m)

    ## indexes for non-zero common values
    im <- which(A > 0, arr.ind=TRUE)

    ## only non-zero values of common
    Aim <- A[im]

    ret <- sparseMatrix(
      i = im[, 1],
      j = im[, 2],
      x = Aim / (CS[im[, 1]] + CS[im[, 2]] - Aim),
      dims = dim(A)
    )
  } else {
    NF <- ncol(m)
    ret <- Matrix(NA, nrow = NF, ncol = NF)

    for (i in 1:NF) {
      for (j in 1:NF) {
        AV <- !is.na(m[, i]) & !is.na(m[, j])
        ret[i,j] <- m[AV, i] %*% m[AV, j] / CS[i]
      }
    }
  }
  colnames(ret) <- colnames(m)
  rownames(ret) <- colnames(m)
  ret <- as.matrix(ret)
  class(ret) <- c('set_matrix', 'matrix')
  return(ret)
}


#' Plot a set distance matrix
#'
#'
plot.set_matrix <- function(o_mat) {
  colnames(o_mat) <- simple_cap(colnames(o_mat))
  rownames(o_mat) <- simple_cap(rownames(o_mat))
  txt_mat <- matrix(as.character(round(o_mat, 2)), nrow = nrow(o_mat))
  ## TODO when NMF 0.23 is released, move labels to left and top
  NMF::aheatmap(o_mat, txt = txt_mat, Rowv = 1:nrow(o_mat), Colv = 1:ncol(o_mat))
}
