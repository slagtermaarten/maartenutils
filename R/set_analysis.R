#' Compute Jaccard indices for matrix of booleans
#'
#' @param m logical matrix. If something else is supplied, coercion will be
#'          attempted
#' @param method method to use for overlap computation.
#'   Default is 'jaccard' for Jaccard matrix, alternatively use 'corrob'
#'   for corroboration matrix (FALSE)
overlap_analysis <- function(m, method = 'jaccard') {
  method <- match.arg(method, several.ok = F, 
    choices = c('jaccard', 'corroboration', 'count'))

  ## total counts for each column (filter)
  m <- Matrix::as.matrix(m)
  CS <- colSums(m, na.rm = T)

  ## count pairwise common entries
  A <- crossprod(m)

  ## indexes for non-zero common values
  im <- which(A > 0, arr.ind=TRUE)

  ## only non-zero values of common
  Aim <- A[im]

  ## Jaccard formula: #common / (#i + #j - #common)
  if (method == 'jaccard') {
    ret <- Matrix::sparseMatrix(
      i = im[, 1],
      j = im[, 2],
      x = Aim / (CS[im[, 1]] + CS[im[, 2]] - Aim),
      dims = dim(A)
    )
  } else if (method == 'corroboration') {
    ret <- Matrix::sparseMatrix(
      i = im[, 1],
      j = im[, 2],
      x = Aim / (CS[im[, 1]]),
      dims = dim(A)
    )
  } else if (method == 'count') {
    ret <- Matrix::sparseMatrix(
      i = im[, 1],
      j = im[, 2],
      x = Aim,
      dims = dim(A)
    )
  }
  colnames(ret) <- colnames(m)
  rownames(ret) <- colnames(m)
  ret <- as.matrix(ret)
  class(ret) <- c('set_matrix', 'matrix')
  return(ret)
}


#' Compute text labels for overlap matrix 
#'
#' Use with display_numbers argument to pheatmap
#'
#'
compute_overlap_labels <- function() {
  CM <- overlap_analysis(m, method = 'count')

  ## Quick and dirty, probably not very performant code
  if (method == 'corrob') {
    denum <- rep(diag(CM), each = ncol(CM))
  } else if (method == 'jaccard') {
    denum <- matrix(rep(diag(CM), each = ncol(CM)), ncol = ncol(CM)) 
    denum <- denum + t(denum) - CM
  } else if (method == 'count') {
    # pass
  }

  matrix(glue::glue('{as.vector(CM)} / {as.vector(denum)}'), 
    byrow = T, ncol = ncol(CM))
}
formals(compute_overlap_labels) <- formals(overlap_analysis)


#' Plot a set distance matrix
#'
#'
plot.set_matrix <- function(o_mat, cap_fun = simple_cap, ...) {
  colnames(o_mat) <- cap_fun(colnames(o_mat))
  rownames(o_mat) <- cap_fun(rownames(o_mat))
  txt_mat <- matrix(as.character(round(o_mat, 2)), nrow = nrow(o_mat))
  pheatmap::pheatmap(o_mat, ...)
}
