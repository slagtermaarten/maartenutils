# options(error = myerror)
check_duplicated_rows <- function(dtf,
                                  by_cols = c('variant_id', 'transcript_id')) {
  # dtf[which(duplicated(dtf, by = by_cols))]
  lapply(1:nrow(dtf), function(i) {
      idx <- apply(dtf, 1, function(x) which(sum(x != dtf[i,]) < 1))
      dtf[c(i, idx), ]
  })
}


#' Merge files and run diagnostics
#'
#' @param maintain_attr attribute names to safeguard during merging
#' @param dup_priority how to handle doubling of column names, prioritise the
#' original table ('f') or the annotation table ('a')?
#' @param clean_up_f Function to apply after merging, typically to clean up some
#' recurrent problems
controlled_merge <- function(f_dtf, 
                             a_dtf,
                             by_cols = intersect(colnames(f_dtf),
                                                 colnames(a_dtf)),
                             cartesian = F,
                             dup_priority = 'a',
                             all.x = T, all.y = F,
                             all = F,
                             maintain_attr = NULL,
                             clean_up_f = function(x) x) {
  setDT(f_dtf)
  setDT(a_dtf)

  if (is.null(a_dtf) || nrow(a_dtf) == 0) {
    return(f_dtf)
  }

  all.x <- all.x || all
  all.y <- all.y || all

  if (!is.null(maintain_attr)) {
    maintain_attr <- maintain_attr[maintain_attr %in% names(attributes(f_dtf))]
    attr_backup <- lapply(auto_name(maintain_attr), function(a) attr(f_dtf, a))
  }

  if (check_merge_dups(a_dtf)) {
    mymessage('controlled_merge', 'detected merge dups in annotated df',
              f = stop)
  }

  if (is.null(by_cols) || is.na(by_cols)) {
    by_cols <- intersect(colnames(f_dtf), colnames(a_dtf))
    if (all(sort(colnames(a_dtf)) == sort(by_cols))) {
       warning('Inferred by_cols cover all columns in annotation dtf')
    }
  }

  missing_f <- setdiff(by_cols, colnames(f_dtf))
  if (length(missing_f) > 0) {
    mymessage('controlled_merge', sprintf('missing cols in f_dtf: %s',
                                          paste(missing_f, collapse = ', ')),
              f = stop)
  }
  missing_a <- setdiff(by_cols, colnames(a_dtf))
  if (length(missing_a) > 0) {
    mymessage('controlled_merge', sprintf('missing cols in a_dtf: %s',
                                          paste(missing_a, collapse = ', ')),
              f = stop)
  }

  ## 2017-11-06 08:40 Check col types of merge cols
  a_types <- unlist(a_dtf[, lapply(.SD, class), .SDcols = by_cols])
  f_types <- unlist(f_dtf[, lapply(.SD, class), .SDcols = by_cols])

  ## Mismatches when one of the two columns are character are harmless, coercion
  ## happens correctly
  char_types <- (a_types == 'character' | f_types == 'character')

  if (!all(a_types[!char_types] == f_types[!char_types])) {
    non_identical <- names(a_types)[which(a_types != f_types)]
    type_vec <- sprintf('%s (%s/%s)', non_identical, a_types[non_identical],
                        f_types[non_identical])

    mymessage('controlled_merge',
              sprintf('merge cols not of same type: %s',
                      paste(type_vec, collapse = ', ')),
              f = stop)
  }

  ## Merging on factors can be problematic when the levels aren't explicitly
  ## defined by the user
  ## Merge cols are of same type so we only have to test one
  if (any(a_types == 'factor') || any(f_types == 'factor')) {
    if (F) {
      mymessage('controlled_merge',
                sprintf('merge cols of type factor: %s',
                        paste(factor_types, collapse = ', ')),
                f = warning)
    }
    for (coln in names(f_types)[f_types == 'factor']) {
      f_dtf[, (coln) := as.character(get(coln))]
    }
    for (coln in names(a_types)[a_types == 'factor']) {
      a_dtf[, (coln) := as.character(get(coln))]
    }
  }

  ## Merge source and annotation df
  dtf_merged <- tryCatch(merge(f_dtf, unique(a_dtf, by = by_cols),
                               all.x = all.x,
                               all.y = all.y,
                               by = by_cols,
                               allow.cartesian = cartesian),
                         error = function(e) {
                           print(e)
                           browser()
                           intersect(colnames(dtf_merged), colnames(a_dtf))
                         })

  ## Check column count and names
  if (check_merge_dups(dtf_merged)) {
    dups <- grep(pattern = '\\.[x|y]$', x = colnames(dtf_merged),
                 perl = T, value = T)
    dups_clean <- unique(gsub('\\.(x|y)', '', dups))
    for (v in dups_clean) {
      f_var <- sprintf('%s.x', v)
      a_var <- sprintf('%s.y', v)
      if (dup_priority == 'f') {
        dtf_merged[is.na(get(f_var)), (f_var) := get(a_var)]
        dtf_merged[, (a_var) := NULL]
        setnames(dtf_merged, f_var, v)
      } else if (dup_priority == 'a') {
        dtf_merged[is.na(get(a_var)), (a_var) := get(f_var)]
        dtf_merged[, (f_var) := NULL]
        setnames(dtf_merged, a_var, v)
      }
    }
  }

  if (cartesian == F) {
    if ((nrow(dtf_merged) != nrow(f_dtf)) && all.y == F) {
      browser()
      # dup_variant_id <- dtf_merged[which(duplicated(dtf_merged,
      #                                               by = 'variant_id')),
      #                          variant_id]
      # a_dtf[variant_id == dup_variant_id]
    }
  }

  if (all(colnames(a_dtf) %nin% colnames(dtf_merged))) {
     mymessage('controlled_merge', 
               'annotation columns absent, merging failed',
               f = stop)
  }

  if (!is.null(maintain_attr) && length(maintain_attr) > 0) {
    for (a in names(attr_backup)) {
      attr(dtf_merged, a) <- attr_backup[[a]]
    }
  }

  dtf_merged <- clean_up_f(dtf_merged)
  keyv <- key(f_dtf)
  setkeyv(dtf_merged, keyv)
  return(dtf_merged)
}


#' Check whether merge did not result in bloat columns
#'
#' Check whether column names contain ".x" or ".y"
#'
#'
check_merge_dups <- function(dtf) {
  dups <- grep(pattern = '\\.[x|y]$', x = colnames(dtf), perl = T, value = T)
  if (!is.null(dups) && !is.na(dups) && length(dups) > 0) {
    # message(paste(sys.calls(), collapse = '\n'))
    # warning('Found duplicated colnames, revise code: ',
    #         paste(dups, collapse = ', '))
    return(T)
  } else {
    return(F)
  }
}


#' Check whether merged columns are identical
#'
#' For when suffering from QC-paranoia
#'
#'
verify_merge_equality <- function(merged) {
  dup_cn_x <- grep('.*\\.x$', colnames(merged), value = T)
  if (length(dup_cn_x) == 0) {
    message('no merge columns detected')
    return(NA)
  }
  dup_cn_y <- grep('.*\\.y$', colnames(merged), value = T)
  dup_cn_clean <- gsub('\\.y$', '', dup_cn_y)
  ret_val <- lapply(setNames(1:length(dup_cn_x), dup_cn_clean),
                    function(idx)
        setDT(merged)[, all(get(dup_cn_x[[idx]]) == get(dup_cn_y[[idx]]))]
  )
  if (all(unlist(ret_val))) {
    return(TRUE)
  } else {
    return(lapply(auto_name(names(ret_val))[!unlist(ret_val)],
                  function(varn) {
      merged[get(sprintf('%s.x', varn)) != get(sprintf('%s.y', varn))]
    }))
  }
  return(ret_val)
}


#' Clean up duplicated columns due to merging
#'
#'
clean_dup_cols <- function(dtf) {
  dup_cn_y <- grep('.*\\.y$', colnames(dtf), value = T)
  dup_cn_x <- grep('.*\\.x$', colnames(dtf), value = T)
  if (length(dup_cn_y) == 0 || length(dup_cn_x) == 0) return(dtf)
  dup_cn <- gsub('\\.x$', '', dup_cn_x)
  dtf[, (dup_cn_y) := rep(NULL, length(dup_cn_y))]
  setnames(dtf, dup_cn_x, dup_cn)
  return(dtf)
}


#' Deduplicate colnames of a data.frame/data.table
#'
#'
dedup_colnames <- function(dtf) {
  dup_colnames <- unique(colnames(dtf)[which(duplicated(colnames(dtf)))])
  colnames(dtf)[colnames(dtf) == dup_colnames] <- 
    colnames(dtf)[colnames(dtf) == dup_colnames] %>%
    { sprintf('%s.%d', ., seq_along(.)) }
  return(dtf)
}


#' Partition a data.table on all combinations of one or more factors
#'
#'
partition_dtf <- function(dtf, partition_vars = c(), min_size = 1) {
  setDT(dtf)
  partition_vars <- setdiff(partition_vars, NA)
  correct_types <- dtf[, sapply(.SD, class)] %>% 
    { names(.)[. %in% c('character', 'factor', 'integer')] } %>%
    intersect(partition_vars)

  if (!is.null(partition_vars) && length(partition_vars) > 0) {
    partition_levs <- purrr::map(auto_name(partition_vars),
      function(pv) {
      dtf[, c(NA, unique(get(pv)))]
    })
    partition_vars <- partition_vars[sapply(partition_levs, length) > 2]
    partition_vars_all_combs <- expand.grid(partition_levs)

    ## Create filtered objects
    f_dtf <- plyr::llply(1:nrow(partition_vars_all_combs), function(j) {
      levs <- partition_vars_all_combs[j, ]
      vars <- partition_vars[which(!is.na(levs))]
      l_dtf <- dtf
      for (j in seq_along(vars)) {
        l_dtf <- l_dtf[get(vars[j]) == levs[j]]
      }
      if (nrow(l_dtf) < min_size) {
        return(NULL)
      } else {
        return(l_dtf)
      }
    })
  } else {
    f_dtf <- list(dtf)
  }
  return(f_dtf)
}
