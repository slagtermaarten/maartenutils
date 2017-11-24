hostname <- Sys.info()[["nodename"]]
servers <- c('paranoid', 'steroid', 'medoid', 'void', 'coley')
local_run <- !(hostname %in% servers)

# if (!local_run) {
#   pacman::p_load('doMC')
#   doMC::registerDoMC(cores = 16)
# }


if (!exists('debug_mode'))
  debug_mode <- F


myerror <- function() {
  vars <- ls(parent.frame(1))
  print(sapply(vars, function(x) head(get(x))))
  traceback()
}


#' Sum function wrapper that ignores NA values (substituting them with 0), but
#' does return NA if all values in the vector are NA
#'
#' @param vec \code{vector} of values to sum up
na_minded_sum <- function(vec) {
  if (all(is.na(vec))) {
    return(NA)
  } else {
    return(sum(vec, na.rm = T))
  }
}


#' Try and get more informative error messages or drop straight into debugger
#'
#'
perm_browser <- function(donor_id = '') {
  if (interactive()) {
    browser()
  } else {
    mymessage(donor_id, 'anomaly')
    print(ls())
    sapply(ls(), function(x) { print(x); print(str(get(x))) })
    return(NULL)
  }
}


#' Set names of vector to actual values in vector
#'
#'
auto_name <- function(vec, force = T) {
  if (force) {
    return(setNames(setNames(vec, NULL), as.character(vec)))
  } else {
    return(setNames(vec, as.character(vec)))
  }
}


eps <- function(v1, v2, epsilon = .01) {
  abs(v1 - v2) < epsilon
}


load_obj <- function(fn) {
  env <- new.env()
  nm <- load(fn, env)[1]
  env[[nm]]
}


percentage_change <- function(new, old) {
  if (old > 0)
    return(100 * (new - old) / old)
  else
    return(NA)
}


## "Not-in" operator
'%nin%' <- function(x,y) !('%in%'(x, y))


bisect_from <- function(li, val = 'TCGA-R6-A6Y0') {
  if (val %nin% li)
    stop(sprintf('%s not in list', val))
  li %>% .[seq_along(.) >= which(. == val)]
}


#' Set data.table column types for an existing data.table
#'
#' @param fh \code{data.table} object for which to change column types
#' @param col_classes named vector of data.types, with the columns of fh as
#' names
#'
#' This was written for debugging purposes, one can and should define column
#' types (if necessary) during data reading using data.table::fread()
set_dt_types <- function(fh, col_classes = NULL) {
  if (is.null(col_classes)) return(fh)
  if (null_dat(fh)) return(NULL)

  ## Filter down col_classes to relevant col classes
  cp <- col_classes[colnames(fh)] %>% { .[!is.na(.)] }
  fh[, (names(cp[cp == 'character'])) := lapply(.SD, as.character),
     .SDcols = names(cp[cp == 'character'])]
  fh[, (names(cp[cp == 'numeric'])) := lapply(.SD, as.numeric),
     .SDcols = names(cp[cp == 'numeric'])]
  fh[, (names(cp[cp == 'integer'])) := lapply(.SD, as.integer),
     .SDcols = names(cp[cp == 'integer'])]
  fh[, (names(cp[cp == 'factor'])) := lapply(.SD, as.factor),
     .SDcols = names(cp[cp == 'factor'])]
  fh[, (names(cp[cp == 'logical'])) := lapply(.SD, as.logical),
     .SDcols = names(cp[cp == 'logical'])]
  return(fh)
}


#' Check file name
#'
#' @param fn filename to check
#' @return T if something is wrong with filename, F otherwise
file_name_checks <- function(fn) {
  is.na(fn) || is.null(fn) || is.na(file.size(fn)) ||
    !file.exists(fn) || file.size(fn) == 0
}


#' Strip file names of (common) root structure
#'
#' Allow for more easily readable file names
#'
#' @param fn file name
#' @param root file name structure to strip away
#'
strip_root <- function(fn, root = rootFolder) {
  gsub(sprintf('%s/', path.expand(rootFolder)), '', fn)
}


#' Normalize data.table colnames
#'
#'
normalize_colnames <- function(dtf) {
  if ('data.table' %in% class(dtf)) {
    setnames(dtf, tolower(colnames(dtf)))
    setnames(dtf, gsub(" ", "_", colnames(dtf)))
    ## Get rid of terminating dots
    setnames(dtf, gsub("\\.$", "", colnames(dtf)))
  }
  return(dtf)
}


#' Generate file loading message including the file size
#'
#'
message_fn_size <- function(fn) {
  fs <- file.size(fn)
  size_units <- 'B'
  if (fs >= 1000) size_units <- 'KB'
  if (fs >= 1e6) size_units <- 'MB'
  if (fs >= 1e9) size_units <- 'GB'
  if (fs >= 1e12) size_units <- 'TB'
  correction_factor <- switch(size_units, 'B' = 1, 'KB' = 1e-3,
                              'MB' = 1e-6, 'GB' = 1e-9, 'TB' = 1e-12)
  sprintf('loading %s (%.1f %s)', strip_root(fn),
          fs * correction_factor, size_units)
}


#' Wrapper around data.table::fread()
#'
#' Predefine col classes but don't require all of the column to be present like
#' fread does
#'
w_fread <- function(fn, col_classes = NULL, use_fread = T,
                    verbose = T,
                    root_folder = path.expand('~/antigenic_space'),
                    normalize_colnames = F) {
  if (file_name_checks(fn)) {
    if (!is.na(fn)) {
      mymessage('w_fread', sprintf('could not read file %s',
                                   strip_root(fn, root = root_folder)))
    }
    return(NULL)
  }

  if (debug_mode || interactive()) {
    mymessage('w_fread', message_fn_size(fn))
  }

  header <- fread(fn, header = T, nrows = 1, verbose = debug_mode)
  ## If the previous failed, return NULL now
  if (is.null(header)) {
    mymessage('w_fread', sprintf('could not read header of file %s',
                                 strip_root(fn, root = root_folder)))
    return(NULL)
  }

  if (!is.null(col_classes)) {
    cols_absent <- base::setdiff(colnames(header), names(col_classes))
    if (length(cols_absent) > 0 && verbose) {
      mymessage('w_fread',
                sprintf('in %s, no specification for column(s): %s',
                        fn, paste(cols_absent, collapse = ", ")))
    }
  }

  if (use_fread == T) {
    if (!is.null(col_classes)) {
      cols_present <- base::intersect(colnames(header), names(col_classes))
      fh <- data.table::fread(fn, header = T,
                              colClasses = col_classes[cols_present],
                              verbose = debug_mode)
    } else {
      fh <- data.table::fread(fn, header = T, verbose = debug_mode)
    }
  } else {
    ext <- tools::file_ext(fn)
    fh <- as.data.table(switch(ext,
                               "tsv" = read.delim(fn),
                               "tsv" = read.delim(fn),
                               "csv" = read.csv(fn)))
    if (!is.null(col_classes)) {
      fh <- suppressWarnings(set_dt_types(fh, col_classes))
    }
  }


  if (normalize_colnames) {
    fh <- normalize_colnames(fh)
  }

  return(fh)
}


#' Set sensible defaults for \code{write.table}
write_tsv <- function(dat, output_file) {
    write.table(dat, file = output_file, quote = FALSE,
                sep = '\t', row.names = FALSE)
}


pick_first_non_NA <- function(...) {
  vec <- list(...)
  vec %>% { .[!is.na(.)][1] } %>% unlist
}


#' Delete a set of colnames for a data.table object
#'
#'
clean_columns <- function(instance = '', fh, col_names = c('mut_context')) {
  if (class(instance)[1] != 'character') {
    mymessage('clean_columns', 'instance is not of class character', f = stop)
  }
  fh <- as.data.table(fh)
  ## Extend to be deleted colnames with potential data.table merging errors
  col_names_m <- apply(expand.grid(col_names, c("", ".x", ".y")),
                       1, paste0, collapse = "")
  col_names_present <- intersect(col_names_m, colnames(fh))
  if (length(col_names_present) > 0) {
    mymessage(instance,
              sprintf('cleaning up: %s',
                      paste(col_names_present, collapse = ", ")))

    for (coln in col_names_present) {
      ## I encountered a data.table with two identically named columns, we have
      ## to be thorough here!
      while (coln %in% colnames(fh)) {
        fh[, (coln) := NULL]
      }
    }
  }
  return(fh)
}


sub_newlines <- function(x) gsub(' ', '\n', x)


#' Short hand for quick sample
qs <- function(x, N = 10) {
  sample(x, N)
}


#' Change colnames only conditional on their presence in data.table
#'
#' @param dtf \code{data.table} object in which to change column names
#' @param old_n \code{vector} of old potentially present column names
#' @param new_n \code{vector} of substitute new column names
cond_setnames <- function(dtf, old_n, new_n) {
  if ('data.table' %nin% class(dtf)) stop('I require a data.table object')
  cols_present <- intersect(colnames(dtf), old_n)
  idx <- base::match(cols_present, old_n)
  setnames(dtf, cols_present, new_n[idx])
  invisible()
}


w_lapply <- function(...) {
  args <- as.list(...)
  lapply(auto_name(args[[1]]), args[[2]])
}


#' Test whether variables are outliers
#'
#' @return vector of booleans, TRUE if not an outlier
test_non_outlier <- function(x, probs = c(.025, .975)) {
  bounds <- quantile(x, probs = probs)
  x >= bounds[1] & x <= bounds[2]
}


#' Subselect rows that do not contain any outliers
#'
#' Caution! Only use for plotting purposes
remove_outliers <- function(dtf, test_cols = colnames(test_cols),
                            by_cols = 'project',
                            probs = c(.025, .975)) {
  outlier_bools <- dtf[, lapply(.SD, test_non_outlier, probs = probs),
                       by = by_cols, .SDcols = test_cols]
  ## All variables must not be outliers
  bools <- outlier_bools[, rowSums(.SD) == length(test_cols),
                         .SDcols = test_cols]
  mymessage('remove_outliers', sprintf('filtering out %.2f percent',
                                       100 * (1 - mean(bools))))
  dtf[bools]
}


#' Look for colnames in data.frame/data.table
#'
#'
find_entries <- function(search_term = "hla", object = donor_summary) {
  if ('data.frame' %in% class(object)) {
    return(grep(pattern = search_term, x = colnames(object), value = T))
  } else if ('list' %in% class(object)) {
    return(grep(pattern = search_term, x = names(object), value = T))
  }
}
find_colnames <- find_entries


#' Clear all warnings
#'
#'
clear_warnings <- function() {
  unlockBinding("last.warning", baseenv())
  assign("last.warning", NULL, envir = baseenv())
  assign("last.warning", NULL, envir = globalenv())
}


#' Convert named vector to two-columned data.table
#'
#'
named_vec_to_dt <- function(vec, dtcolnames = names(fh)) {
  fh <- data.table(names(vec), vec)
  setnames(fh, dtcolnames)
  return(fh)
}


#' Table function that supports inclusion of unseen labels
#'
#'
my_table <- function(vec = c('A', 'A', 'C'), expected = c('A', 'B', 'C')) {
  ret <- table(vec)
  missing_labs <- auto_name(setdiff(expected, names(ret)))
  ret <- c(setNames(rep(0, length(missing_labs)), missing_labs), ret)
  return(ret[expected])
}
# my_table()
# my_table(c(''))


#' Remove object conditional on its existence in environment env
#'
#'
ifrm <- function(obj, ref = -1) {
  if (!is.character(obj)) {
    obj <- deparse(substitute(obj))
  }
  if (exists(obj, where = ref)) {
    rm(obj, pos = ref)
  } else {
    mymessage('ifrm', msg = paste0(obj, ' not found'))
  }
}
# a <- 4
# ifrm(a)
# a <- 4
# ifrm('a')
# ls('package:pacman')


check_columns <- function(dtf, query_colnames) {
  stopifnot(all(c('data.frame', 'data.table') %in% class(dtf)))
  missing_colnames <- setdiff(query_colnames, colnames(dtf))
  if (length(missing_colnames) > 0) {
    mymessage('check_colnames', 
              sprintf('missing colnames: %s', 
                      paste(missing_colnames, collapse = ', ')),
              f = stop)
  }
}


#' Search through (named) vector and return entries matching search query
#'
#'
filter_contains <- function(match, vec, search.names = T, ignore.case = T) {
  stopifnot(is.character(match), is.vector(vec))
  if (search.names == T) { 
    search_vec <- names(vec)
  } else {
    search_vec <- vec
  }
  ind <- as.logical(grepl(match, search_vec, ignore.case = ignore.case))
  vec[ind]
}


#' Back up a file, automatically generating a new file name in same directory
#'
#'
back_up <- function(fn = 'myfilename.txt') {
  ## Default back up file name
  idx <- 1
  bu_fn <- paste0(fn, '.bak.', idx)
  ## Generate new back up file if back up if bu file already exists and has
  ## different contents (i.e. different md5 check sum...) from source file
  while(file.exists(bu_fn) && tools::md5sum(bu_fn) != tools::md5sum(fn)) {
    idx <- idx + 1
    bu_fn <- paste0(fn, '.bak.', idx)
  }

  file.copy(fn, bu_fn)
  mymessage('back_up', sprintf('backup to %s', bu_fn))
}


#' View text file directly in R session
#'
#'
less <- function(fn) {
  system(sprintf('less -JN %s', fn))
}
