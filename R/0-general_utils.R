hostname <- Sys.info()[["nodename"]]
servers <- c('paranoid', 'steroid', 'medoid', 'void', 'coley')
local_run <- !(hostname %in% servers)

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


gen_dump_fn <- function(instance, dump_dir = '~/R_dumps') {
  if (!dir.exists(dump_dir)) dir.create(dump_dir)
  file.path(dump_dir, sprintf('%s_%s.Rframes', instance, 
                              format(Sys.time(), '%Y-%m-%d_%H:%M')))
}


#' Try and get more informative error messages or drop straight into debugger
#'
#'
perm_browser <- function(instance = base::get('donor_id', inherits = T), 
                         dump_dir = '~/R_dumps') {
  if (interactive()) {
    browser(skipCalls = 1L)
  } else {
    dump_fn <- gen_dump_fn(instance = instance, dump_dir = dump_dir)
    dump.frames(dumpto = dump_fn, to.file = TRUE,
                include.GlobalEnv = FALSE)
    # debugger(dump = last.dump)
    mymessage(instance, 'anomaly')
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


#' Search call stack for item
#'
#' @param name Object to look for in call stack	
#' @param env Environment to start looking in. 
callstack_get <- function(name, env = parent.frame()) {
  if (identical(env, emptyenv())) {
    stop("Can't find ", name, call. = FALSE)
  } else if (exists(name, envir = env, inherits = FALSE)) {
    get(name, envir = env)
  } else {
    callstack_get(name, parent.env(env))
  }
}


eps <- function(v1, v2 = 0, epsilon = .01) {
  abs(v1 - v2) < epsilon
}


load_obj <- function(fn) {
  env <- new.env()
  nm <- load(fn, env)[1]
  env[[nm]]
}


percentage_change <- function(new, old) {
  stopifnot(is.numeric(old))
  stopifnot(is.numeric(new))
  if (old > 0)
    return(100 * (new - old) / old)
  else
    return(NA)
}


#' "Not-in" operator
#'
#'
'%nin%' <- function(x,y) !('%in%'(x, y))


#' "NAND' operator
#'
#'
'%nand%' <- function(x, y) !(x & y)
nand <- function(x, y) !(x & y)


#' Replace NA values
#'
#'
replace_na <- function(vec, replacement = 0) {
  vec[is.na(vec)] <- replacement
  return(vec)
}


bisect_from <- function(li, val = 'TCGA-R6-A6Y0') {
  if (val %nin% li)
    stop(sprintf('%s not in list', val))
  li %>% .[seq_along(.) >= which(. == val)]
}


#' NULL data detection, both non-existing or zero row data.frames/data.tables
#'
#' @return TRUE if data is of class \code{data.frame} or \code{data.table} and
#' not NULL
null_dat <- function(dtf) {
  if (is.null(dtf)) return(TRUE)
  # if (!(any(class(dtf) == 'data.frame') || any(class(dtf) == 'data.table'))) 
  #   return(TRUE)
  if (nrow(dtf) == 0 || ncol(dtf) == 0) return(TRUE)
  ## Got here
  return(FALSE)
}


#' Set data.table column types for an existing data.table
#'
#' @param fh \code{data.table} object for which to change column types
#' @param col_classes named vector of data.types, with the columns of fh as
#' names
#' @param convert_commas Whether to treat commas in character to numeric
#' conversions (as Excel likes to write out) as decimal separators
#'
#' This was written for debugging purposes, one can and should define column
#' types (if necessary) during data reading using data.table::fread()
set_dt_types <- function(fh, col_classes = NULL, convert_commas = T) {
  if (is.null(col_classes)) return(fh)
  if (null_dat(fh)) return(fh)

  convert_numeric <- function(vec) {
    if (convert_commas && any(class(vec) == 'character')) {
      return(as.numeric(gsub(',', '.', vec)))
    } else {
      return(as.numeric(vec))
    }
  }

  ## Filter down col_classes to relevant col classes
  cp <- col_classes[colnames(fh)] %>% { .[!is.na(.)] }
  if (length(cp) == 0) {
    messagef('No columns to convert')
    return(fh)
  }

  if (any(cp == 'character')) {
    suppressWarnings(fh[, (names(cp[cp == 'character'])) := 
                     lapply(.SD, as.character),
     .SDcols = names(cp[cp == 'character'])])
  }

  if (any(cp == 'numeric')) {
    suppressWarnings(fh[, (names(cp[cp == 'numeric'])) := 
                     lapply(.SD, convert_numeric),
       .SDcols = names(cp[cp == 'numeric'])])
  }

  if (any(cp == 'integer')) {
    suppressWarnings(fh[, (names(cp[cp == 'integer'])) := 
                     lapply(.SD, as.integer),
       .SDcols = names(cp[cp == 'integer'])])
  }

  if (any(cp == 'factor')) {
    suppressWarnings(fh[, (names(cp[cp == 'factor'])) := 
                     lapply(.SD, as.factor),
       .SDcols = names(cp[cp == 'factor'])])
  }

  if (any(cp == 'logical')) {
    suppressWarnings(fh[, (names(cp[cp == 'logical'])) := 
                     lapply(.SD, as.logical),
       .SDcols = names(cp[cp == 'logical'])])
  }

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
#' @param fn File name for which to isolate root
#' @param root File name structure to strip away
#'
strip_root <- function(fn, root_folder = path.expand('~/')) {
  if (!grepl(root_folder, fn)) {
    if (!file.exists(file.path(root_folder, fn)) && 
        !dir.exists(file.path(root_folder, fn))) {
      mywarning(msg = sprintf('%s does not seem to have root %s', fn, root_folder))
    }
  }
  gsub(sprintf('%s/', path.expand(root_folder)), '', fn)
}


#' Turn character string into a variable name
#'
#' Create variable names separated by underscores from string
#' This has sufficed for me thus far, probably not all encompassing though
#'
#' @param vec Character vector to edit
#'
variabilize_character <- function(vec) {
  stopifnot(is.character(vec))
  vec %>% 
    tolower %>%
    { gsub("-|\\.|\\s", "_", .) } %>%
    # { gsub("^\\s*|\\s*$", "", .) } %>% ## Get rid of prefixed or postfixed spaces
    { gsub("^_*|_*$", "", .) } ## %>% ## Get rid of prefixed or postfixed underscores
    # { gsub("_*$", "", .) } ## Get rid of terminating underscores
}


#' Normalize data.table colnames
#'
#'
normalize_colnames <- function(dtf) {
  if ('data.table' %in% class(dtf)) {
    setnames(dtf, variabilize_character(colnames(dtf)))
  } else if (any(c('data.frame', 'matrix') %in% class(dtf))) {
    colnames(dtf) <- variabilize_character(colnames(dtf))
  }
  return(dtf)
}


#' Generate file loading message including the file size
#'
#'
message_fn_size <- function(fn, root_folder = '~/') {
  fs <- file.size(fn)
  size_units <- 'B'
  if (fs >= 1000) size_units <- 'KB'
  if (fs >= 1e6) size_units <- 'MB'
  if (fs >= 1e9) size_units <- 'GB'
  if (fs >= 1e12) size_units <- 'TB'
  correction_factor <- switch(size_units, 'B' = 1, 'KB' = 1e-3,
                              'MB' = 1e-6, 'GB' = 1e-9, 'TB' = 1e-12)
  sprintf('loading %s (%.1f %s)', strip_root(fn, root_folder = root_folder),
          fs * correction_factor, size_units)
}


#' Wrapper around data.table::fread()
#'
#' Predefine col classes but don't require all of the column to be present like
#' fread does
#'
w_fread <- function(fn, col_classes = NULL, use_fread = T,
                    verbose = F,
                    root_folder = path.expand('~/'),
                    normalize_colnames = F) {
  if (file_name_checks(fn)) {
    if (!is.na(fn) && verbose) {
      mymessage('w_fread', sprintf('could not read file %s',
                                   strip_root(fn, root_folder = root_folder)))
    }
    return(NULL)
  }

  if ((debug_mode || interactive()) && verbose) {
    mymessage('w_fread', message_fn_size(fn, root_folder = root_folder))
  }

  header <- fread(fn, header = T, nrows = 1, verbose = verbose)
  ## If the previous failed, return NULL now
  if (is.null(header)) {
    if (verbose) {
      mymessage('w_fread', sprintf('could not read header of file %s',
          strip_root(fn, root = root_folder)))
    }
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


#' Check whether file is write accessible to current user
#'
#'
write_accessible <- function(filename) {
  if (!file.exists(filename))
    return(write_accessible(dirname(filename)))

  return(file.access(filename, mode = 2) == 0)
}


#' Write tsv using \code{write.table}
#'
#'
write_tsv <- function(dat, output_file) {
  if (write_accessible(output_file)) {
    write.table(dat, file = output_file, quote = FALSE,
                sep = '\t', row.names = FALSE)
  } else {
    mymessage('write_tsv', sprintf('cannot write to %s', output_file))
  }
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
  invisible(dtf)
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
  dtf <- as.data.table(dtf)
  ## Test which observations are outliers by at least one variable
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
cond_rm <- function(..., env = globalenv(), verbose = F) {
  fcall <- match.call()
  ## Strip away function name
  object_list <- as.list(fcall)[-1]
  ## Strip away other arguments to function call
  if (!is.null(names(object_list))) {
    ## Determine what named arguments need to disappear
    o_names <- intersect(names(object_list), c('env', 'verbose'))
    object_list <- object_list[names(object_list) %nin% o_names]
  }
  object_list <- unique(as.character(object_list))

  for (obj in object_list) {
    if (exists(obj, envir = env)) {
      rm(list = c(obj), envir = env)
    } else if (verbose) {
      mymessage(msg = paste0(obj, ' not found'))
    }
  }
}


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


#' Inspect a matrix without plotting all of the dimension annotation
#'
#'
inspect_mat <- function(mat, nrow = 5, ncol = nrow) {
  print(dim(mat))
  nrow <- min(nrow(mat), nrow)
  ncol <- min(ncol(mat), ncol)
  print(mat[1:nrow, 1:ncol])
}


#' Filter out rows with at least one NA value
#'
#'
filter_na_rows <- function(dtf) {
  dtf[apply(dtf, 1, function(x) !any(is.na(x))), ]
}


#' Read RDS file or return NULL
#'
#'
cond_readRDS <- function(fn) { 
  if (file.exists(fn)) readRDS(fn)
  else return(NULL)
}


#' Z-transform a vector 
#'
#'
z_transform <- function(vec) {
  vec <- unlist(vec)
  return((vec - mean(vec, na.rm = T)) / sd(vec, na.rm = T))
}


#' Change attribute of an object, returning the object directly
#'
#' Useful in piped chains of commands like dplyr advocates
attr_pass <- function(x, attribute = 'class', value = 'test') {
  attr(x, attribute) <- value
  return(x)
}


#' Detect outliers in vector, defined as values exceeding 1.5 * IQR
#'
#'
detect_outliers <- function(vec) {
  stopifnot(is.numeric(vec) || is.integer(vec))
  quantile(vec, probs = c(.25, .75)) %>%
    { diff(range(.)) * 1.5 } %>%
    { . * c(-1, +1) + quantile(vec, probs = c(.5)) } %>%
    { vec <= .[1] | vec >= .[2] }
}


#' Transpose a data.table, maintaining row names 
#'
#'
transpose_data.table <- function(dtf) {
  old_class <- class(dtf)
  row_names <- rownames(dtf)
  col_names <- colnames(dtf)
  dtf <- t(dtf)
  if ('data.table' %in% old_class) {
    dtf <- as.data.table(dtf)
  }
  colnames(dtf) <- row_names
  rownames(dtf) <- col_names
  return(dtf)
}


#' Export data set to table view in browser
#'
#'
explore_in_browser <- function(dtf, ...) {
  DT::datatable(dtf, filter = 'top', ...)
}


#' Turn named vector into data.table
#'
#'
named_vec_to_dt <- function(vec, name_var = NULL, value_var = NULL) {
  name_var <- name_var %||% 'name'
  value_var <- value_var %||% 'value'
  dtf <- data.table('name' = names(vec), 'value' = vec)
  setnames(dtf, c('name', 'value'), c(name_var, value_var))
  return(dtf)
}


systemf <- function(com, ...) {
  system(sprintf(com, ...))
}
