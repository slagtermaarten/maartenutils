`%||%` <- function(a, b) {
  if (is.null(a)) return(b) else return(a)
}


#' Somewhat intelligently turn a variable into a flag to be used in a
#' filename
#'
#' The flag will be of the form '-{variable_name}={variable_value}'
#' unless the value is FALSE, NULL, NA or equal to the user-defined
#' default variable, in which case it will be '' (an empty string).
#'
#' @param arg Variable for which flag is to be generated
#' @param default Variable for which flag is to be generated
make_flag <- function(arg, default = NULL) {
  if (is.null(arg) || is.na(arg) ||
      (!is.null(default) && !is.logical(arg) && arg == default)) {
    flag <- ''
  } else if (is.logical(arg) &&
    (is.null(default) || default == TRUE)) {
    comp <- tryCatch(arg == (default %||% TRUE),
                     error = function(e) { browser() })
    flag <- ifelse(comp,
                   stringr::str_c('-', deparse(substitute(arg))), '')
  } else if (length(arg) == 1) {
    flag <- stringr::str_c('-', deparse(substitute(arg)), '=', arg)
  } else {
    stop('Unexpected input')
  }
  return(flag)
}
a = 5; stopifnot(make_flag(a) == '-a=5')
a = FALSE; stopifnot(make_flag(a) == '')
a = TRUE; stopifnot(make_flag(a, FALSE) == '-a=TRUE')
a = TRUE; stopifnot(make_flag(a) == '-a')
a = NULL; stopifnot(make_flag(a) == ''); rm(a)


print.mod_time_comparator <- function(fun) {
  msg <- paste('Mod time assessing function',
    ' minimum modification time is: ',
    environment(fun)$minimum_mod_time, '\n')
  cat(msg)
}
# f <- mod_time_comparator(minimum_mod_time = "2019-09-24 11:17:29 CEST")
# print(f)


#' Determine whether a file should be (re-)generated
#'
#' Create a function that compares the modification time of an argument
#' object to a pre-defined reference time
#'
mod_time_comparator <- function(
  minimum_mod_time = '2019-09-24 11:17:29 CEST',
  verbose = T) {

  force(minimum_mod_time)
  verbose_default <- verbose
  fun <- function(fns, verbose = verbose_default) {
    vapply(fns, function(fn) {
      if (is.null(fn)) {
        return(FALSE)
      } 
      ret_val <- 
        !file.exists(fn) || file.mtime(fn) < minimum_mod_time
      if (is.na(ret_val)) ret_val <- TRUE
      if (verbose) {
        mod_time_str <- if (is.na(file.mtime(fn))) {
          ''
        } else {
          sprintf(', mod time: %s', file.mtime(fn))
        }
        msg <- sprintf('%s %s %s', basename(fn),
          ifelse(ret_val, 'needs computation', 'already finished'),
          mod_time_str)
        if (ret_val) {
          msg <- crayon::blue(msg)
        } else {
          msg <- crayon::green(msg)
        }
        maartenutils::mymessage(
          instance = 'mod_time_comparator',
          msg = msg
        )
      }
      return(ret_val)
    }, logical(1))
  }
  class(fun) <- c('mod_time_comparator', 'function')
  return(fun)
}


default_mod_comparator <- function(arg) TRUE


print.result_caching_function <- function(f) {
  arg_string <- paste(names(formals(environment(f)$f)),
    collapse = ', ')
  func_string <- glue::glue('{environment(f)$f_name}({arg_string})')
  # if (is.character(environment(f)$f_name)) {
  #   # cat(glue::glue('{func_string} caching results in: \\
  #   #                {environment(f)$f_name}\n\n'))
  # } else {
  # }
  cat(glue::glue('{func_string} caching results by default\n'))
}


#' Wrap a (deterministic) function into a function that caches results of the
#' first function such that repeated calls with the same argument combinations
#' will not require repeated computation.
#'
#' The altered function will gain 5 arguments:
#'  1. \code{caching} (logical), whether to store results of the evaluation
#'  (default is TRUE)
#'  2. \code{redo} (logical), which allows redoing the computation even though a
#'  cached result is available already (TRUE, default is FALSE)
#'  3. \code{skip_missing} (logical), which allows not evaluating the function
#'  for argument combinations that haven't been evaluated yet (TRUE, default is
#'  FALSE). This is useful when a quick overview of already computed evaluated
#'  combinations is required.
#'  5. \code{max_tries} Max number of tries to compute the result (default:
#'  1). In development.
#' Otherwise, the function arguments will remain unaltered.
#'
result_cacher <- function(
  f,
  filename = 'f-x={{x}}.rds',
  allow_null_result = FALSE,
  verbose = F,
  min_mod_time = '2020-12-11 11:00') {

  f_name <- deparse(substitute(f))
  force(f)
  force(allow_null_result)
  force(min_mod_time)
  if (is.na(min_mod_time) || is.null(min_mod_time)) {
    min_mod_time <- '1989-01-01 00:00'
  }

  rc <- mod_time_comparator(min_mod_time, verbose = F)
  append_args <- list(redo = FALSE, caching = TRUE,
                      skip_missing = FALSE, load_try = 1)

  f_alt <- function() {
    forward_args <- as.list(environment())
    for (arg_name in names(append_args)) {
      forward_args[[arg_name]] <- NULL
    }
    # if (f_name == 'master_function') browser()
    # forward_args$arg_name <- NULL

    if (is.function(filename)) {
      formals(filename) <- forward_args
      filename <- purrr::exec(filename, !!!forward_args)
    } else if (is.character(filename)) {
      ## Evaluate filename to cache results into, populating the filename
      ## framework with the current arguments
      filename <- glue::glue_data(.x = forward_args, filename)
    } else if (is.null(filename)) {
      ## Override the default
      caching <- FALSE
    } else {
      stop('Unexpected type of the filename argument, ',
           'give function or character')
    }
    if (!is.null(filename))
      filename <- as.character(filename)

    # browser()
    # file.exists(filename)
    # rc(filename)

    if (is.null(filename) || rc(filename) || redo) {
      if (skip_missing) {
        return(NULL)
      }
      start <- Sys.time()
      res <- purrr::exec(f, !!!forward_args)
      end <- Sys.time()
      if (!is.null(filename) && caching && 
          (!is.null(res) || allow_null_result)) {
        if (!dir.exists(dirname(filename)))
          dir.create(dirname(filename), recursive = TRUE)
        if (!is.null(res)) {
					attr(res, 'cache_filename') <- filename
          attr(res, 'computation_time') <- end - start
          attr(res, 'caching_moment') <- Sys.time()
        }
        tryCatch(saveRDS(res, filename),
                 error = function(e) { print(e) })
      }
    } else {
      res <- tryCatch(readRDS(filename),
                      error = function(e) {
                        print(e); file.remove(filename); NULL
                      })
      if (verbose) {
        message('Reading in: ', filename, '\nMod time: ',
                attr(res, 'caching_moment'))
      }
      if (is.null(res)) {
        ## TODO call myself recursively and try computation again
        # match.call()
        # as.list(environment())
        # load_try = load_try + 1
      } else {
        attr(res, 'cache_filename') <- filename
      }
    }
    return(res)
  }

  formals(f_alt) <- c(formals(f), append_args)
  class(f_alt) <- c('result_caching_function', 'function')

  return(f_alt)
}


#' Overwrite original file with an updated version of it, maintaining
#' mod time
#'
#'
overwrite_updated_file <- function(obj, fn) {
  e_fn <- add_flag(fn, '_tmp')
  saveRDS(so, e_fn)
  system(glue('touch -r {fn} {e_fn}'))
  # print(file.mtime(fn)); print(file.mtime(e_fn))
  stopifnot(file.mtime(fn) == file.mtime(e_fn))
  system(glue::glue('mv {e_fn} {fn}'))
  return(invisible(T))
}
