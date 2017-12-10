#' Messaging function
#'
#'
mymessage <- function(instance = as.character(dplyr::nth(sys.calls(), -2))[[1]],
                      msg = '', f = message, ...) {
  nodename <- Sys.info()[['nodename']]
  if (is.null(instance) || is.na(instance)) instance <- ''
  long_msg <- sprintf('%s %s %s %s',
                      stringr::str_pad(string = instance, width = 20,
                                       pad = ' ', side = 'right'),
                      nodename, Sys.time(), msg)
  f(long_msg, ...)
}


#' Messaging function raising a warning
#'
#'
mywarning <- function(instance = NULL, msg = '', ...) {
  mymessage(instance = instance, msg = msg, f = warning, call. = F, ...)
}


#' Messaging function raising an error
#'
#'
mystop <- function(instance = NULL, msg = '', ...) {
  mymessage(instance = instance, msg = msg, f = stop, call. = F, ...)
}


#' Messaging function, format message using sprintf
#'
#'
messagef <- function(msg, ...) {
  message(sprintf(msg, ...))
}


#' Warning function, format message using sprintf
#'
#'
warningf <- function(msg, ...) {
  warning(sprintf(msg, ...))
}


#' Stop function, format message using sprintf
#'
#'
stopf <- function(msg, ...) {
  stop(sprintf(msg, ...))
}
