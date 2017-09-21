#' Messaging function
#'
#' @noRd
mymessage <- function(instance = '', msg = '', f = message, ...) {
  nodename <- Sys.info()[['nodename']]
  # msg <- sprintf(msg, ...)
  long_msg <- sprintf('%s %s %s %s',
                      stringr::str_pad(string = instance, width = 20,
                                       pad = ' ', side = 'right'),
                      nodename, Sys.time(), msg)
  f(long_msg, ...)
}


mywarning = function(instance = '', msg = '', ...) {
  mymessage(instance = instance, msg = msg, f = warning, ...)
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


