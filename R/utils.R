write_excel <- function(dtf, fn) {
  pacman::p_load('openxlsx')
  hs <- createStyle(textDecoration = 'BOLD', fontColour = '#FFFFFF', 
    fontSize=12, fontName='Arial Narrow', fgFill = '#4F80BD')
  suppressWarnings(openxlsx::write.xlsx(dtf, fn, colWidths = 'auto', 
                                        colNames = TRUE, borders = "rows", 
                                        headerStyle = hs))
}


read_excel <- function(fn) {
  pacman::p_load('openxlsx')
  suppressWarnings(openxlsx::read.xlsx(fn))
}


#' Set attribute of object
#'
#' For use with magrittr/dplyr pipes
#'
set_attr <- function(x, att, value) {
  attr(x, att) <- value 
  return(x)
}


gen_table_grob <- function(dtf) {
  gridExtra::tableGrob(
    dtf, 
    rows = NULL,
    theme = gridExtra::ttheme_default(base_size = 6, 
      core = list(fg_params = list(parse=TRUE)),
      padding = grid::unit(c(2, 2), 'mm'),
      colhead = list(fg_params = list(parse=TRUE))))
}
