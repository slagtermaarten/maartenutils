write_excel <- function(dtf, fn) {
  pacman::p_load('openxlsx')
  hs <- createStyle(textDecoration = 'BOLD', fontColour = '#FFFFFF', 
    fontSize=12, fontName='Arial Narrow', fgFill = '#4F80BD')
  suppressWarnings(openxlsx::write.xlsx(dtf, fn, colWidths = 'auto', colNames = TRUE, borders = "rows", headerStyle = hs))
}
