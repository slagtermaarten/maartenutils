get_script_path <- function() {
  cmd.args <- commandArgs()
  if (length(cmd.args) == 1) {
    message('No command line args given, returning current working dir')
    return(getwd())
  }
  m <- regexpr('(?<=^--file=).+', cmd.args, perl=TRUE)
  script.dir <- dirname(regmatches(cmd.args, m))
  if (length(script.dir) == 0) {
    stop('cannot determine script dir: please call the script with Rscript')
  }
  if (length(script.dir) > 1) {
    stop('cannot determine script dir: more than one --file argument detected')
  }
  return(script.dir)
}
