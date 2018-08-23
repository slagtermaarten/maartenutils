#' Lazily assign all objects in rds folder
#'
#' Avoid manual calls to readRDS and lazily load all files in a particular
#' folder
#'
#' @param source_dir directory to find rds's in
lazy_assign_rds <- function(source_dir = rds_dir,
                            assign_env = parent.env(environment())) {
                            # assign_env = as.environment('package:fasanalysis'))
                            # assign_env = parent.env(environment())) {
  ## R objects are named after rds file names
  file_names <- list.files(source_dir, pattern = '.*\\.rds')
  object_names <- gsub('\\.rds', '', file_names)
  rds_files <- setNames(list.files(source_dir, full.names = T, 
                                   pattern = '.*\\.rds'), object_names)
  messagef('Loading %d file%s in %s: %s', 
           length(rds_files), 
           ifelse(length(rds_files) > 1, 's', ''), 
           source_dir, 
           paste(object_names, collapse = ', '))

  ## Start with clean slate
  already_assigned <- intersect(names(rds_files), ls(assign_env))
  rm(list = already_assigned, envir = assign_env)

  ## Generate environments to do file look ups
  envs <- lapply(seq_along(rds_files), function(idx) {
    e <- new.env()
    e$file_name <- rds_files[idx]
    return(e)
  })

  ## Do assignments
  for (idx in seq_along(rds_files)) {
    obj_n <- names(rds_files)[idx]
    if (!any(ls(assign_env) == obj_n)) {
      delayedAssign(obj_n, readRDS(file_name), assign.env = assign_env,
                    eval.env = envs[[idx]])
    }
  }
  invisible()
}
