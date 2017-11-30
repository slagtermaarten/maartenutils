load_devpackage <- function(pkg_name, 
                            dev_dir = file.path('~/libs', pkg_name)) { 
  if (dir.exists(dev_dir)) {
    if (!pkg_name %in% installed.packages()[, 1]) {
      ## we need to install at least once in order to get all pkg deps
      devtools::install(dev_dir)
    }
    devtools::load_all(dev_dir)
  } else {
    stopf('pkg dir of %s could not be found in %s', pkg_name, dev_dir)
  }
}

