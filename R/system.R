#' Open file using Mac OS X open command
#'
#'
sys_file_open <- function(fn) {
  if (Sys.info()[['sysname']] == 'Darwin') {
    open_command <- 'open'
  } else if (Sys.info()[['sysname']] == 'Linux') {
    open_command <- 'xdg-open'
  }

  for (fni in fn) {
    system(sprintf('%s %s', open_command, fni))
  }
}


#' Extract file from tar archive
#'
#' @param archive fname of tar file to be extracted
#' @param fname file name of target file
extractFile <- function(archive, fname, extractquery) {
  fileList <- utils::untar(archive, list = TRUE)
  fileList.f <- grep(extractquery, fileList, value = T)

  extractRoot <- dirname(fname)
  extractSub <- strsplit(fileList, "/")[[1]][1]

  utils::untar(archive, files = fileList.f, exdir = extractRoot)
  file.rename(from = file.path(dirname(fname), fileList.f), to = fname)
  delFolder <- file.path(extractRoot, extractSub)
  unlink(delFolder, recursive = TRUE)

  return(invisible(fname))
}


#' Check whether md5 of target file corresponds to true md5, extract target file
#' if so and return filename of extracted file(s)
#'
#' @param (TCGA) project sequencing project name
#' @param archive file name of archive
#' @param md5file md5 filename to check output against
#' @param extractquery regular expression matching files in tar file listings
#' @return bool whether file download is complete
md5_extract <- function(archive, md5file, fname, extractquery) {
  if (file.exists(fname)) {
    return(T)
  }

  if (!file.exists(archive) || !file.exists(md5file)) {
    mymessage(project, 'archive and/or md5 do not exist yet')
    return(F)
  }

  orig_checksum <- strsplit(readLines(md5file), " ")[[1]][1]
  if (is.na(orig_checksum)) browser()
  download_checksum <- tools::md5sum(archive)
  if (is.na(download_checksum)) {
    mywarning('md5extract',
              sprintf('%s could not be check summed', md5file))
  }

  if (download_checksum == orig_checksum || is.na(download_checksum)) {
    extractFile(archive, fname, extractquery)
    return(T)
  } else {
    mymessage(project, paste('md5 do not match, deleting archive/md5 files',
                             'and redownloading'))
    unlink(archive)
    unlink(md5file)
    return(F)
  }

  return(F)
}


#' Inventorise partial files of larger object
#'
#' Partial file names must be formatted as {basename}_{idx/object_name}.rds
#'
#' @return data.frame of filenames and associated indices/object names
inventorise_partial_files <- function(full_fn, prefix = '') {
  file_pattern <- sprintf('^%s%s-\\d+\\.\\w+', prefix,
    gsub('\\.rds$', '', basename(full_fn)))
  files_root <- dirname(full_fn)
  listed_files <- list.files(files_root, pattern = file_pattern)
  if (length(listed_files) == 0) {
    warningf('No partial files found for: %s', full_fn)
    return(NULL)
  }
  dtf <- listed_files %>%
    { naturalsort::naturalsort(.) } %>%
    { .[!sapply(., function(x) is.null(x)) & !is.na(.)] } %>%
    {
      data.table(
        'idx' = as.integer(gsub('.*-(\\d+)\\.rds', '\\1', .)),
        'fn' = file.path(files_root, .)
      )
    }
  setkey(dtf, idx)
  return(dtf)
}


check_missing_partial_files <- function(full_fn, prefix = '',
  expected_extensions = 1:80) {
  dtf <- inventorise_partial_files(full_fn = full_fn, prefix = prefix)
  missing_ext <- setdiff(expected_extensions, dtf$idx)
  return(missing_ext)
}


#' Index is expected right before file name extension and after a hyphen
#'
#'
extract_idx_from_fn <- function(full_fns) {
  vapply(full_fns, function(fn) {
    if (is.null(fn) || is.na(fn) || length(fn) == 0)
      return(NULL)
    as.integer(gsub('.*-(\\d+)\\.\\w+$', '\\1', fn))
  }, integer(1))
}


#'  Prepend a character string to the basename of a filename
#'
#'
prepend_to_base_fn <- function(l_fn, pre = 'power_analysis_', post = F) {
  if (post == F) {
    file.path(dirname(l_fn), sprintf('%s%s', pre, basename(l_fn)))
  } else {
    pre <- prepend_hyphen(pre)
    l_fn <- gsub('(.*)(-\\d+)\\.(\\w+)', glue::glue('\\1{pre}\\2.\\3'), basename(l_fn))
    file.path(dirname(l_fn), l_fn)
  }
}


#'  Append a character string to the basename of a filename
#'
#'
append_to_base_fn <- function(l_fn, pre = 'power_analysis_') {
  prepend_to_base_fn(l_fn, pre, post = T)
}


#' Notify me of (important) messages via email
#'
#'
mail_notify <- function(subject = 'run_LOHHLA_partial', msg = 'tst',
  email_address = Sys.getenv('EMAIL')) {
  system(glue::glue('echo "{msg}" | mail -s "{subject}" -t {email_address}'))
}


#' Create overview of filenames and modification times
#'
#'
gen_file_overview <- function(dir, pat = '*', include_full = F) {
  overview <- list.files(dir, pat, full.names = include_root) %>%
    { data.table(short_fn = ., full_fn = file.path(dir, .)) } %>%
    .[, mtime := file.mtime(full_fn)] %>%
    .[order(mtime)]
  if (!include_full) {
    overview[, full_fn := NULL]
  }
  return(overview)
}
