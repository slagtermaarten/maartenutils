sys_file_open <- function(fn) system(sprintf('open %s', fn))


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


#' Check whether md5 of target file corresponds to tru md5, extract target file
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
