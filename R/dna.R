#' Compute complement of DNA nucleotide
#'
#' TODO: check for non nucleotide corresponding letters
dna_comp <- function(vec = c('A', 'G')) {
  sapply(strsplit(vec, ''), function(x) {
         paste0(nuc_subs[x], collapse = "")
  })
}
# dna_comp(c('A', 'TT', 'TG', 'Ch'))



#' Compute reverse complement of DNA nucleotide
#'
#' TODO: check for non nucleotide corresponding letters
dna_rev_comp <- function(vec = c('A', 'G')) {
  sapply(strsplit(vec, ''), function(x) {
         paste0(rev(nuc_subs[x]), collapse = "")
         })
}
# dna_rev_comp(c('A', 'TT', 'TG', 'Ch'))

