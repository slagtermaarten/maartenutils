caplist_def <- c('UV', 'G2M', 'PAM50', 'IL6', 'IL2', 'PI3K', 'TNFa', 'NFkB',
                 'JAK', 'E2F', 'WNT', 'DNA', 'STAT3', 'STAT5', 'MYC', 'TGF',
                 'AKT', 'mTOR', 'mTORC', 'mTORC1', 'KRAS', 'of', 'UP', 'DN',
                 'in')

simple_cap <- function(x, split_regex = ' |_|,',
                       gene_regex = '[a-zA-Z0-9]+\\d+',
                       caplist = caplist_def) {
  stopifnot(class(x) == 'character')
  sapply(x, function(x_i) {
    ## Split into words
    w <- base::strsplit(x_i, split_regex)[[1]]

    ## Select gene symbols and capitalize
    w_g <- grepl(gene_regex, w)
    w[w_g] <- toupper(w[w_g])

    ## Replace with predefined symbols
    w_m <- match(tolower(w), tolower(caplist))
    for (idx in which(!is.na(w_m))) {
      w[idx] <- caplist[w_m[idx]]
    }

    ## Capitalize first letter of the words which have not been manually adjusted
    for (idx in setdiff(seq_along(w), which(!is.na(w_m)))) {
      w[idx] <- paste0(toupper(substring(w[idx], 1, 1)),
                       substring(w[idx], 2))
    }
    return(paste(w, collapse=' '))
  })
}
stopifnot(simple_cap('mtorc1 kras Kras1,Pam50') == 'mTORC1 KRAS KRAS1 PAM50')
# simple_cap('homo sapiens in laudanum erat')
# simple_cap('in horto sedent')


#' Compute frac and format into pretty string
#'
#'
format_frac <- function(num = 5, denom = 10, msg = NULL) {
  stopifnot(class(num) %in% c('integer', 'double', 'numeric'))
  stopifnot(class(denom) %in% c('integer', 'double', 'numeric'))
  stopifnot(denom > 0)
  sprintf('%s%d/%d (%.2f)', ifelse(is.null(msg), '', sprintf('%s: ', msg)), 
          num, denom, num/denom)
}
# format_frac(4, 6)
