caplist_def <- c('UV', 'G2M', 'PAM50', 'IL6', 'IL2', 'PI3K', 'TNFa', 'NFkB',
                 'JAK', 'E2F', 'WNT', 'DNA', 'STAT3', 'STAT5', 'MYC', 'TGF',
                 'AKT', 'mTOR', 'mTORC', 'mTORC1', 'KRAS', 'of', 'UP', 'DN',
                 'in', 'NK')

#' Automated capitalizing of words
#'
#' @param gene_regex Regex to detect words that should be capped entirely
#' @param caplist List of class \code{vector} explicitly defining words and how
#' they should be capitalized, matching to these is done in a case-insensitive
#' manner
simple_cap <- function(x, split_regex = ' |_|,',
                       gene_regex = '[a-zA-Z0-9]+\\d+',
                       caplist = caplist_def,
                       cap_first_word_only = F) {
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

    ## Capitalize first letter of the words which have not been manually
    ## adjusted
    cap_words_idx <- setdiff(seq_along(w), which(!is.na(w_m)))

    if (cap_first_word_only) {
      cap_words_idx <- setdiff(cap_words_idx, seq(2, length(cap_words_idx)))
    }

    for (idx in cap_words_idx) {
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


#' Append duplicated entries in vector with appendix label
#'
#'
index_duplicates <- function(vec = c(1, 2, 2, 3)) {
  dup_entries <- unique(vec[which(duplicated(vec))])
  for (f in dup_entries) {
    no_dups <- sum(vec == f, na.rm = T)
    vec[vec == f] <- sprintf('%s.%d', as.character(f), 1:no_dups)
  }
  return(vec)
}
stopifnot(sum(duplicated(index_duplicates())) == 0)


#' Fancy scientific string formatting of exponents
#'
#' Returns a string in plotmath format
#'
fancy_scientific <- function(l, digits = 3) {
  if (is.null(l)) return(NULL)
  l <- signif(l, digits = digits)
  ret_val <- sapply(l, function(li) {
    if (is.null(li)) return(NULL)
    if (any(is.na(li))) return(NA)
    ## We don't need scientific notation for numbers between .1 and 10
    if (abs(as.numeric(li)) > .1 && abs(as.numeric(li)) <= 10) {
      return(as.expression(li))
    }
    ## Turn in to character string in scientific notation
    li <- format(li, scientific = TRUE)
    ## Quote the part before the exponent to keep all the digits
    li <- gsub("^(.*)e", "'\\1'e", li)
    ## Turn the 'e+' into plotmath format
    li <- gsub('e(.*)$', '%*%10^{\\1}', li)
    ## Some more edits
    ## We don't need to multiply by one
    l_e <- gsub("'1'%\\*%", "", li)
    ## Nor we do need to exponentiate numbers by the first power
    # l_e <- gsub("(.*)\\^\\+01", "\\1^", l_e)
    ## Replace numbers to their zeroth power to just one
    l_e <- gsub('\\d+\\^00', '1^', l_e)
    l_e <- gsub('%\\*%(.*)\\^\\+00', '', l_e)
    l_e <- gsub('\\^\\+(\\d+)', '^\\1', l_e)
    l_e <- gsub('\\^\\{\\+0*(\\d+)\\}', '^\\1', l_e)
    ## Try and get rid of new lines
    l_e <- gsub('\n', '', l_e)
    l_e <- gsub(' ', '', l_e)
    ## Return this as an expression
    parse(text=l_e)
  })
  return(ret_val)
}


#' Format a flag (i.e. name and value pair) for use in a filename
#'
#'
format_flag <- function(val, name) {
  if (is.null(val) || is.na(val)) {
    val <- 'NA'
  }
  if (is.character(name))
    name <- variabilize_character(name)
  if (is.character(val))
    val <- variabilize_character(val)
  return(glue::glue('-{name}={val}'))
}
