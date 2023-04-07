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
    w_m <- match(tolower(w), tolower(caplist), nomatch = NULL)
    for (idx in which(!is.na(w_m))) {
      w[idx] <- caplist[w_m[idx]]
    }

    ## Capitalize first letter of the words which have not been manually
    ## adjusted
    cap_words_idx <- setdiff(seq_along(w), which(!is.na(w_m) & !w_g))
    if (cap_first_word_only) {
      cap_words_idx <- setdiff(cap_words_idx, 
        seq(2, max(2, length(w))))
    }

    for (idx in cap_words_idx) {
      w[idx] <- paste0(toupper(substring(w[idx], 1, 1)),
                       substring(w[idx], 2))
    }
    return(paste(w, collapse=' '))
  })
}
# stopifnot(simple_cap('mtorc1 kras Kras1,Pam50') == 'mTORC1 KRAS KRAS1 PAM50')
# simple_cap('adipogenesis', cap_first_word_only = T)
# simple_cap('homo sapiens in laudanum erat', cap_first_word_only = T)
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


#' 'Fancy' scientific string formatting of exponents
#'
#' Returns a string in plotmath format or expression object
#'
fancy_scientific <- function(l, digits = 3, parse = F) {
  if (is.null(l)) return(NULL)
  l <- signif(l, digits = digits)
  ret_val <- lapply(l, function(li) {
    if (is.null(li)) return(NULL)
    if (any(is.na(li))) return(NA)
    ## We don't need scientific notation for numbers between .1 and 10
    if (abs(as.numeric(li)) > .1 && abs(as.numeric(li)) <= 10) {
      if (parse) {
        return(as.expression(li))
      } else {
        return(li)
      }
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
    # l_e <- gsub('\\^\\{\\-0*(\\d+)\\}', '^\\1', l_e)
    ## Try and get rid of new lines
    l_e <- gsub('\n', '', l_e)
    l_e <- gsub(' ', '', l_e)
    ## Return this as an expression
    if (parse) {
      return(parse(text=l_e))
    } else {
      return(l_e)
    }
  })
  return(ret_val)
}


fancy_p <- function(v) {
  purrr::map(fancy_scientific(v), ~glue::glue('italic(p)=={.x}'))
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



#' Compute SEM and CI of the mean
#'
#'
std_error <- function(x, ...) sd(x, ...) / sqrt(sum(!is.na(x)))
mean_CI <- function(x, Z = 1.96, ...) {
  rep(mean(x, na.rm = T, ...), 3) +
    c(-Z * std_error(x, na.rm = T, ...), 0, 
      Z * std_error(x, na.rm = T, ...)) %>%
  set_names(c('CI_l', 'mean', 'CI_h'))
}


print.mean_CI <- function(x, percentage = F, Z = 1.96, round = 2, ...) {
  res <- mean_CI(x, Z = Z, ...)
  if (percentage) {
    res <- scales::percent(res, accuracy = 10^(-1 * round))
  } else {
    if (!is.na(round) && is.numeric(round) || is.integer(round)) {
      res <- round(res, digits = round)
    }
  }
  ## Compute the size of the CI from the Z-value
  CI_perc <- 2 * pnorm(Z) - 1
  sprintf('%s (%s CI: [%s, %s])',
          res[2], scales::percent(CI_perc), res[1], res[3])
}


print.median_range <- function(x, percentage = F, round = 2,
                               lower_bound = 0, upper_bound = 1,
                               na.rm = T, ...) {
  stopifnot(lower_bound >= 0 && lower_bound < 1)
  stopifnot(upper_bound > 0 && upper_bound <= 1)
  res <- quantile(x, probs = c(lower_bound, .5, upper_bound), na.rm = T)
  if (percentage) {
    res <- scales::percent(res, round = 10^(-1 * round))
  } else {
    if (!is.na(round) && is.numeric(round) || is.integer(round)) {
      res <- round(res, digits = round)
    }
  }
  res <- as.character(res)
  if (lower_bound == 0 && upper_bound == 1) {
    return(sprintf('%s (range: [%s, %s])', res[2], res[1], res[3]))
  } else {
    return(sprintf('%s (%dth %%ile: %s, %dth %%ile: %s)',
                   res[2],
                   round(100 * lower_bound), res[1],
                   round(100 * upper_bound), res[3]))
  }
}
# print.median_range(rnorm(10, 1, 2), percentage = F, round = 2,
#                    lower_bound = .1, upper_bound = .9, na.rm = T)


extract_var_from_string <- function(char, var) {
  if (grepl(var, char)) {
    res <- gsub(sprintf('^.*%s=(.*)', var), '\\1', char)
    res <- gsub('-.*$', '', res)
    res <- infer_class(res)
  } else {
    res <- NA
  }
  return(res)
}


infer_class <- function(char) {
  stopifnot(is.character(char) || is.na(as.character(char)))
  char <- as.character(char)
  if (suppressWarnings(!is.na(as.numeric(char)))) {
    char <- as.numeric(char)
  }
  return(char)
}
