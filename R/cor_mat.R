get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}


get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}


reorder_cormat <- function(cormat){
  cormat
  dd <- as.dist((1-cormat)/2)
  which(is.na(dd))
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}


#' Prefilter data for informative features before doing a correlation analysis
#'
#'
prefilter_correlation_data <- function(dtf, epsilon = 1e-5) {
  setDT(dtf)
  dtf <- 
    dtf[, !eps(apply(dtf, 2, var, na.rm = T), 0, epsilon = epsilon), with = F]
  return(dtf)
}


#' Compute coefficient of variation
#'
#' @param x Numeric vector
coef_variation <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }

  std_dev <- sqrt(var(x, na.rm = T)) 
  X_bar <- mean(x, na.rm = T)

  if (eps(X_bar, 0)) {
    return(0)
  } else {
    return(std_dev / X_bar)
  }
}


#' Create a correlation plot
#'
#'
create_corplot <- function(cormat, base_size = 6, print_coefs = F) {
  cormat <- reorder_cormat(cormat)
  upper_tri <- get_upper_tri(cormat)
  # Melt the correlation matrix
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  # Create a ggheatmap
  ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value)) +
    geom_tile(color = 'white') +
    scale_fill_gradient2(low = 'blue', high = 'red', mid = 'white', 
      midpoint = 0, limit = c(-1,1), space = 'Lab', 
      name='Spearman\nCorrelation') +
    theme_ms(base_size = base_size) + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    coord_fixed() + 
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(0, 1),
      legend.position = c(0.1, 0.9),
      legend.direction = 'horizontal')+
      guides(fill = guide_colorbar(#barwidth = 7, barheight = 1,
          title.position = 'top', title.hjust = 0.5))

  if (print_coefs) {
    ggheatmap <- ggheatmap + 
      geom_text(aes(Var2, Var1, label = round(value, 3)), color = 'black', 
        size = .8 * sqrt(base_size))
  }
  return(ggheatmap)
}
