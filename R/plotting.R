gg_legend_alpha_cancel <-
  ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(alpha = 1)),
         colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))

gg_remove_x_labels <- ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                            axis.text.x = ggplot2::element_blank())



#' Wrapper around ggplot2::ggsave
#'
#' @param golden_ratio whether to apply golden ratio, if yes user only has to
#' supply height \code{h} and corresponding width will be set automatically
#' @param fn filename without extension - .png and .pdf files will be generated
w_ggsave <- function(fn, plot = last_plot(), plot_ratio = 'norm', h = 16, w = NA,
                     img_folder = fasanalysis::img_loc, units = 'cm', 
                     filetypes = c('pdf'), ...) {

  plot_ratio <- match.arg(plot_ratio, c('golden_ratio', 'square', 'norm'), 
                          several.ok = F)
  filetypes = match.arg(filetypes, choices = c('png', 'pdf', 'rds', 'grob'), 
                        several.ok = T)
  if (!dir.exists(img_folder)) dir.create(img_folder)
  lheight = h
  if (is.na(w)) {
    if (plot_ratio == 'golden_ratio') {
      lwidth = 1.5 * lheight
    } else if (plot_ratio == 'square') {
      lwidth = 1.2 * lheight
      plot <- plot + ggplot2::theme(aspect.ratio = 1)
    } else {
      lwidth = 1.3 * lheight
    }
  } else {
    lwidth = w
  }
  fns <- sapply(setdiff(filetypes, c('rds', 'grob')), function(ext)
                file.path(img_folder, sprintf('%s.%s', fn, ext)))
  lapply(fns, function(x)
         ggsave(x, plot = plot, height = lheight, width = lwidth, dpi = 800,
                units = units, limitsize = F, ...))
  if ('rds' %in% filetypes) {
    saveRDS(plot, file.path(img_folder, sprintf('%s.rds', fn)))
  }
  if ('grob' %in% filetypes) {
    # plot
    # gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot))
    gt <- to_g(plot)
    class(gt)
    saveRDS(gt, file.path(img_folder, sprintf('%s.grob.rds', fn)))
  }
}


#' Extract gtable from ggplot object 
#'
#'
to_g <- function(li, ...) {
  to_g_helper <- function(x) {
    if (all(c('gtable', 'gTree', 'grob', 'gDesc') %nin% class(x)) &&
        any(c('ggplot', 'gg') %in% class(x))) {
      return(ggplot2::ggplot_gtable(ggplot2::ggplot_build(x)))
    } else {
      return(x)
    }
  }

  ## If not a single ggplot object, assume an iterable object of graphic objects
  if ('gg' %nin% class(li)) {
    return(lapply(li, to_g_helper))
  } else {
    return(to_g_helper(li))
  }
}

normalize_grob_widths <- function(plots, norm_func = 'max', ...) {
  norm_func <- match.arg(arg = norm_func, choices = c('max', 'min'))
  if (norm_func == 'max') f <- grid::unit.pmax
  if (norm_func == 'min') f <- grid::unit.pmin
  gtabs <- to_g(as.list(plots, ...))
  ref <- do.call(f, lapply(gtabs, function(x) x$widths[2:3]))
  gtabs_c <- lapply(gtabs,
                    function(x) { x$widths[2:3] <- ref; return(x) })
  return(gtabs_c)
}


normalize_grob_heights <- function(plots, ...) {
  gtabs <- to_g(as.list(plots, ...))
  max_h <- do.call(grid::unit.pmax,
                       lapply(gtabs, function(x) x$heights[6]))
  gtabs_c <- lapply(gtabs,
                    function(x) { x$heights[6] <- max_h; return(x) })
  return(gtabs_c)
}
