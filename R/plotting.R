#' Another ggplot theme
#'
#'
theme_ms <- function(base_size = global_base_size, base_family = 'sans',
                     rotate_labels = NA, legend_pos = 'bottom', ...) {
  l_theme <-
    ggplot2::theme_grey(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      rect = element_rect(fill = 'white', linetype = 0, color = NULL),
      text = element_text(size = base_size, family = base_family),
      title = element_text(hjust = 0.0),
      axis.text = element_text(size = rel(0.8)),
      plot.title = element_text(size = rel(.9), hjust = .5),
      axis.title = element_text(size = rel(1.0), hjust = .5),
      legend.position = legend_pos,
      legend.key.size = unit(10, 'mm'),
      legend.title = element_text(size = rel(0.7), hjust = .5),
      legend.margin = margin(1, 1, 1, 1, unit = 'mm'),
      legend.spacing = unit(10, 'mm'),
      panel.spacing = unit(.1, "lines"),
      strip.background = element_rect(fill='#F0F8FF', size = 0.5),
      panel.background = element_blank(),
      panel.grid.major = element_line(colour='grey95', size=0.5),
      panel.grid.minor = element_line(colour='grey97', size=0.4),
      panel.border = element_rect(colour = 'grey20', fill=NA, size=1),
      ## Top, right, bottom, left
      plot.margin = unit(c(.2, .2, .2, .2), 'cm'),
      strip.text = element_text(size = rel(1.0)),
      legend.key = element_rect(fill = '#FFFFFF00'),
      legend.text = element_text(size = rel(.8)))

  l_theme <- l_theme + rotate_x_labels(rotate_labels)
  l_theme <- l_theme + ggplot2::theme(...)
  return(l_theme)
}


#' Angle specific justification in geom text
#'
#'
angle_adj_just <- list('90' = list('h' = 1, 'v' = .5),
                       '45' = list('h' = 1, 'v' = 1),
                       '30' = list('h' = 1, 'v' = 1),
                       '0'  = list('h' = .5, 'v' = 1))

rotate_x_labels <- function(rotate_labels) {
  if (is.na(rotate_labels) || is.null(rotate_labels)) {
    return(ggplot2::theme())
  }
  rot_str <- as.character(rotate_labels)
  ggplot2::theme(axis.text.x =
    ggplot2::element_text(angle = rotate_labels,
                          hjust = angle_adj_just[[rot_str]][['h']],
                          vjust = angle_adj_just[[rot_str]][['v']]))
}


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


#' Fixate panel size of ggplot
#'
#' @param p ggplot object
#' @param g ggplotGrob
#' @param file file name to save result to. File is saved using \code{ggplot2::ggsave()}
#' @param margin \code{grid::unit()} object indicating margin size 
#' @param width \code{grid::unit()} object indicating panel width
#' @param height \code{grid::unit()} object indicating panel height
#' @return gtable object
set_panel_size <- function(p=NULL, g=ggplotGrob(p), 
                           file=NULL,
                           margin = unit(1,'mm'),
                           width=unit(4, 'cm'),
                           height=unit(4, 'cm')) {
  panels <- grep('panel', g$layout$name)
  panel_index_w<- unique(g$layout$l[panels])
  panel_index_h<- unique(g$layout$t[panels])
  nw <- length(panel_index_w)
  nh <- length(panel_index_h)

	if (getRversion() < '3.3.0') {
		 ## The following conversion is necessary
		 ## because there is no `[<-`.unit method
		 ## so promoting to unit.list allows standard list indexing
		 g$widths <- grid:::unit.list(g$widths)
		 g$heights <- grid:::unit.list(g$heights)
		 g$widths[panel_index_w] <-  rep(list(width),  nw)
		 g$heights[panel_index_h] <- rep(list(height), nh)
	} else {
		 g$widths[panel_index_w] <-  rep(width,  nw)
		 g$heights[panel_index_h] <- rep(height, nh)
	}

  if(!is.null(file))
    ggsave(file, g,
           width = convertWidth(sum(g$widths) + margin,
                                unitTo = 'in', valueOnly = TRUE),
           height = convertHeight(sum(g$heights) + margin,
                                  unitTo = 'in', valueOnly = TRUE))

  invisible(g)
}
