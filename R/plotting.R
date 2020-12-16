#' Another ggplot theme
#'
#'
theme_ms <- function(base_size = 8, base_family = 'sans',
                     rotate_labels = NA, legend_pos = 'bottom', ...) {
  l_theme <-
    ggplot2::theme_grey(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      rect = ggplot2::element_rect(fill = 'white', linetype = 0, color = NULL),
      text = ggplot2::element_text(size = base_size, family = base_family),
      title = ggplot2::element_text(hjust = 0.0),
      axis.text = ggplot2::element_text(size = rel(0.8)),
      plot.title = ggplot2::element_text(size = rel(1), hjust = 0),
      axis.title = ggplot2::element_text(size = rel(1.0), hjust = .5),
      legend.position = legend_pos,
      legend.key = ggplot2::element_rect(fill = '#FFFFFF00', size = 4),
      legend.key.size = grid::unit(3, 'mm'),
      legend.text = ggplot2::element_text(size = rel(0.8)),
      legend.title = ggplot2::element_text(size = rel(0.7), hjust = .5),
      legend.margin = ggplot2::margin(1, 1, 1, 1, unit = 'mm'),
      legend.box = 'vertical',
      legend.box.just = 'bottom',
      legend.direction = 'horizontal',
      # legend.box.margin = margin(1,1,1,1, unit = 'mm'),
      legend.spacing = grid::unit(10, 'mm'),
      panel.spacing = grid::unit(.1, "lines"),
      strip.background = ggplot2::element_rect(fill='#F0F8FF', 
        colour = 'grey80', size = 0.5),
      panel.grid.minor = ggplot2::element_line(colour='grey97', size = 0.4),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "grey80", fill = NA, 
                                           size = .5, linetype = 'solid'),
      ## Top, right, bottom, left
      plot.margin = grid::unit(c(.2, .2, .2, .2), 'cm'),
      strip.text = ggplot2::element_text(size = rel(1.0)))

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



find_img_dir <- function(img_dir_pat = 'plots|img|fig') {
  cur_dir <- getwd()
  sub_dirs <- list.dirs(cur_dir, full.names = F, recursive = F)
  fig_cand_dirs <- grep(img_dir_pat, sub_dirs, value = T)
  fig_cand_dirs <- grep('cache', fig_cand_dirs, invert = T, value = T)
  if (length(fig_cand_dirs) == 0) {
    return(getwd())
  } else if (length(fig_cand_dirs) == 1) {
    return(fig_cand_dirs)
  } else {
    messagef('multiple fig dirs found in WD: %s, returning WD.', getwd())
    return(getwd())
  }
}


#' Wrapper around ggplot2::ggsave
#'
#' @param golden_ratio whether to apply golden ratio, if yes user only has to
#' supply height \code{h} and corresponding width will be set automatically
#' @param fn filename without extension - .png and .pdf files will be generated
w_ggsave <- function(fn, plot = last_plot(), plot_ratio = 'norm',
                     h = 16, w = NA,
                     img_folder = find_img_dir('img|fig|plots'), 
                     units = 'cm',
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
  lapply(fns, function(x) {
     ggsave(filename = x, plot = plot, height = lheight, 
            width = lwidth, dpi = 800,
            units = units, limitsize = F, ...)
     mymessage(msg = sprintf('saved image to %s', x))
  })
  if ('rds' %in% filetypes) {
    full_fn <- file.path(img_folder, sprintf('%s.rds', fn))
    saveRDS(plot, full_fn)
    mymessage(msg = sprintf('saved image to %s', full_fn))
  }
  if ('grob' %in% filetypes) {
    # plot
    # gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot))
    gt <- to_g(plot)
    full_fn <- file.path(img_folder, sprintf('%s.grob.rds', fn))
    saveRDS(gt, full_fn)
    mymessage(msg = sprintf('saved image to %s', full_fn))
  }
  return(invisible())
}


#' Extract gtable from ggplot object
#'
#'
to_g <- function(li, ...) {
  to_g_helper <- function(x) {
    if (all(c('gtable', 'gTree', 'grob', 'gDesc') %nin% class(x)) &&
        any(c('ggplot', 'gg') %in% class(x))) {
      g <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(x))
      ## Ugly hack to bypass the plotting device that ggplot_build() opens
      if (length(dev.list()) > 0)
        dev.off()
      return(g)
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
#'
#' @param margin \code{grid::unit()} object indicating margin size
#' @param width \code{grid::unit()} object indicating panel width
#' @param height \code{grid::unit()} object indicating panel height
#' @return gtable object
set_panel_size <- function(p=NULL, g=ggplotGrob(p),
                           margin = grid::unit(1,'mm'),
                           width = grid::unit(4, 'cm'),
                           height = grid::unit(4, 'cm')) {
  panels <- grep('panel', g$layout$name)
  panel_index_w <- unique(g$layout$l[panels])
  panel_index_h <- unique(g$layout$t[panels])
  nw <- length(panel_index_w)
  nh <- length(panel_index_h)

  if (getRversion() < '3.3.0') {
    ## The following conversion is necessary
    ## because there is no `[<-`.unit method
    ## so promoting to unit.list allows standard list indexing
    g$widths <- grid:::unit.list(g$widths)
    g$heights <- grid:::unit.list(g$heights)
    g$widths[panel_index_w] <- rep(list(width),  nw)
    g$heights[panel_index_h] <- rep(list(height), nh)
	} else {
    if (!is.null(width)) {
      g$widths[panel_index_w] <- rep(width,  nw)
    }
    if (!is.null(height)) {
      g$heights[panel_index_h] <- rep(height, nh)
    }
	}
  invisible(g)
}


#' Wrapper around cowplot::plot_grid()
#'
#'
plot_panel <- function(plots, constant = 'ccf_table',
                       opts = default_opts, h = 10, w = 5,
                       normalize_grob_widths = T,
                       normalize_grob_heights = F,
                       save_bool = F, ncol = min(3, length(plots)),
                       labels = LETTERS[seq_along(plots)],
                       label_size = global_label_size,
                       ...) {
  if (normalize_grob_widths) {
    plots <- normalize_grob_widths(plots)
  }
  if (normalize_grob_heights) {
    plots <- normalize_grob_heights(plots)
  }

  panel <- do.call(cowplot::plot_grid,
                   c(plots, list('label_size' = label_size,
                                 'labels' = labels, 'ncol' = ncol, ...)))

  if (save_bool) {
    fn <- gen_parmscan_img_fn(constant = constant, opts)
    w_ggsave(fn, h = h, w = w, img_folder = file.path(img_loc, 'parmscan'))
  }
  return(panel)
}


#' Test whether panel is in last row of page
#'
#' Helper function to plot_panel_layout()
#'
#' @param N_plots Total amount of plots
#' @param N_ppp Plots per page
#' @param nrow Amount of panel rows per page
#' @param ncol Amount of panel columns per page
#'
#' @value is TRUE if not in last row of page, FALSE otherwise
test_last_row <- function(ii, N_plots = 9,
                          N_ppp = min(nrow * ncol, 12),
                          nrow = ceiling(sqrt(N_plots)),
                          ncol = floor(sqrt(N_plots))) {
  N_pages <- ceiling(N_plots / N_ppp)
  N_rows_final <- ceiling((N_plots %% N_ppp) / ncol)
  plt_page <- ceiling(ii / N_ppp)
  N_plots_on_prev_pages <- (plt_page - 1) * N_ppp

  ## We're not on the last page yet
  not_last_page_bool <- (plt_page < N_pages) &&
    ((ii - N_plots_on_prev_pages) <= ((nrow-1) * ncol))

  ## We are on the last page
  last_page_bool <- plt_page == N_pages &&
    ((ii - N_plots_on_prev_pages) <= (N_rows_final-1) * ncol)

  bool <- not_last_page_bool || last_page_bool
  return(bool)
}


#' Test whether panel is last on page
#'
#' Helper function to plot_panel_layout()
#'
#' @value is TRUE if panel is last plot on page, FALSE otherwise
test_last_panel <- function(ii, N_plots = 9,
                            N_ppp = min(nrow * ncol, 12),
                            nrow = ceiling(sqrt(N_plots)),
                            ncol = floor(sqrt(N_plots))) {
  N_pages <- ceiling(N_plots / N_ppp)
  N_rows_final <- ceiling((N_plots %% N_ppp) / ncol)
  plt_page <- ceiling(ii / N_ppp)
  N_plots_on_prev_pages <- (plt_page - 1) * N_ppp

  ## We're not on the last page yet
  not_last_page_bool <- (plt_page < N_pages) &&
    (ii %% N_ppp == 0)

  bool <- not_last_page_bool || ii == N_plots
  return(bool)
}


#' Plot panel off ggplots and define layout of plots with matrix
#'
#' cowplot::plot_grid largely fulfills the same goal but does not allow you to
#' specify a layout matrix, this function complements that void in functionality
#'
#' @param ref_panel_idx Index of panel to use as reference for equally sizing
#'   all panels. Can also be set to NULL or FALSE to forego of this behaviour.
#' @param w Width of panel to be outputted to pdf in cm
#' @param h Height of panel to be outputted to pdf in cm
#' @param clear_redundant_labels Applicable to panel of equally labelled plots,
#'   clear the axes labels
#'
#' @export
plot_panel_layout <- function(plots,
                              offs = grid::unit(.35, 'cm'),
                              filename = NULL,
                              plot_direct = F,
                              layout_mat = NULL,
                              nrow = NULL,
                              ncol = NULL,
                              labels = LETTERS,
                              panel_padding = .5,
                              clear_redundant_labels = F,
                              clear_redundant_legends = F,
                              ref_panel_idx = NULL,
                              label_size = 8,
                              widths = rep(1, ncol(layout_mat)),
                              heights = rep(1, nrow(layout_mat)),
                              w = 17.4, h = 25) {
  ## Input checking, set to NULL if FALSE
  if (!is.null(ref_panel_idx) && ref_panel_idx == FALSE) {
    ref_panel_idx <- NULL
  }

  plots <- plots[!sapply(plots, is.null)]

  if (is.null(labels)) {
    labels <- rep(c(''), length(plots))
  }

  if (is.null(layout_mat)) {
    if (is.null(nrow) && !is.null(ncol)) {
      nrow <- ceiling(length(plots) / ncol)
    } else if (!is.null(nrow) && is.null(ncol)) {
      ncol <- ceiling(length(plots) / nrow)
    } else if (is.null(nrow) && is.null(ncol)) {
      ncol <- 2
      nrow <- ceiling(length(plots) / ncol)
    }
    N_ppp <- nrow * ncol
    layout_mat <- matrix(1:N_ppp, ncol = ncol, nrow = nrow, byrow = T)
    N_pages <- ceiling(length(plots) / N_ppp)
  } else {
    nrow <- nrow(layout_mat)
    ncol <- ncol(layout_mat)
    N_ppp <- nrow * ncol
    N_pages <- ceiling(length(plots) / N_ppp)
  }

  if (!is.null(ref_panel_idx)) {
    ref_plot <- cowplot::plot_to_gtable(plots[[ref_panel_idx]])
  }

  ## Add labels to ggplot grobs
  gs <- lapply(seq_along(plots), function(ii) {
    label_grob <- grid::textGrob(labels[ii], x = offs,
                                 y = grid::unit(1, 'npc') - offs,
                                 gp = grid::gpar(fontsize = label_size,
                                                 col='black',
                                                 fontface = 'bold'))
    plt <- plots[[ii]]
    if (all(is.na(plt))) {
      return(label_grob)
    }

    if (clear_redundant_labels && (ii %% ncol != 1)) {
      plt <- plt + ggplot2::theme(axis.title.y = ggplot2::element_blank())
    }

    if (clear_redundant_labels &&
        test_last_row(ii, N_plots = length(plots), N_ppp = N_ppp,
                      nrow = nrow, ncol = ncol)) {
      plt <- plt + ggplot2::theme(axis.title.x = ggplot2::element_blank())
    }

    if (clear_redundant_legends &&
        !test_last_panel(ii, N_plots = length(plots), N_ppp = N_ppp,
                         nrow = nrow, ncol = ncol)) {
      plt <- plt + ggplot2::theme(legend.position = 'none')
    }

    g_tab <- cowplot::plot_to_gtable(plt)

    if (!is.null(ref_panel_idx)) {
      g_tab$widths[2:5] <- ref_plot$widths[2:5]
      g_tab$heights[2:5] <- ref_plot$heights[2:5]
    }

    if (!is.null(panel_padding) && !eps(panel_padding, 0)) {
      g_tab <- gtable::gtable_add_padding(
        g_tab, grid::unit(panel_padding, "cm"))
    }

    return(grid::grobTree(g_tab, label_grob))
  })

  p <- gridExtra::marrangeGrob(
    grobs = gs, layout_matrix = layout_mat,
    widths = widths, heights = heights, top = '', npages = N_pages)

  if (!is.null(filename)) {
    # pdf(filename, width = w/2.54, height = h/2.54)
    # grid::grid.draw(p)
    # dev.off()
    ggplot2::ggsave(plot = p, filename = filename, useDingbats = F,
      width = w, height = h, units = 'cm')
  }

  if (plot_direct) {
    return(p)
  } else {
    return(invisible(filename))
  }
}


#' Darken color
#'
#' @export
darken <- function(color_vec, factor = 1.4) {
  was_color_vec <- is.color_vector(color_vec)
  stopifnot(is.numeric(factor) && all(factor > 0) && all(!is.infinite(factor)))

  max_length <- max(length(factor), length(color_vec))
  if (length(factor) < max_length) {
    factor <- rep_len(factor, max_length)
  }
  if (length(color_vec) < max_length) {
    color_vec <- rep_len(color_vec, max_length)
  }
   
  color_vec <- purrr::imap(setNames(color_vec, NULL), function(color, idx) {
    color <- grDevices::col2rgb(color)
    color <- color / factor[idx]
    color <- apply(color, 1, 
                   function(x) {
                     if (x < 0) {
                       return(0) 
                     } else if (x > 255) { 
                       return(255) 
                     } else { 
                       return(x) 
                     }})
    color <- grDevices::rgb(t(as.matrix(color)), maxColorValue = 255)
    name <- names(color_vec)[idx]
    if (is.null(name) || is.na(name)) {
      names(color) <- color
    } else {
      names(color) <- name
    }
    return(color)
  }) %>% unlist
  if (was_color_vec) {
    class(color_vec) <- unique(c('color_vector', class(color_vec)))
  }
  return(color_vec)
}


lighten <- function(color_vec, factor = 1.4) {
  return(darken(color_vec, 1/factor))
}


#' Convert HSL coordinates to RGB coordinates
#' 
#' @return r/g/b in a vector 
hsl_to_rgb <- function(h, s, l) {
  h <- h / 360
  r <- g <- b <- 0.0
  if (s == 0) {
    r <- g <- b <- l
  } else {
    hue_to_rgb <- function(p, q, t) {
      if (t < 0) { t <- t + 1.0 }
      if (t > 1) { t <- t - 1.0 }
      if (t < 1/6) { return(p + (q - p) * 6.0 * t) }
      if (t < 1/2) { return(q) }
      if (t < 2/3) { return(p + ((q - p) * ((2/3) - t) * 6)) }
      return(p)
    }
    q <- ifelse(l < 0.5, l * (1.0 + s), l + s - (l*s))
    p <- 2.0 * l - q
    r <- hue_to_rgb(p, q, h + 1/3)
    g <- hue_to_rgb(p, q, h)
    b <- hue_to_rgb(p, q, h - 1/3)
  }
  return(c('r' = r, 'g' = g, 'b' = b))
}


#' Convert RGB coordinates to HSL coordinates
#' 
#' h = 0-360 deg, s = 0.0 - 1 (0-100%), l = 0.0 - 1 (0-100%)
#' 
#' @return h/s/l in a vector 
rgb_to_hsl <- function(r, g, b) {
  val_max <- max(c(r, g, b))
  val_min <- min(c(r, g, b))
  h <- s <- l <- (val_max + val_min) / 2
  if (val_max == val_min){
    h <- s <- 0
  } else {
    d <- val_max - val_min
    s <- ifelse(l > 0.5, d / (2 - val_max - val_min), d / (val_max + val_min))
    if (val_max == r) { h <- (g - b) / d + (ifelse(g < b, 6, 0)) }
    if (val_max == g) { h <- (b - r) / d/ + 2 }
    if (val_max == b) { h <- (r - g) / d + 4 }
    h <- (h / 6) * 360
  }
  return(c('h' = h, 's' = s , 'l' = l))
}


#' Convert HSL coordinates to hexadecimal color code
#' 
#' @return HEX color code
hsl_to_hex <- function(h, s, l) {
  rgb_coords <- do.call(hsl_to_rgb, list('h' = h, 's' = s, 'l' = l))
  do.call(rgb_to_hex, as.list(rgb_coords))
}


#' convert rgb coordinates to hexadecimal color code
#' 
#' @return hex color code
rgb_to_hex <- function(r, g, b, nf = 255) {
  do.call(rgb, as.list(c('red' = r, 'green' = g, 'blue' = b) / nf))
}


#' Convert hex code to hexadecimal color code
#' 
#' @return RGB coordinates (scale: [0, 255])
hex_to_rgb <- function(hex) {
  as.vector(col2rgb(hex))
}


#' Create vector of colors from color palette
#'
#' @param n Amount of primary colors to interpolate from palette
#' @param prims Amount of primary colors to use from palette
#'
#' @return a vector of color names
#' @export
gen_color_vector <- function(name = 'Spectral', n = 30, prims = NA) {
  pal <- gen_color_palette(name = name, n = n, prims = prims)(n)
  # class(pal) <- c('color_vector', class(pal))
  class(pal) <- 'color_vector'
  return(pal)
}


#' Plot a color palette to inspect its colors
#'
#' @param color_vector character vector of (potentially named) colors
#' @return invisibly, plotting the color palette using R base graphics
#'
#' @export
plot.color_vector <- function(color_vector) {
  old_par <- par()
  par(mar = rep(0, 4), plt = c(0, 1, 0, 1), oma = c(0, 0, 0, 0))
  on.exit(par(old_par))
  plot(NA, xlim = c(0, 1), ylim = c(0, length(color_vector)), axes = F,
       xlab = '', ylab = '')
  for (i in 1:length(color_vector)) {
    polygon(y = c(i-1, i, i, i-1), x = c(0, 0, 1, 1), col = color_vector[i])
    if (!is.null(names(color_vector))) {
      text(x = .5, y = i-.5, labels = names(color_vector)[i])
    }
  }
  invisible()
}
# color_vector.plot <- function(x) plot_palette(x)
is.color_vector <- function(x) inherits(x, "color_vector")
# length.color_vector <- function(x) length(x)
# `%[%`.color_vector <- function(x, idx) x[idx]
# `[.color_vector` <- function(x, idx) x[idx]
# `c.color_vector` <- function(...) c(...)
# rev.color.vector <- function(x) x[length(x):1]


#' Interpolate between color palettes
#'
#' @return a color ramp
#' @export
gen_color_palette <- function(name = 'Set1', n = 30L, prims = NA) {
  # devtools::install_github('karthik/wesanderson')
  palette_prims <- c('Set1' = 5L, 'Dark1' = 8L, 'Dark2' = 8L, 'Spectral' = 6L,
                     'Greys' = 9L,
                     'GrandBudapest' = 4, 
                     'GrandBudapest1' = 4,
                     'GrandBudapest2' = 4, 
                     'Moonrise1' = 4, 'Moonrise2' = 4, 'Moonrise3' = 4, 
                     'BottleRocket' = 4, 
                     'BottleRocket1' = 4, 
                     'Chevalier' = 4,
                     'Chevalier1' = 4,
                     'FantasticFox' = 5L, 
                     'FantasticFox1' = 5L, 
                     'Zissou' = 5L, 
                     'Zissou1' = 5L, 
                     'Cavalcanti' = 5L,
                     'Cavalcanti1' = 5L,
                     'Royal1' = 4L, 
                     'Darjeeling1' = 5L,
                     'Darjeeling' = 5L)
  name <- match.arg(name, names(palette_prims))

  available_prims <- palette_prims[name]
  if (is.na(prims)) {
    prims <- available_prims
  } else {
    ## Ensure we're not asking too many prims and cause an error
    prims <- min(prims, available_prims)
  }

  if (name %in% names(palette_prims)[1:5]) {
    pal <- RColorBrewer::brewer.pal(name = name, n = prims)
  } else {
    ## Palette must be one of Wes Anderson's
    pal <- wesanderson::wes_palette(name = name, n = prims,
                                    type = 'discrete')
  }

  ramp <- colorRampPalette(pal, alpha = TRUE)
  return(ramp)
}


color_palette <- function(cols) {
}


#' Desaturate colors
#'
#' @export
adjust_colors <- function(cols, sat=1, brightness = 1.2) {
  X <- diag(c(1, sat, brightness)) %*% rgb2hsv(col2rgb(cols))
  hsv(pmin(X[1, ], 1),
      pmin(X[2, ], 1),
      pmin(X[3, ], 1))
}


#' Get rid of two outermost breaks in otherwise normal ggplot scale
#'
#' In order to tighly pack panels (facets) without getting overlapping labels
internal_breaks <- function (n = 5, left_i = 1, right_i = 1, ...) {
    function(x) {
				scale_labels <- scales::extended_breaks(n = n, ...)(x)
				return(scale_labels[1+left_i:(n-right_i)])
    }
}


#' Extract range from ggplot object
#'
#'
get_ggplot_range <- function(plot, axis = 'x') {
  hor_types <- c('x', 'horizontal')
  ver_types <- c('y', 'vertical')
  axis <- match.arg(tolower(axis), c(hor_types, ver_types), several.ok = F)
  axis <- ifelse(axis %in% hor_types, 'x', 'y')
 
  if (packageVersion('ggplot2') < '2.2.1.9000')
    return(ggplot_build(plot)$layout$panel_ranges[[1]][[sprintf('%s.range', axis)]])

  x_scales <- ggplot_build(plot)$layout$panel_scales_x[[1]]
  y_scales <- ggplot_build(plot)$layout$panel_scales_y[[1]]

  if (!is.null(x_scales$range)) 
    range_x_c <- 'RangeContinuous' %in% (class(x_scales$range))
  if (!is.null(y_scales$range)) 
    range_y_c <- 'RangeContinuous' %in% (class(y_scales$range))

  if (packageVersion('ggplot2') >= '3') {
    if (axis == 'x' && range_x_c)
      return(x_scales$range$range)
    else if (axis == 'x' && !range_x_c)
      return(x_scales$range_c$range)
    else if (axis == 'y' && range_y_c)
      return(y_scales$range$range)
    else if (axis == 'y' && !range_y_c)
      return(y_scales$range_c$range)
    
  } else {
    if (axis == 'x')
      return(x_scales$range$range)
    else if (axis == 'y')
      return(y_scales$range$range)
  }
}


#' Interpolate linearly on horizontal scale
#'
#'
interpolate_in_range <- function(range, degree) {
  stopifnot(length(range) == 2)
  diff <- range[2] - range[1]
  return(range[1] + diff * degree)
}


#' Get position relative to ggplot axis
#'
#'
interpolate_in_gg_range <- function(plot, axis = 'x', degree = .1) {
  interpolate_in_range(get_ggplot_range(plot, axis = axis), degree = degree)
}

remove_x <- 
  ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                 axis.text.x = ggplot2::element_blank(),
                 axis.ticks.x = ggplot2::element_blank())

remove_y <- 
  ggplot2::theme(axis.title.y = ggplot2::element_blank(), 
                 axis.text.y = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank())

remove_strip <- 
  ggplot2::theme(strip.text = ggplot2::element_blank() , 
                 strip.background = ggplot2::element_blank(), 
                 plot.margin = grid::unit(c(0, 0, 0, 0), units = 'lines'))

remove_legend <- ggplot2::theme(legend.position = 'none')

gg_legend_alpha_cancel <-
  ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(alpha = 1)),
         colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))

gg_remove_x_labels <- ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                                     axis.text.x = ggplot2::element_blank())

transparent_legend <- ggplot2::theme(
  legend.background = ggplot2::element_rect(fill = 'transparent'),
  legend.key = ggplot2::element_rect(fill = 'transparent',
                                     color = 'transparent')
)

transparent_plot <- ggplot2::theme(
  panel.background = ggplot2::element_rect(fill = 'transparent',
                                           color = 'transparent')
)

var_to_label <- function(p_var, reps = NULL, cap_first_word_only = T) {
  if (!is.null(reps)) {
    ## Match each p_var to most similar
    p_var <- vapply(p_var, 
      function(l_var) {
        matching <- tryCatch(match.arg(l_var, choices = names(reps), 
                              several.ok = F), 
                             error = function(e) { return(NULL) }) 
        if (is.null(matching) || matching == '') {
          ret_val <- l_var
        } else {
          ret_val <- reps[which(matching == names(reps))]
        }
        return(ret_val)
      }, character(1))
  }
  simple_cap(gsub('_', ' ', p_var), cap_first_word_only = cap_first_word_only)
}


#' Generic scatter plot code that shows correlation coefficient by default
#'
#'
plot_scatter_cor <- function(x_var = 'adaptive_t_cells',
  y_var = 'rna_t_cell',
  trans = identity,
  dtf = patient_labels_tmp,
  cor_method = 'spearman',
  point_alpha = .8,
  axis_labeller = NULL,
  outlier_label_var = NULL,
  position = 'topleft') {
  setDT(dtf)

  axis_labeller <- axis_labeller %||% identity

  p <- ggplot(dtf, aes_string(x = x_var, y = y_var)) + 
    geom_point(alpha = point_alpha) + 
    scale_x_continuous(name = axis_labeller(x_var), expand = c(0, 0)) +
    scale_y_continuous(name = axis_labeller(y_var), expand = c(0, 0)) +
    theme(aspect.ratio = 1)

  if (!is.null(outlier_label_var)) {
    outlier_dat <- 
      dtf[detect_outliers(get(x_var)) | detect_outliers(get(y_var))]
    p <- p + ggrepel::geom_label_repel(data = outlier_dat, 
      aes_string(label = outlier_label_var))
  }

  if (!is.null(cor_method)) {
    if (position == 'topleft') {
      ann_x <- interpolate_in_gg_range(p, axis = 'x', degree = .05)
      ann_y <- interpolate_in_gg_range(p, axis = 'y', degree = .95)
      vjust <- 1
      hjust <- 0
    } else if (position == 'bottomright') {
      ann_x <- interpolate_in_gg_range(p, axis = 'x', degree = .95)
      ann_y <- interpolate_in_gg_range(p, axis = 'y', degree = .05)
      vjust <- 0
      hjust <- 1
    } else {
      stop('Not implemented')
    }
    corr <- dtf[, cor(get(x_var), get(y_var), use = 'pairwise.complete.obs')]

    p <- p + ggplot2::annotate('text', x = ann_x, y = ann_y, 
                               label = sprintf("italic(r)==%.3f", corr), 
                               parse = TRUE, vjust = vjust, hjust = hjust)
  }
  return(p)
}


#' Plot all pairwise relationships in data.frame/data.table
#'
#' Plot all pairwise relationships between explanatory variables (columns) in
#' wide data and one fixed response variable
#'
#' @param dtf Wide \code{data.frame} or \code{data.table} object
#' @param y_var Response variable
#' @param y_var_trans Transformation to apply to response variable (function)
#' @param blacklist_vars Candidate explanatory variables to exclude
#' @param filename Filename to save result to
#' @param nrow Amount of rows per page in result
#' @param ncol Amount of columns per page in result
#'
plot_pairwise_relationships <- function(dtf = rna_sample_annotation,
                                        y_var = 'y_var',
                                        y_var_trans = identity,
                                        var_labeller = identity,
                                        blacklist_vars = c(),
                                        filename = sprintf('%s_correlates.pdf', 
                                                           y_var),
                                        nrow = 5, 
                                        ncol = 3) {
  setDT(dtf)
  dtf[, (y_var) := y_var_trans(get(y_var))]

  factor_plots <- 
    dtf[, lapply(.SD, 
                 function(x) is.factor(x) || is.logical(x) || 
                   is.character(x))] %>%
    unlist %>% 
    { .[. == T] } %>%
    names %>%
    { dtf[, lapply(.SD, function(x) uniqueN(x) > 1), .SDcols = .] } %>%
    unlist %>% 
    { .[. == T] } %>%
    names %>%
    setdiff(c(y_var, blacklist_vars)) %>%
    auto_name %>%
    purrr::map(function(x_var) {
      tryCatch(ggplot(data = dtf, aes_string(x = x_var, y = y_var)) + 
                 geom_boxplot() + 
                 ggpubr::stat_compare_means() + 
                 xlab(var_labeller(x_var)) + 
                 ylab(var_labeller(y_var)) +
                 ggplot2::theme(text = element_text(size = 6)), 
               error = function(e) { print(e); return(NULL) }) 
    })

  numerical_plots <- dtf[, lapply(.SD, is.numeric)] %>%
    unlist %>%
    {.[. == T] } %>%
    names %>%
    setdiff(c(y_var, blacklist_vars)) %>%
    auto_name %>%
    purrr::map(function(x_var) {
      plot_scatter_cor(x_var = x_var, y_var = y_var,
                       dtf = dtf, axis_labeller = var_labeller) +
        ggplot2::theme(text = element_text(size = 6))
    })


  ## Order plots according to col order in original data.frame
  intersect(colnames(dtf), 
            c(names(factor_plots), names(numerical_plots))) %>%
    { c(factor_plots, numerical_plots)[.] } %>%
    { plot_panel_layout(., filename = filename, nrow = nrow, ncol = ncol,
                        labels = NULL) }
  sys_file_open(filename)
  invisible()
}

