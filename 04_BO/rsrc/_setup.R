# ------------------------------------------------------------------------------
# Shared setup for 04_BO figure scripts.
# Source this from every figure-generating R script (assuming WD = 04_BO/).
# Loads the libraries used by every script; per-script extras (`ranger`,
# `lhs`, `ggExtra`, `patchwork`) are loaded in the calling script.
# ------------------------------------------------------------------------------

library(bbotk)
library(mlr3mbo)
library(mlr3learners)
library(data.table)
library(ggplot2)


# Global default theme: bigger base_size so axis text stays readable when
# the figure is embedded at half-slide-width or smaller. Single-panel scripts
# call theme_minimal() with this base_size explicitly; landscape / multi-panel
# scripts can opt out by passing their own size.
theme_set(theme_minimal(base_size = 13))


# myggsave
#
#   Convenience wrapper around ggplot2::ggsave that:
#     - saves into the 04_BO/images/ directory (WD must be 04_BO/)
#     - writes PDF via cairo_pdf (proper font metrics => clean Greek subscripts)
#
# Arguments
#   name    file stem without extension or directory, e.g. "initdes_random"
#   plot    ggplot object to save
#   width   in inches (default 5)
#   height  in inches (default 4)
#   ...     any other argument passed on to ggplot2::ggsave
myggsave = function(name, plot, width = 5, height = 4, ...) {
  ggsave(
    filename = file.path("images", paste0(name, ".pdf")),
    plot     = plot,
    width    = width,
    height   = height,
    device   = cairo_pdf,
    ...
  )
}


# Branin on [-5, 10] x [0, 15]
branin = function(x1, x2) {
  a = 1; b = 5.1 / (4 * pi^2); c = 5 / pi
  r = 6; s = 10; t = 1 / (8 * pi)
  a * (x2 - b * x1^2 + c * x1 - r)^2 + s * (1 - t) * cos(x1) + s
}

# Branin as bbotk objective
branin_obj_rfundt = ObjectiveRFunDt$new(
  fun = function(xdt) data.table(y = branin(xdt$x1, xdt$x2)),
  domain = ps(x1 = p_dbl(lower = -5, upper = 10),
              x2 = p_dbl(lower =  0, upper = 15)),
  codomain = ps(y = p_dbl(tags = "minimize"))
)

# 1D toy objective used across 1D BO loop figures: 2 * x * sin(14 * x) on [0, 1]
toy1d_obj_rfundt = ObjectiveRFunDt$new(
  fun = function(xdt) data.table(y = 2 * xdt$x * sin(14 * xdt$x)),
  domain = ps(x = p_dbl(lower = 0, upper = 1)),
  codomain = ps(y = p_dbl(tags = "minimize"))
)


#  Standard GP surrogate used throughout the 04_BO figure scripts
my_gp_surrogate = function(archive) {
  srlrn(
    lrn("regr.km", covtype = "matern5_2", optim.method = "BFGS",
        nugget.stability = 1e-8, control = list(trace = FALSE)),
    archive = archive
  )
}


# Refit surrogate and write posterior mean + one-sigma band onto `grid`.
# add cols: y_hat, y_min (mean - se), y_max (mean + se).
# returns the prediction object (mean, se) for callers that need the raw values
update_surrogate_and_grid = function(surrogate, grid) {
  surrogate$update()
  prediction = surrogate$predict(grid)
  set(grid, j = "y_hat", value = prediction$mean)
  set(grid, j = "y_min", value = prediction$mean - prediction$se)
  set(grid, j = "y_max", value = prediction$mean + prediction$se)
  prediction
}


# Shared style for 2D landscape plots: mako-viridis fill + a light theme
# tuned for raster + contour overlays. Returns a list of ggplot layers; use
# as `+ landscape_style()` after the call site's geoms and coords.
landscape_style = function(base_size = 10) list(
  scale_fill_viridis_c(option = "mako", direction = -1),
  theme_minimal(base_size = base_size),
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    plot.subtitle = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )
)


# Base landscape plot for 2D acquisition/surrogate visualizations:
# mako raster + white contours of `value` + yellow-x markers for the design
# points. Callers add their own point geoms (batch picks, annotations, ...)
# and labels on top.
#
# `grid`   data.table with columns x1, x2, <value>
# `design` data.frame with columns x1, x2 (evaluated design points)
# `value`  name of the column in `grid` to render (e.g. "ei", "pi", "ucb",
#          "y_hat"); default "ei"
# `xlim`, `ylim`  plot limits; default to the Branin domain used across 04_BO
acqf_base_plot = function(grid, design, value = "ei",
                          xlim = c(-5, 10), ylim = c(0, 15)) {
  ggplot(grid, aes(x = x1, y = x2)) +
    geom_raster(aes(fill = .data[[value]])) +
    geom_contour(aes(z = .data[[value]]), colour = "white", alpha = 0.4,
                 linewidth = 0.3, bins = 8) +
    geom_point(aes(x = x1, y = x2), data = design,
               shape = 4, colour = "#ffcc00", size = 3, stroke = 1.3) +
    coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
    landscape_style()
}


# Render a colored perspective plot of a 2D surface and save as cairo_pdf.
#
# Arguments
#   name       file stem (without "images/" or extension)
#   x1, x2     grid axis values along each dimension
#   z          surface heights; either a length(x1) * length(x2) vector
#              (in column-major order) or a matrix with rows indexed by x1
#   pal_cols   color stops for the surface palette
#   points     optional data.table with columns x1, x2, z; rendered on top
#              as yellow filled circles
#   theta, phi viewing angles for persp()
#   zlab       label for the z axis (literal string; persp does not accept
#              expressions, hence x/y labels are also literal unicode)
#   width, height  in inches
save_persp = function(name, x1, x2, z, pal_cols,
                      points = NULL, theta = 25, phi = 22, zlab = "",
                      width = 4, height = 3) {
  pal = colorRampPalette(pal_cols)(120)
  if (!is.matrix(z)) z = matrix(z, nrow = length(x1), ncol = length(x2))
  zfacet = (z[-1, -1] + z[-1, -ncol(z)] + z[-nrow(z), -1] + z[-nrow(z), -ncol(z)]) / 4
  facet_col = pal[cut(zfacet, length(pal), include.lowest = TRUE)]

  path = file.path("images", paste0(name, ".pdf"))
  cairo_pdf(path, width = width, height = height)
  par(mar = c(0.4, 1.4, 0.3, 0.8))
  tmat = persp(x1, x2, z,
               theta = theta, phi = phi, expand = 0.55,
               xlab = "λ₁", ylab = "λ₂", zlab = zlab,
               ticktype = "detailed", nticks = 4,
               col = facet_col, border = "grey40", lwd = 0.2,
               cex.axis = 0.6, cex.lab = 0.8)
  if (!is.null(points)) {
    p3 = trans3d(points$x1, points$x2, points$z, tmat)
    points(p3$x, p3$y, pch = 21, bg = "#ffcc00", col = "black",
           cex = 1.1, lwd = 0.8)
  }
  dev.off()
}

