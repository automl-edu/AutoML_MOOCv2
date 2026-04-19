# ------------------------------------------------------------------------------
# Shared setup for 04_BO figure scripts.
# Source this from every figure-generating R script (assuming WD = 04_BO/).
# ------------------------------------------------------------------------------

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
  ggplot2::ggsave(
    filename = file.path("images", paste0(name, ".pdf")),
    plot     = plot,
    width    = width,
    height   = height,
    device   = grDevices::cairo_pdf,
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
  mlr3mbo::srlrn(
    mlr3::lrn("regr.km", covtype = "matern5_2", optim.method = "BFGS",
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
  data.table::set(grid, j = "y_hat", value = prediction$mean)
  data.table::set(grid, j = "y_min", value = prediction$mean - prediction$se)
  data.table::set(grid, j = "y_max", value = prediction$mean + prediction$se)
  prediction
}


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
  v = ggplot2::sym(value)
  ggplot2::ggplot(grid, ggplot2::aes(x = x1, y = x2)) +
    ggplot2::geom_raster(ggplot2::aes(fill = !!v)) +
    ggplot2::geom_contour(ggplot2::aes(z = !!v), colour = "white", alpha = 0.4,
                          linewidth = 0.3, bins = 8) +
    ggplot2::geom_point(ggplot2::aes(x = x1, y = x2), data = design,
                        shape = 4, colour = "#ffcc00", size = 3, stroke = 1.3) +
    ggplot2::scale_fill_viridis_c(option = "mako", direction = -1) +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_text(face = "bold")
    )
}

