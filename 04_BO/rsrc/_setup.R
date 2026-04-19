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


#  Standard GP surrogate used throughout the 04_BO figure scripts
my_gp_surrogate = function(archive) {
  mlr3mbo::srlrn(
    mlr3::lrn("regr.km", covtype = "matern5_2", optim.method = "BFGS",
              nugget.stability = 1e-8, control = list(trace = FALSE)),
    archive = archive
  )
}

