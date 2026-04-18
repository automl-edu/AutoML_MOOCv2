# Used in: 04_BO/02_bo_basics.tex
#
# render BO loop on 1D toy example

library(bbotk)
library(data.table)
library(mlr3mbo)
library(mlr3learners)
library(ggplot2)
source("rsrc/_setup.R")

set.seed(123)

# ------------------------------------------------------------------------------

objective = ObjectiveRFunDt$new(
 fun = function(xdt) data.table(y = 2 * xdt$x * sin(14 * xdt$x)),
 domain = ps(x = p_dbl(lower = 0, upper = 1)),
 codomain = ps(y = p_dbl(tags = "minimize"))
)
instance = OptimInstanceSingleCrit$new(
  objective = objective,
  terminator = trm("none")
)

# define initial design manually
xdt = data.table(x = c(0.1, 0.3, 0.65, 1))
instance$eval_batch(xdt)

surrogate = srlrn(lrn("regr.km", covtype = "matern5_2", optim.method = "BFGS"), archive = instance$archive)
acq_function = acqf("mean", surrogate = surrogate)

# `grid` is a fine 1D evaluation grid used for plotting.
# cols: x (input), y (true objective), y_hat (surrogate mean), mean (mean-AF value)
grid = generate_design_grid(instance$search_space, resolution = 1001L)$data
set(grid, j = "y", value = objective$eval_dt(grid)$y)

# Refit surrogate and recompute both surrogate-mean predictions on the grid
# and the mean-acquisition-function argmin. Returns the argmin row.
refresh_surrogate = function() {
  acq_function$surrogate$update()
  prediction = surrogate$predict(grid)
  set(grid, j = "y_hat", value = prediction$mean)

  acq_function$update()
  set(grid, j = "mean", value = acq_function$eval_dt(grid[, "x"])$acq_mean)
  grid[which.min(mean), ]
}


# loop_plot -- plot BO iteration
#
# Arguments
#   grid           see above
#   design         optional data.table of evaluated points (x, y); drawn as black dots
#   proposal       optional data.table with a single row (x, y_hat); drawn as red dot
#   show_truth     draw the true objective curve
#   show_surrogate draw the surrogate mean (dashed steelblue)
#   xlim, ylim     axis limits
loop_plot = function(grid,
                     design         = NULL,
                     proposal       = NULL,
                     show_truth     = FALSE,
                     show_surrogate = FALSE,
                     xlim           = c(0, 1),
                     ylim           = c(-2, 2.2)) {
  g = ggplot2::ggplot(ggplot2::aes(x = x, y = y), data = grid)
  if (show_truth) {
    g = g + ggplot2::geom_line()
  }
  if (show_surrogate) {
    g = g + ggplot2::geom_line(
      ggplot2::aes(x = x, y = y_hat), colour = "steelblue", linetype = 2
    )
  }
  if (!is.null(design)) {
    g = g + ggplot2::geom_point(
      ggplot2::aes(x = x, y = y), size = 3L, colour = "black", data = design
    )
  }
  if (!is.null(proposal)) {
    g = g + ggplot2::geom_point(
      ggplot2::aes(x = x, y = y_hat), size = 3L, colour = "darkred", data = proposal
    )
  }
  g +
    ggplot2::xlim(xlim) +
    ggplot2::ylim(ylim) +
    ggplot2::labs(x = expression(lambda), y = expression(c)) +
    ggplot2::theme_minimal()
}


mean_argmin = refresh_surrogate()
archive     = instance$archive$data

# ------------------------------------------------------------------------------

# 0: true function only
myggsave("bo_loop_1d_toy_0", plot = loop_plot(grid, show_truth = TRUE))

# 1: initial design
myggsave("bo_loop_1d_toy_1", plot = loop_plot(grid, design = archive))

# 2: initial design + surrogate
myggsave("bo_loop_1d_toy_2", plot = loop_plot(grid, design = archive, show_surrogate = TRUE))

# 3: + argmin of surrogate (proposal)
myggsave("bo_loop_1d_toy_3", plot = loop_plot(grid, design = archive, proposal = mean_argmin, show_surrogate = TRUE))

# 4: eval proposal, show updated design (surrogate not yet refit)
instance$eval_batch(mean_argmin[, "x", with = FALSE])
archive = instance$archive$data
myggsave("bo_loop_1d_toy_4", plot = loop_plot(grid, design = archive, show_surrogate = TRUE))

# 5: refit surrogate on extended design
mean_argmin = refresh_surrogate()
myggsave("bo_loop_1d_toy_5", plot = loop_plot(grid, design = archive, show_surrogate = TRUE))

# 6: + new proposal
myggsave("bo_loop_1d_toy_6", plot = loop_plot(grid, design = archive, proposal = mean_argmin, show_surrogate = TRUE))

# 7: eval new proposal
instance$eval_batch(mean_argmin[, "x", with = FALSE])
archive = instance$archive$data
myggsave("bo_loop_1d_toy_7", plot = loop_plot(grid, design = archive, show_surrogate = TRUE))

# 8: refit surrogate
mean_argmin = refresh_surrogate()
myggsave("bo_loop_1d_toy_8", plot = loop_plot(grid, design = archive, show_surrogate = TRUE))

# 9: + new proposal
myggsave("bo_loop_1d_toy_9", plot = loop_plot(grid, design = archive, proposal = mean_argmin, show_surrogate = TRUE))

# 10: eval proposal
instance$eval_batch(mean_argmin[, "x", with = FALSE])
archive = instance$archive$data
myggsave("bo_loop_1d_toy_10", plot = loop_plot(grid, design = archive, show_surrogate = TRUE))

# 11: show truth as well -- we missed the global optimum
myggsave("bo_loop_1d_toy_11", plot = loop_plot(grid, design = archive, show_truth = TRUE, show_surrogate = TRUE))
