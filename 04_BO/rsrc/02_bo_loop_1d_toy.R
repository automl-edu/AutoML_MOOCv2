# Used in: 04_BO/02_bo_basics.tex
#
# render BO loop on 1D toy example

source("rsrc/_setup.R")

set.seed(123)

# ------------------------------------------------------------------------------

objective = toy1d_obj_rfundt
instance = OptimInstanceSingleCrit$new(
  objective = objective,
  terminator = trm("none")
)

# define initial design manually
xdt = data.table(x = c(0.1, 0.3, 0.65, 1))
instance$eval_batch(xdt)

surrogate = my_gp_surrogate(instance$archive)
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
#   latest         optional data.table with a single row (x, y) of the just-evaluated
#                  point; drawn as red dot (carries the proposal color into the eval frame)
#   show_truth     draw the true objective curve
#   show_surrogate draw the surrogate mean (dashed steelblue)
#   xlim, ylim     axis limits
loop_plot = function(grid,
                     design         = NULL,
                     proposal       = NULL,
                     latest         = NULL,
                     show_truth     = FALSE,
                     show_surrogate = FALSE,
                     xlim           = c(0, 1),
                     ylim           = c(-2, 2.2)) {
  g = ggplot(aes(x = x, y = y), data = grid)
  if (show_truth) {
    g = g + geom_line()
  }
  if (show_surrogate) {
    g = g + geom_line(
      aes(x = x, y = y_hat), colour = "steelblue", linetype = 2
    )
  }
  if (!is.null(design)) {
    g = g + geom_point(
      aes(x = x, y = y), size = 3L, colour = "black", data = design
    )
  }
  if (!is.null(proposal)) {
    g = g + geom_point(
      aes(x = x, y = y_hat), size = 5L, colour = "darkred", data = proposal
    )
  }
  if (!is.null(latest)) {
    g = g + geom_point(
      aes(x = x, y = y), size = 5L, colour = "darkred", data = latest
    )
  }
  g +
    xlim(xlim) +
    ylim(ylim) +
    labs(x = expression(lambda), y = expression(c)) +
    theme_minimal()
}


mean_argmin = refresh_surrogate()
archive     = instance$archive$data

# ------------------------------------------------------------------------------

# 0: true function only
myggsave("02_bo_loop_1d_toy_0", plot = loop_plot(grid, show_truth = TRUE))

# 1: initial design
myggsave("02_bo_loop_1d_toy_1", plot = loop_plot(grid, design = archive))

# 2: initial design + surrogate
myggsave("02_bo_loop_1d_toy_2", plot = loop_plot(grid, design = archive, show_surrogate = TRUE))

# 3: + argmin of surrogate (proposal)
myggsave("02_bo_loop_1d_toy_3", plot = loop_plot(grid, design = archive, proposal = mean_argmin, show_surrogate = TRUE))

# 4: eval proposal, show updated design (surrogate not yet refit)
instance$eval_batch(mean_argmin[, "x", with = FALSE])
archive = instance$archive$data
latest  = archive[.N]
old     = archive[-.N]
myggsave("02_bo_loop_1d_toy_4", plot = loop_plot(grid, design = old, latest = latest, show_surrogate = TRUE))

# 5: refit surrogate on extended design
mean_argmin = refresh_surrogate()
myggsave("02_bo_loop_1d_toy_5", plot = loop_plot(grid, design = archive, show_surrogate = TRUE))

# 6: + new proposal
myggsave("02_bo_loop_1d_toy_6", plot = loop_plot(grid, design = archive, proposal = mean_argmin, show_surrogate = TRUE))

# 7: eval new proposal
instance$eval_batch(mean_argmin[, "x", with = FALSE])
archive = instance$archive$data
latest  = archive[.N]
old     = archive[-.N]
myggsave("02_bo_loop_1d_toy_7", plot = loop_plot(grid, design = old, latest = latest, show_surrogate = TRUE))

# 8: refit surrogate
mean_argmin = refresh_surrogate()
myggsave("02_bo_loop_1d_toy_8", plot = loop_plot(grid, design = archive, show_surrogate = TRUE))

# 9: + new proposal
myggsave("02_bo_loop_1d_toy_9", plot = loop_plot(grid, design = archive, proposal = mean_argmin, show_surrogate = TRUE))

# 10: eval proposal
instance$eval_batch(mean_argmin[, "x", with = FALSE])
archive = instance$archive$data
latest  = archive[.N]
old     = archive[-.N]
myggsave("02_bo_loop_1d_toy_10", plot = loop_plot(grid, design = old, latest = latest, show_surrogate = TRUE))

# 11: show truth as well -- we missed the global optimum
myggsave("02_bo_loop_1d_toy_11", plot = loop_plot(grid, design = archive, show_truth = TRUE, show_surrogate = TRUE))
