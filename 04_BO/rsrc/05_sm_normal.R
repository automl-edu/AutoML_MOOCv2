# Used in: 04_BO/03_gps.tex, 04_BO/05_acqf.tex
#
# Surrogate posterior at a test point: visualizes that, at any fixed x, the
# GP posterior predictive is a 1D normal. Plot shows truth, GP fit, archive,
# and a vertical normal density at the EI argmax.

library(bbotk)
library(mlr3mbo)
library(mlr3learners)
library(data.table)
library(ggplot2)
source("rsrc/_setup.R")

set.seed(123)

objective = toy1d_obj_rfundt
instance = OptimInstanceSingleCrit$new(objective = objective, terminator = trm("none"))
instance$eval_batch(data.table(x = c(0.100, 0.300, 0.650, 1.000, 0.348, 0.400, 0.349)))

surrogate = my_gp_surrogate(instance$archive)
acq_function = acqf("ei", surrogate = surrogate)

grid = generate_design_grid(instance$search_space, resolution = 1001L)$data
set(grid, j = "y", value = objective$eval_dt(grid)$y)
update_surrogate_and_grid(surrogate, grid)
acq_function$update()
set(grid, j = "ei", value = acq_function$eval_dt(grid[, "x"])$acq_ei)
ei_argmax = grid[which.max(ei), ]

# Vertical normal density at ei_argmax, scaled to fit a thin horizontal slot
norm_curve = data.table(y = seq(-2, 2.2, by = 0.01))
norm_curve[, d := dnorm(y, mean = ei_argmax$y_hat, sd = ei_argmax$y_max - ei_argmax$y_hat)]
norm_curve[, x := (d / max(d)) / 10]

g = ggplot(grid, aes(x = x, y = y)) +
  geom_line() +
  geom_line(aes(y = y_hat), colour = "steelblue", linetype = 2) +
  geom_ribbon(aes(ymin = y_min, ymax = y_max), fill = "steelblue", alpha = 0.1) +
  geom_point(aes(y = y), size = 3L, colour = "black", data = instance$archive$data) +
  geom_path(aes(y = y), data = norm_curve, colour = "darkgrey") +
  geom_point(aes(y = y_hat), data = ei_argmax, size = 3L, colour = "darkgrey") +
  geom_segment(aes(x = 0, xend = 0.1, y = ei_argmax$y_hat, yend = ei_argmax$y_hat),
               colour = "darkgrey") +
  xlim(c(0, 1)) + ylim(c(-2, 2.2)) +
  labs(x = expression(lambda), y = expression(c)) + theme_minimal()

myggsave("05_bayesian_loop_sm_normal", plot = g, width = 5, height = 4)
