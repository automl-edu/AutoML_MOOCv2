# ------------------------------------------------------------------------------
# bayesian optimization

# FIG: performs Bayesian optimization using a Gaussian Process Regression
#      to approximate the function.
# Used in: 04_BO/02_bo_basics.tex
# ------------------------------------------------------------------------------

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

xdt = data.table(x = c(0.1, 0.3, 0.65, 1))
instance$eval_batch(xdt)

surrogate = srlrn(lrn("regr.km", covtype = "matern5_2", optim.method = "BFGS"), archive = instance$archive)
acq_function = acqf("mean", surrogate = surrogate)

grid = generate_design_grid(instance$search_space, resolution = 1001L)$data
set(grid, j = "y", value = objective$eval_dt(grid)$y)

acq_function$surrogate$update()
prediction = surrogate$predict(grid)
set(grid, j = "y_hat", value = prediction$mean)

acq_function$update()
set(grid, j = "mean", value = acq_function$eval_dt(grid[, "x"])$acq_mean)
mean_argmin = grid[which.min(mean), ]

# function 
g = ggplot(aes(x = x, y = y), data = grid) +
  geom_line() +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.2)) +
  theme_minimal()

myggsave("loop_0", plot = g, width = 5, height = 4)

# initial design
g = ggplot(aes(x = x, y = y), data = grid) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.2)) +
  theme_minimal()

myggsave("loop_1", plot = g, width = 5, height = 4)

# intial design + surrogate prediction
g = ggplot(aes(x = x, y = y), data = grid) +
  geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.2)) +
  theme_minimal()

myggsave("loop_2", plot = g, width = 5, height = 4)

# initial design + surrogate prediction + arg min of surrogate prediction
g = ggplot(aes(x = x, y = y), data = grid) +
  geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  geom_point(aes(x = x, y = y_hat), size = 3L, colour = "darkred", data = mean_argmin) +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.2)) +
  theme_minimal()

myggsave("loop_3", plot = g, width = 5, height = 4)

instance$eval_batch(mean_argmin[, "x", with = FALSE])

# initial design + surrogate prediction + arg min of surrogate prediction
g = ggplot(aes(x = x, y = y), data = grid) +
  geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.2)) +
  theme_minimal()

myggsave("loop_4", plot = g, width = 5, height = 4)

# eval arg min of surrogate prediction, refit surrogate and obtain new arg min of surrogate prediction
acq_function$surrogate$update()
prediction = surrogate$predict(grid)
set(grid, j = "y_hat", value = prediction$mean)

acq_function$update()
set(grid, j = "mean", value = acq_function$eval_dt(grid[, "x"])$acq_mean)
mean_argmin = grid[which.min(mean), ]

g = ggplot(aes(x = x, y = y), data = grid) +
  geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.2)) +
  theme_minimal()

myggsave("loop_5", plot = g, width = 5, height = 4)

g = ggplot(aes(x = x, y = y), data = grid) +
  geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  geom_point(aes(x = x, y = y_hat), size = 3L, colour = "darkred", data = mean_argmin) +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.2)) +
  theme_minimal()

myggsave("loop_6", plot = g, width = 5, height = 4)

instance$eval_batch(mean_argmin[, "x", with = FALSE])

g = ggplot(aes(x = x, y = y), data = grid) +
  geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.2)) +
  theme_minimal()

myggsave("loop_7", plot = g, width = 5, height = 4)

# eval arg min of surrogate prediction, refit surrogate and obtain new arg min of surrogate prediction
acq_function$surrogate$update()
prediction = surrogate$predict(grid)
set(grid, j = "y_hat", value = prediction$mean)

acq_function$update()
set(grid, j = "mean", value = acq_function$eval_dt(grid[, "x"])$acq_mean)
mean_argmin = grid[which.min(mean), ]

g = ggplot(aes(x = x, y = y), data = grid) +
  geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.2)) +
  theme_minimal()

myggsave("loop_8", plot = g, width = 5, height = 4)

g = ggplot(aes(x = x, y = y), data = grid) +
  geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  geom_point(aes(x = x, y = y_hat), size = 3L, colour = "darkred", data = mean_argmin) +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.2)) +
  theme_minimal()

myggsave("loop_9", plot = g, width = 5, height = 4)

instance$eval_batch(mean_argmin[, "x", with = FALSE])

g = ggplot(aes(x = x, y = y), data = grid) +
  geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.2)) +
  theme_minimal()

myggsave("loop_10", plot = g, width = 5, height = 4)

# same plot as before but also showing that we actually missed the global optimum
g = ggplot(aes(x = x, y = y), data = grid) +
  geom_line() +
  geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.2)) +
  theme_minimal()

myggsave("loop_11", plot = g, width = 5, height = 4)

