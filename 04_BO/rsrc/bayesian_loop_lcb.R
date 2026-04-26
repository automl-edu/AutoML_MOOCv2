# Used in: 04_BO/05_acqf.tex
#
# BO loop on the 1D toy with Lower Confidence Bound (LCB) at tau = 3.
# The upper-plot uncertainty band is also drawn at +-tau*sd so that the
# darkred lower edge of the band coincides with the LCB curve below.

library(bbotk)
library(data.table)
library(mlr3mbo)
library(mlr3learners)
library(ggplot2)
library(patchwork)
source("rsrc/_setup.R")

set.seed(123)

# ------------------------------------------------------------------------------

objective = toy1d_obj_rfundt
instance = OptimInstanceSingleCrit$new(
  objective = objective,
  terminator = trm("none")
)

# only use the four initial data points + 0.370
xdt = data.table(x = c(0.100, 0.300, 0.370, 0.650, 1.000))
instance$eval_batch(xdt)

grid = generate_design_grid(instance$search_space, resolution = 1001L)$data
set(grid, j = "y", value = objective$eval_dt(grid)$y)

tau = 3
surrogate = my_gp_surrogate(instance$archive)
acq_function = acqf("cb", surrogate = surrogate, lambda = tau)

prediction = update_surrogate_and_grid(surrogate, grid)

# Widen band to +-tau*sd so the lower edge matches LCB at tau
set(grid, j = "y_min", value = prediction$mean - tau * prediction$se)
set(grid, j = "y_max", value = prediction$mean + tau * prediction$se)

acq_function$update()
set(grid, j = "cb", value = acq_function$eval_dt(grid[, "x"])$acq_cb)
cb_argmin = grid[which.min(cb), ]

# initial design + surrogate prediction + arg max of cb + cb
g = ggplot(aes(x = x, y = y), data = grid) +
  geom_line() +
  geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
  geom_ribbon(aes(min = y_min, max = y_max), fill = "steelblue", colour = NA, alpha = 0.25) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  geom_line(aes(x = x, y = y_min), colour = "darkred") +
  xlim(c(0, 1)) +
  ylim(c(-2, 3.8)) +
  labs(x = expression(lambda), y = expression(c)) + theme_minimal()

cb = ggplot(aes(x = x, y = cb), data = grid) +
  geom_line(colour = "darkred") +
  geom_point(aes(x = x, y = cb), size = 3L, colour = "darkred", data = cb_argmin) +
  xlim(c(0, 1)) +
  ylim(c(-2, 3.8)) +
  ylab(bquote("LCB, " * tau == .(tau))) +
  labs(x = expression(lambda)) + theme_minimal()

myggsave("bayesian_loop_lcb_0_obj", plot = g,  width = 5, height = 2)
myggsave("bayesian_loop_lcb_0_acq", plot = cb, width = 5, height = 2)

