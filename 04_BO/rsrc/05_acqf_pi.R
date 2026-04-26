# Used in: 04_BO/05_acqf.tex
#
# PI illustration: GP posterior at one test point, with the area below c_min
# shaded as the probability of improvement.

source("rsrc/_setup.R")

set.seed(123)

# Fit GP on a small archive, predict at lambda = 0.
instance = OptimInstanceSingleCrit$new(objective = toy1d_obj_rfundt, terminator = trm("none"))
instance$eval_batch(data.table(x = c(0.100, 0.300, 0.650, 1.000, 0.348, 0.400, 0.349)))

surrogate = my_gp_surrogate(instance$archive)
surrogate$update()
pred = surrogate$predict(data.table(x = 0))
cmin = instance$archive$best()$y

# Posterior density at the test point.
pi_normal = data.table(y = seq(-2, 2.2, length.out = 1001L))
pi_normal[, d := dnorm(y, mean = pred$mean, sd = pred$se)]

g = ggplot(pi_normal, aes(x = y, y = d)) +
  geom_area(data = pi_normal[y <= cmin]) +
  geom_line(colour = "darkgrey") +
  geom_vline(xintercept = cmin, colour = "#00A64F", linetype = 2) +
  labs(x = expression(Y(lambda)), y = "density") +
  theme_minimal()

myggsave("05_bayesian_loop_pi_0", plot = g, width = 5, height = 4)
