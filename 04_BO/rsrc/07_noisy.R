# Used in: 04_BO/07_noisy.tex
#
# Three figures for the noisy-BO chapter:
#   1) 07_noisy_data       -- truth + noisy observations (with replicates)
#   2) 07_noisy_nugget     -- nugget GP fit: posterior smooths through noise
#   3) 07_noisy_incumbent  -- the empirical argmin (lucky outlier) vs. the
#                             model-based argmin

library(DiceKriging)
source("rsrc/_setup.R")

set.seed(7)

# Smooth 1D objective + Gaussian observation noise
truth = function(x) sin(2 * pi * x) + 0.5 * x
sigma_noise = 0.25

# Training design with replicates so noise is visible at repeated points
x_train = c(0.05, 0.05,
            0.25, 0.25,
            0.45,
            0.65, 0.65,
            0.85, 0.95)
y_train = truth(x_train) + rnorm(length(x_train), sd = sigma_noise)
train = data.table(x = x_train, y = y_train)

# Plotting grid + truth
gx   = seq(0, 1, length.out = 401)
grid = data.table(x = gx, truth = truth(gx))

# ---- nugget GP fit ----------------------------------------------------------
# Pass the known noise variance so the GP is in regression (smoothing) mode
m = km(~1,
       design   = data.frame(x = x_train),
       response = y_train,
       covtype  = "matern5_2",
       noise.var = rep(sigma_noise^2, length(x_train)),
       control  = list(trace = FALSE))

pred = predict(m, newdata = data.frame(x = gx), type = "UK")
grid[, mean := pred$mean]
grid[, lo   := pred$mean - pred$sd]
grid[, hi   := pred$mean + pred$sd]

# Posterior mean at the training points -- needed to compare incumbents
pred_train = predict(m, newdata = data.frame(x = x_train), type = "UK")
train[, mean := pred_train$mean]

emp_min = train[which.min(y),    .(x, y)]      # argmin_i y_i (noisy outlier)
mod_min = train[which.min(mean), .(x, mean)]   # argmin_i \hat f(\lambda_i)

# ---- shared theme -----------------------------------------------------------
noisy_theme = function() list(
  ylim(c(-1.8, 2.0)),
  labs(x = expression(lambda), y = expression(c)),
  theme_minimal(base_size = 10),
  theme(panel.grid.minor = element_blank())
)

# ---- (1) data only ----------------------------------------------------------
p1 = ggplot(grid, aes(x = x)) +
  geom_line(aes(y = truth), colour = "black", linewidth = 0.6) +
  geom_point(data = train, aes(x = x, y = y),
             colour = "darkred", size = 2.4) +
  noisy_theme()
myggsave("07_noisy_data", plot = p1, width = 5, height = 3)

# ---- (2) nugget GP fit ------------------------------------------------------
p2 = ggplot(grid, aes(x = x)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "steelblue", alpha = 0.18) +
  geom_line(aes(y = truth), colour = "black", linewidth = 0.5) +
  geom_line(aes(y = mean), colour = "steelblue",
            linetype = 2, linewidth = 0.7) +
  geom_point(data = train, aes(x = x, y = y),
             colour = "darkred", size = 2.4) +
  noisy_theme()
myggsave("07_noisy_nugget", plot = p2, width = 5, height = 3)

# ---- (3) empirical vs. model-based incumbent --------------------------------
p3 = ggplot(grid, aes(x = x)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "steelblue", alpha = 0.18) +
  geom_line(aes(y = truth), colour = "black", linewidth = 0.5) +
  geom_line(aes(y = mean), colour = "steelblue",
            linetype = 2, linewidth = 0.7) +
  geom_point(data = train, aes(x = x, y = y),
             colour = "darkred", size = 2.4) +
  geom_point(data = emp_min, aes(x = x, y = y),
             shape = 21, size = 5, colour = "darkorange", stroke = 1.6,
             fill = NA) +
  geom_point(data = mod_min, aes(x = x, y = mean),
             shape = 22, size = 5, colour = "#00A64F", stroke = 1.6,
             fill = NA) +
  annotate("text", x = emp_min$x, y = emp_min$y - 0.25,
           label = expression(argmin[i] ~ y[i]),
           colour = "darkorange", size = 3) +
  annotate("text", x = mod_min$x, y = mod_min$mean + 0.30,
           label = expression(argmin[i] ~ hat(f)(lambda[i])),
           colour = "#00A64F", size = 3) +
  noisy_theme()
myggsave("07_noisy_incumbent", plot = p3, width = 5, height = 3)
