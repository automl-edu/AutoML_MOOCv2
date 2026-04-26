# Used in: 04_BO/04_rf_unc.tex
#
# Extra-trees uncertainty on a 1D toy problem. Same toy data as
# rf_unc_var_estimators.R so the band is visually comparable to the naive
# between-tree band of a standard RF. Plot shows: truth, design, extra-trees
# forest mean, and the +-2 sd band from per-tree disagreement.

library(ranger)
library(data.table)
library(ggplot2)
library(bbotk)
source("rsrc/_setup.R")

set.seed(42)

# ---- toy 1D problem ---------------------------------------------------------
truth = function(x) sin(2 * pi * x)
n_dense = 25
n_gap   = 4
x_train = c(
  runif(n_dense, 0.00, 0.40),
  runif(n_gap,   0.40, 0.70),
  runif(n_dense, 0.70, 1.00)
)
y_train = truth(x_train) + rnorm(length(x_train), sd = 0.15)
train = data.table(x = x_train, y = y_train)

grid = data.table(x = seq(0, 1, length.out = 401))
grid[, truth := truth(x)]

# ---- extra-trees: split point chosen uniformly at random --------------------
B = 500
rf_xt = ranger(y ~ x, data = train, num.trees = B,
               splitrule = "extratrees", num.random.splits = 1L)
preds = predict(rf_xt, data = grid, predict.all = TRUE)$predictions
grid[, mean := rowMeans(preds)]
grid[, sd   := apply(preds, 1, sd)]

g = ggplot(grid, aes(x = x)) +
  geom_ribbon(aes(ymin = mean - 2 * sd, ymax = mean + 2 * sd),
              fill = "steelblue", alpha = 0.25) +
  geom_line(aes(y = truth), colour = "black") +
  geom_line(aes(y = mean), colour = "steelblue", linetype = 2) +
  geom_point(data = train, aes(x = x, y = y),
             colour = "black", size = 1.2, alpha = 0.7) +
  coord_cartesian(ylim = c(-2, 2)) +
  labs(x = expression(x), y = expression(f),
       subtitle = "extra-trees: between-tree variance") +
  theme_minimal()

myggsave("extra_trees", plot = g, width = 4.2, height = 3.2)
