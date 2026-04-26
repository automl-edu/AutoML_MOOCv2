# Used in: 04_BO/04_rf_unc.tex
#
# 1D visualizations of RF-based variance estimators discussed in the chapter:
# naive between-tree variance and quantile regression forests. Both fits use
# the same toy 1D data (sin with a data gap in the middle of the domain) so
# the plots are directly comparable.

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

# ---- ranger forest with everything we need ---------------------------------
B = 500
rf = ranger(y ~ x, data = train, num.trees = B,
            keep.inbag = TRUE, quantreg = TRUE)

preds = predict(rf, data = grid, predict.all = TRUE)$predictions
grid[, mean := rowMeans(preds)]

# ---- naive between-tree variance --------------------------------------------
grid[, sd_naive := apply(preds, 1, sd)]

# ---- QRF: 10 / 50 / 90% quantiles -------------------------------------------
q = predict(rf, data = grid, type = "quantiles",
            quantiles = c(0.1, 0.5, 0.9))$predictions
grid[, qrf_lo  := q[, 1]]
grid[, qrf_med := q[, 2]]
grid[, qrf_hi  := q[, 3]]

# ---- plotting helpers -------------------------------------------------------
band_plot = function(grid, train, mean_col, sd_col) {
  ggplot(grid, aes(x = x)) +
    geom_ribbon(aes(ymin = .data[[mean_col]] - 2 * .data[[sd_col]],
                    ymax = .data[[mean_col]] + 2 * .data[[sd_col]]),
                fill = "steelblue", alpha = 0.25) +
    geom_line(aes(y = truth), colour = "black") +
    geom_line(aes(y = .data[[mean_col]]),
              colour = "steelblue", linetype = 2) +
    geom_point(data = train, aes(x = x, y = y),
               colour = "black", size = 1.2, alpha = 0.7) +
    coord_cartesian(ylim = c(-2, 2)) +
    labs(x = expression(x), y = expression(f)) +
    theme_minimal()
}

myggsave("04_rf_unc_naive",
         band_plot(grid, train, "mean", "sd_naive"),
         width = 4.2, height = 3.2)

qrf_plot = ggplot(grid, aes(x = x)) +
  geom_ribbon(aes(ymin = qrf_lo, ymax = qrf_hi),
              fill = "steelblue", alpha = 0.25) +
  geom_line(aes(y = truth), colour = "black") +
  geom_line(aes(y = qrf_med),
            colour = "steelblue", linetype = 2) +
  geom_point(data = train, aes(x = x, y = y),
             colour = "black", size = 1.2, alpha = 0.7) +
  coord_cartesian(ylim = c(-2, 2)) +
  labs(x = expression(x), y = expression(f)) +
  theme_minimal()

myggsave("04_rf_unc_qrf", qrf_plot,
         width = 4.2, height = 3.2)
