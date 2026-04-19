# Used in: 04_BO/02_bo_basics.tex
#
# Visualize random vs LHS init designs

library(bbotk)
library(data.table)
library(ggplot2)
library(ggExtra)
library(lhs)
source("rsrc/_setup.R")

set.seed(5)

# ------------------------------------------------------------------------------

domain = ps(x1 = p_dbl(lower = 0, upper = 1), x2 = p_dbl(lower = 0, upper = 1))

qs = seq(from = 0, to = 1, length.out = 11)

xdt_random = generate_design_random(domain, n = 10L)$data
xdt_random[, method := "random"]
xdt_lhs = generate_design_lhs(domain, n = 10L)$data
xdt_lhs[, method := "lhs"]
xdt = rbind(xdt_random, xdt_lhs)

# simple 2d plot of init des, with lines for cells
plot_design = function(data, title, file) {
  g = ggplot(aes(x = x1, y = x2), data = data) +
    geom_point(size = 3L) +
    geom_vline(xintercept = qs, linetype = 2) +
    geom_hline(yintercept = qs, linetype = 2) +
    labs(title = title, x = expression(lambda[1]), y = expression(lambda[2])) +
    theme_minimal()
  g = ggMarginal(g, type = "histogram", bins = 11)
  myggsave(file, plot = g, width = 5, height = 4)
}

plot_design(xdt[method == "random"], "Random Design", "initdes_randomvslhs_random")
plot_design(xdt[method == "lhs"],    "LHS",            "initdes_randomvslhs_lhs")

