# Used in: 04_BO/02_bo_basics.tex
# 
# Visualize random vs LHS init designs

library(bbotk)
library(data.table)
library(mlr3mbo)
library(mlr3learners)
library(ggplot2)
library(ggExtra)
library(lhs)

set.seed(5)

# ------------------------------------------------------------------------------

domain = ps(x1 = p_dbl(lower = 0, upper = 1), x2 = p_dbl(lower = 0, upper = 1))

qs = seq(from = 0, to = 1, length.out = 11)

xdt_random = generate_design_random(domain, n = 10L)$data
xdt_random[, method := "random"]
xdt_lhs = generate_design_lhs(domain, n = 10L)$data
xdt_lhs[, method := "lhs"]
xdt = rbind(xdt_random, xdt_lhs)

g = ggplot(aes(x = x1, y = x2), data = xdt[method == "random"]) +
  geom_point(size = 3L) +
  geom_vline(xintercept = qs, linetype = 2) +
  geom_hline(yintercept = qs, linetype = 2) +
  labs(title = "Random Design", x = expression(lambda[1]), y = expression(lambda[2])) +
  theme_minimal()

g = ggMarginal(g, type = "histogram", bins = 11)

ggsave("images/initdes_randomvslhs_random.png", plot = g, width = 5, height = 4)

g = ggplot(aes(x = x1, y = x2), data = xdt[method == "lhs"]) +
  geom_point(size = 3L) +
  geom_vline(xintercept = qs, linetype = 2) +
  geom_hline(yintercept = qs, linetype = 2) +
  labs(title = "LHS", x = expression(lambda[1]), y = expression(lambda[2])) +
  theme_minimal()

g = ggMarginal(g, type = "histogram", bins = 11)

ggsave("images/initdes_randomvslhs_lhs.png", plot = g, width = 5, height = 4)

