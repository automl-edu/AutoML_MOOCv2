# Used in: 04_BO/01_big_picture.tex
# 
# Visualize initial design: random
# ------------------------------------------------------------------------------

library(bbotk)
library(data.table)
library(ggplot2)

set.seed(123)

# ------------------------------------------------------------------------------

domain = ps(x1 = p_dbl(lower = 0, upper = 1), x2 = p_dbl(lower = 0, upper = 1))

xdt_random = generate_design_random(domain, n = 100L)$data

g = ggplot(aes(x = x1, y = x2), data = xdt_random) +
  geom_point(size = 3L) +
  geom_rug() +
  labs(x = expression(lambda[1]), y = expression(lambda[2])) +
  theme_minimal()

ggsave("images/initdes_random.png", plot = g, width = 5, height = 4)
