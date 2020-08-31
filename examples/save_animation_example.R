# This is MWE of making an animation for slides following the gnlab norms.

# Making graphs requires various stylistic decisions.
# To make animations, we will have to be slightly more proactive in this decision 
# making process.

### General process:
# - Make final graph (not shown in MWE)
# - Break into separate plots stating with `base`
# - for each plot add the layers on top of `base` 
#   saving intermediate plots as `plot_i`
# - store all the plots in a list and save to png using `save_animation()`


source("prelim.R")
library(tidyverse)
library(RColorBrewer)


# To make a static legend specify order 
order <- levels(diamonds$cut)[c(1,5)]
colors <- brewer.pal("Set2", n=2)

# associate order with aesthetics (works with linetype, shape etc)
names(colors) <- order


# Define all auxiliaries to the plotting as the base (e.g. themes, scales, legends)
base <-
  diamonds %>%
  ggplot(aes(color = cut, x = carat, y = price)) + 
  scale_color_manual(values = colors, breaks = order) +
  fte_theme() +
  # Pin the top left corner of legend so it stays in place
  # as additional legend items are added below previous items
  theme(legend.position = c(0, 1),
        legend.justification = c(0,1))


# for bigger animations this would be in a loop
plot_1 <- 
  base + 
  geom_smooth(data = . %>% filter(cut == order[1]), se = FALSE)

plot_2 <-
  plot_1 +
  geom_smooth(data = . %>% filter(cut == order[2]), se = FALSE)


plots <- list(plot_1, plot_2)

save_animation(plots, out_path = "./", out_name = "my_plots")
