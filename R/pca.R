library(tidyverse)
library(broom)
theme_set(theme_light())

wq <- read_csv("data/2020_olympics/wq.csv")
wq_cleaned <- wq %>%
  mutate(
    a = as.numeric(str_remove_all(a, "[A-z]")),
    b = as.numeric(str_remove_all(b, "[A-z]")),
    speed_best = ifelse(is.na(a), b,
                        ifelse(is.na(b), a,
                               ifelse(a < b, a, b))),
    bould_tops = as.numeric(str_sub(results, 1, 1)),
    lead_hr = ifelse(
      str_detect(hr, "\\+"),
      as.numeric(str_sub(hr, 1, nchar(hr) - 1)) + 0.5,
      as.numeric(hr)
    )
  ) %>%
  select(climber, speed_best, bould_tops, lead_hr) %>%
  column_to_rownames(var = "climber")

# SOME EDA

# pca fit
wq_pca <- wq_cleaned %>%
  prcomp(scale = TRUE)

# plot
library(ggfortify)
wq_pca %>% 
  autoplot(label = TRUE, 
           label.size = 2,
           loadings = TRUE,
           loadings.label = TRUE, 
           loadings.label.size = 3)


# percent variability
wq_pca %>% 
  tidy(matrix = "eigenvalues")

wq_pca %>% 
  tidy(matrix = "eigenvalues") %>% 
  ggplot(aes(PC, percent)) +
  geom_col()

# scores and loadings
wq_pca %>% 
  tidy(matrix = "scores")

wq_pca %>% 
  tidy(matrix = "loadings")

# how each variable contributes to each PC
wq_pca %>% 
  tidy(matrix = "loadings") %>% 
  ggplot(aes(value, column)) +
  facet_wrap(~ PC) +
  geom_col()

wq_pca %>% 
  tidy(matrix = "rotation")

# attach pca fit to original data
wq_pca %>% 
  augment(wq_cleaned)



# https://clauswilke.com/blog/2020/09/07/pca-tidyverse-style/

# arrow_style <- arrow(
#   angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
# )
# 
# wq_pca %>%
#   tidy(matrix = "rotation") %>%
#   pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") %>%
#   ggplot(aes(PC1, PC2)) +
#   geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
#   geom_text(
#     aes(label = column),
#     hjust = 1, nudge_x = -0.02, 
#     color = "#904C2F"
#   ) +
#   coord_fixed() +
#   xlim(-1.25, .5) + ylim(-.5, 1)
