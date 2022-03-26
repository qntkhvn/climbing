# Simulation study

# Packages
library(tidyverse)
theme_set(theme_minimal())
qual_dist_list <- final_dist_list <- list()

# simulation, assuming uniform ranks
# this function takes in the number of simulations and players 
# and returns a simulated data frame with the following attributes: 
# player ID, the discipline ranks, overall score, final rank, sim number
# Here we add in correlation between bouldering and lead. 
# We test 5 values 0, 0.25, 0.5, 0.75, 1

climbing_sim_cor <- function(nsim = 10000, nplay, rho = 0) {
  sims <- list()
  #Number of points in the population 
  N <- 100000
  e1pop <- runif(N)
  e2pop <- runif(N)
  e3pop <- runif(N)
  
  #Pick some indexes
  ind <- sample(1:N, rho * N, replace = FALSE)
  e2pop[ind] <- sort(e2pop[ind])
  e3pop[ind] <- sort(e3pop[ind])
  
  for (i in 1:nsim) {
    
    #Sample nplay rows all together
    ind2 <- sample(1:N, nplay, replace = FALSE)
    
    sims[[i]] <-
      bind_cols(
        player = 1:nplay,
        e1 = rank(e1pop[ind2]),
        e2 = rank(e2pop[ind2]),
        e3 = rank(e3pop[ind2]), 
      ) %>%
      mutate(sim = i)
  }
  results <- bind_rows(sims) %>%
    mutate(score = e1 * e2 * e3) %>%
    group_by(sim) %>%
    mutate(rank = rank(score, ties.method = "random")) %>%
    ungroup()
  
  return(results)
}

# simulate qualification and final rounds
set.seed(1)
cor_vec <- seq(0, 1, length = 5)
qual <- final <- list()
for (c in 1:length(cor_vec)) {
  
  qual[[c]] <- climbing_sim_cor(nsim = 10000, nplay = 20, rho = cor_vec[c]) %>% 
    mutate(rho = cor_vec[c], round = "Qualification")
  
  final[[c]] <- climbing_sim_cor(nsim = 10000, nplay = 8, rho = cor_vec[c]) %>% 
    mutate(rho = cor_vec[c], round = "Final")
}  

sim_results <- bind_rows(qual) %>% 
  bind_rows(final)

sim_results <- sim_results %>% 
  group_by(rho, round) %>% 
  filter(e1 == 1) %>%
  count(rank) %>%
  mutate(Probability = n / sum(n),
         Cumulative = cumsum(Probability)) %>%
  ungroup() %>% 
  select(-n) %>% 
  pivot_longer(Probability:Cumulative, names_to = "prob") %>% 
  mutate(type = "Win Speed") %>% 
  bind_rows(
    sim_results %>% 
      group_by(rho, round) %>% 
      filter(e2 == 1 | e3 == 1) %>%
      count(rank) %>%
      mutate(Probability = n / sum(n),
             Cumulative = cumsum(Probability)) %>%
      ungroup() %>% 
      select(-n) %>% 
      pivot_longer(Probability:Cumulative, names_to = "prob") %>% 
      mutate(type = "Win Bouldering or Lead")
  ) %>% 
  filter(rank == 1 & prob == "Cumulative") %>% 
  write_rds("sim_results_plot.rds")
  
# fig 1: prob of winning qual and final, given speed 1st vs boulder/lead 1st
sim_results %>% 
  mutate(round = fct_rev(round),
         type = fct_rev(type)) %>%
  ggplot(aes(rho, value, color = type)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ round) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  scale_color_manual(values = c("maroon", "midnightblue")) +
  labs(color = "Probability",
       x = "Spearman Correlation",
       y = "Win Probability") +
  theme(legend.margin = margin(-5),
        legend.position = "bottom",
        legend.key.size = unit(0.4, "cm"),
        panel.grid.minor = element_blank()) +
  expand_limits(y = c(0, 1))
  
  
# Question: Given that a climber wins any event, what's the probability...
# 1) of advancing to the final for a qualifier?
# 2) of winning a medal for a finalist?

# simulate qualification and final rounds
# for spearman cor of 0.75
set.seed(1)
qual <- climbing_sim_cor(nsim = 10000, nplay = 20, rho = 0.75)
final <- climbing_sim_cor(nsim = 10000, nplay = 8, rho = 0.75)

# fig 2
final_dist <- final %>%
  filter(e1 == 1 | e2 == 1 | e3 == 1) %>%
  count(rank) %>%
  mutate(Probability = n / sum(n),
         Cumulative = cumsum(Probability)) %>%
  select(-n) %>%
  pivot_longer(!rank, names_to = "type") %>%
  mutate(round = "Final")
qual_dist <- qual %>%
  filter(e1 == 1 | e2 == 1 | e3 == 1) %>%
  count(rank) %>%
  mutate(Probability = n / sum(n),
         Cumulative = cumsum(Probability)) %>%
  select(-n) %>%
  pivot_longer(!rank, names_to = "type") %>%
  mutate(round = "Qualification")
final_dist %>%
  bind_rows(qual_dist) %>%
  mutate(round = fct_relevel(round, "Qualification")) %>%
  ggplot(aes(rank, value, fill = type)) +
  geom_col(position = "dodge") +
  facet_wrap(~ round, scales = "free") +
  scale_x_reverse(breaks = 1:11) +
  coord_flip() +
  labs(x = "Rank",
       y = "Probability Density",
       fill = "Distribution") +
  scale_fill_manual(values = c("maroon", "midnightblue")) +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        legend.margin = margin(-5),
        legend.key.size = unit(0.4, "cm"))

# fig 3
# Question: What's the expected score for each rank in both qualification and final?
# again, using spearman cor of 0.75
# for final, also create color code for gold, silver, bronze

final %>%
  group_by(rank) %>%
  summarize(avg_score = mean(score)) %>%
  mutate(round = "Final",
         color = ifelse(rank == 1, "gold",
                        ifelse(rank == 2, "#C0C0C0",
                               ifelse(rank == 3, "#A77044", "lightblue")))) %>%
  bind_rows(
    qual %>%
      group_by(rank) %>%
      summarize(avg_score = mean(score)) %>%
      filter(rank <= 10) %>%
      mutate(round = "Qualification",
             color = ifelse(rank < 9, "maroon", "lightblue"))
  ) %>%
  mutate(round = fct_relevel(round, "Qualification")) %>%
  ggplot(aes(rank, avg_score, fill = color)) +
  geom_col() +
  geom_text(aes(label = ceiling(avg_score)), color = "black", size = 2.7, vjust = -0.3) +
  facet_wrap(~ round, scales = "free") +
  scale_x_continuous(breaks = 1:10) +
  scale_fill_identity() +
  labs(x = "Rank",
       y = "Average Score") +
  theme(panel.grid.major.x = element_blank())   
