library(tidyverse)
theme_set(theme_minimal())
library(copula)

# Greg's old-man base R code
# theta <- iRho(normalCopula(), rho = c(0, 0, 1))
# U <- rCopula(10000, copula = normalCopula(theta, dim = 3, dispstr = "un"))
# cor(U, method = "kendall")
# U <- apply(U, 2, rank)
# cor(U, method = "kendall")
# apply(U, 1, prod)

# simulation, assuming uniform ranks
# this function takes in the number of simulations and players 
# and returns a simulated data frame with the following attributes: 
# player ID, the discipline ranks, overall score, final rank, sim number
# Here we add in correlation between bouldering and lead. 
# We test 5 values 0, 0.25, 0.5, 0.75, 1

climbing_sim_cop <- function(nsim, nplay, rho) { 
  
  theta <- iTau(normalCopula(), tau = c(0, 0, rho))
  
  sims <- list()
  
  for(i in 1:nsim) {
    
    cop <- rCopula(nplay, copula = normalCopula(theta, dim = 3, dispstr = "un"))
    
    sims[[i]] <- bind_cols(player = 1:nplay,
                           e1 = rank(cop[, 1]),
                           e2 = rank(cop[, 2]),
                           e3 = rank(cop[, 3])) %>% 
      mutate(sim = i) 
  }
  
  results <- bind_rows(sims) %>% 
    mutate(score = e1 * e2 * e3) %>% 
    group_by(sim) %>% 
    mutate(rank = rank(score, ties.method = "random")) %>% 
    ungroup()
  
  return(results)
}

set.seed(1)
cor_vec <- seq(0, 1, length = 5)
qual <- final <- list()
for (c in 1:length(cor_vec)) {
  
  qual[[c]] <- climbing_sim_cop(nsim = 10000, nplay = 20, rho = cor_vec[c]) %>% 
    mutate(rho = cor_vec[c], round = "Qualification")
  final[[c]] <- climbing_sim_cop(nsim = 10000, nplay = 8, rho = cor_vec[c]) %>% 
    mutate(rho = cor_vec[c], round = "Final")
}  

sim_results <- bind_rows(qual) %>% 
  bind_rows(final) 

# prob of winning qual and final, given speed 1st vs boulder/lead 1st
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
  filter(rank == 1 & prob == "Cumulative")

sim_results %>% 
  mutate(round = fct_rev(round),
         type = fct_rev(type)) %>%
  ggplot(aes(rho, value, color = type)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ round) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  scale_color_manual(values = c("#E69F00", "#0072B2")) +
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

# qualification lead-boulder correlation
qcor <- read_csv("https://raw.githubusercontent.com/qntkhvn/climbing/main/data/2020_olympics/wq.csv") %>% 
  summarize(qcor = cor(bouldering, lead, method = "kendall")) %>% 
  pull(qcor)

# final lead-boulder correlation
fcor <- read_csv("https://raw.githubusercontent.com/qntkhvn/climbing/main/data/2020_olympics/wf.csv") %>% 
  summarize(fcor = cor(bouldering, lead, method = "kendall")) %>% 
  pull(fcor)

# generate sims and obtain probabilities
set.seed(1)
qual <- climbing_sim_cop(nsim = 10000, nplay = 20, rho = qcor)
final <- climbing_sim_cop(nsim = 10000, nplay = 8, rho = fcor)

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
  scale_fill_manual(values = c("#E69F00", "#0072B2")) +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        legend.margin = margin(-5),
        legend.key.size = unit(0.4, "cm"))

# check for variability between simulations
# set.seed(1)
# qq <- list()
# for (i in 1:100) {
#  print(i)
#  qq[[i]] <- climbing_sim(nsim = 10000, nplay = 20, rho = qcor) %>% 
#    mutate(segment = i)
# }
# qq %>% 
#  bind_rows() %>% 
#  filter(e1 == 1 | e2 == 1 | e3 == 1) %>%
#  group_by(segment) %>% 
#  count(rank) %>%
#  mutate(Probability = n / sum(n),
#         Cumulative = cumsum(Probability)) %>% 
#  group_by(rank) %>% 
#  summarize(variance = var(Probability))

# small variance

# Question: What's the expected score for each rank in both qualification and final?

final %>%
  group_by(rank) %>%
  summarize(avg_score = mean(score),
            low_lim = quantile(score, 0.025),
            high_lim = quantile(score, 0.975)) %>%
  mutate(round = "Final",
         color = ifelse(rank == 1, "gold",
                        ifelse(rank == 2, "#C0C0C0",
                               ifelse(rank == 3, "#A77044", "#0072B2")))) %>%
  bind_rows(
    qual %>%
      group_by(rank) %>%
      summarize(avg_score = mean(score),
                low_lim = quantile(score, 0.025),
                high_lim = quantile(score, 0.975)) %>%
      filter(rank <= 10) %>%
      mutate(round = "Qualification",
             color = ifelse(rank < 9, "#E69F00", "#0072B2"))
  ) %>%
  mutate(round = fct_relevel(round, "Qualification")) %>%
  ggplot(aes(rank, avg_score, fill = color)) +
  geom_col() +
  geom_errorbar(aes(ymin = low_lim, ymax = high_lim), width = 0.3, alpha = 0.7) +
  # geom_text(aes(label = ceiling(avg_score)), color = "black", size = 2.7, vjust = -0.3) +
  facet_wrap(~ round, scales = "free") +
  scale_x_continuous(breaks = 1:10) +
  scale_fill_identity() +
  labs(x = "Rank",
       y = "Average Score") +
  theme(panel.grid.major.x = element_blank())  
