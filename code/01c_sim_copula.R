library(tidyverse)
theme_set(theme_minimal())
library(copula)

# theta <- iRho(normalCopula(), rho = c(0, 0, 1))

# U <- rCopula(10000, copula = normalCopula(theta, dim = 3, dispstr = "un"))
# cor(U, method = "kendall")
# U <- apply(U, 2, rank)
# cor(U, method = "kendall")
# apply(U, 1, prod)

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
  bind_rows(final) %>% 
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
  
  qcor <- read_csv("https://raw.githubusercontent.com/qntkhvn/climbing/main/data/2020_olympics/wq.csv") %>% 
  summarize(qcor = cor(bouldering, lead, method = "kendall")) %>% 
  pull(qcor)
  
fcor <- read_csv("https://raw.githubusercontent.com/qntkhvn/climbing/main/data/2020_olympics/wf.csv") %>% 
  summarize(fcor = cor(bouldering, lead, method = "kendall")) %>% 
  pull(fcor)

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
