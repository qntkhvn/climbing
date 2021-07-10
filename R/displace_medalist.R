# Hannah Butler
# 07-09-2021
# Olympic Sport Climbing Scoring
##### Description #####
# estimate the probability of a medalist being displaced if another competitor drops out
##### Libraries #####
library(tidyverse)
library(ggplot2)
##### set constants #####

##### VISUALIZE #####
# just look at this part
load("R/medal_displacement_probabilities.rdata")

displacement_probs %>%
  ggplot() +
  geom_density(aes(x = displ_prob, fill =  medal), alpha = 0.3) +
  facet_wrap(~dropped_rank)

##### Define Functions #####
assign_ranks <- function(c = 8) {
  # randomly assign ranks in 3 events & compute total
  # ranking in an event is considered to be independent from ranking in another event
  output <- data.frame(climber = sample(LETTERS, c, replace = FALSE)
                       , speed = sample(1:c, c, replace = FALSE)
                       , bould = sample(1:c, c, replace = FALSE)
                       , lead = sample(1:c, c, replace = FALSE)
                       ) %>%
    mutate(total = speed*bould*lead) %>%
    arrange(total)
  
  return(output)
}

drop_climber <- function(results, r = 4) {
  # drop a competitor and recompute rankings and totals
  ud_results <- results[-r, ] %>%
    mutate(speed = rank(speed)
           , bould = rank(bould)
           , lead = rank(lead)
           , total = speed*bould*lead
           ) %>%
    arrange(total)
  return(ud_results)
}

##### begin code #####
set.seed(80085)

displace_climber <- function(nsim = 1000, drop_rank = 4, m_place = 1) {
  # simulate 1000 rankings
  rank_perms <- lapply(1:nsim, function(x) assign_ranks())
  # drop ith competitor and recalculate ranks
  dropout_ranks <- lapply(rank_perms, function(x) drop_climber(x, r = drop_rank))
  
  # compare mth place & calculate % not matched
  pct_displ <- sapply(1:nsim, 
                      function(x) {
                        rank_perms[[x]]$climber[m_place] != dropout_ranks[[x]]$climber[m_place]
                        }
                      ) %>%
    sum()/nsim
  
  return(pct_displ)
}

first <- lapply(4:8, function(x) replicate(100, displace_climber(drop_rank = x, m_place = 1)))
second <- lapply(4:8, function(x) replicate(100, displace_climber(drop_rank = x, m_place = 2)))
third <- lapply(4:8, function(x) replicate(100, displace_climber(drop_rank = x, m_place = 3)))

# save(first, second, third, file = "R/prob_displace_medalists.rdata")
####################################
# format data
gold_df <- lapply(1:5
                  , function(x) {
                    data.frame(medal = "gold", dropped_rank = 3+x, displ_prob = first[[x]])
                    }
                  ) %>%
  do.call(rbind, .)
  
silv_df <- lapply(1:5
                  , function(x) {
                    data.frame(medal = "silver", dropped_rank = 3+x, displ_prob = second[[x]])
                    }
                  ) %>%
  do.call(rbind, .)

bron_df <- lapply(1:5
                  , function(x) {
                    data.frame(medal = "bronze", dropped_rank = 3+x, displ_prob = third[[x]])
                    }
                  ) %>%
  do.call(rbind, .)

displacement_probs <- rbind(gold_df, silv_df, bron_df)

# save(displacement_probs, file = "R/medal_displacement_probabilities.rdata")
#########################################
displacement_probs %>%
  ggplot() +
  geom_density(aes(x = displ_prob, fill =  medal), alpha = 0.3) +
  facet_wrap(~dropped_rank)
