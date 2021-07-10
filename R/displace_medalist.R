# Hannah Butler
# 07-09-2021
# Olympic Sport Climbing Scoring

## Libraries
library(tidyverse)
## set constants

## define functions
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

## begin code

set.seed(80085)
first <- vector(mode = "numeric", length = 1000)
second <- vector(mode = "numeric", length = 1000)
third <- vector(mode = "numeric", length = 1000)

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

first <- replicate(100, displace_climber(m_place = 1))
second <- replicate(100, displace_climber(m_place = 2))
third <- replicate(100, displace_climber(m_place = 3))
