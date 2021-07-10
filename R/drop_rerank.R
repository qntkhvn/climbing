library(tidyverse)
theme_set(theme_light())

women_final <- read_csv("data/youth_olympics_2018/women_final.csv")
rerank <- list()
for (i in 1:nrow(women_final)){
  rerank[[i]] <- women_final[-i,] %>%
    mutate(rank_drop = i)
}

rerank_df <- women_final %>% 
  mutate(rank_drop = 0) %>% 
  bind_rows(rerank) %>% 
  group_by(rank_drop) %>% 
  mutate(speed = rank(speed),
         bould = rank(bould),
         lead = rank(lead),
         total = speed * bould * lead) %>% 
  arrange(total, .by_group = TRUE) %>% 
  mutate(rank = row_number()) %>% 
  ungroup()



rerank_df %>% 
  ggplot(aes(x = reorder(last, -rank), y = total)) +
  geom_col() +
  geom_text(aes(label = total), hjust = -0.1, size = 3) +
  coord_flip() +
  facet_wrap(~ rank_drop, ncol = 3) +
  labs(title = "Women's final rankings",
       subtitle = "0 is the original rankings, each facet number is the dropped rank",
       x = "")


# tie breaker
# add a lag variable

