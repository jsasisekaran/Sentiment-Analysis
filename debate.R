library(tidyverse)
library(tidytext)

#analyzing the us presidential debate 
#words used largely by candidates
debate %>% 
  group_by(Speaker) %>%
  unnest_tokens(word, Text) %>% 
  group_by(Speaker) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = T) %>% 
  mutate(word = str_extract(word, "[a-z]+")) %>% 
  na.omit() %>% 
  filter(n > 30) %>%
  ggplot(., aes(reorder(word, n), n, fill = Speaker)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Score") +
  xlab("Sentiments")


#Get speakers with most negative words
debate %>% 
  unnest_tokens(word, Text) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing")) %>% 
  group_by(sentiment, Speaker) %>% 
  count(word, sentiment, sort = T) %>% 
  top_n(10)
  
#get the sentiments score for clinton and trump participant
#trump
debate %>% 
  filter(Speaker %in% c("Trump","Clinton")) %>% 
  unnest_tokens(word, Text) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(Speaker, index = Line, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>% 
  ggplot(.,aes(index, sentiment, fill = Speaker)) +
           geom_col(show.legend = FALSE) +
           facet_wrap(~Speaker, ncol = 10, scales = "free_x")

#Clinton and Trump used more of negative words


#plot a comparison of postive and negative words used by contestants (trump vs clinton)
debate %>% 
  filter(Speaker %in% c("Trump","Clinton")) %>% 
  unnest_tokens(word, Text) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing")) %>% 
  group_by(sentiment, Speaker) %>% 
  count(word) %>% 
  top_n(10) %>% 
  ggplot(., aes(reorder(word, n), n, fill = Speaker)) +
  geom_col(show.legend = T) +
  coord_flip() +
  facet_wrap(~sentiment, scales = "free_y") +
  xlab("Words") +
  ylab("frequency")


#Apparently trump used more begative words than clinton from the above

#create a workd cloud of positive and negative words for hilary and clinton
debate %>% 
  filter(Speaker %in% c("Trump","Clinton")) %>% 
  unnest_tokens(word, Text) %>% 
  mutate(word = gsub("problems", "problem", word)) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment) %>% 
  acast(word~sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(color = c("#1b2a49", "#00909e"),
                 max.words = 100)


