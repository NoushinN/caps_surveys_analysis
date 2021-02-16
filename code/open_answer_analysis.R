# declare dependencies
if (!exists("setup_sourced")) source(here::here("R", "setup.R"))
library(RColorBrewer)
library(viridis)
display.brewer.all(10)
library(tidytext)
library(textdata)
library(stringr)

#---------------------------------------------------------------------

# extract open ended questions
covid_survey <- fread(here::here("data", "CAPS-ACSP survey on returning to workplace.csv"))
open_ended<- covid_survey[,20:23]

open_ended <- open_ended %>%
  rename(opportunities = `Do you feel your mentor/organization will continue to provide opportunities for skills development?`,
         when = `When will you feel confident and comfortable going back to work?`,
         what = `What will make you feel confident and comfortable going back to work?`,
         comments = `Please add any comments or concerns you have below`)


open_ended %>%
  unnest_tokens(word, opportunities) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  filter(n > 5) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

open_ended %>%
  unnest_tokens(word, opportunities) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  filter(n > 6) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  theme(legend.position="none") +
  labs(y = "most common keyword concerns", x = NULL)


# get sentiment words
sentiment_neg <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

sentiment_pos <- get_sentiments("bing") %>% 
  filter(sentiment == "positive")


open_ended %>%
  unnest_tokens(word, opportunities) %>%
  anti_join(stop_words) %>%
  inner_join(sentiment_neg) %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  theme(legend.position="none") +
  labs(y = "most common negative keywords", x = NULL)



open_ended %>%
  unnest_tokens(word, opportunities) %>%
  anti_join(stop_words) %>%
  inner_join(sentiment_pos) %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word), fill = word) +
  geom_col() +
  theme(legend.position="none") +
  labs(y = "most common positive keywords", x = NULL)



bing_word_counts <- open_ended %>%
  unnest_tokens(word, opportunities) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()


bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL) + 
  theme_classic()


library(wordcloud)

open_ended %>%
  unnest_tokens(word, opportunities) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  with(wordcloud(word, n, max.words = 100))
