library(dplyr)
library(stringr)
library(tidytext)
library(janeaustenr)
library(ggplot2)
library(tidyr)
library(igraph)
library(ggraph)
library(stringr)
library(gridExtra)

tweet2010<- as.vector(read.csv("C://Users/sunna/Downloads/archive/2010.csv")['tweet'], 'character')
tweet2011<- as.vector(read.csv("C://Users/sunna/Downloads/archive/2011.csv")['tweet'], 'character')
tweet2012<- as.vector(read.csv("C://Users/sunna/Downloads/archive/2012.csv")['tweet'], 'character')
tweet2013<- as.vector(read.csv("C://Users/sunna/Downloads/archive/2013.csv")['tweet'], 'character')
tweet2014<- as.vector(read.csv("C://Users/sunna/Downloads/archive/2014.csv")['tweet'], 'character')
tweet2015<- as.vector(read.csv("C://Users/sunna/Downloads/archive/2015.csv")['tweet'], 'character')
tweet2016<- as.vector(read.csv("C://Users/sunna/Downloads/archive/2016.csv")['tweet'], 'character')
tweet2017<- as.vector(read.csv("C://Users/sunna/Downloads/archive/2017.csv")['tweet'], 'character')
tweet2018<- as.vector(read.csv("C://Users/sunna/Downloads/archive/2018.csv")['tweet'], 'character')
tweet2019<- as.vector(read.csv("C://Users/sunna/Downloads/archive/2019.csv")['tweet'], 'character')
tweet2020<- as.vector(read.csv("C://Users/sunna/Downloads/archive/2020.csv")['tweet'], 'character')
tweet2021<- as.vector(read.csv("C://Users/sunna/Downloads/archive/2021.csv")['tweet'], 'character')
tweet2022<- as.vector(read.csv("C://Users/sunna/Downloads/archive/2022.csv")['tweet'], 'character')

#exclude http adress
tweet2010<-str_replace_all(tweet2010, "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", " ")
tweet2011<-str_replace_all(tweet2011, "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", " ")
tweet2012<-str_replace_all(tweet2012, "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", " ")
tweet2013<-str_replace_all(tweet2013, "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", " ")
tweet2014<-str_replace_all(tweet2014, "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", " ")
tweet2015<-str_replace_all(tweet2015, "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", " ")
tweet2016<-str_replace_all(tweet2016, "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", " ")
tweet2017<-str_replace_all(tweet2017, "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", " ")
tweet2018<-str_replace_all(tweet2018, "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", " ")
tweet2019<-str_replace_all(tweet2019, "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", " ")
tweet2020<-str_replace_all(tweet2020, "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", " ")
tweet2021<-str_replace_all(tweet2021, "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", " ")
tweet2022<-str_replace_all(tweet2022, "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", " ")

# 1. Word tokenize and frequency. exclude stop words.
count2010 <- tibble(text = tweet2010) %>%
  unnest_tokens(output = word, input = text, token = "words") %>%
  filter(!word %in% stop_words$word) %>%
  count(word, sort = TRUE)
count2011 <- tibble(text = tweet2011) %>%
  unnest_tokens(output = word, input = text, token = "words") %>%
  filter(!word %in% stop_words$word) %>%
  count(word, sort = TRUE)
count2012 <- tibble(text = tweet2012) %>%
  unnest_tokens(output = word, input = text, token = "words") %>%
  filter(!word %in% stop_words$word) %>%
  count(word, sort = TRUE)
count2013 <- tibble(text = tweet2013) %>%
  unnest_tokens(output = word, input = text, token = "words") %>%
  filter(!word %in% stop_words$word) %>%
  count(word, sort = TRUE)
count2014 <- tibble(text = tweet2014) %>%
  unnest_tokens(output = word, input = text, token = "words") %>%
  filter(!word %in% stop_words$word) %>%
  count(word, sort = TRUE)
count2015 <- tibble(text = tweet2015) %>%
  unnest_tokens(output = word, input = text, token = "words") %>%
  filter(!word %in% stop_words$word) %>%
  count(word, sort = TRUE)
count2016 <- tibble(text = tweet2016) %>%
  unnest_tokens(output = word, input = text, token = "words") %>%
  filter(!word %in% stop_words$word) %>%
  count(word, sort = TRUE)
count2017 <- tibble(text = tweet2017) %>%
  unnest_tokens(output = word, input = text, token = "words") %>%
  filter(!word %in% stop_words$word) %>%
  count(word, sort = TRUE)
count2018 <- tibble(text = tweet2018) %>%
  unnest_tokens(output = word, input = text, token = "words") %>%
  filter(!word %in% stop_words$word) %>%
  count(word, sort = TRUE)
count2019 <- tibble(text = tweet2019) %>%
  unnest_tokens(output = word, input = text, token = "words") %>%
  filter(!word %in% stop_words$word) %>%
  count(word, sort = TRUE)
count2020 <- tibble(text = tweet2020) %>%
  unnest_tokens(output = word, input = text, token = "words") %>%
  filter(!word %in% stop_words$word) %>%
  count(word, sort = TRUE)
count2021 <- tibble(text = tweet2021) %>%
  unnest_tokens(output = word, input = text, token = "words") %>%
  filter(!word %in% stop_words$word) %>%
  count(word, sort = TRUE)
count2022 <- tibble(text = tweet2022) %>%
  unnest_tokens(output = word, input = text, token = "words") %>%
  filter(!word %in% stop_words$word) %>%
  count(word, sort = TRUE)

#2. Show top10 words
#please check the variables on the upper right panel
top10_2010<-count2010[1:10,]
top10_2011<-count2011[1:10,]
top10_2012<-count2012[1:10,]
top10_2013<-count2013[1:10,]
top10_2014<-count2014[1:10,]
top10_2015<-count2015[1:10,]
top10_2016<-count2016[1:10,]
top10_2017<-count2017[1:10,]
top10_2018<-count2018[1:10,]
top10_2019<-count2019[1:10,]
top10_2020<-count2020[1:10,]
top10_2021<-count2021[1:10,]
top10_2022<-count2022[1:10,]

#you can also use print function to see the top10 values
print(top10_2010)
print(top10_2011)
print(top10_2012)
print(top10_2013)
print(top10_2014)
print(top10_2015)
print(top10_2016)
print(top10_2017)
print(top10_2018)
print(top10_2019)
print(top10_2020)
print(top10_2021)
print(top10_2022)

#3. Plot histogram of word frequencies
#2010
histogram_2010<-ggplot(count2010, aes(n/total)) +
  geom_histogram(show.legend = FALSE) +
  ggtitle("2010 histogram of word frequencies")
histogram_2010
#2011
histogram_2011<-ggplot(count2011, aes(n/total)) +
  geom_histogram(show.legend = FALSE) +
  ggtitle("2011 histogram of word frequencies")
histogram_2011
#2012
histogram_2012<-ggplot(count2012, aes(n/total)) +
  geom_histogram(show.legend = FALSE) +
  ggtitle("2012 histogram of word frequencies")
histogram_2012
#2013
histogram_2013 <-ggplot(count2013, aes(n/total)) +
  geom_histogram(show.legend = FALSE) +
  ggtitle("2013 histogram of word frequencies")
histogram_2013
#2014
histogram_2014<-ggplot(count2014, aes(n/total)) +
  geom_histogram(show.legend = FALSE) +
  ggtitle("2014 histogram of word frequencies")
histogram_2014
#2015
histogram_2015<-ggplot(count2015, aes(n/total)) +
  geom_histogram(show.legend = FALSE) +
  ggtitle("2015 histogram of word frequencies")
histogram_2015

histogram_2016<-ggplot(count2016, aes(n/total)) +
  geom_histogram(show.legend = FALSE) +
  ggtitle("2016 histogram of word frequencies")
histogram_2016

histogram_2017<-ggplot(count2017, aes(n/total)) +
  geom_histogram(show.legend = FALSE) +
  ggtitle("2017 histogram of word frequencies")
histogram_2017

histogram_2018<-ggplot(count2018, aes(n/total)) +
  geom_histogram(show.legend = FALSE) +
  ggtitle("2018 histogram of word frequencies")
histogram_2018

histogram_2019<-ggplot(count2019, aes(n/total)) +
  geom_histogram(show.legend = FALSE) +
  ggtitle("2019 histogram of word frequencies")
histogram_2019

histogram_2020<-ggplot(count2020, aes(n/total)) +
  geom_histogram(show.legend = FALSE) +
  ggtitle("2020 histogram of word frequencies")
histogram_2020

histogram_2021<-ggplot(count2021, aes(n/total)) +
  geom_histogram(show.legend = FALSE) +
  ggtitle("2021 histogram of word frequencies")
histogram_2021

histogram_2022<-ggplot(count2022, aes(n/total)) +
  geom_histogram(show.legend = FALSE) +
  ggtitle("2022 histogram of word frequencies")
histogram_2022

#graph combined
#Please click Plot "Zoom"! to the graph 
grid.arrange(histogram_2010,histogram_2011,histogram_2012,histogram_2013, 
             histogram_2014,histogram_2015,histogram_2016,histogram_2017,
             histogram_2018,histogram_2019,histogram_2020,histogram_2021, 
             histogram_2022, nrow=5, ncol=3)

# 4. Zipf's law and plot log-log plot for preq and rank
freq_rank_2010 <- count2010 %>% mutate(rank = row_number(), `term frequency` = n/total)

freq_rank_2010<-freq_rank_2010 %>%
  ggplot(aes(rank, `term frequency`)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() + scale_y_log10()

freq_rank_2011 <- count2011 %>% mutate(rank = row_number(), `term frequency` = n/total)

freq_rank_2011<-freq_rank_2011 %>%
  ggplot(aes(rank, `term frequency`)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() + scale_y_log10()

freq_rank_2012 <- count2012 %>% mutate(rank = row_number(), `term frequency` = n/total)

freq_rank_2012<-freq_rank_2012 %>%
  ggplot(aes(rank, `term frequency`)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() + scale_y_log10()

freq_rank_2013 <- count2013 %>% mutate(rank = row_number(), `term frequency` = n/total)

freq_rank_2013<-freq_rank_2013 %>%
  ggplot(aes(rank, `term frequency`)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() + scale_y_log10()

freq_rank_2014 <- count2014 %>% mutate(rank = row_number(), `term frequency` = n/total)

freq_rank_2014<-freq_rank_2014 %>%
  ggplot(aes(rank, `term frequency`)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() + scale_y_log10()

freq_rank_2015 <- count2015 %>% mutate(rank = row_number(), `term frequency` = n/total)

freq_rank_2015<-freq_rank_2015 %>%
  ggplot(aes(rank, `term frequency`)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() + scale_y_log10()

freq_rank_2016 <- count2016 %>% mutate(rank = row_number(), `term frequency` = n/total)

freq_rank_2016<-freq_rank_2016 %>%
  ggplot(aes(rank, `term frequency`)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() + scale_y_log10()

freq_rank_2017 <- count2017 %>% mutate(rank = row_number(), `term frequency` = n/total)

freq_rank_2017<-freq_rank_2017 %>%
  ggplot(aes(rank, `term frequency`)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() + scale_y_log10()

freq_rank_2018 <- count2018 %>% mutate(rank = row_number(), `term frequency` = n/total)

freq_rank_2018<-freq_rank_2018 %>%
  ggplot(aes(rank, `term frequency`)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() + scale_y_log10()

freq_rank_2019 <- count2019 %>% mutate(rank = row_number(), `term frequency` = n/total)

freq_rank_2019<-freq_rank_2019 %>%
  ggplot(aes(rank, `term frequency`)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() + scale_y_log10()

freq_rank_2020 <- count2020 %>% mutate(rank = row_number(), `term frequency` = n/total)

freq_rank_2020<-freq_rank_2020 %>%
  ggplot(aes(rank, `term frequency`)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() + scale_y_log10()

freq_rank_2021 <- count2021 %>% mutate(rank = row_number(), `term frequency` = n/total)

freq_rank_2021<-freq_rank_2021 %>%
  ggplot(aes(rank, `term frequency`)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() + scale_y_log10()

freq_rank_2022 <- count2022 %>% mutate(rank = row_number(), `term frequency` = n/total)

freq_rank_2022<-freq_rank_2022 %>%
  ggplot(aes(rank, `term frequency`)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() + scale_y_log10()



#dev.new()
#Please click "Zoom" on the right panel, to see the graph 
#or open with the new window with dev.new()
grid.arrange(freq_rank_2010,freq_rank_2011,freq_rank_2012,freq_rank_2013, 
             freq_rank_2014,freq_rank_2015,freq_rank_2016,freq_rank_2017,
             freq_rank_2018,freq_rank_2019,freq_rank_2020,freq_rank_2021, 
             freq_rank_2022, nrow=5, ncol=3)
#dev.off()

# 5. bigrams
bigrams_2010 <- text_df_2010 %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams_sep_2010 <- bigrams_2010 %>% separate(bigram, c("word1", "word2"), sep = " ")
bigrams_2011 <- text_df_2011 %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams_sep_2011 <- bigrams_2011 %>% separate(bigram, c("word1", "word2"), sep = " ")
bigrams_2012 <- text_df_2012 %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams_sep_2012 <- bigrams_2012 %>% separate(bigram, c("word1", "word2"), sep = " ")
bigrams_2013 <- text_df_2013 %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams_sep_2013 <- bigrams_2013 %>% separate(bigram, c("word1", "word2"), sep = " ")
bigrams_2014 <- text_df_2014 %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams_sep_2014 <- bigrams_2014 %>% separate(bigram, c("word1", "word2"), sep = " ")
bigrams_2015 <- text_df_2015 %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams_sep_2015 <- bigrams_2015 %>% separate(bigram, c("word1", "word2"), sep = " ")
bigrams_2016 <- text_df_2016 %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams_sep_2016 <- bigrams_2016 %>% separate(bigram, c("word1", "word2"), sep = " ")
bigrams_2017 <- text_df_2017 %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams_sep_2017 <- bigrams_2017 %>% separate(bigram, c("word1", "word2"), sep = " ")
bigrams_2018 <- text_df_2018 %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams_sep_2018 <- bigrams_2018 %>% separate(bigram, c("word1", "word2"), sep = " ")
bigrams_2019 <- text_df_2019 %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams_sep_2019 <- bigrams_2019 %>% separate(bigram, c("word1", "word2"), sep = " ")
bigrams_2020 <- text_df_2020 %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams_sep_2020 <- bigrams_2020 %>% separate(bigram, c("word1", "word2"), sep = " ")
bigrams_2021 <- text_df_2021 %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams_sep_2021 <- bigrams_2021 %>% separate(bigram, c("word1", "word2"), sep = " ")
bigrams_2022 <- text_df_2022 %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams_sep_2022 <- bigrams_2022 %>% separate(bigram, c("word1", "word2"), sep = " ")

# Counting bigrams
bigram_counts_2010 <- bigrams_sep_2010 %>% count(word1, word2, sort = TRUE)
bigram_counts_2011 <- bigrams_sep_2011 %>% count(word1, word2, sort = TRUE)
bigram_counts_2012 <- bigrams_sep_2012 %>% count(word1, word2, sort = TRUE)
bigram_counts_2013 <- bigrams_sep_2013 %>% count(word1, word2, sort = TRUE)
bigram_counts_2014 <- bigrams_sep_2014 %>% count(word1, word2, sort = TRUE)
bigram_counts_2015 <- bigrams_sep_2015 %>% count(word1, word2, sort = TRUE)
bigram_counts_2016 <- bigrams_sep_2016 %>% count(word1, word2, sort = TRUE)
bigram_counts_2017 <- bigrams_sep_2017 %>% count(word1, word2, sort = TRUE)
bigram_counts_2018 <- bigrams_sep_2018 %>% count(word1, word2, sort = TRUE)
bigram_counts_2019 <- bigrams_sep_2019 %>% count(word1, word2, sort = TRUE)
bigram_counts_2020 <- bigrams_sep_2020 %>% count(word1, word2, sort = TRUE)
bigram_counts_2021 <- bigrams_sep_2021 %>% count(word1, word2, sort = TRUE)
bigram_counts_2022 <- bigrams_sep_2022 %>% count(word1, word2, sort = TRUE)

# Visualizing bigrams
library(igraph)

#2010
bigram_graph_2010 <- bigram_counts_2010 %>% filter(n > 10) %>% graph_from_data_frame()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph_2010, layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.5), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#2011
bigram_graph_2011 <- bigram_counts_2011 %>% filter(n > 10) %>% graph_from_data_frame()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph_2011, layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.5), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#2012
bigram_graph_2012 <- bigram_counts_2012 %>% filter(n > 10) %>% graph_from_data_frame()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph_2012, layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.5), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#2013
bigram_graph_2013 <- bigram_counts_2013 %>% filter(n > 10) %>% graph_from_data_frame()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph_2013, layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.5), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#2014
bigram_graph_2014 <- bigram_counts_2014 %>% filter(n > 10) %>% graph_from_data_frame()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph_2014, layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.5), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#2015
bigram_graph_2015 <- bigram_counts_2015 %>% filter(n > 10) %>% graph_from_data_frame()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph_2015, layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.5), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#2016
bigram_graph_2016 <- bigram_counts_2016 %>% filter(n > 10) %>% graph_from_data_frame()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph_2016, layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.5), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#2017
bigram_graph_2017 <- bigram_counts_2017 %>% filter(n > 10) %>% graph_from_data_frame()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph_2017, layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.5), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#2018
bigram_graph_2018 <- bigram_counts_2018 %>% filter(n > 10) %>% graph_from_data_frame()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph_2018, layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.5), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#2019
bigram_graph_2019 <- bigram_counts_2019 %>% filter(n > 10) %>% graph_from_data_frame()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph_2019, layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.5), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#2020
bigram_graph_2020 <- bigram_counts_2020 %>% filter(n > 10) %>% graph_from_data_frame()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph_2020, layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.5), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#2021
bigram_graph_2021 <- bigram_counts_2021 %>% filter(n > 10) %>% graph_from_data_frame()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph_2021, layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.5), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#2022
bigram_graph_2022 <- bigram_counts_2022 %>% filter(n > 10) %>% graph_from_data_frame()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph_2022, layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.5), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()



