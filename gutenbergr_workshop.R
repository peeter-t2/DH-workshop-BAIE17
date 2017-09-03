#Digital Humanities workshop in the Summer School for Baltic Enlightenment and its inheritance (Sommerschule "Die baltische Aufklärung und ihr Erbe"), 2017
#Compiled by Peeter Tinits, 05-09-2017.

#First we check if the packages are installed, and install if needed.
lapply(c("tidytext", "gutenbergr", "dplyr", "scales", "ggplot2","scales"), 
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

#Then we read the packages into R environment.
library(tidytext)
library(gutenbergr)
library(dplyr)
library(scales)
library(ggplot2)

#generally use this for comparisons instead
#bind_rows("neg" = neg_df, "pos" = pos_df, .id = "neg_pos") -> sentiment_df
#also consider saving the data of example corpora into zipfile, at least the inital comparison should be there.



# %>% - carry the data into function
# filter - take subset of the data
# str_detect - find part of string

gutenberg_metadata %>%
  filter(str_detect(author,"Wells, H. G."))

gutenberg_metadata %>%
  filter(str_detect(title,"Time Machine"))

gutenberg_metadata %>%
  filter(has_text==TRUE) %>%
  filter(str_detect(title,"Time Machine"))

gutenberg_metadata %>%
  filter(has_text==TRUE) %>%
  filter(str_detect(language,"en"))

gutenberg_metadata %>%
  filter(has_text==TRUE) %>%
  filter(str_detect(language,"de"))

gutenberg_metadata %>%
  filter(has_text==TRUE) %>%
  filter(str_detect(language,"de")) %>%
  filter(str_detect(author,"Shakespeare"))

gutenberg_metadata %>%
  filter(has_text==TRUE) %>%
  filter(str_detect(author,"Wells, H. G.")) %>%
  filter(str_detect(language,"en"))

hgwells_texts <- gutenberg_download(hgwells_index$gutenberg_id[1:15], meta_fields = "title")

#count (number of lines per book)
sherlock_texts %>%
  count(title)

#unnest_tokens - make text into tokens
sherlock_texts %>%
  unnest_tokens(word, text) %>%
  count(title)

#group_by - group by the item (fur future operations)
#count - by group, count the words
sherlock_texts %>%
  unnest_tokens(word, text) %>%
  group_by(title) %>%
  count(word, sort = TRUE) #can also be done with count(title, word, sort=TRUE)

#anti_join - remove the matching rows
#stop_words - dataset of stopwords
data("stop_words")
sherlock_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(title) %>%
  count(word, sort = TRUE)

#let's make two groups
comparison1 <- sherlock_texts %>%
  unnest_tokens(word, text) %>%
  filter(title=="The Return of Sherlock Holmes") %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)

comparison2 <- sherlock_texts %>%
  unnest_tokens(word, text) %>%
  filter(title=="The Memoirs of Sherlock Holmes") %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)
  
#rename - rename column for practical reasons
#inner_join - keep only the matching items
#mutate - create new variable
comparison <- comparison1 %>%
  rename(comparison1 = n) %>%
  inner_join(comparison2,"word") %>%
  rename(comparison2 = n) %>%
  mutate(comparison1 = comparison1 / sum(comparison1),
         comparison2 = comparison2 / sum(comparison2))

#this is just to plot the comparison
ggplot(comparison, aes(comparison1, comparison2)) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = word), check_overlap = TRUE,
            vjust = 1, hjust = 1) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")


#we can do the same thing with more texts
sherlock_texts %>%
  count(title)
comparison1 <- sherlock_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)

detective_texts %>%
  count(title)
comparison2 <- detective_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)

comparison <- comparison1 %>%
  rename(comparison1 = n) %>%
  inner_join(comparison2,"word") %>%
  rename(comparison2 = n) %>%
  mutate(comparison1 = comparison1 / sum(comparison1),
         comparison2 = comparison2 / sum(comparison2))

#this is just to plot the comparison
ggplot(comparison, aes(comparison1, comparison2)) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = word), check_overlap = TRUE,
            vjust = 1, hjust = 1) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")





#let's now consider the location within books
#ungroup - ungroup
sherlock_texts <- sherlock_texts %>%
  group_by(title) %>%
  mutate(linenumber = row_number()) %>%
  ungroup()
sherlock_texts

detective_texts <- detective_texts %>%
  group_by(title) %>%
  mutate(linenumber = row_number()) %>%
  ungroup()


#get_sentiments - vocabulary sentimetns
#spread - make one column into two
sherlocksentiment <- sherlock_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(title, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(sherlocksentiment, aes(index, sentiment, fill = title)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~title, ncol = 2, scales = "free_x")

detectivesentiment <- detective_texts %>%
  group_by(title) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(title, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(detectivesentiment, aes(index, sentiment, fill = title)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~title, scales = "free_x")

sherlock_texts %>% #or detective_texts
  group_by(title) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(title, index = linenumber %/% 80, sentiment) %>% 
  mutate(n = ifelse(sentiment == 'negative', -n, n)) %>% # reverse negative values
  ggplot(aes(x = index, y = n)) +    
  geom_area(aes(fill = n > 0),stat = 'identity') +
  scale_fill_manual(values = c('red','green')) + # change colors
  geom_smooth(method = "loess", se = F, col = "black") + 
  facet_wrap(~ title, scale = "free_x")




all_texts_comp1 <- sherlock_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(title) %>%
  mutate(word_position = row_number() / n()) %>%
  ungroup() %>%
  mutate(decile = ceiling(word_position * 10) / 10) %>%
  count(decile, word) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(decile) %>%
  summarize(score = sum(score * n) / sum(n))

all_texts_comp2 <- detective_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(title) %>%
  mutate(word_position = row_number() / n()) %>%
  ungroup() %>%
  mutate(decile = ceiling(word_position * 10) / 10) %>%
  count(decile, word) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(decile) %>%
  summarize(score = sum(score * n) / sum(n))

ggplot(data=all_texts_comp1,aes(decile, score)) +
  geom_line(colour="red") +
  geom_line(data=all_texts_comp2,colour="blue") +
  scale_x_continuous(labels = percent_format()) +
  expand_limits(y = 0) +
  labs(x = "Position within a story",
       y = "Average AFINN sentiment score")



sherlock_tf_idf <- sherlock_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(title) %>%
  count(word, sort = TRUE) %>%
  bind_tf_idf(word, title, n) %>%
  arrange(desc(tf_idf))


sherlock_tf_idf %>% 
  top_n(10) %>%
  ggplot(aes(word, tf_idf, fill = title)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()+
  facet_wrap(~title,scale="free_y")




detective_tf_idf <- detective_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(title) %>%
  count(word, sort = TRUE) %>%
  bind_tf_idf(word, title, n) %>%
  arrange(desc(tf_idf))

detective_tf_idf %>% 
  top_n(10) %>%
  ggplot(aes(word, tf_idf, fill = title)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()+
  facet_wrap(~title,scale="free_y")



sherlock_texts %>%
#detective_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(title) %>%
  mutate(word_position = row_number() / n()) %>%
  ungroup() %>%
  mutate(decile = ceiling(word_position * 10) / 10) %>%
  count(decile, word) %>%
  bind_tf_idf(word, decile, n) %>%
  arrange(desc(tf_idf)) %>%
  top_n(10) %>%
  ggplot(aes(word, tf_idf, fill = factor(decile))) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()+
  facet_wrap(~decile,scale="free_y")


#comparison can maybe be done with this
#rbind + ids
#rbind(sherlock_texts,detective_texts)

#as an example, just compare hgwells with jules verne



#and then the options to pick your own dataset and try it out...

#first find the texts you want to look at & compare,
#then download and run the analysis
#finally plot the results, see if you find something...


#additional ideas to try, 

#get some of 18th century authors, with examples

#get all tarzan books vs crusoe books

#get all sherlock holmes books vs other mystery books (hint: anti_join)
#etc, but code is done for now...