#Digital Humanities workshop in the Summer School for Baltic Enlightenment and its inheritance (Sommerschule "Die baltische Aufklärung und ihr Erbe"), 2017
#Compiled by Peeter Tinits, 05-09-2017.


#First we check if the packages are installed, and install if needed.
lapply(c("tidytext", "gutenbergr", "dplyr", "scales", "ggplot2","stringr","humaniformat","gender","purrr","readr"),
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

#Then we read the packages into R environment.
library(tidytext)
library(gutenbergr)
library(dplyr)
library(scales)
library(ggplot2)
library(stringr)
library(tidyr)
library(readr)
library(purrr)

#Recommended to turn on soft-wrap:
#Tools -> Global options -> Code -> Editing -> Soft-wrap R source files (turn it on)


#The library "gutenbergr" gave us some data to work with, for example "gutenberg_metadata"
#To look at a variable we just type it in
View(gutenberg_metadata)



#And the library tidytext allows us to do simple transformations with it, eventually towards quite complex results

# %>% - carry the data into function
# filter - take subset of the data
# str_detect - find part of string

#basic model is the following
#data %>%
  #process()

gutenberg_metadata %>%
  filter(str_detect(author,"Wells, H. G."))

gutenberg_metadata %>%
  filter(str_detect(author,"Austen"))

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
  filter(str_detect(language,"en")) -> hgwells_index

gutenberg_metadata %>%
  filter(has_text==TRUE) %>%
  filter(str_detect(author,"Verne, Jules")) %>%
  filter(str_detect(language,"en")) -> jverne_index

#hgwells_texts <- gutenberg_download(hgwells_index$gutenberg_id[1:15], meta_fields = "title")
#jverne_texts <- gutenberg_download(jverne_index$gutenberg_id[1:15], meta_fields = "title")
#lazy option with no internet
load("data/wells_verne.RData")

#count (number of lines per book)
hgwells_texts %>%
  count(title)

jverne_texts %>%
  count(title)

#unnest_tokens - make text into tokens
hgwells_texts %>%
  unnest_tokens(word, text) %>%
  count(title)

#group_by - group by the item (fur future operations)
#count - by group, count the words
hgwells_texts %>%
  unnest_tokens(word, text) %>%
  group_by(title) %>%
  count(word, sort = TRUE) #can also be done with count(title, word, sort=TRUE)

#anti_join - remove the matching rows
#stop_words - dataset of stopwords
data("stop_words")
hgwells_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(title) %>%
  count(word, sort = TRUE)

#let's make two groups
comparison1 <- hgwells_texts %>%
  unnest_tokens(word, text) %>%
  filter(title=="The Time Machine") %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)

comparison2 <- jverne_texts %>%
  unnest_tokens(word, text) %>%
  filter(title=="A Journey into the Interior of the Earth") %>%
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
hgwells_texts %>%
  count(title)
comparison1 <- hgwells_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)

jverne_texts %>%
  count(title)
comparison2 <- jverne_texts %>%
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
jverne_texts <- jverne_texts %>%
  group_by(title) %>%
  mutate(linenumber = row_number()) %>%
  ungroup()
jverne_texts

hgwells_texts <- hgwells_texts %>%
  group_by(title) %>%
  mutate(linenumber = row_number()) %>%
  ungroup()

#get_sentiments - vocabulary sentimetns
#spread - make one column into two
hgwellssentiment <- hgwells_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(title, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(hgwellssentiment, aes(index, sentiment, fill = title)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~title, ncol = 2, scales = "free_x")

jvernesentiment <- jverne_texts %>%
  group_by(title) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(title, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(jvernesentiment, aes(index, sentiment, fill = title)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~title, scales = "free_x")

hgwells_texts %>% #or jverne_texts
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




all_texts_comp1 <- hgwells_texts %>%
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

all_texts_comp2 <- jverne_texts %>%
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



hgwells_tf_idf <- hgwells_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(title) %>%
  count(word, sort = TRUE) %>%
  bind_tf_idf(word, title, n) %>%
  arrange(desc(tf_idf)) %>%
  ungroup()


hgwells_tf_idf %>% 
  group_by(title) %>%
  filter(n>10) %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = title)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()+
  guides(fill=FALSE) +
  facet_wrap(~title,scale="free_y")




jverne_tf_idf <- jverne_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(title) %>%
  count(word, sort = TRUE) %>%
  bind_tf_idf(word, title, n) %>%
  arrange(desc(tf_idf)) %>%
  ungroup()

jverne_tf_idf %>% 
  group_by(title) %>%
  filter(n>10) %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = title)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()+
  guides(fill=FALSE) +
  facet_wrap(~title,scale="free_y")


#plot by location within text
#hgwells_texts %>%
jverne_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(title) %>%
  mutate(word_position = row_number() / n()) %>%
  ungroup() %>%
  mutate(decile = ceiling(word_position * 10) / 10) %>%
  count(decile, word) %>%
  bind_tf_idf(word, decile, n) %>%
  arrange(desc(tf_idf)) %>%
  filter(n>30) %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = factor(decile))) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()+
  guides(fill=FALSE) +
  facet_wrap(~decile,scale="free_y")


#Now you can try it yourself:

#1) Find the texts you want to work with and download them
#2) Figure out what you want to test, and process the texts
#3) Visualize the results and save the files


#Some more resources and ideas to try

gutenberg_subjects #lists topics considered in each book

#get an overview of the subjects
subjects <- gutenberg_subjects %>%
  count(subject) %>%
  arrange(desc(n))
View(subjects)

#Consider only the ones that have text in them
subjects <- gutenberg_subjects %>%
  inner_join(gutenberg_metadata,"gutenberg_id") %>%
  filter(has_text==TRUE) %>%
  count(subject) %>%
  arrange(desc(n))
View(subjects)


#It contains for example markings on the characters present
subjects %>%
  filter(str_detect(subject,"character"))

#For example we could get all the Tarzan stories
tarzan_index <- gutenberg_subjects %>%
  filter(str_detect(subject,"Tarzan")) %>%
  inner_join(gutenberg_metadata,"gutenberg_id") %>%
  filter(has_text==TRUE) %>%
  filter(language=="en") #8 books

#And compare them with the Robinson Crusoe stories
crusoe_index <- gutenberg_subjects %>%
  filter(str_detect(subject,"Crusoe, Robinson")) %>%
  inner_join(gutenberg_metadata,"gutenberg_id") %>%
  filter(has_text==TRUE) %>%
  filter(language=="en") #8 books


#We can also look for particular authors
jausten_index <- gutenberg_metadata %>%
  filter(str_detect(author,"Austen, Jane")) %>%
  filter(has_text==TRUE) %>%
  filter(language=="en")

cbronte_index <- gutenberg_metadata %>%
  filter(str_detect(author,"Bront")) %>%
  filter(str_detect(author,"Charlotte")) %>%
  filter(has_text==TRUE) %>%
  filter(language=="en")

shelley_index <- gutenberg_metadata %>%
  filter(str_detect(author,"Shelley")) %>%
  filter(str_detect(author,"Mary")) %>%
  filter(has_text==TRUE) %>%
  filter(language=="en")


#In some cases a category fits in another. In this case its useful to substract one from the other (anti_join)
sherlock_index <- gutenberg_subjects %>%
  filter(str_detect(subject,"Holmes, Sherlock")) %>%
  inner_join(gutenberg_metadata,"gutenberg_id") %>%
  filter(has_text==TRUE) %>%
  filter(language=="en") #19 books

detective_index <- gutenberg_subjects %>%
  filter(subject == "Detective and mystery stories") %>%
  inner_join(gutenberg_metadata,"gutenberg_id") %>%
  filter(has_text==TRUE) %>%
  filter(language=="en") %>%
  anti_join(sherlock_index, "gutenberg_id") #455 book


#Let's download Tarzan and Robinson Crusoe collections
#the numbers behing gutenberg_id, say which books to take, for now, don't take more than 20, as downloading will otherwise take too much time
#tarzan_texts <- gutenberg_download(tarzan_index$gutenberg_id[1:8], meta_fields = "title")
#crusoe_texts <- gutenberg_download(crusoe_index$gutenberg_id[1:8], meta_fields = "title")

#check the texts we got
tarzan_texts %>%
  count(title)
crusoe_texts %>%
  count(title)


#For author texts
#jausten_texts <- gutenberg_download(jausten_index$gutenberg_id, meta_fields = "title")
#cbronte_texts <- gutenberg_download(cbronte_index$gutenberg_id, meta_fields = "title")
#shelley_texts <- gutenberg_download(shelley_index$gutenberg_id, meta_fields = "title")

#For sherlock texts
#sherlock_texts <- gutenberg_download(sherlock_index$gutenberg_id[1:19], meta_fields = "title")
#detective_texts <- gutenberg_download(detective_index$gutenberg_id[1:19], meta_fields = "title")

#If you can't connect to internet with the gutenberg_download function, you can simply open the example datasets
load("data/austen_bronte_shelley.RData")
load("data/tarzan_crusoe.RData")
#load("data/wells_verne.RData")




#See what you can find there, or make your own sample.

#1) How does the vocabulary differ between Crusoe and Tarzan stories?

#2) What are the distinct words for each Tarzan story (compared to all of them)

#3) What are the distinct words for the Tarzan stories compared to the Crusoe stories

#4) How are the words distributed within text, what are the key words in the first 10th of the text?

#5) How is the sentiment distributed within Tarzan and Crusoe stories? Are there noticeable differences between them?





#There is also a list of authors
gutenberg_authors

#We can get some 18th century authors
enl_authors <- gutenberg_authors %>%
  filter(birthdate < 1800) %>%
  filter(birthdate > 1700)

c18_texts <- gutenberg_metadata %>%
  filter(author %in% enl_authors$author)

c18_subjects <- c18_texts  %>%
  left_join(gutenberg_subjects, by = c("gutenberg_id"="gutenberg_id"))

subjects_counts <- c18_subjects %>%
  count(subject) %>%
  arrange(desc(n))
View(subjects_counts)

c18_histfiction <- c18_subjects %>%
  filter(subject == "Historical fiction") %>%
  filter(language == "en")


#Find a set of comparison between authors in the 18th century, see what differences you can find!



########################
###Extra 
########################
#As an additional neat thing, you can add gender information based on the first names of these authors. This can be a subject for future comparisons. Again, watch out for downloading too many texts simultaneously.S
library(gender)
library(humaniformat)

enl_authors <- gutenberg_authors %>%
  filter(birthdate < 1800) %>%
  filter(birthdate > 1700)  %>% 
  mutate(reversename = format_reverse(author))
temp_df <- data.frame(reversename=enl_authors$reversename,parse_names(enl_authors$reversename),stringsAsFactors = FALSE)
enl_authors <- enl_authors %>% 
    left_join(temp_df, by = c("reversename" = "reversename"))
genders <- gender(enl_authors$first_name, years = c(1789,1850), method = "ipums")
genders <- genders[!duplicated(genders),]
enl_authors <- enl_authors %>% 
    left_join(genders, by = c("first_name" = "name"))

enl_authors %>%
  count(gender)


c19_authors <- gutenberg_authors %>%
  filter(birthdate < 1900) %>%
  filter(birthdate > 1800)  %>% 
  mutate(reversename = format_reverse(author))
temp_df <- data.frame(reversename=c19_authors$reversename,parse_names(c19_authors$reversename),stringsAsFactors = FALSE)
c19_authors <- c19_authors %>% 
    left_join(temp_df, by = c("reversename" = "reversename"))
genders <- gender(c19_authors$first_name, years = c(1800,1900), method = "ipums")
genders <- genders[!duplicated(genders),]
c19_authors <- c19_authors %>% 
    left_join(genders, by = c("first_name" = "name"))

c19_authors %>%
  count(gender)

c19_authors %>%
  filter(gender=="female")


############################
#####Extra 2
###########################

#Read your own files
#Here it reads the files from the corpus included in the dataset
library(purrr)
filelist <- list.files("data/corpus/",full.names=T)
texts <- map_df(filelist, ~ data_frame(txt = read_lines(.x)) %>%
                  mutate(filename = .x)) %>%
  mutate(filename= gsub("data/corpus/","",filename))








