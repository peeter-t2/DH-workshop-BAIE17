#Digital Humanities workshop in the Summer School for Baltic Enlightenment and its inheritance (Sommerschule "Die baltische Aufklärung und ihr Erbe"), 2017
#Compiled by Peeter Tinits, 05-09-2017.

#Workshop 2 materials.
#Here we look at in more detail how you could go about processing a single text.
#We use more packages here than in workshop 1.
lapply(c("tidytext", "gutenbergr", "dplyr", "scales", "ggplot2","zoo","igraph","ggraph","tm","scales","readr","grid","stringr"), 
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

#Then we read the packages into R environment.
library(zoo)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)
library(tm)
library(scales)
library(readr)
library(stringr)
library(tidyr)
library(dplyr)

hofmeister <- read.table("data/hofmeister/7hfms10.txt", sep="\t",quote = "",header=FALSE,blank.lines.skip=FALSE,stringsAsFactors=F)#,  fileEncoding = "UTF-8"
names(hofmeister) <- "line"


#same as
#gutenberg_metadata %>%
 #filter(title == "Der Hofmeister")
#hofmeister2 <- gutenberg_download(6821)




#there's extra comments before and after text, mark beginning and end
#lag() - compare with the previous row instead
hofmeister <- hofmeister %>%
  mutate(is_book=ifelse(lag(line)=="Der Hofmeister odor Vortheile der Privaterziehung",yes="book",no=NA)) %>%
  mutate(is_book=ifelse(line=="Ende dieses Projekt Gutenberg Etextes Der Hofmeister odor",yes="nobook",no=is_book))

#na.locf - fill the NAs before or after data
hofmeister <- na.locf(hofmeister)

#keep only the parts that are the book
hofmeister <- hofmeister %>%
  filter(is_book=="book")


#mark beginning of play itself
hofmeister <- hofmeister %>%
  mutate(is_text=ifelse(lead(line)=="Erster Akt.",yes="text",no=NA))
#na.locf - fill the NAs before or after data
hofmeister <- na.locf(hofmeister)

#there's useful information before
namelist <- hofmeister %>%
  filter(is.na(is_text))
write.csv(namelist,"data/hofmeister/names_raw.csv")


hofmeister <- hofmeister %>%
  filter(is_text=="text")


#lead() - one row after
#lag(,2) - two rows before
#is.na() - is something na
#! - negation
#str_replace() - substite words with words
hofmeister <- hofmeister %>%
  mutate(rownumber=1:nrow(hofmeister)) %>%
  mutate(akt=ifelse(grepl("Akt",line),yes=line,no=NA)) %>%
  mutate(scene=ifelse(grepl("Scene.",line,fixed=TRUE),yes=line,no=NA)) %>%
  mutate(scene=str_replace(scene,"Zweite Scene.","Zweyte Scene.")) %>%
  mutate(speaker=ifelse(line!=""&lag(line)=="",yes=line,no=NA)) %>%
  mutate(scene_description=ifelse(line!=""&lag(line)==""&lag(line,2)==""&!is.na(scene),yes="scene_descr_begin",no=NA)) %>%
  mutate(scene_description=ifelse(lag(line)!=""&line==""&lead(line)=="",yes="scene_descr_end",no=scene_description))

#now we fill the blanks again, this time for durations of acts, scenes, speaker turns etc
hofmeister <- na.locf(hofmeister)


hofmeister %>%
  filter(scene_description=="scene_descr_begin")

dialogues <- hofmeister %>%
  filter(!is.na(akt),scene_description!="scene_descr_begin")

#let's check the speakers we have
speakers <- dialogues %>%
  count(speaker) %>%
  arrange(desc(n))

#str_extract - get part of the string that matches the query
#trimws() - trim whitespace around the word, clean up the leftover spaces
dialogues <- dialogues %>% 
  mutate(activity=str_extract(speaker, "\\(.*\\)")) %>%
  mutate(activity=str_extract(speaker, "\\(.*")) %>% 
  mutate(speaker_clean=str_replace(speaker, "\\(.*\\)","")) %>%
  mutate(speaker_clean=str_replace(speaker, "\\(.*","")) %>% 
  mutate(speaker_clean=trimws(speaker_clean))

speakers <- dialogues %>% 
  count(speaker_clean) %>%
  arrange(desc(n))
write.csv(speakers,"data/hofmeister/speakers_text.csv")

#we can manually combine the files
#additionally we can add the gender markers to them
names_gender <- read.csv("data/hofmeister/names.csv",stringsAsFactors = F)

#we find that even after cleaning, some names still have multiple expressions
#we can simply replace them
dialogues <- dialogues %>% 
  mutate(speaker_clean=str_replace(speaker_clean, "Fritz v. Berg.","Fritz.")) %>%
  mutate(speaker_clean=str_replace(speaker_clean, "Graf Wermuth.","Graf.")) %>% 
  mutate(speaker_clean=str_replace(speaker_clean, "Seiffenblase.","Herr von Seiffenblase."))

#join the names with the main data
dialogues <- dialogues %>%
  left_join(names_gender,by="speaker_clean")

#we can remove speaker information from text to keep only dialogues
#if the line == speaker, replace with empty line
dialogues <- dialogues %>%
  mutate(line=ifelse(line==speaker,yes="",no=line))


#we can remove all unmarked lines as they are no longer necessary
dialogues <- dialogues %>%
  filter(!str_detect(speaker_clean,"Akt")) %>%
  filter(!str_detect(speaker_clean,"Scene")) %>%
  filter(!str_detect(speaker_clean,"Zu")) %>%
  filter(!line=="") #also remove all empty lines....

#we can also mark the beginning and end of an dialogue turn, by marking when the speaker changed
#ifelse(condition, whatifyes, whatifno) - do if condition is fulfilled, and something else if not
#if no=variable, then it means it keeps its current value
dialogues <- dialogues %>%
  mutate(turnswitch=ifelse(speaker_clean!=lead(speaker_clean),yes="turnswitch",no=NA)) %>%
  mutate(turnswitch=ifelse(rownumber==max(rownumber),yes="turnswitch",no=turnswitch))

#and we can give unique ids to each row and dialogue turn through numbers
#rownumber was set before
#rank() - the place in the ordered sequence
dialogues <- dialogues %>%
  mutate(dialnr=ifelse(turnswitch=="turnswitch",yes=rownumber,no=NA)) %>%
  mutate(dialrank=ifelse(turnswitch=="turnswitch",yes=rank(dialnr),no=NA))

#fill again the place in sequence - in this case from last to first (because in this case, first will be NAs)
dialogues$dialrank <- na.locf(dialogues$dialrank,fromLast=TRUE)


#how many turns did the people have?
turnsperperson <- dialogues %>%
  group_by(nameshort) %>%
  summarize(n=n_distinct(dialrank)) %>%
  arrange(desc(n))


#how many participants in each scene
participants <- dialogues %>%
  group_by(akt, scene) %>%
  summarize(n=n_distinct(nameshort)) %>%
  arrange(desc(n))


#how many lines per person per scene
linecountperscene <- dialogues %>%
  group_by(akt, scene, nameshort,gender) %>%
  count(n_distinct(dialrank))


wordcountperscene <- dialogues %>%
  unnest_tokens(word,line) %>%
  group_by(akt, scene, nameshort, gender) %>%#
  count() %>%
  ungroup() %>%
  mutate(akt =  factor(akt, levels = unique(dialogues$akt))) %>%
  mutate(scene =  factor(scene, levels = unique(dialogues$scene))) %>%
  mutate(akt_scene =  paste(as.numeric(factor(akt, levels = unique(dialogues$akt))),as.numeric(factor(scene, levels = unique(dialogues$scene))),sep=".")) %>% #once they are in order we can use that to make the a compact listing of them
  arrange(akt, scene) #same as arrange(akt_scene)


#x=akt:name - the combination of act and name
linecountperscene %>% 
  filter(!is.na(nameshort)) %>%
  filter(!is.na(akt)) %>%
  mutate(name=factor(nameshort)) %>%
  arrange(desc(scene)) %>%
  ggplot(aes(x=interaction(akt,nameshort),y=n,fill=scene)) +#interaction(akt,scene)
  geom_col() +
  labs(x = NULL, y = "number of lines")+
  coord_flip()


linecountperscene %>% 
  filter(!is.na(nameshort)) %>%
  mutate(name=factor(nameshort)) %>%
  arrange(desc(scene)) %>%
  ggplot(aes(x=name,y=n,group=akt,fill=scene))+
  geom_col() +
  labs(x = NULL, y = "number of lines")+
  coord_flip()+
  facet_wrap(~akt, scales = "free")


linecountperscene %>% 
  ggplot(aes(x=scene,y=n, group=akt, fill = nameshort)) +#interaction(akt,scene)
  geom_col() +
  labs(x = NULL, y = "number of lines")+
  coord_flip()+
  facet_wrap(~akt, scales = "free")


#the Bechdel test

#this one doesn't plot properly, because it thinks that first scene of acts 1 and 2 are the same
linecountperscene %>% 
  filter(!is.na(nameshort)) %>%
  mutate(name=factor(nameshort)) %>%
  arrange(desc(scene)) %>%
  ggplot(aes(x=name,y=n,fill=scene)) +#interaction(akt,scene)
  geom_col() +
  facet_wrap(~scene, scales = "free")+# +
  labs(x = NULL, y = "number of lines")+
  coord_flip()

#but interactions of them will create a messy plot
linecountperscene %>% 
  filter(!is.na(nameshort)) %>%
  mutate(name=factor(nameshort)) %>%
  arrange(desc(scene)) %>%
  ggplot(aes(x=name,y=n,fill=gender)) +#interaction(akt,scene)
  geom_col() +
  facet_wrap(~akt+scene, scales = "free")+# +
  labs(x = NULL, y = "number of lines")+
  coord_flip()

#a possible solution just to plot each act separately
#ggplot facet_wrap size?
linecountperscene %>% 
  filter(!is.na(nameshort)) %>%
  filter(akt==unique(dialogues$akt)[3]) %>%
  mutate(name=factor(nameshort)) %>%
  arrange(desc(scene)) %>%
  ggplot(aes(x=name,y=n,fill=gender)) +
  geom_col() +
  facet_wrap(~scene, scales = "free")+
  labs(x = NULL, y = "number of lines")+
  coord_flip()


#and these are the possible candidates we find

#they are talking about a man
akt2 <- dialogues %>%
  filter(akt==unique(dialogues$akt)[2]) %>%
  filter(scene==unique(dialogues$scene)[4])

#maybe - talking about staying at home?
akt4 <- dialogues %>%
  filter(akt==unique(dialogues$akt)[4]) %>%
  filter(scene==unique(dialogues$scene)[2])

#was es war, das in meinem Busen auf- und abstieg, wenn ich Sie aus dem Fenster sah;  
akt5 <- dialogues %>%
  filter(akt==unique(dialogues$akt)[5]) %>%
  filter(scene==unique(dialogues$scene)[7])




#people who occurred in the same scene
members_of_scenes <- dialogues %>%
  group_by(akt,scene) %>%
  distinct(name)

totalscenes <- members_of_scenes %>%
  group_by(name) %>%
  count() %>%
  arrange(desc(n))

pairs_in_scenes <- members_of_scenes %>%
  left_join(members_of_scenes, by=c("akt","scene")) %>%
  filter(name.x!=name.y) %>%
  group_by(name.x,name.y) %>%
  count() %>%
  arrange(desc(n))


pairs_in_scenes %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()




pairs_proportions <- pairs_in_scenes %>%
  rename(name=name.x) %>%
  left_join(totalscenes,by="name") %>% #here bidirectionality comes really handy actually
  mutate(proportion=n.x/n.y)

set.seed(2017)
pairs_proportions %>%
  filter(proportion > 0.3) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = proportion), show.legend = FALSE, arrow= grid::arrow(type = "closed", length = unit(.15, "inches")), end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "darkred", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()





#what we did before
stopwords(kind = "de")
stop_wordsde <- tibble(word=stopwords(kind = "de"))


dial_M <- dialogues %>%
  filter(gender=="M") %>%
  unnest_tokens(word, line)# %>%
  anti_join(stop_wordsde, by = "word")
dial_F <- dialogues %>%
  filter(gender=="F") %>%
  unnest_tokens(word, line)# %>%
  anti_join(stop_wordsde, by = "word")


comparison <- dial_M %>%
  count(word = word) %>%
  rename(M = n) %>%
  inner_join(count(dial_F, word)) %>%
  rename(F = n) %>%
  mutate(F = F / sum(F),
         M = M / sum(M))



ggplot(comparison, aes(M, F)) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = word), check_overlap = TRUE,
            vjust = 1, hjust = 1) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")



name_counts <- dialogues %>%
  unnest_tokens(word, line) %>%
  anti_join(stop_wordsde, by = "word") %>%
  count(nameshort, word, sort = TRUE) %>%
  arrange(desc(word)) %>%
  bind_tf_idf(word, nameshort, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

name_counts %>% 
  ungroup() %>% 
  top_n(20) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  ggplot(aes(word, tf_idf, fill = nameshort)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()

name_counts %>% 
  group_by(nameshort) %>% 
  top_n(10) %>% 
  ungroup %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  ggplot(aes(word, tf_idf, fill = nameshort)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~nameshort, ncol = 5, scales = "free") +
  coord_flip()



dialogues %>%
  unnest_tokens(word, line) %>%
  anti_join(stop_wordsde, by = "word") %>%
  count(gender, word, sort = TRUE) %>%
  arrange(desc(word)) %>%
  bind_tf_idf(word, gender, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  top_n(20) %>%
  ggplot(aes(word, tf_idf, fill = gender)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()




#extra, german sentiments

neg_df <- read_tsv("data/sentiments_de/SentiWS_v1.8c_Negative.txt", col_names = FALSE)
names(neg_df) <- c("Wort_POS", "Wert", "Inflektionen")

glimpse(neg_df)

neg_df %>% 
  mutate(Wort = str_sub(Wort_POS, 1, regexpr("\\|", .$Wort_POS)-1),
         POS = str_sub(Wort_POS, start = regexpr("\\|", .$Wort_POS)+1)) -> neg_df


pos_df <- read_tsv("data/sentiments_de/SentiWS_v1.8c_Positive.txt", col_names = FALSE)
names(pos_df) <- c("Wort_POS", "Wert", "Inflektionen")
pos_df %>% 
  mutate(Wort = str_sub(Wort_POS, 1, regexpr("\\|", .$Wort_POS)-1),
         POS = str_sub(Wort_POS, start = regexpr("\\|", .$Wort_POS)+1)) -> pos_df


bind_rows("neg" = neg_df, "pos" = pos_df, .id = "neg_pos") -> sentiment_df
sentiment_df %>% select(neg_pos, Wort, Wert, Inflektionen, -Wort_POS) -> sentiment_df


basic <- sentiment_df %>%
  select(neg_pos,Wert,Wort) %>%
  unnest_tokens(word,Wort) %>%
  distinct()
declinated <- sentiment_df %>%
  unnest_tokens(word,Inflektionen) %>%
  select(neg_pos,Wert,word)

bind_rows(basic,declinated) -> de_sent

dial_words <- dialogues %>%
  unnest_tokens(word, line) %>%
  anti_join(stop_wordsde, by = "word")

dial_sentiments <- dial_words %>%
  inner_join(de_sent, by = c(word = "word")) %>%
  group_by(akt,scene) %>%
  count(neg_pos) %>%
  ungroup() %>%
  spread(neg_pos, n, fill = 0) %>%
  mutate(sentiment = pos - neg) %>%
  arrange(sentiment)


sentiments_ordered <- dial_sentiments %>%
  mutate(akt =  factor(akt, levels = unique(dialogues$akt))) %>%
  mutate(scene =  factor(scene, levels = unique(dialogues$scene))) %>%
  arrange(akt, scene)


#just use colon for this, for interactions on the ggplot
sentiments_ordered %>% # plot
  ggplot(aes(x = akt:scene, y = sentiment)) + 
  geom_bar(stat = "identity", aes(fill = akt)) + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  coord_flip() + 
  ylim(-50, 50) +
  ggtitle("Centered sentiment scores", 
          subtitle = "Acts and Scenes in Hofmeister")
