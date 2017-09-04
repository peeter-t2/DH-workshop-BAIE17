scholarrefs <- read.csv("data/PoPCites_DH_phraseintitle_top1000.csv")

library(ggplot2)
ggplot(data=scholarrefs, mapping=aes(x=Year))+
  labs(title="Papers with the phrase /digital humanites/ in title")+
  geom_histogram(binwidth = 1, fill="light blue")+
  coord_cartesian(xlim = c(2000, 2018)) 
  

