#Digital Humanities workshop in the Summer School for Baltic Enlightenment and its inheritance (Sommerschule "Die baltische Aufklärung und ihr Erbe"), 2017
#Compiled by Peeter Tinits, 05-09-2017.

#A pre-installation script for the required packages

lapply(c("tidytext", "gutenbergr", "dplyr", "scales", "ggplot2","zoo","igraph","ggraph","tm","scales","readr","grid"), 
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))