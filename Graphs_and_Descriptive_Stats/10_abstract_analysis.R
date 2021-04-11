
########## We import the data

setwd("C:/Users/jgrad/Desktop/Network_Quality/Network_Quality/Data")

load("Data_5_nodes.rda")

load("Data_4_Abstracts.rda")


######## We import the packages

library(readr)
library(stringr)
library(dplyr)
library(janeaustenr) #Inutile je pense
library(tidytext)
library(gutenbergr)  #Idem
library(pdftools)
library(tidyverse)
library(qdapRegex)

##### We complete manually some abstracts for the first decade

Data_8_Complement_abstract <- read_delim("Data_8_Complement_abstract.txt", "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)


##### We remove parentheses in the publication years

desco$Datepubli<-str_replace_all(string = desco$Datepubli, pattern = "\\(", replacement = "")
desco$Datepubli<-str_replace_all(string = desco$Datepubli, pattern = "\\)", replacement = "")
desco$Datepubli<-as.numeric(desco$Datepubli)


##### We extract the name of the journal

desco$publi2<-gsub("\\,.*", "",desco$publi)


##### We remove nodes with na values

zalerit<-zalerit[zalerit$Cartel!=18888,]

zalerit<-zalerit[zalerit$Cartel!=13119,]

zalerit<-zalerit[zalerit$Cartel!=131191,]

zalerit<-zalerit[zalerit$Cartel!=148413,]

zalerit<-zalerit[zalerit$Cartel!=148866,]

zalerit<-zalerit[zalerit$Cartel!=151086,]


####### We associate publication years with the abstracts

zalerit$anneecitant<-0
for(i in 1:nrow(zalerit)){
  j<-which(zalerit$Cartel[i] == desco$Cartel)
  zalerit$anneecitant[i] <- desco$Datepubli[j]}


####### We split the database according to the years

freq19811990<-zalerit[zalerit$anneecitant==1980|zalerit$anneecitant==1981|zalerit$anneecitant==1982|zalerit$anneecitant==1983|zalerit$anneecitant==1984|zalerit$anneecitant==1985|zalerit$anneecitant==1986|zalerit$anneecitant==1987|zalerit$anneecitant==1988|zalerit$anneecitant==1989,]
freq19912000<-zalerit[zalerit$anneecitant==1990|zalerit$anneecitant==1991|zalerit$anneecitant==1992|zalerit$anneecitant==1993|zalerit$anneecitant==1994|zalerit$anneecitant==1995|zalerit$anneecitant==1996|zalerit$anneecitant==1997|zalerit$anneecitant==1998|zalerit$anneecitant==1999,]
freq20012010<-zalerit[zalerit$anneecitant==2000|zalerit$anneecitant==2001|zalerit$anneecitant==2002|zalerit$anneecitant==2003|zalerit$anneecitant==2004|zalerit$anneecitant==2005|zalerit$anneecitant==2006|zalerit$anneecitant==2007|zalerit$anneecitant==2008|zalerit$anneecitant==2009,]
freq20112020<-zalerit[zalerit$anneecitant==2010|zalerit$anneecitant==2011|zalerit$anneecitant==2012|zalerit$anneecitant==2013|zalerit$anneecitant==2014|zalerit$anneecitant==2015|zalerit$anneecitant==2016|zalerit$anneecitant==2017|zalerit$anneecitant==2018|zalerit$anneecitant==2019,]

####### We determine nodes withtout abstraacts for the first period to detemrine which abstract we can add manually

joi<-desco[desco$Datepubli==1980|desco$Datepubli==1981|desco$Datepubli==1982|desco$Datepubli==1983|desco$Datepubli==1984|desco$Datepubli==1985|desco$Datepubli==1986|desco$Datepubli==1987|desco$Datepubli==1988|desco$Datepubli==1989,]

taba<-as.numeric(joi$Cartel)
tabate<-as.numeric(freq19811990$Cartel)
setdiff(taba,tabate)



##### We keep only some columns

freq19811990<-freq19811990[,c(1,3)]
freq19912000<-freq19912000[,c(1,3)]
freq20012010<-freq20012010[,c(1,3)]
freq20112020<-freq20112020[,c(1,3)]


##### We change the name of columns

freq19811990$anneecitant<-"1980-1989"
freq19912000$anneecitant<-"1990-1999"
freq20012010$anneecitant<-"2000-2009"
freq20112020$anneecitant<-"2010-2019"


###### We combine the abstracts for the first decades

colnames(Data_8_Complement_abstract) <- c("V1","anneecitant")
freq19811990<-rbind(freq19811990,Data_8_Complement_abstract)


# We create the global database

Basetotale<- rbind(freq19811990,freq19912000,freq20012010,freq20112020)

colnames(Basetotale) <- c("text","id")

# Step to count the frequency of all words

Basetotale_words <- Basetotale %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)


# We remove one/two letters words 

Basetotale_words$word<-rm_nchar_words(Basetotale_words$word, 2)
Basetotale_words$word<-rm_nchar_words(Basetotale_words$word, 1)


# We remove the NA

Basetotale_words$word[Basetotale_words$word==""] <- NA
Basetotale_words <- na.omit(Basetotale_words)


# We remove words polluting figures, and names

mystopwords <- tibble(word = c("maskin","schmalensee","rubinstein","wolinsky","ing","1981","1985","akerlof","yellen","adams","stokey","stokey's","trucks","withheld","survey","retroviral","issues","forgone","findings","online","approach","across","china","eco","person's","selten's","2009","1995","2013","2015","omas","oma","2012","2011","2016","2008","1997","2004","2018","2007","2010","1804","1993","1994","1992","2001","2003","2002","2005","2006","2017","2019", "2014"))


#### We prepare the graphs

Basetotale_words <- anti_join(Basetotale_words, mystopwords,
                              by = "word")


plot_Basetotale <- Basetotale_words %>%
  bind_tf_idf(word, id, n) %>%
  mutate(word = str_remove_all(word, "_")) %>%
  group_by(id) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, id)) %>%
  mutate(id = factor(id, levels = c("1980-1989","1990-1999", "2000-2009","2010-2019")))


###### We represent the graph

ggplot(plot_Basetotale, aes(word, tf_idf, fill = id)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~id, ncol = 2, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  scale_fill_manual(values=c("#2874A6", "#F4D03F", "#58D68D","#DE9493"))















