###### We upload the packages 


library(dplyr)
library(janeaustenr) #Inutile je pense
library(tidytext)
library(gutenbergr)  #Idem
library(pdftools)
library(tidyverse)
library(qdapRegex)


###########################################################################################
###########################################################################################
##### 1980-1989 (25 items)
###########################################################################################
###########################################################################################

# We set the working directory

setwd("C:/Users/jgrad/Desktop/Network_Quality/Autre/Top80-89")


# We list all the files in the folder

temp = list.files(pattern="*.pdf")


# We upload these files

for(i in 1:length(temp)){
  assign(paste0('x', i), as.data.frame(pdf_text(temp[i]) %>%
                                         readr::read_lines()))
}


# We merge them in one database (we have 23 files here)


top8089 <- rbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23)


# We associate a decade ID


top8089[,2]<-"1980-1989"


###########################################################################################
###########################################################################################
##### Same for 1990-1999 (38 items: because on the top-50, 12 were books)
###########################################################################################
###########################################################################################


setwd("C:/Users/jgrad/Desktop/Network_Quality/Autre/Top90-99")

temp = list.files(pattern="*.pdf")

for(i in 1:length(temp)){
  assign(paste0('x', i), as.data.frame(pdf_text(temp[i]) %>%
                                         readr::read_lines()))
}



# We merge 

top9099 <- rbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28,x29,x30,x31,x32,x33,x34,x35,x36,x37,x38)


# unique ID

top9099[,2]<-"1990-1999"


###########################################################################################
###########################################################################################
##### 2000-2009
###########################################################################################
###########################################################################################


setwd("C:/Users/jgrad/Desktop/Network_Quality/Autre/Top00-09")

temp = list.files(pattern="*.pdf")

for(i in 1:length(temp)){
  assign(paste0('x', i), as.data.frame(pdf_text(temp[i]) %>%
           readr::read_lines()))
}



top0009 <- rbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28,x29,x30,x31,x32,x33,x34,x35,x36,x37,x38,x39)


top0009[,2]<-"2000-2009"


###########################################################################################
###########################################################################################
##### 2010-2019
###########################################################################################
###########################################################################################


setwd("C:/Users/jgrad/Desktop/Network_Quality/Autre/Top10-19")

temp = list.files(pattern="*.pdf")

for(i in 1:length(temp)){
  assign(paste0('x', i), as.data.frame(pdf_text(temp[i]) %>%
                                         readr::read_lines()))
}



top1019 <- rbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28,x29,x30,x31,x32,x33,x34,x35,x36,x37,x38,x39,x40,x41,x42,x43,x44,x45,x46,x47,x48,x49)


top1019[,2]<-"2010-2019"


####################################################################################
####################################################################################
# We create the global database
####################################################################################
####################################################################################


Basetotale<- rbind(top8089,top9099,top0009,top1019)


##### Rename columns

colnames(Basetotale) <- c("text","id")



# Create list of all words used and their frequency


Basetotale_words <- Basetotale %>%
unnest_tokens(word, text) %>%
count(id, word, sort = TRUE)


# We remove one/two letter words 


Basetotale_words$word<-rm_nchar_words(Basetotale_words$word, 2)
Basetotale_words$word<-rm_nchar_words(Basetotale_words$word, 1)



# We remove NA 

Basetotale_words$word[Basetotale_words$word==""] <- NA
Basetotale_words <- na.omit(Basetotale_words)


##### Replace ??? by the chinese symbol corresponding to U+4860

# Words poluting the figure that we remove. We also remove the NAME

mystopwords <- tibble(word = c("???","awards","gij","1817","qx2","visscher","takahashi","aki","yuval","pxq","gerbing","chemical","sgn","cma","1989","1990","1996","employees","preevent","1985","1986","1991","1999","2000","2012","12s","krugman","hummels","2011","stiglitz","2016","2008","is0","helpman","1997","kortum","2004","flynn","joreskog","2018","2007","2010","melitz","schott","flynn","juran","deming","szpe","valorem","1804","shilony","1993","1994","1992","bentler","crossref","2001","2003","2002","2005","ssrn","2006","uif","2017","http","fri","4u1","www.journals.uchicago.edu","128.135.012.127","138.231.176.", "about.jstor.org", "apr", "june", "2019", "130.238.007.040", "january", "copyright", "2014", "sun", "qur", "jun", "185..."))


Basetotale_words <- anti_join(Basetotale_words, mystopwords,
by = "word")


#### Creating the tf-idf dataframe

plot_Basetotale <- Basetotale_words %>%
bind_tf_idf(word, id, n) %>%
mutate(word = str_remove_all(word, "_")) %>%
group_by(id) %>%
slice_max(tf_idf, n = 15) %>%
ungroup() %>%
mutate(word = reorder_within(word, tf_idf, id)) %>%
mutate(id = factor(id, levels = c("1980-1989","1990-1999", "2000-2009","2010-2019")))


#### Represent the figure


ggplot(plot_Basetotale, aes(word, tf_idf, fill = id)) +
geom_col(show.legend = FALSE) +
labs(x = NULL, y = "tf-idf") +
facet_wrap(~id, ncol = 2, scales = "free") +
coord_flip() +
scale_x_reordered() +
scale_fill_manual(values=c("#2874A6", "#F4D03F", "#58D68D","#DE9493"))


library(readr)
library(stringr)




#########################################################################
#########################################################################
# Export data on excel
########################################################################
########################################################################



kk<-plot_Basetotale$word
kk<-as.data.frame(kk)

kk$date<- str_extract_all(kk$kk, "___.*")
kk$kk<-gsub("___.*", "",kk$kk)
kk<-as.data.frame(kk)
kk$date<-as.character(kk$date)


cb <- function(df, sep="\t", dec=",", max.size=(200*1000)){
  # Copy a data.frame to clipboard
  write.table(df, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=FALSE, dec=dec)
}

cb(kk)
