
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


###### We remove parentheses around the publication year of nodes

desco$Datepubli<-str_replace_all(string = desco$Datepubli, pattern = "\\(", replacement = "")
desco$Datepubli<-str_replace_all(string = desco$Datepubli, pattern = "\\)", replacement = "")
desco$Datepubli<-as.numeric(desco$Datepubli)


###### We extract the name of the journal


desco$publi2<-gsub("\\,.*", "",desco$publi)


###### We split the database according to the decade

freq19811990<-desco[desco$Datepubli==1980|desco$Datepubli==1981|desco$Datepubli==1982|desco$Datepubli==1983|desco$Datepubli==1984|desco$Datepubli==1985|desco$Datepubli==1986|desco$Datepubli==1987|desco$Datepubli==1988|desco$Datepubli==1989,]
freq19912000<-desco[desco$Datepubli==1990|desco$Datepubli==1991|desco$Datepubli==1992|desco$Datepubli==1993|desco$Datepubli==1994|desco$Datepubli==1995|desco$Datepubli==1996|desco$Datepubli==1997|desco$Datepubli==1998|desco$Datepubli==1999,]
freq20012010<-desco[desco$Datepubli==2000|desco$Datepubli==2001|desco$Datepubli==2002|desco$Datepubli==2003|desco$Datepubli==2004|desco$Datepubli==2005|desco$Datepubli==2006|desco$Datepubli==2007|desco$Datepubli==2008|desco$Datepubli==2009,]
freq20112020<-desco[desco$Datepubli==2010|desco$Datepubli==2011|desco$Datepubli==2012|desco$Datepubli==2013|desco$Datepubli==2014|desco$Datepubli==2015|desco$Datepubli==2016|desco$Datepubli==2017|desco$Datepubli==2018|desco$Datepubli==2019,]


####### We rank the journals according to occurrence


freq19811990<-table(freq19811990$publi2)
freq19811990<-as.data.frame(freq19811990)
freq19811990<-freq19811990[order(-freq19811990$Freq),]

freq19912000<-table(freq19912000$publi2)
freq19912000<-as.data.frame(freq19912000)
freq19912000<-freq19912000[order(-freq19912000$Freq),]

freq20012010<-table(freq20012010$publi2)
freq20012010<-as.data.frame(freq20012010)
freq20012010<-freq20012010[order(-freq20012010$Freq),]

freq20112020<-table(freq20112020$publi2)
freq20112020<-as.data.frame(freq20112020)
freq20112020<-freq20112020[order(-freq20112020$Freq),]


##### We create a function to export on Excel


cb <- function(df, sep="\t", dec=",", max.size=(200*10000)){
  # Copy a data.frame to clipboard
  write.table(df, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=FALSE, dec=dec)
}

cb(freq19912000)
cb(freq19912000)
cb(freq20012010)
cb(freq20112020)




####################################################################################################################
####################################################################################################################
# Once we have associate journals with their CNRS categories, we import the file to compute weighted tables
####################################################################################################################
####################################################################################################################

#### We import the file

library(readr)
revue <- read_delim("Data_10_journals.txt", "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
library(questionr)


#### We generate the table and we precise the weight vector

j<-wtd.table(revue$X3,weights = revue$X2)
k<-wtd.table(revue$X6,weights = revue$X5)
l<-wtd.table(revue$X9,weights = revue$X8)


j<-as.data.frame(j)
k<-as.data.frame(k)
l<-as.data.frame(l)


cb <- function(df, sep="\t", dec=",", max.size=(200*1000)){
  # Copy a data.frame to clipboard
  write.table(df, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=FALSE, dec=dec)
}

cb(j)
cb(k)
cb(l)
