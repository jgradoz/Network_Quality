setwd("C:/Users/jgrad/Desktop/Network_Quality/Network_Quality/Data")


load("Data_7_finalbase.rda")


#####################################################################################################################
#####################################################################################################################
# Modification qualitative d'erreurs restantes constatées en explorant la base: Can be continued -------------------
#####################################################################################################################
#####################################################################################################################


eoeoe$identifiant[eoeoe$identifiant=="LANCASTER (1966) 74  65) 132"]<-"LANCASTER (1966) 74 132"
eoeoe$identifiant[eoeoe$identifiant=="LANCASTER (1966) AB 74"]<-"LANCASTER (1966) 74 132"
eoeoe$identifiant[eoeoe$identifiant=="LANCASTER (1971) AB 40"]<-"LANCASTER (1971) AB AB"
eoeoe$identifiant[eoeoe$identifiant=="LANCASTER (1972) AB AB"]<-"LANCASTER (1971) AB AB"
eoeoe$identifiant[eoeoe$identifiant=="LANCASTER , 1971. AB AB"]<-"LANCASTER (1971) AB AB"
eoeoe$identifiant[eoeoe$identifiant=="LANCASTER (1980) 116 281"]<-"LANCASTER (1979) AB AB"        
eoeoe$identifiant[eoeoe$identifiant=="AKERLOF (1970) AB 89"]<-"AKERLOF (1970) 84 488"
eoeoe$identifiant[eoeoe$identifiant=="AKERLOF (1970) AB AB"]<-"AKERLOF (1970) 84 488"
eoeoe$identifiant[eoeoe$identifiant=="MUSSA (1978) AB AB"]<-"MUSSA (1978) 18 301"
eoeoe$identifiant[eoeoe$identifiant=="BAGWELL (1985) AB AB"]<-"BAGWELL (1988) 19 59"
eoeoe$identifiant[eoeoe$identifiant=="BAGWELL (2007) AB AB"]<-"BAGWELL (2007) 3 1701"
eoeoe$identifiant[eoeoe$identifiant=="BAGWELL (1988) AB AB"]<-"BAGWELL (1991) 81 224"
eoeoe$identifiant[eoeoe$identifiant=="BAGWELL (1991) 81 . AB"]<-"BAGWELL (1991) 81 224"
eoeoe$identifiant[eoeoe$identifiant=="BAGWELL 1991 81 AB"]<-"BAGWELL (1991) 81 224"
eoeoe$identifiant[eoeoe$identifiant=="TIROLE (1990) AB AB"]<-"TIROLE (1988) AB AB"
eoeoe$identifiant[eoeoe$identifiant=="TIROLE (1994) AB AB"]<-"TIROLE (1988) AB AB"
eoeoe$identifiant[eoeoe$identifiant=="TIROLE (1998) AB AB"]<-"TIROLE (1988) AB AB"
eoeoe$identifiant[eoeoe$identifiant=="SPENCE (1973) AB AB"]<-"SPENCE (1973) 87 355"


# On calcule la fréquence de chaque identifiant

freq<-table(eoeoe$identifiant)
freq<-as.data.frame(freq)

# On stocke la fréquence comme nouvelle variable, ce qui nous servira à renseigner la taille des noeuds dans les graphes

eoeoe$frequence<-0
for(i in 1:nrow(eoeoe)){
  j<-which(eoeoe$identifiant[i] == freq$Var1)
  eoeoe$frequence[i]<-freq$Freq[j]
}





#################################################################################################################
#################################################################################################################
###### On fait la liste des auteurs les plus cités par décénnie --------------------------------------
#################################################################################################################
#################################################################################################################


load("Data_5_nodes.rda")

###### We identify the year of publication of the citing article and we paste this information in the global database

desco$Datepubli<-as.character(desco$Datepubli)
eoeoe$anneecitant<-0
for(i in 1:nrow(eoeoe)){
  j<-which(eoeoe$V2[i] == desco$Cartel)
  eoeoe$anneecitant[i] <- desco$Datepubli[j]
}


library(readr)
library(stringr)

#### We remove the parenthese

eoeoe$anneecitant<-str_replace_all(string = eoeoe$anneecitant, pattern = "\\(", replacement = "")
eoeoe$anneecitant<-str_replace_all(string = eoeoe$anneecitant, pattern = "\\)", replacement = "")
eoeoe$anneecitant<-as.numeric(eoeoe$anneecitant)


##### We split according to citing year

freq19811990<-eoeoe[eoeoe$anneecitant==1980|eoeoe$anneecitant==1981|eoeoe$anneecitant==1982|eoeoe$anneecitant==1983|eoeoe$anneecitant==1984|eoeoe$anneecitant==1985|eoeoe$anneecitant==1986|eoeoe$anneecitant==1987|eoeoe$anneecitant==1988|eoeoe$anneecitant==1989,]
freq19912000<-eoeoe[eoeoe$anneecitant==1990|eoeoe$anneecitant==1991|eoeoe$anneecitant==1992|eoeoe$anneecitant==1993|eoeoe$anneecitant==1994|eoeoe$anneecitant==1995|eoeoe$anneecitant==1996|eoeoe$anneecitant==1997|eoeoe$anneecitant==1998|eoeoe$anneecitant==1999,]
freq20012010<-eoeoe[eoeoe$anneecitant==2000|eoeoe$anneecitant==2001|eoeoe$anneecitant==2002|eoeoe$anneecitant==2003|eoeoe$anneecitant==2004|eoeoe$anneecitant==2005|eoeoe$anneecitant==2006|eoeoe$anneecitant==2007|eoeoe$anneecitant==2008|eoeoe$anneecitant==2009,]
freq20112020<-eoeoe[eoeoe$anneecitant==2010|eoeoe$anneecitant==2011|eoeoe$anneecitant==2012|eoeoe$anneecitant==2013|eoeoe$anneecitant==2014|eoeoe$anneecitant==2015|eoeoe$anneecitant==2016|eoeoe$anneecitant==2017|eoeoe$anneecitant==2018|eoeoe$anneecitant==2019,]


##### We adapt to the good format


freq19811990<-table(freq19811990$identifiant)
freq19811990<-as.data.frame(freq19811990)

freq19912000<-table(freq19912000$identifiant)
freq19912000<-as.data.frame(freq19912000)

freq20012010<-table(freq20012010$identifiant)
freq20012010<-as.data.frame(freq20012010)

freq20112020<-table(freq20112020$identifiant)
freq20112020<-as.data.frame(freq20112020)


###### We rank items according to their popularity

top19811990<-freq19811990[rev(order(freq19811990$Freq)),]
top19912000<-freq19912000[rev(order(freq19912000$Freq)),]
top20012010<-freq20012010[rev(order(freq20012010$Freq)),]
top20112020<-freq20112020[rev(order(freq20112020$Freq)),]


###### We keep about 2/3 of items

top19811990<-top19811990[1:2000,]
top19912000<-top19912000[1:5000,]
top20012010<-top20012010[1:15000,]
top20112020<-top20112020[1:10000,]


####### We replace ID with the complete reference in order to extract authors


for(i in 1:nrow(top19811990)){
  j<-which(top19811990$Var1[i] == eoeoe$identifiant)
  z<-which(paste(eoeoe$Firstauthor[j],eoeoe$Datepubli[j],eoeoe$Issue[j],eoeoe$Page[j])==eoeoe$identifiant[j])[1]
  top19811990$V1[i] <- eoeoe$V1[j[z]]
}

for(i in 1:nrow(top19912000)){
  j<-which(top19912000$Var1[i] == eoeoe$identifiant)
  z<-which(paste(eoeoe$Firstauthor[j],eoeoe$Datepubli[j],eoeoe$Issue[j],eoeoe$Page[j])==eoeoe$identifiant[j])[1]
  top19912000$V1[i] <- eoeoe$V1[j[z]]
}

for(i in 1:nrow(top20012010)){
  j<-which(top20012010$Var1[i] == eoeoe$identifiant)
  z<-which(paste(eoeoe$Firstauthor[j],eoeoe$Datepubli[j],eoeoe$Issue[j],eoeoe$Page[j])==eoeoe$identifiant[j])[1]
  top20012010$V1[i] <- eoeoe$V1[j[z]]
}

for(i in 1:nrow(top20112020)){
  j<-which(top20112020$Var1[i] == eoeoe$identifiant)
  z<-which(paste(eoeoe$Firstauthor[j],eoeoe$Datepubli[j],eoeoe$Issue[j],eoeoe$Page[j])==eoeoe$identifiant[j])[1]
  top20112020$V1[i] <- eoeoe$V1[j[z]]
}



#### Extraction of the authors for the first database witht REGEX (conceive for at max 9 authors in the article)

top19811990$authors<- str_extract_all(top19811990$V1, "(^.*\\, [[:alpha:]]\\.[[:alpha:]]\\.\\, )|(^.*\\, [[:alpha:]]\\.\\, )")
top19811990$authors<-as.character(top19811990$authors)
top19811990$authors2<- str_extract_all(top19811990$authors, "\\.\\, [A-z].*\\, .*\\.\\,")
top19811990$authors<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top19811990$authors)
top19811990$authors2<-as.character(top19811990$authors2)
top19811990$authors2<-str_replace(string = top19811990$authors2, pattern = "^\\.\\, ", replacement = "")
top19811990$authors3<- str_extract_all(top19811990$authors2, "\\.\\, [A-z].*\\, .*\\.\\,")
top19811990$authors2<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top19811990$authors2)
top19811990$authors3<-as.character(top19811990$authors3)
top19811990$authors3<-str_replace(string = top19811990$authors3, pattern = "^\\.\\, ", replacement = "")
top19811990$authors4<- str_extract_all(top19811990$authors3, "\\.\\, [A-z].*\\, .*\\.\\,")
top19811990$authors3<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top19811990$authors3)
top19811990$authors4<-as.character(top19811990$authors4)
top19811990$authors4<-str_replace(string = top19811990$authors4, pattern = "^\\.\\, ", replacement = "")
top19811990$authors5<- str_extract_all(top19811990$authors4, "\\.\\, [A-z].*\\, .*\\.\\,")
top19811990$authors4<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top19811990$authors4)
top19811990$authors5<-as.character(top19811990$authors5)
top19811990$authors5<-str_replace(string = top19811990$authors5, pattern = "^\\.\\, ", replacement = "")
top19811990$authors6<- str_extract_all(top19811990$authors5, "\\.\\, [A-z].*\\, .*\\.\\,")
top19811990$authors5<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top19811990$authors5)
top19811990$authors6<-as.character(top19811990$authors6)
top19811990$authors6<-str_replace(string = top19811990$authors6, pattern = "^\\.\\, ", replacement = "")
top19811990$authors7<- str_extract_all(top19811990$authors6, "\\.\\, [A-z].*\\, .*\\.\\,")
top19811990$authors6<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top19811990$authors6)
top19811990$authors7<-as.character(top19811990$authors7)
top19811990$authors7<-str_replace(string = top19811990$authors7, pattern = "^\\.\\, ", replacement = "")
top19811990$authors8<- str_extract_all(top19811990$authors7, "\\.\\, [A-z].*\\, .*\\.\\,")
top19811990$authors7<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top19811990$authors7)
top19811990$authors8<-as.character(top19811990$authors8)
top19811990$authors8<-str_replace(string = top19811990$authors8, pattern = "^\\.\\, ", replacement = "")
top19811990$authors9<- str_extract_all(top19811990$authors7, "\\.\\, [A-z].*\\, .*\\.\\,")
top19811990$authors8<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top19811990$authors8)
top19811990$authors9<-as.character(top19811990$authors9)
top19811990$authors9<-str_replace(string = top19811990$authors9, pattern = "^\\.\\, ", replacement = "")


##### We clean the data

top19811990$authors<-str_replace(string = top19811990$authors, pattern = "\\.\\, .*", replacement = "")
top19811990$authors2<-str_replace(string = top19811990$authors2, pattern = "\\.\\, .*", replacement = "")
top19811990$authors3<-str_replace(string = top19811990$authors3, pattern = "\\.\\, .*", replacement = "")
top19811990$authors4<-str_replace(string = top19811990$authors4, pattern = "\\.\\, .*", replacement = "")
top19811990$authors5<-str_replace(string = top19811990$authors5, pattern = "\\.\\, .*", replacement = "")
top19811990$authors6<-str_replace(string = top19811990$authors6, pattern = "\\.\\, .*", replacement = "")
top19811990$authors7<-str_replace(string = top19811990$authors7, pattern = "\\.\\, .*", replacement = "")
top19811990$authors8<-str_replace(string = top19811990$authors8, pattern = "\\.\\, .*", replacement = "")
top19811990$authors9<-str_replace(string = top19811990$authors9, pattern = "\\.\\, .*", replacement = "")


##### We replace the absence of author with AB

top19811990$authors[top19811990$authors == "character(0)"] <- "AB"
top19811990$authors2[top19811990$authors2 == "character(0)"] <- "AB"
top19811990$authors3[top19811990$authors3 == "character(0)"] <- "AB"
top19811990$authors4[top19811990$authors4 == "character(0)"] <- "AB"
top19811990$authors5[top19811990$authors5 == "character(0)"] <- "AB"
top19811990$authors6[top19811990$authors6 == "character(0)"] <- "AB"
top19811990$authors7[top19811990$authors7 == "character(0)"] <- "AB"
top19811990$authors8[top19811990$authors8 == "character(0)"] <- "AB"
top19811990$authors9[top19811990$authors9 == "character(0)"] <- "AB"


#### Same for second base


top19912000$authors<- str_extract_all(top19912000$V1, "(^.*\\, [[:alpha:]]\\.[[:alpha:]]\\.\\, )|(^.*\\, [[:alpha:]]\\.\\, )")
top19912000$authors<-as.character(top19912000$authors)
top19912000$authors2<- str_extract_all(top19912000$authors, "\\.\\, [A-z].*\\, .*\\.\\,")
top19912000$authors<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top19912000$authors)
top19912000$authors2<-as.character(top19912000$authors2)
top19912000$authors2<-str_replace(string = top19912000$authors2, pattern = "^\\.\\, ", replacement = "")
top19912000$authors3<- str_extract_all(top19912000$authors2, "\\.\\, [A-z].*\\, .*\\.\\,")
top19912000$authors2<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top19912000$authors2)
top19912000$authors3<-as.character(top19912000$authors3)
top19912000$authors3<-str_replace(string = top19912000$authors3, pattern = "^\\.\\, ", replacement = "")
top19912000$authors4<- str_extract_all(top19912000$authors3, "\\.\\, [A-z].*\\, .*\\.\\,")
top19912000$authors3<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top19912000$authors3)
top19912000$authors4<-as.character(top19912000$authors4)
top19912000$authors4<-str_replace(string = top19912000$authors4, pattern = "^\\.\\, ", replacement = "")
top19912000$authors5<- str_extract_all(top19912000$authors4, "\\.\\, [A-z].*\\, .*\\.\\,")
top19912000$authors4<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top19912000$authors4)
top19912000$authors5<-as.character(top19912000$authors5)
top19912000$authors5<-str_replace(string = top19912000$authors5, pattern = "^\\.\\, ", replacement = "")
top19912000$authors6<- str_extract_all(top19912000$authors5, "\\.\\, [A-z].*\\, .*\\.\\,")
top19912000$authors5<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top19912000$authors5)
top19912000$authors6<-as.character(top19912000$authors6)
top19912000$authors6<-str_replace(string = top19912000$authors6, pattern = "^\\.\\, ", replacement = "")
top19912000$authors7<- str_extract_all(top19912000$authors6, "\\.\\, [A-z].*\\, .*\\.\\,")
top19912000$authors6<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top19912000$authors6)
top19912000$authors7<-as.character(top19912000$authors7)
top19912000$authors7<-str_replace(string = top19912000$authors7, pattern = "^\\.\\, ", replacement = "")
top19912000$authors8<- str_extract_all(top19912000$authors7, "\\.\\, [A-z].*\\, .*\\.\\,")
top19912000$authors7<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top19912000$authors7)
top19912000$authors8<-as.character(top19912000$authors8)
top19912000$authors8<-str_replace(string = top19912000$authors8, pattern = "^\\.\\, ", replacement = "")
top19912000$authors9<- str_extract_all(top19912000$authors7, "\\.\\, [A-z].*\\, .*\\.\\,")
top19912000$authors8<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top19912000$authors8)
top19912000$authors9<-as.character(top19912000$authors9)
top19912000$authors9<-str_replace(string = top19912000$authors9, pattern = "^\\.\\, ", replacement = "")



top19912000$authors<-str_replace(string = top19912000$authors, pattern = "\\.\\, .*", replacement = "")
top19912000$authors2<-str_replace(string = top19912000$authors2, pattern = "\\.\\, .*", replacement = "")
top19912000$authors3<-str_replace(string = top19912000$authors3, pattern = "\\.\\, .*", replacement = "")
top19912000$authors4<-str_replace(string = top19912000$authors4, pattern = "\\.\\, .*", replacement = "")
top19912000$authors5<-str_replace(string = top19912000$authors5, pattern = "\\.\\, .*", replacement = "")
top19912000$authors6<-str_replace(string = top19912000$authors6, pattern = "\\.\\, .*", replacement = "")
top19912000$authors7<-str_replace(string = top19912000$authors7, pattern = "\\.\\, .*", replacement = "")
top19912000$authors8<-str_replace(string = top19912000$authors8, pattern = "\\.\\, .*", replacement = "")
top19912000$authors9<-str_replace(string = top19912000$authors9, pattern = "\\.\\, .*", replacement = "")


top19912000$authors[top19912000$authors == "character(0)"] <- "AB"
top19912000$authors2[top19912000$authors2 == "character(0)"] <- "AB"
top19912000$authors3[top19912000$authors3 == "character(0)"] <- "AB"
top19912000$authors4[top19912000$authors4 == "character(0)"] <- "AB"
top19912000$authors5[top19912000$authors5 == "character(0)"] <- "AB"
top19912000$authors6[top19912000$authors6 == "character(0)"] <- "AB"
top19912000$authors7[top19912000$authors7 == "character(0)"] <- "AB"
top19912000$authors8[top19912000$authors8 == "character(0)"] <- "AB"
top19912000$authors9[top19912000$authors9 == "character(0)"] <- "AB"


# Same for third base


top20012010$authors<- str_extract_all(top20012010$V1, "(^.*\\, [[:alpha:]]\\.[[:alpha:]]\\.\\, )|(^.*\\, [[:alpha:]]\\.\\, )")
top20012010$authors<-as.character(top20012010$authors)
top20012010$authors2<- str_extract_all(top20012010$authors, "\\.\\, [A-z].*\\, .*\\.\\,")
top20012010$authors<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top20012010$authors)
top20012010$authors2<-as.character(top20012010$authors2)
top20012010$authors2<-str_replace(string = top20012010$authors2, pattern = "^\\.\\, ", replacement = "")
top20012010$authors3<- str_extract_all(top20012010$authors2, "\\.\\, [A-z].*\\, .*\\.\\,")
top20012010$authors2<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top20012010$authors2)
top20012010$authors3<-as.character(top20012010$authors3)
top20012010$authors3<-str_replace(string = top20012010$authors3, pattern = "^\\.\\, ", replacement = "")
top20012010$authors4<- str_extract_all(top20012010$authors3, "\\.\\, [A-z].*\\, .*\\.\\,")
top20012010$authors3<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top20012010$authors3)
top20012010$authors4<-as.character(top20012010$authors4)
top20012010$authors4<-str_replace(string = top20012010$authors4, pattern = "^\\.\\, ", replacement = "")
top20012010$authors5<- str_extract_all(top20012010$authors4, "\\.\\, [A-z].*\\, .*\\.\\,")
top20012010$authors4<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top20012010$authors4)
top20012010$authors5<-as.character(top20012010$authors5)
top20012010$authors5<-str_replace(string = top20012010$authors5, pattern = "^\\.\\, ", replacement = "")
top20012010$authors6<- str_extract_all(top20012010$authors5, "\\.\\, [A-z].*\\, .*\\.\\,")
top20012010$authors5<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top20012010$authors5)
top20012010$authors6<-as.character(top20012010$authors6)
top20012010$authors6<-str_replace(string = top20012010$authors6, pattern = "^\\.\\, ", replacement = "")
top20012010$authors7<- str_extract_all(top20012010$authors6, "\\.\\, [A-z].*\\, .*\\.\\,")
top20012010$authors6<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top20012010$authors6)
top20012010$authors7<-as.character(top20012010$authors7)
top20012010$authors7<-str_replace(string = top20012010$authors7, pattern = "^\\.\\, ", replacement = "")
top20012010$authors8<- str_extract_all(top20012010$authors7, "\\.\\, [A-z].*\\, .*\\.\\,")
top20012010$authors7<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top20012010$authors7)
top20012010$authors8<-as.character(top20012010$authors8)
top20012010$authors8<-str_replace(string = top20012010$authors8, pattern = "^\\.\\, ", replacement = "")
top20012010$authors9<- str_extract_all(top20012010$authors7, "\\.\\, [A-z].*\\, .*\\.\\,")
top20012010$authors8<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top20012010$authors8)
top20012010$authors9<-as.character(top20012010$authors9)
top20012010$authors9<-str_replace(string = top20012010$authors9, pattern = "^\\.\\, ", replacement = "")



top20012010$authors<-str_replace(string = top20012010$authors, pattern = "\\.\\, .*", replacement = "")
top20012010$authors2<-str_replace(string = top20012010$authors2, pattern = "\\.\\, .*", replacement = "")
top20012010$authors3<-str_replace(string = top20012010$authors3, pattern = "\\.\\, .*", replacement = "")
top20012010$authors4<-str_replace(string = top20012010$authors4, pattern = "\\.\\, .*", replacement = "")
top20012010$authors5<-str_replace(string = top20012010$authors5, pattern = "\\.\\, .*", replacement = "")
top20012010$authors6<-str_replace(string = top20012010$authors6, pattern = "\\.\\, .*", replacement = "")
top20012010$authors7<-str_replace(string = top20012010$authors7, pattern = "\\.\\, .*", replacement = "")
top20012010$authors8<-str_replace(string = top20012010$authors8, pattern = "\\.\\, .*", replacement = "")
top20012010$authors9<-str_replace(string = top20012010$authors9, pattern = "\\.\\, .*", replacement = "")


top20012010$authors[top20012010$authors == "character(0)"] <- "AB"
top20012010$authors2[top20012010$authors2 == "character(0)"] <- "AB"
top20012010$authors3[top20012010$authors3 == "character(0)"] <- "AB"
top20012010$authors4[top20012010$authors4 == "character(0)"] <- "AB"
top20012010$authors5[top20012010$authors5 == "character(0)"] <- "AB"
top20012010$authors6[top20012010$authors6 == "character(0)"] <- "AB"
top20012010$authors7[top20012010$authors7 == "character(0)"] <- "AB"
top20012010$authors8[top20012010$authors8 == "character(0)"] <- "AB"
top20012010$authors9[top20012010$authors9 == "character(0)"] <- "AB"



## Same for fourth base



top20112020$authors<- str_extract_all(top20112020$V1, "(^.*\\, [[:alpha:]]\\.[[:alpha:]]\\.\\, )|(^.*\\, [[:alpha:]]\\.\\, )")
top20112020$authors<-as.character(top20112020$authors)
top20112020$authors2<- str_extract_all(top20112020$authors, "\\.\\, [A-z].*\\, .*\\.\\,")
top20112020$authors<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top20112020$authors)
top20112020$authors2<-as.character(top20112020$authors2)
top20112020$authors2<-str_replace(string = top20112020$authors2, pattern = "^\\.\\, ", replacement = "")
top20112020$authors3<- str_extract_all(top20112020$authors2, "\\.\\, [A-z].*\\, .*\\.\\,")
top20112020$authors2<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top20112020$authors2)
top20112020$authors3<-as.character(top20112020$authors3)
top20112020$authors3<-str_replace(string = top20112020$authors3, pattern = "^\\.\\, ", replacement = "")
top20112020$authors4<- str_extract_all(top20112020$authors3, "\\.\\, [A-z].*\\, .*\\.\\,")
top20112020$authors3<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top20112020$authors3)
top20112020$authors4<-as.character(top20112020$authors4)
top20112020$authors4<-str_replace(string = top20112020$authors4, pattern = "^\\.\\, ", replacement = "")
top20112020$authors5<- str_extract_all(top20112020$authors4, "\\.\\, [A-z].*\\, .*\\.\\,")
top20112020$authors4<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top20112020$authors4)
top20112020$authors5<-as.character(top20112020$authors5)
top20112020$authors5<-str_replace(string = top20112020$authors5, pattern = "^\\.\\, ", replacement = "")
top20112020$authors6<- str_extract_all(top20112020$authors5, "\\.\\, [A-z].*\\, .*\\.\\,")
top20112020$authors5<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top20112020$authors5)
top20112020$authors6<-as.character(top20112020$authors6)
top20112020$authors6<-str_replace(string = top20112020$authors6, pattern = "^\\.\\, ", replacement = "")
top20112020$authors7<- str_extract_all(top20112020$authors6, "\\.\\, [A-z].*\\, .*\\.\\,")
top20112020$authors6<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top20112020$authors6)
top20112020$authors7<-as.character(top20112020$authors7)
top20112020$authors7<-str_replace(string = top20112020$authors7, pattern = "^\\.\\, ", replacement = "")
top20112020$authors8<- str_extract_all(top20112020$authors7, "\\.\\, [A-z].*\\, .*\\.\\,")
top20112020$authors7<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top20112020$authors7)
top20112020$authors8<-as.character(top20112020$authors8)
top20112020$authors8<-str_replace(string = top20112020$authors8, pattern = "^\\.\\, ", replacement = "")
top20112020$authors9<- str_extract_all(top20112020$authors7, "\\.\\, [A-z].*\\, .*\\.\\,")
top20112020$authors8<-gsub("\\.\\, [A-z].*\\, .*\\.\\,", "",top20112020$authors8)
top20112020$authors9<-as.character(top20112020$authors9)
top20112020$authors9<-str_replace(string = top20112020$authors9, pattern = "^\\.\\, ", replacement = "")



top20112020$authors<-str_replace(string = top20112020$authors, pattern = "\\.\\, .*", replacement = "")
top20112020$authors2<-str_replace(string = top20112020$authors2, pattern = "\\.\\, .*", replacement = "")
top20112020$authors3<-str_replace(string = top20112020$authors3, pattern = "\\.\\, .*", replacement = "")
top20112020$authors4<-str_replace(string = top20112020$authors4, pattern = "\\.\\, .*", replacement = "")
top20112020$authors5<-str_replace(string = top20112020$authors5, pattern = "\\.\\, .*", replacement = "")
top20112020$authors6<-str_replace(string = top20112020$authors6, pattern = "\\.\\, .*", replacement = "")
top20112020$authors7<-str_replace(string = top20112020$authors7, pattern = "\\.\\, .*", replacement = "")
top20112020$authors8<-str_replace(string = top20112020$authors8, pattern = "\\.\\, .*", replacement = "")
top20112020$authors9<-str_replace(string = top20112020$authors9, pattern = "\\.\\, .*", replacement = "")


top20112020$authors[top20112020$authors == "character(0)"] <- "AB"
top20112020$authors2[top20112020$authors2 == "character(0)"] <- "AB"
top20112020$authors3[top20112020$authors3 == "character(0)"] <- "AB"
top20112020$authors4[top20112020$authors4 == "character(0)"] <- "AB"
top20112020$authors5[top20112020$authors5 == "character(0)"] <- "AB"
top20112020$authors6[top20112020$authors6 == "character(0)"] <- "AB"
top20112020$authors7[top20112020$authors7 == "character(0)"] <- "AB"
top20112020$authors8[top20112020$authors8 == "character(0)"] <- "AB"
top20112020$authors9[top20112020$authors9 == "character(0)"] <- "AB"



##### We create new row for each coauthor, with the information on the number of occurrence


top19811990 <- na.omit(top19811990)

# This command replicates the row

top19811990 <- top19811990[rep(row.names(top19811990), ifelse(top19811990$authors9!="AB",9,ifelse(top19811990$authors8!="AB",8,ifelse(top19811990$authors7!="AB",7,ifelse(top19811990$authors6!="AB",6,ifelse(top19811990$authors5!="AB",5,ifelse(top19811990$authors4!="AB",4,ifelse(top19811990$authors3!="AB",3,ifelse(top19811990$authors2!="AB",2,1))))))))),]


# If we have a second author, put the name of this author on the second replicated row

for(i in 1:(nrow(top19811990)-1)){if(top19811990[i,3]==top19811990[i+1,3]){
  top19811990[i+1,4]<-top19811990[i+1,5]
}else{top19811990[i+1,4]<-top19811990[i+1,4]}}


for(i in 1:(nrow(top19811990)-2)){if(top19811990[i,3]==top19811990[i+2,3]){
  top19811990[i+2,4]<-top19811990[i,6]
}else{top19811990[i+2,4]<-top19811990[i+2,4]}}

for(i in 1:(nrow(top19811990)-3)){if(top19811990[i,3]==top19811990[i+3,3]){
  top19811990[i+3,4]<-top19811990[i,7]
}else{top19811990[i+3,4]<-top19811990[i+3,4]}}


for(i in 1:(nrow(top19811990)-4)){if(top19811990[i,3]==top19811990[i+4,3]){
  top19811990[i+4,4]<-top19811990[i,8]
}else{top19811990[i+4,4]<-top19811990[i+4,4]}}


for(i in 1:(nrow(top19811990)-5)){if(top19811990[i,3]==top19811990[i+5,3]){
  top19811990[i+5,4]<-top19811990[i,9]
}else{top19811990[i+5,4]<-top19811990[i+5,4]}}


for(i in 1:(nrow(top19811990)-6)){if(top19811990[i,3]==top19811990[i+6,3]){
  top19811990[i+6,4]<-top19811990[i,10]
}else{top19811990[i+6,4]<-top19811990[i+6,4]}}



for(i in 1:(nrow(top19811990)-7)){if(top19811990[i,3]==top19811990[i+7,3]){
  top19811990[i+7,4]<-top19811990[i,11]
}else{top19811990[i+7,4]<-top19811990[i+7,4]}}


for(i in 1:(nrow(top19811990)-8)){if(top19811990[i,3]==top19811990[i+8,3]){
  top19811990[i+8,4]<-top19811990[i,12]
}else{top19811990[i+8,4]<-top19811990[i+8,4]}}


### We keep the columns of interest

top19811990<-top19811990[,c(2,3,4)]


#### We clean the data

top19811990$authors<-str_replace(string = top19811990$authors, pattern = "\\,$", replacement = "")
top19811990$authors<-str_replace(string = top19811990$authors, pattern = "\\.$", replacement = "")

top19811990$authors<-iconv(top19811990$authors,from="UTF-8",to="ASCII//TRANSLIT")

##### On passe les noms en majuscule

top19811990$authors<-toupper(top19811990$authors)

top19811990$authors<-trimws(top19811990$authors)




###### Same for the second database



top19912000 <- na.omit(top19912000)

top19912000 <- top19912000[rep(row.names(top19912000), ifelse(top19912000$authors9!="AB",9,ifelse(top19912000$authors8!="AB",8,ifelse(top19912000$authors7!="AB",7,ifelse(top19912000$authors6!="AB",6,ifelse(top19912000$authors5!="AB",5,ifelse(top19912000$authors4!="AB",4,ifelse(top19912000$authors3!="AB",3,ifelse(top19912000$authors2!="AB",2,1))))))))),]



for(i in 1:(nrow(top19912000)-1)){if(top19912000[i,3]==top19912000[i+1,3]){
  top19912000[i+1,4]<-top19912000[i+1,5]
}else{top19912000[i+1,4]<-top19912000[i+1,4]}}


for(i in 1:(nrow(top19912000)-2)){if(top19912000[i,3]==top19912000[i+2,3]){
  top19912000[i+2,4]<-top19912000[i,6]
}else{top19912000[i+2,4]<-top19912000[i+2,4]}}

for(i in 1:(nrow(top19912000)-3)){if(top19912000[i,3]==top19912000[i+3,3]){
  top19912000[i+3,4]<-top19912000[i,7]
}else{top19912000[i+3,4]<-top19912000[i+3,4]}}


for(i in 1:(nrow(top19912000)-4)){if(top19912000[i,3]==top19912000[i+4,3]){
  top19912000[i+4,4]<-top19912000[i,8]
}else{top19912000[i+4,4]<-top19912000[i+4,4]}}


for(i in 1:(nrow(top19912000)-5)){if(top19912000[i,3]==top19912000[i+5,3]){
  top19912000[i+5,4]<-top19912000[i,9]
}else{top19912000[i+5,4]<-top19912000[i+5,4]}}


for(i in 1:(nrow(top19912000)-6)){if(top19912000[i,3]==top19912000[i+6,3]){
  top19912000[i+6,4]<-top19912000[i,10]
}else{top19912000[i+6,4]<-top19912000[i+6,4]}}



for(i in 1:(nrow(top19912000)-7)){if(top19912000[i,3]==top19912000[i+7,3]){
  top19912000[i+7,4]<-top19912000[i,11]
}else{top19912000[i+7,4]<-top19912000[i+7,4]}}


for(i in 1:(nrow(top19912000)-8)){if(top19912000[i,3]==top19912000[i+8,3]){
  top19912000[i+8,4]<-top19912000[i,12]
}else{top19912000[i+8,4]<-top19912000[i+8,4]}}



top19912000<-top19912000[,c(2,3,4)]



top19912000$authors<-str_replace(string = top19912000$authors, pattern = "\\,$", replacement = "")
top19912000$authors<-str_replace(string = top19912000$authors, pattern = "\\.$", replacement = "")

top19912000$authors<-iconv(top19912000$authors,from="UTF-8",to="ASCII//TRANSLIT")

##### On passe les noms en majuscule

top19912000$authors<-toupper(top19912000$authors)

top19912000$authors<-trimws(top19912000$authors)



###### Same for the third database


top20012010 <- na.omit(top20012010)

top20012010 <- top20012010[rep(row.names(top20012010), ifelse(top20012010$authors9!="AB",9,ifelse(top20012010$authors8!="AB",8,ifelse(top20012010$authors7!="AB",7,ifelse(top20012010$authors6!="AB",6,ifelse(top20012010$authors5!="AB",5,ifelse(top20012010$authors4!="AB",4,ifelse(top20012010$authors3!="AB",3,ifelse(top20012010$authors2!="AB",2,1))))))))),]



for(i in 1:(nrow(top20012010)-1)){if(top20012010[i,3]==top20012010[i+1,3]){
  top20012010[i+1,4]<-top20012010[i+1,5]
}else{top20012010[i+1,4]<-top20012010[i+1,4]}}


for(i in 1:(nrow(top20012010)-2)){if(top20012010[i,3]==top20012010[i+2,3]){
  top20012010[i+2,4]<-top20012010[i,6]
}else{top20012010[i+2,4]<-top20012010[i+2,4]}}

for(i in 1:(nrow(top20012010)-3)){if(top20012010[i,3]==top20012010[i+3,3]){
  top20012010[i+3,4]<-top20012010[i,7]
}else{top20012010[i+3,4]<-top20012010[i+3,4]}}


for(i in 1:(nrow(top20012010)-4)){if(top20012010[i,3]==top20012010[i+4,3]){
  top20012010[i+4,4]<-top20012010[i,8]
}else{top20012010[i+4,4]<-top20012010[i+4,4]}}


for(i in 1:(nrow(top20012010)-5)){if(top20012010[i,3]==top20012010[i+5,3]){
  top20012010[i+5,4]<-top20012010[i,9]
}else{top20012010[i+5,4]<-top20012010[i+5,4]}}


for(i in 1:(nrow(top20012010)-6)){if(top20012010[i,3]==top20012010[i+6,3]){
  top20012010[i+6,4]<-top20012010[i,10]
}else{top20012010[i+6,4]<-top20012010[i+6,4]}}



for(i in 1:(nrow(top20012010)-7)){if(top20012010[i,3]==top20012010[i+7,3]){
  top20012010[i+7,4]<-top20012010[i,11]
}else{top20012010[i+7,4]<-top20012010[i+7,4]}}


for(i in 1:(nrow(top20012010)-8)){if(top20012010[i,3]==top20012010[i+8,3]){
  top20012010[i+8,4]<-top20012010[i,12]
}else{top20012010[i+8,4]<-top20012010[i+8,4]}}



top20012010<-top20012010[,c(2,3,4)]



top20012010$authors<-str_replace(string = top20012010$authors, pattern = "\\,$", replacement = "")
top20012010$authors<-str_replace(string = top20012010$authors, pattern = "\\.$", replacement = "")

top20012010$authors<-iconv(top20012010$authors,from="UTF-8",to="ASCII//TRANSLIT")

##### On passe les noms en majuscule

top20012010$authors<-toupper(top20012010$authors)

top20012010$authors<-trimws(top20012010$authors)




###### Same for the fourth database




top20112020 <- na.omit(top20112020)

top20112020 <- top20112020[rep(row.names(top20112020), ifelse(top20112020$authors9!="AB",9,ifelse(top20112020$authors8!="AB",8,ifelse(top20112020$authors7!="AB",7,ifelse(top20112020$authors6!="AB",6,ifelse(top20112020$authors5!="AB",5,ifelse(top20112020$authors4!="AB",4,ifelse(top20112020$authors3!="AB",3,ifelse(top20112020$authors2!="AB",2,1))))))))),]



for(i in 1:(nrow(top20112020)-1)){if(top20112020[i,3]==top20112020[i+1,3]){
  top20112020[i+1,4]<-top20112020[i+1,5]
}else{top20112020[i+1,4]<-top20112020[i+1,4]}}


for(i in 1:(nrow(top20112020)-2)){if(top20112020[i,3]==top20112020[i+2,3]){
  top20112020[i+2,4]<-top20112020[i,6]
}else{top20112020[i+2,4]<-top20112020[i+2,4]}}

for(i in 1:(nrow(top20112020)-3)){if(top20112020[i,3]==top20112020[i+3,3]){
  top20112020[i+3,4]<-top20112020[i,7]
}else{top20112020[i+3,4]<-top20112020[i+3,4]}}


for(i in 1:(nrow(top20112020)-4)){if(top20112020[i,3]==top20112020[i+4,3]){
  top20112020[i+4,4]<-top20112020[i,8]
}else{top20112020[i+4,4]<-top20112020[i+4,4]}}


for(i in 1:(nrow(top20112020)-5)){if(top20112020[i,3]==top20112020[i+5,3]){
  top20112020[i+5,4]<-top20112020[i,9]
}else{top20112020[i+5,4]<-top20112020[i+5,4]}}


for(i in 1:(nrow(top20112020)-6)){if(top20112020[i,3]==top20112020[i+6,3]){
  top20112020[i+6,4]<-top20112020[i,10]
}else{top20112020[i+6,4]<-top20112020[i+6,4]}}



for(i in 1:(nrow(top20112020)-7)){if(top20112020[i,3]==top20112020[i+7,3]){
  top20112020[i+7,4]<-top20112020[i,11]
}else{top20112020[i+7,4]<-top20112020[i+7,4]}}


for(i in 1:(nrow(top20112020)-8)){if(top20112020[i,3]==top20112020[i+8,3]){
  top20112020[i+8,4]<-top20112020[i,12]
}else{top20112020[i+8,4]<-top20112020[i+8,4]}}



top20112020<-top20112020[,c(2,3,4)]



top20112020$authors<-str_replace(string = top20112020$authors, pattern = "\\,$", replacement = "")
top20112020$authors<-str_replace(string = top20112020$authors, pattern = "\\.$", replacement = "")

top20112020$authors<-iconv(top20112020$authors,from="UTF-8",to="ASCII//TRANSLIT")

##### On passe les noms en majuscule

top20112020$authors<-toupper(top20112020$authors)

top20112020$authors<-trimws(top20112020$authors)



#### We convert tot he adequate format


freqauth119811990<-table(top19811990$authors)
freqauth119811990<-as.data.frame(freqauth119811990)

freqauth119912000<-table(top19912000$authors)
freqauth119912000<-as.data.frame(freqauth119912000)

freqauth120012010<-table(top20012010$authors)
freqauth120012010<-as.data.frame(freqauth120012010)

freqauth120112020<-table(top20112020$authors)
freqauth120112020<-as.data.frame(freqauth120112020)


library(questionr)



###### We create a weighted datable (in other words we count how often "Rosen" is quoted, weighted with the number of 
###### citation for each occurrence)

freqauth19811990<-wtd.table(x = top19811990$authors, weights = top19811990$Freq)
freqauth19811990<-as.data.frame(freqauth19811990)

freqauth19912000<-wtd.table(x = top19912000$authors, weights = top19912000$Freq)
freqauth19912000<-as.data.frame(freqauth19912000)

freqauth20012010<-wtd.table(x = top20012010$authors, weights = top20012010$Freq)
freqauth20012010<-as.data.frame(freqauth20012010)

freqauth20112020<-wtd.table(x = top20112020$authors, weights = top20112020$Freq)
freqauth20112020<-as.data.frame(freqauth20112020)



###### We merge the table

auth19811990<-merge(freqauth119811990,freqauth19811990, by="Var1")
auth19912000<-merge(freqauth119912000,freqauth19912000, by="Var1")
auth20012010<-merge(freqauth120012010,freqauth20012010, by="Var1")
auth20112020<-merge(freqauth120112020,freqauth20112020, by="Var1")


###### We change the name of the columns

names(auth19811990)[names(auth19811990) == "Var1"] <- "Name"
names(auth19811990)[names(auth19811990) == "Freq.x"] <- "Number of Ocurrences"
names(auth19811990)[names(auth19811990) == "Freq.y"] <- "Items"


names(auth19912000)[names(auth19912000) == "Var1"] <- "Name"
names(auth19912000)[names(auth19912000) == "Freq.x"] <- "Number of Ocurrences"
names(auth19912000)[names(auth19912000) == "Freq.y"] <- "Items"


names(auth20012010)[names(auth20012010) == "Var1"] <- "Name"
names(auth20012010)[names(auth20012010) == "Freq.x"] <- "Number of Ocurrences"
names(auth20012010)[names(auth20012010) == "Freq.y"] <- "Items"


names(auth20112020)[names(auth20112020) == "Var1"] <- "Name"
names(auth20112020)[names(auth20112020) == "Freq.x"] <- "Number of Ocurrences"
names(auth20112020)[names(auth20112020) == "Freq.y"] <- "Items"


###### We rank according to a decreasing order

auth19811990<-auth19811990[rev(order(auth19811990$`Number of Ocurrences`)),]
auth19912000<-auth19912000[rev(order(auth19912000$`Number of Ocurrences`)),]
auth20012010<-auth20012010[rev(order(auth20012010$`Number of Ocurrences`)),]
auth20112020<-auth20112020[rev(order(auth20112020$`Number of Ocurrences`)),]


auth19811990$intrus<-auth19811990$Name=="AB"
auth19912000$intrus<-auth19912000$Name=="AB"
auth20012010$intrus<-auth20012010$Name=="AB"
auth20112020$intrus<-auth20112020$Name=="AB"


###### We select how much authors we keep for each decade

auth19811990<-auth19811990[auth19811990$intrus==F,]
auth19811990<-auth19811990[1:100,]
auth19811990<-auth19811990[,c(1:3)]


auth19912000<-auth19912000[auth19912000$intrus==F,]
auth19912000<-auth19912000[1:100,]
auth19912000<-auth19912000[,c(1:3)]


auth20012010<-auth20012010[auth20012010$intrus==F,]
auth20012010<-auth20012010[1:100,]
auth20012010<-auth20012010[,c(1:3)]


auth20112020<-auth20112020[auth20112020$intrus==F,]
auth20112020<-auth20112020[1:100,]
auth20112020<-auth20112020[,c(1:3)]


##### We indicate the decade

auth19811990$YEAR<-"1981-1990"
auth19912000$YEAR<-"1991-2000"
auth20012010$YEAR<-"2001-2010"
auth20112020$YEAR<-"2011-2020"


auth19811990$`Number of Ocurrences`<-1:nrow(auth19811990)
auth19912000$`Number of Ocurrences`<-1:nrow(auth19912000)
auth20012010$`Number of Ocurrences`<-1:nrow(auth20012010)
auth20112020$`Number of Ocurrences`<-1:nrow(auth20112020)


### WE keep the adequate column

auth19811990<-auth19811990[,c(1)]
auth19912000<-auth19912000[,c(1)]
auth20012010<-auth20012010[,c(1)]
auth20112020<-auth20112020[,c(1)]


#### We convert to the good format

auth19811990<-as.data.frame(auth19811990)
auth19912000<-as.data.frame(auth19912000)
auth20012010<-as.data.frame(auth20012010)
auth20112020<-as.data.frame(auth20112020)


names(auth19811990)[names(auth19811990) == "auth19811990"] <- "1980-1989"
names(auth19912000)[names(auth19912000) == "auth19912000"] <- "1990-1999"
names(auth20012010)[names(auth20012010) == "auth20012010"] <- "2000-2009"
names(auth20112020)[names(auth20112020) == "auth20112020"] <- "2010-2019"


auth19811990$`1980-1989`<-as.character(auth19811990$`1980-1989`)
auth19912000$`1990-1999`<-as.character(auth19912000$`1990-1999`)
auth20012010$`2000-2009`<-as.character(auth20012010$`2000-2009`)
auth20112020$`2010-2019`<-as.character(auth20112020$`2010-2019`)




##### We create the global database


Rank<-1:nrow(auth19811990)

authy<-cbind(Rank,auth19811990,auth19912000,auth20012010,auth20112020)


##### We export the table on excel with the following function


cb <- function(df, sep="\t", dec=",", max.size=(200*10000)){
  # Copy a data.frame to clipboard
  write.table(df, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=FALSE, dec=dec)
}

cb(authy)


#############################################################################################################################
#############################################################################################################################
# We export the result on excel, aadd the classification of authors, and import again the data (see end of the script)
#############################################################################################################################
#############################################################################################################################


##### We apply the same commands (just in case we want to differentiate the number of authors for the bumpchart and for the exported table)

auth19811990<-merge(freqauth119811990,freqauth19811990, by="Var1")
auth19912000<-merge(freqauth119912000,freqauth19912000, by="Var1")
auth20012010<-merge(freqauth120012010,freqauth20012010, by="Var1")
auth20112020<-merge(freqauth120112020,freqauth20112020, by="Var1")



names(auth19811990)[names(auth19811990) == "Var1"] <- "Name"
names(auth19811990)[names(auth19811990) == "Freq.x"] <- "Number of Ocurrences"
names(auth19811990)[names(auth19811990) == "Freq.y"] <- "Items"


names(auth19912000)[names(auth19912000) == "Var1"] <- "Name"
names(auth19912000)[names(auth19912000) == "Freq.x"] <- "Number of Ocurrences"
names(auth19912000)[names(auth19912000) == "Freq.y"] <- "Items"


names(auth20012010)[names(auth20012010) == "Var1"] <- "Name"
names(auth20012010)[names(auth20012010) == "Freq.x"] <- "Number of Ocurrences"
names(auth20012010)[names(auth20012010) == "Freq.y"] <- "Items"


names(auth20112020)[names(auth20112020) == "Var1"] <- "Name"
names(auth20112020)[names(auth20112020) == "Freq.x"] <- "Number of Ocurrences"
names(auth20112020)[names(auth20112020) == "Freq.y"] <- "Items"



auth19811990<-auth19811990[rev(order(auth19811990$`Number of Ocurrences`)),]
auth19912000<-auth19912000[rev(order(auth19912000$`Number of Ocurrences`)),]
auth20012010<-auth20012010[rev(order(auth20012010$`Number of Ocurrences`)),]
auth20112020<-auth20112020[rev(order(auth20112020$`Number of Ocurrences`)),]


auth19811990$intrus<-auth19811990$Name=="AB"
auth19912000$intrus<-auth19912000$Name=="AB"
auth20012010$intrus<-auth20012010$Name=="AB"
auth20112020$intrus<-auth20112020$Name=="AB"



auth19811990<-auth19811990[auth19811990$intrus==F,]
auth19811990<-auth19811990[1:50,]
auth19811990<-auth19811990[,c(1:3)]


auth19912000<-auth19912000[auth19912000$intrus==F,]
auth19912000<-auth19912000[1:50,]
auth19912000<-auth19912000[,c(1:3)]


auth20012010<-auth20012010[auth20012010$intrus==F,]
auth20012010<-auth20012010[1:50,]
auth20012010<-auth20012010[,c(1:3)]


auth20112020<-auth20112020[auth20112020$intrus==F,]
auth20112020<-auth20112020[1:50,]
auth20112020<-auth20112020[,c(1:3)]


auth19811990$YEAR<-"1981-1990"
auth19912000$YEAR<-"1991-2000"
auth20012010$YEAR<-"2001-2010"
auth20112020$YEAR<-"2011-2020"


auth19811990$`Number of Ocurrences`<-1:nrow(auth19811990)
auth19912000$`Number of Ocurrences`<-1:nrow(auth19912000)
auth20012010$`Number of Ocurrences`<-1:nrow(auth20012010)
auth20112020$`Number of Ocurrences`<-1:nrow(auth20112020)




auth<-rbind(auth19811990,auth19912000,auth20012010,auth20112020)


library(ggplot2)
library(dplyr)



####### We introduce the theme for the bumpchart


my_theme <- function() {
  
  # Colors
  color.background = "white"
  color.text = "#22211d"
  
  # Begin construction of chart
  theme_bw(base_size=15) +
    
    # Format background colors
    theme(panel.background = element_rect(fill=color.background,
                                          color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background,
                                          color=color.background)) +
    theme(panel.border     = element_rect(color=color.background)) +
    theme(strip.background = element_rect(fill=color.background,
                                          color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.ticks       = element_blank()) +
    
    # Format the legend
    theme(legend.position = "none") +
    
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=5, face = "bold")) +
    theme(axis.title.x     = element_text(size=0, color="black", face = "bold")) +
    theme(axis.title.y     = element_text(size=0, color="black", face = "bold",
                                          vjust=1.25)) +
    theme(axis.text.x      = element_text(size=0, vjust=0.5, hjust=0.5,
                                          color = color.text)) +
    ### Taille des nombre
    theme(axis.text.y      = element_text(size=2.5, color = color.text)) +
    theme(strip.text       = element_text(face = "bold")) +
    
    # Plot margins
    # Contour du graphique
    theme(plot.margin = unit(c(0, 0.2, 0.3, 0), "cm"))
}




library(ggiraph)


# We create the graph


gg_point=ggplot(auth,aes(x = as.factor(YEAR), y = `Number of Ocurrences`, group = Name)) +
  # Taille des lignes
  geom_line_interactive(aes(color = Name, alpha = 1,tooltip=Name, data_id = Name), size = 0.3) +
  # Taille des points
  geom_point_interactive(aes(x = as.factor(YEAR), y = `Number of Ocurrences`, group = Name, color = Name, alpha = 1,tooltip = Name, data_id = Name), size = 1) +
  geom_point_interactive(color = "#FFFFFF", size = 0.01) +
  scale_y_reverse(breaks = 1:table(auth$YEAR)[[1]]) + 
  scale_x_discrete(breaks = 1:length(table(auth$YEAR))) +
  theme(legend.position = 'none') +
  # le x designe le positionnement des auteurs à gauche
  geom_text(data = auth %>% filter(YEAR == "1981-1990"),
            aes(label = Name, x = 0.42) , hjust = 0.0,
            fontface = "bold", color = "#888888", size = 0.7) +
  # idem mais à droite
  geom_text(data = auth %>% filter(YEAR == "2011-2020"),
            aes(label = Name, x = 4.5) , hjust = 1.0,
            fontface = "bold", color = "#888888", size = 0.7) +
  labs(x = '', y = '', title = 'Most quoted authors (1980-1989, 1990-1999, 2000-2009, 2010-2019)') +
  my_theme() 


#### We represent the graph

girafe(ggobj = gg_point, width_svg = 3, height_svg = 2,
       options = list(
         opts_hover_inv(css = "opacity:0.1;"),
         opts_hover(css = "stroke-width:2;")
       ))



##################################################################################################################
##################################################################################################################
# We import the authors after the categorization 
##################################################################################################################
##################################################################################################################


library(readr)
library(stringr)
library(tidyverse)
library(biblionetwork)


gafam <- read_delim("Data_11_authors.txt", "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

cb <- function(df, sep="\t", dec=",", max.size=(200*1000)){
  # Copy a data.frame to clipboard
  write.table(df, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=FALSE, dec=dec)
}

cb(table(gafam$X2))
cb(table(gafam$X4))
cb(table(gafam$X6))











