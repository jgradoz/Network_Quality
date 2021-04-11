

setwd("C:/Users/jgrad/Desktop/Network_Quality/Network_Quality/Data")


load("Data_7_finalbase.rda")


#####################################################################################################################
#####################################################################################################################
# Last errros observed by exploring the base-------------------
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


# We compute the frequency of each cited article

freq<-table(eoeoe$identifiant)
freq<-as.data.frame(freq)

# On stocke la fréquence comme nouvelle variable, ce qui nous servira à renseigner la taille des noeuds dans les graphes

eoeoe$frequence<-0
for(i in 1:nrow(eoeoe)){
  j<-which(eoeoe$identifiant[i] == freq$Var1)
  eoeoe$frequence[i]<-freq$Freq[j]
}


### We lord the databse on nodes


load("Data_5_nodes.rda")


### We copy the year of publication of each node in our general database

desco$Datepubli<-as.character(desco$Datepubli)
eoeoe$anneecitant<-0
for(i in 1:nrow(eoeoe)){
  j<-which(eoeoe$V2[i] == desco$Cartel)
  eoeoe$anneecitant[i] <- desco$Datepubli[j]
}


library(readr)
library(stringr)


#### We remove parentheses around the years 

eoeoe$anneecitant<-str_replace_all(string = eoeoe$anneecitant, pattern = "\\(", replacement = "")
eoeoe$anneecitant<-str_replace_all(string = eoeoe$anneecitant, pattern = "\\)", replacement = "")
eoeoe$anneecitant<-as.numeric(eoeoe$anneecitant)

##### We split the databse according to the publication year of the citing article

freq19811990<-eoeoe[eoeoe$anneecitant==1980|eoeoe$anneecitant==1981|eoeoe$anneecitant==1982|eoeoe$anneecitant==1983|eoeoe$anneecitant==1984|eoeoe$anneecitant==1985|eoeoe$anneecitant==1986|eoeoe$anneecitant==1987|eoeoe$anneecitant==1988|eoeoe$anneecitant==1989,]
freq19912000<-eoeoe[eoeoe$anneecitant==1990|eoeoe$anneecitant==1991|eoeoe$anneecitant==1992|eoeoe$anneecitant==1993|eoeoe$anneecitant==1994|eoeoe$anneecitant==1995|eoeoe$anneecitant==1996|eoeoe$anneecitant==1997|eoeoe$anneecitant==1998|eoeoe$anneecitant==1999,]
freq20012010<-eoeoe[eoeoe$anneecitant==2000|eoeoe$anneecitant==2001|eoeoe$anneecitant==2002|eoeoe$anneecitant==2003|eoeoe$anneecitant==2004|eoeoe$anneecitant==2005|eoeoe$anneecitant==2006|eoeoe$anneecitant==2007|eoeoe$anneecitant==2008|eoeoe$anneecitant==2009,]
freq20112020<-eoeoe[eoeoe$anneecitant==2010|eoeoe$anneecitant==2011|eoeoe$anneecitant==2012|eoeoe$anneecitant==2013|eoeoe$anneecitant==2014|eoeoe$anneecitant==2015|eoeoe$anneecitant==2016|eoeoe$anneecitant==2017|eoeoe$anneecitant==2018|eoeoe$anneecitant==2019,]

##### We convert in the appropriate format

freq19811990<-table(freq19811990$identifiant)
freq19811990<-as.data.frame(freq19811990)

freq19912000<-table(freq19912000$identifiant)
freq19912000<-as.data.frame(freq19912000)

freq20012010<-table(freq20012010$identifiant)
freq20012010<-as.data.frame(freq20012010)

freq20112020<-table(freq20112020$identifiant)
freq20112020<-as.data.frame(freq20112020)


##### We rank each base according to frequency

top19811990<-freq19811990[rev(order(freq19811990$Freq)),]
top19912000<-freq19912000[rev(order(freq19912000$Freq)),]
top20012010<-freq20012010[rev(order(freq20012010$Freq)),]
top20112020<-freq20112020[rev(order(freq20112020$Freq)),]


###### We keep all the observations

top19811990<-top19811990[1:nrow(top19811990),]
top19912000<-top19912000[1:nrow(top19912000),]
top20012010<-top20012010[1:nrow(top20012010),]
top20112020<-top20112020[1:nrow(top20112020),]


###### We change the name of rows to obtein a rank

row.names(top19811990)<-1:nrow(top19811990)
row.names(top19912000)<-1:nrow(top19912000)
row.names(top20012010)<-1:nrow(top20012010)
row.names(top20112020)<-1:nrow(top20112020)

###### Finally, we keep only the 25 most quoted references

top19811990<-top19811990[1:1000,]
top19912000<-top19912000[1:8000,]
top20012010<-top20012010[1:20000,]
top20112020<-top20112020[1:15000,]

####### We replace the id by the complete ref. To do so, among all the complete ref with the ID, we take the first one 
###### for which the first author extracted, the issue, the page and the year are corresponding to the ID (because most
###### id have been obtained with the previous script, and complete ref have errors)

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


##### We keep only the column necessary

top19811990<-top19811990[,c(3)]
top19912000<-top19912000[,c(3)]
top20012010<-top20012010[,c(3)]
top20112020<-top20112020[,c(3)]


##### We convert to the appropriate format

top19811990<-as.data.frame(top19811990)
top19912000<-as.data.frame(top19912000)
top20012010<-as.data.frame(top20012010)
top20112020<-as.data.frame(top20112020)


##### We extract the year of the reference

top19811990$Datepubli<- str_extract_all(top19811990$top19811990, "\\((17|18|19|20)\\d{2}\\)")
top19811990$Datepubli<-as.character(top19811990$Datepubli)
top19811990$Datepubli[top19811990$Datepubli == "character(0)"] <- "AB"


top19912000$Datepubli<- str_extract_all(top19912000$top19912000, "\\((17|18|19|20)\\d{2}\\)")
top19912000$Datepubli<-as.character(top19912000$Datepubli)
top19912000$Datepubli[top19912000$Datepubli == "character(0)"] <- "AB"



top20012010$Datepubli<- str_extract_all(top20012010$top20012010, "\\((17|18|19|20)\\d{2}\\)")
top20012010$Datepubli<-as.character(top20012010$Datepubli)
top20012010$Datepubli[top20012010$Datepubli == "character(0)"] <- "AB"



top20112020$Datepubli<- str_extract_all(top20112020$top20112020, "\\((17|18|19|20)\\d{2}\\)")
top20112020$Datepubli<-as.character(top20112020$Datepubli)
top20112020$Datepubli[top20112020$Datepubli == "character(0)"] <- "AB"



##### We remove the parentheses

top19811990$Datepubli<-str_replace_all(string = top19811990$Datepubli, pattern = "\\(", replacement = "")
top19811990$Datepubli<-str_replace_all(string = top19811990$Datepubli, pattern = "\\)", replacement = "")
top19811990$Datepubli<-as.numeric(top19811990$Datepubli)

top19912000$Datepubli<-str_replace_all(string = top19912000$Datepubli, pattern = "\\(", replacement = "")
top19912000$Datepubli<-str_replace_all(string = top19912000$Datepubli, pattern = "\\)", replacement = "")
top19912000$Datepubli<-as.numeric(top19912000$Datepubli)

top20012010$Datepubli<-str_replace_all(string = top20012010$Datepubli, pattern = "\\(", replacement = "")
top20012010$Datepubli<-str_replace_all(string = top20012010$Datepubli, pattern = "\\)", replacement = "")
top20012010$Datepubli<-as.numeric(top20012010$Datepubli)

top20112020$Datepubli<-str_replace_all(string = top20112020$Datepubli, pattern = "\\(", replacement = "")
top20112020$Datepubli<-str_replace_all(string = top20112020$Datepubli, pattern = "\\)", replacement = "")
top20112020$Datepubli<-as.numeric(top20112020$Datepubli)



###### We recode the years according to the decades

top19811990$Datepubli[top19811990$Datepubli<1970]<-"Before 1970"
#top19811990$Datepubli[top19811990$Datepubli==1950|top19811990$Datepubli==1951|top19811990$Datepubli==1952|top19811990$Datepubli==1953|top19811990$Datepubli==1954|top19811990$Datepubli==1955|top19811990$Datepubli==1956|top19811990$Datepubli==1957|top19811990$Datepubli==1958|top19811990$Datepubli==1959]<-"1950-1959"
#top19811990$Datepubli[top19811990$Datepubli==1960|top19811990$Datepubli==1961|top19811990$Datepubli==1962|top19811990$Datepubli==1963|top19811990$Datepubli==1964|top19811990$Datepubli==1965|top19811990$Datepubli==1966|top19811990$Datepubli==1967|top19811990$Datepubli==1968|top19811990$Datepubli==1969]<-"1960-1969"
top19811990$Datepubli[top19811990$Datepubli==1970|top19811990$Datepubli==1971|top19811990$Datepubli==1972|top19811990$Datepubli==1973|top19811990$Datepubli==1974|top19811990$Datepubli==1975|top19811990$Datepubli==1976|top19811990$Datepubli==1977|top19811990$Datepubli==1978|top19811990$Datepubli==1979]<-"1970-1979"
top19811990$Datepubli[top19811990$Datepubli==1980|top19811990$Datepubli==1981|top19811990$Datepubli==1982|top19811990$Datepubli==1983|top19811990$Datepubli==1984|top19811990$Datepubli==1985|top19811990$Datepubli==1986|top19811990$Datepubli==1987|top19811990$Datepubli==1988|top19811990$Datepubli==1989]<-"1980-1989"
top19811990$Datepubli[top19811990$Datepubli==1990|top19811990$Datepubli==1991|top19811990$Datepubli==1992|top19811990$Datepubli==1993|top19811990$Datepubli==1994|top19811990$Datepubli==1995|top19811990$Datepubli==1996|top19811990$Datepubli==1997|top19811990$Datepubli==1998|top19811990$Datepubli==1999]<-"1990-1999"
top19811990$Datepubli[top19811990$Datepubli==2000|top19811990$Datepubli==2001|top19811990$Datepubli==2002|top19811990$Datepubli==2003|top19811990$Datepubli==2004|top19811990$Datepubli==2005|top19811990$Datepubli==2006|top19811990$Datepubli==2007|top19811990$Datepubli==2008|top19811990$Datepubli==2009]<-"2000-2009"
top19811990$Datepubli[top19811990$Datepubli==2010|top19811990$Datepubli==2011|top19811990$Datepubli==2012|top19811990$Datepubli==2013|top19811990$Datepubli==2014|top19811990$Datepubli==2015|top19811990$Datepubli==2016|top19811990$Datepubli==2017|top19811990$Datepubli==2018|top19811990$Datepubli==2019]<-"2010-2019"


top19912000$Datepubli[top19912000$Datepubli<1970]<-"Before 1970"
#top19912000$Datepubli[top19912000$Datepubli==1950|top19912000$Datepubli==1951|top19912000$Datepubli==1952|top19912000$Datepubli==1953|top19912000$Datepubli==1954|top19912000$Datepubli==1955|top19912000$Datepubli==1956|top19912000$Datepubli==1957|top19912000$Datepubli==1958|top19912000$Datepubli==1959]<-"1950-1959"
#top19912000$Datepubli[top19912000$Datepubli==1960|top19912000$Datepubli==1961|top19912000$Datepubli==1962|top19912000$Datepubli==1963|top19912000$Datepubli==1964|top19912000$Datepubli==1965|top19912000$Datepubli==1966|top19912000$Datepubli==1967|top19912000$Datepubli==1968|top19912000$Datepubli==1969]<-"1960-1969"
top19912000$Datepubli[top19912000$Datepubli==1970|top19912000$Datepubli==1971|top19912000$Datepubli==1972|top19912000$Datepubli==1973|top19912000$Datepubli==1974|top19912000$Datepubli==1975|top19912000$Datepubli==1976|top19912000$Datepubli==1977|top19912000$Datepubli==1978|top19912000$Datepubli==1979]<-"1970-1979"
top19912000$Datepubli[top19912000$Datepubli==1980|top19912000$Datepubli==1981|top19912000$Datepubli==1982|top19912000$Datepubli==1983|top19912000$Datepubli==1984|top19912000$Datepubli==1985|top19912000$Datepubli==1986|top19912000$Datepubli==1987|top19912000$Datepubli==1988|top19912000$Datepubli==1989]<-"1980-1989"
top19912000$Datepubli[top19912000$Datepubli==1990|top19912000$Datepubli==1991|top19912000$Datepubli==1992|top19912000$Datepubli==1993|top19912000$Datepubli==1994|top19912000$Datepubli==1995|top19912000$Datepubli==1996|top19912000$Datepubli==1997|top19912000$Datepubli==1998|top19912000$Datepubli==1999]<-"1990-1999"
top19912000$Datepubli[top19912000$Datepubli==2000|top19912000$Datepubli==2001|top19912000$Datepubli==2002|top19912000$Datepubli==2003|top19912000$Datepubli==2004|top19912000$Datepubli==2005|top19912000$Datepubli==2006|top19912000$Datepubli==2007|top19912000$Datepubli==2008|top19912000$Datepubli==2009]<-"2000-2009"
top19912000$Datepubli[top19912000$Datepubli==2010|top19912000$Datepubli==2011|top19912000$Datepubli==2012|top19912000$Datepubli==2013|top19912000$Datepubli==2014|top19912000$Datepubli==2015|top19912000$Datepubli==2016|top19912000$Datepubli==2017|top19912000$Datepubli==2018|top19912000$Datepubli==2019]<-"2010-2019"


top20012010$Datepubli[top20012010$Datepubli<1970]<-"Before 1970"
#top20012010$Datepubli[top20012010$Datepubli==1950|top20012010$Datepubli==1951|top20012010$Datepubli==1952|top20012010$Datepubli==1953|top20012010$Datepubli==1954|top20012010$Datepubli==1955|top20012010$Datepubli==1956|top20012010$Datepubli==1957|top20012010$Datepubli==1958|top20012010$Datepubli==1959]<-"1950-1959"
#top20012010$Datepubli[top20012010$Datepubli==1960|top20012010$Datepubli==1961|top20012010$Datepubli==1962|top20012010$Datepubli==1963|top20012010$Datepubli==1964|top20012010$Datepubli==1965|top20012010$Datepubli==1966|top20012010$Datepubli==1967|top20012010$Datepubli==1968|top20012010$Datepubli==1969]<-"1960-1969"
top20012010$Datepubli[top20012010$Datepubli==1970|top20012010$Datepubli==1971|top20012010$Datepubli==1972|top20012010$Datepubli==1973|top20012010$Datepubli==1974|top20012010$Datepubli==1975|top20012010$Datepubli==1976|top20012010$Datepubli==1977|top20012010$Datepubli==1978|top20012010$Datepubli==1979]<-"1970-1979"
top20012010$Datepubli[top20012010$Datepubli==1980|top20012010$Datepubli==1981|top20012010$Datepubli==1982|top20012010$Datepubli==1983|top20012010$Datepubli==1984|top20012010$Datepubli==1985|top20012010$Datepubli==1986|top20012010$Datepubli==1987|top20012010$Datepubli==1988|top20012010$Datepubli==1989]<-"1980-1989"
top20012010$Datepubli[top20012010$Datepubli==1990|top20012010$Datepubli==1991|top20012010$Datepubli==1992|top20012010$Datepubli==1993|top20012010$Datepubli==1994|top20012010$Datepubli==1995|top20012010$Datepubli==1996|top20012010$Datepubli==1997|top20012010$Datepubli==1998|top20012010$Datepubli==1999]<-"1990-1999"
top20012010$Datepubli[top20012010$Datepubli==2000|top20012010$Datepubli==2001|top20012010$Datepubli==2002|top20012010$Datepubli==2003|top20012010$Datepubli==2004|top20012010$Datepubli==2005|top20012010$Datepubli==2006|top20012010$Datepubli==2007|top20012010$Datepubli==2008|top20012010$Datepubli==2009]<-"2000-2009"
top20012010$Datepubli[top20012010$Datepubli==2010|top20012010$Datepubli==2011|top20012010$Datepubli==2012|top20012010$Datepubli==2013|top20012010$Datepubli==2014|top20012010$Datepubli==2015|top20012010$Datepubli==2016|top20012010$Datepubli==2017|top20012010$Datepubli==2018|top20012010$Datepubli==2019]<-"2010-2019"


top20112020$Datepubli[top20112020$Datepubli<1970]<-"Before 1970"
#top20112020$Datepubli[top20112020$Datepubli==1950|top20112020$Datepubli==1951|top20112020$Datepubli==1952|top20112020$Datepubli==1953|top20112020$Datepubli==1954|top20112020$Datepubli==1955|top20112020$Datepubli==1956|top20112020$Datepubli==1957|top20112020$Datepubli==1958|top20112020$Datepubli==1959]<-"1950-1959"
#top20112020$Datepubli[top20112020$Datepubli==1960|top20112020$Datepubli==1961|top20112020$Datepubli==1962|top20112020$Datepubli==1963|top20112020$Datepubli==1964|top20112020$Datepubli==1965|top20112020$Datepubli==1966|top20112020$Datepubli==1967|top20112020$Datepubli==1968|top20112020$Datepubli==1969]<-"1960-1969"
top20112020$Datepubli[top20112020$Datepubli==1970|top20112020$Datepubli==1971|top20112020$Datepubli==1972|top20112020$Datepubli==1973|top20112020$Datepubli==1974|top20112020$Datepubli==1975|top20112020$Datepubli==1976|top20112020$Datepubli==1977|top20112020$Datepubli==1978|top20112020$Datepubli==1979]<-"1970-1979"
top20112020$Datepubli[top20112020$Datepubli==1980|top20112020$Datepubli==1981|top20112020$Datepubli==1982|top20112020$Datepubli==1983|top20112020$Datepubli==1984|top20112020$Datepubli==1985|top20112020$Datepubli==1986|top20112020$Datepubli==1987|top20112020$Datepubli==1988|top20112020$Datepubli==1989]<-"1980-1989"
top20112020$Datepubli[top20112020$Datepubli==1990|top20112020$Datepubli==1991|top20112020$Datepubli==1992|top20112020$Datepubli==1993|top20112020$Datepubli==1994|top20112020$Datepubli==1995|top20112020$Datepubli==1996|top20112020$Datepubli==1997|top20112020$Datepubli==1998|top20112020$Datepubli==1999]<-"1990-1999"
top20112020$Datepubli[top20112020$Datepubli==2000|top20112020$Datepubli==2001|top20112020$Datepubli==2002|top20112020$Datepubli==2003|top20112020$Datepubli==2004|top20112020$Datepubli==2005|top20112020$Datepubli==2006|top20112020$Datepubli==2007|top20112020$Datepubli==2008|top20112020$Datepubli==2009]<-"2000-2009"
top20112020$Datepubli[top20112020$Datepubli==2010|top20112020$Datepubli==2011|top20112020$Datepubli==2012|top20112020$Datepubli==2013|top20112020$Datepubli==2014|top20112020$Datepubli==2015|top20112020$Datepubli==2016|top20112020$Datepubli==2017|top20112020$Datepubli==2018|top20112020$Datepubli==2019]<-"2010-2019"


##### We create the tables

b19811990<-table(top19811990$Datepubli)
b19912000<-table(top19912000$Datepubli)
b20012010<-table(top20012010$Datepubli)
b20112020<-table(top20112020$Datepubli)


#### Function to copy to excel


cb <- function(df, sep="\t", dec=",", max.size=(200*1000)){
  # Copy a data.frame to clipboard
  write.table(df, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=FALSE, dec=dec)
}

##### Successive commmands to copy to excel

cb(b19811990)
cb(b19912000)
cb(b20012010)
cb(b20112020)



