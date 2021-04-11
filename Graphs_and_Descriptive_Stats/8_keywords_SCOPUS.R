library(readr)
library(stringr)
library(tidyverse)
library(biblionetwork)



##### Set working directory

setwd("C:/Users/jgrad/Desktop/Network_Quality/Network_Quality/Data")

##### Import file used

zal <- read_delim("Data_3_Core.txt", "\t", escape_double = FALSE, trim_ws = TRUE)


##################################################################################################################
##################################################################################################################
# We extract the keywords like we have extracted the abstracts previously, so we have the same script as before, 
# and we don't detail
##################################################################################################################
##################################################################################################################



zal[,2]<-3
for(i in 1:nrow(zal)){
  if(str_sub (zal[i,1], 1,11)=="REFERENCES:"){zal[i,2]<-T}else{zal[i,2]<-F}
}



table(zal[,2],useNA="always")



#### Maintenant, on identifie les balises qui suivent généralement la bibliographie 
#### ("CORRESPONDENCE", "ISSN"...)
#### Du coup, on a identifié la borne inférieure et supérieure de la bibliographie des articles

zal[,3]<-3
for(i in 1:nrow(zal)){
  if(str_sub (zal[i,1], 1,14)=="CORRESPONDENCE"|str_sub (zal[i,1], 1,5)=="ISSN:"|str_sub (zal[i,1], 1,10)=="PUBLISHER:"){zal[i,3]<-T}else{zal[i,3]<-F}
}


################################################################################
# On fait une boucle qui va permettre d'identifier les références biblios de chaque article
# Pour ce faire, l'idée de la boucle est la suivante: "dès que tu détectes une borne inférieure,
# tu commences à marquer les lignes jusqu'à ce que tu rencontres une borne supérieure, puis tu 
# recommences quand tu tombes sur la borne inférieure d'après, etc...". 
# Du coup, ça identifie les références bibliographiques, et donne un identifiant unique
# aux articles citant (d'où le fait que deux colonnes sont créées)
################################################################################


zal[,4]<-4
zal[,5]<-5
k<-0


for(i in 1:nrow(zal)){
  
  j<-0
  k<-k+1
  
  if(zal[i,2]==1){while(zal[i+j,3]==0){
    
    zal[i+j,4]<-1
    
    zal[i+j,5]<-k
    
    j<-j+1
    
  }
    
    
  }else{zal[i,4]<-zal[i,4]}
}




###################################################################################################
###################################################################################################
##### Constitution des infos sur les noeuds -----------------------------------------
###################################################################################################
###################################################################################################

###### On créé la base sur laquelle on va travailler

zaler<-zal


##### On enregistre les identifiants uniques des articles citant


ids<-unique(zaler$...5)
ids<-as.data.frame(ids)

##### On remplace le 5 (valeur par défaut de la boucle précédente) par un 1

ids[1,]<-1


##### On identifie les lignes où les biblios cessent (ce qui permettra de supprimer les refs biblios ensuite)

zaler$v1<-6
for(i in 1:(nrow(zaler)-1)){if(zaler[i,4]==1 & zaler[i+1,4]==4){zaler[i+1,6]<-T}else{zaler[i+1,6]<-F}}


ids<-ids[-1,]


##### On detecte la présence de la balise SOURCE, qui signifie la fin des articles

zaler$Cace<-str_detect (string =zaler$V1, pattern = "SOURCE: ")


##### On extrait la position des lignes où on passe à l'article suivant (on définit ainsi un intervalle pour chaque
##### article, ce qui evitera de dépasser dans une des boucles ultérieure)
#### Rôle central de la fonction which dans ce contexte

Cave2<-which(zaler[,7]==T)
Cave2<-as.data.frame(Cave2)
Cave2<-rbind(1,Cave2)


##### Boucle qui permet d'associer l'identifiant unique à toutes les infos de l'article
##### (avant il n'était associé qu'aux références biblios)


j<-0
zaler$Cartel<-16790
for(i in 1:(nrow(Cave2)-1)){
  if(is.element(1,zaler$...4[Cave2[i,]:Cave2[i+1,]]))
  {j<-j+1
  zaler$Cartel[Cave2[i,]:Cave2[i+1,]]<-ids[j]}else{zaler$Cartel[Cave2[i,]:Cave2[i+1,]]<-18888}}



zalerit<-zaler



zalerit$topor<-3
for(i in 1:nrow(zalerit)){
  if(str_sub (zalerit[i,1], 1,15)=="INDEX KEYWORDS:"){zalerit$topor[i]<-T}else{zalerit$topor[i]<-F}
}


zalerit<-zalerit[zalerit$topor==1,]
zalerit<-zalerit[,c(1,8)]
zalerit$V1<-str_replace_all(string = zalerit$V1, pattern = "INDEX KEYWORDS:", replacement = "")


#save(zalerit, file = "Data_9_keywords.rda")



##########################################################################################
##########################################################################################
# We analyze the keywords
##########################################################################################
##########################################################################################


setwd("C:/Users/jgrad/Desktop/Network_Quality/Network_Quality/Data")


load("Data_5_nodes.rda")

load("Data_9_keywords.rda")


library(readr)
library(stringr)
library(tidyverse)
library(biblionetwork)


##### We remove nodes with NA

zalerit<-zalerit[zalerit$Cartel!=18888,]

zalerit<-zalerit[zalerit$Cartel!=13119,]

zalerit<-zalerit[zalerit$Cartel!=131191,]

zalerit<-zalerit[zalerit$Cartel!=148413,]

zalerit<-zalerit[zalerit$Cartel!=148866,]

zalerit<-zalerit[zalerit$Cartel!=151086,]


#### We remove parentheses of publication year

desco$Datepubli<-str_replace_all(string = desco$Datepubli, pattern = "\\(", replacement = "")
desco$Datepubli<-str_replace_all(string = desco$Datepubli, pattern = "\\)", replacement = "")
desco$Datepubli<-as.numeric(desco$Datepubli)


##### We associate keywords of article with their year of publication

zalerit$anneecitant<-0
for(i in 1:nrow(zalerit)){
  j<-which(zalerit$Cartel[i] == desco$Cartel)
  zalerit$anneecitant[i] <- desco$Datepubli[j]}


##### We split according to the decades

freq19811990<-zalerit[zalerit$anneecitant==1980|zalerit$anneecitant==1981|zalerit$anneecitant==1982|zalerit$anneecitant==1983|zalerit$anneecitant==1984|zalerit$anneecitant==1985|zalerit$anneecitant==1986|zalerit$anneecitant==1987|zalerit$anneecitant==1988|zalerit$anneecitant==1989,]
freq19912000<-zalerit[zalerit$anneecitant==1990|zalerit$anneecitant==1991|zalerit$anneecitant==1992|zalerit$anneecitant==1993|zalerit$anneecitant==1994|zalerit$anneecitant==1995|zalerit$anneecitant==1996|zalerit$anneecitant==1997|zalerit$anneecitant==1998|zalerit$anneecitant==1999,]
freq20012010<-zalerit[zalerit$anneecitant==2000|zalerit$anneecitant==2001|zalerit$anneecitant==2002|zalerit$anneecitant==2003|zalerit$anneecitant==2004|zalerit$anneecitant==2005|zalerit$anneecitant==2006|zalerit$anneecitant==2007|zalerit$anneecitant==2008|zalerit$anneecitant==2009,]
freq20112020<-zalerit[zalerit$anneecitant==2010|zalerit$anneecitant==2011|zalerit$anneecitant==2012|zalerit$anneecitant==2013|zalerit$anneecitant==2014|zalerit$anneecitant==2015|zalerit$anneecitant==2016|zalerit$anneecitant==2017|zalerit$anneecitant==2018|zalerit$anneecitant==2019,]


###########################################################################################################################
###########################################################################################################################
# treatment of the keywords of the first decade
###########################################################################################################################
###########################################################################################################################

###### We replace coma with dot coma

freq19912000$V1<-paste(freq19912000$V1,";")
freq19912000$V1<-gsub(",", "\\;",freq19912000$V1)


##### We paste all the keywords of the decade on the same line

freq19912000$comp<-"AB"
freq19912000$comp[1]<-freq19912000$V1[1]
for(i in 1:(nrow(freq19912000)-1)){
  freq19912000$comp[i+1]<-paste(freq19912000$comp[i],freq19912000$V1[i+1])
}

##### We keep the string with all the keywords

yol<-freq19912000$comp[nrow(freq19912000)]


###### We separate them with the ; separator

yolo <- unlist(strsplit(yol, ";"))


###### We standardize the keywords


yolo<-iconv(yolo,from="UTF-8",to="ASCII//TRANSLIT")

##### We capitalize the words

yolo<-toupper(yolo)

yolo<-trimws(yolo)

yolo<-table(yolo)

yolo<-as.data.frame(yolo)



###############################################################################
###############################################################################
# Second database
###############################################################################
###############################################################################

freq20012010$V1<-paste(freq20012010$V1,";")
freq20012010$V1<-gsub(",", "\\;",freq20012010$V1)


freq20012010$comp<-"AB"
freq20012010$comp[1]<-freq20012010$V1[1]
for(i in 1:(nrow(freq20012010)-1)){
  freq20012010$comp[i+1]<-paste(freq20012010$comp[i],freq20012010$V1[i+1])
}


yol<-freq20012010$comp[nrow(freq20012010)]


yolo2 <- unlist(strsplit(yol, ";"))


yolo2<-iconv(yolo2,from="UTF-8",to="ASCII//TRANSLIT")

##### On passe les noms en majuscule

yolo2<-toupper(yolo2)

yolo2<-trimws(yolo2)

yolo2<-table(yolo2)

yolo2<-as.data.frame(yolo2)


####################################################################################################
####################################################################################################
##### Third database
####################################################################################################
####################################################################################################

freq20112020$V1<-paste(freq20112020$V1,";")
freq20112020$V1<-gsub(",", "\\;",freq20112020$V1)


freq20112020$comp<-"AB"
freq20112020$comp[1]<-freq20112020$V1[1]
for(i in 1:(nrow(freq20112020)-1)){
  freq20112020$comp[i+1]<-paste(freq20112020$comp[i],freq20112020$V1[i+1])
}


yol<-freq20112020$comp[nrow(freq20112020)]


yolo3 <- unlist(strsplit(yol, ";"))


yolo3<-iconv(yolo3,from="UTF-8",to="ASCII//TRANSLIT")

##### On passe les noms en majuscule

yolo3<-toupper(yolo3)

yolo3<-trimws(yolo3)

yolo3<-table(yolo3)

yolo3<-as.data.frame(yolo3)



### We rank the keywords according to their frequency


yolo<-yolo[order(-yolo$Freq),]
yolo2<-yolo2[order(-yolo2$Freq),]
yolo3<-yolo3[order(-yolo3$Freq),]



##### We extract the dataframe to Excel

cb <- function(df, sep="\t", dec=",", max.size=(200*1000)){
  # Copy a data.frame to clipboard
  write.table(df, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=FALSE, dec=dec)
}

cb(yolo)
cb(yolo2)
cb(yolo3)



