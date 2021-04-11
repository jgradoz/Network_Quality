
setwd("C:/Users/jgrad/Desktop/Network_Quality/Network_Quality/Data")

load("Data_6_Core.rda")


library(readr)
library(stringr)
library(tidyverse)
library(biblionetwork)


############################################################################################################################
############################################################################################################################
# Première itération (seuil élevé) : OK
############################################################################################################################
############################################################################################################################


# On calcule la fréquence de chaque identifiant

freq<-table(eoeoe$identifiant)
freq<-as.data.frame(freq)

# On stocke la fréquence comme nouvelle variable, ce qui nous servira à renseigner la taille des noeuds dans les graphes

eoeoe$frequence<-0
for(i in 1:nrow(eoeoe)){
  j<-which(eoeoe$identifiant[i] == freq$Var1)
  eoeoe$frequence[i]<-freq$Freq[j]
}


# On trie alphabétiquement les identifiants dans une nouvelle base

eoeoe2<-eoeoe[order(eoeoe$identifiant),]


# Dans cette nouvelle base, si deux identifiant (i et i+1) ont : 
# le même auteur + la même date + même identifiant + (même issue OU même page)
# On les marque avec un nombre (en gros, sur les quatre critères qui forment l'identifiant, il faut que trois soient similaires,
# avec deux bloqués et un pouvant varier, dans la suite on altérnera les deux bloqués)

eoeoe2$kalro<-0
for(i in 2:(nrow(eoeoe2)-1)){if(eoeoe2$Firstauthor[i]==eoeoe2$Firstauthor[i+1] & eoeoe2$Datepubli[i]==eoeoe2$Datepubli[i+1] & ((eoeoe2$Issue[i]==eoeoe2$Issue[i+1] & eoeoe2$Page[i]!=eoeoe2$Page[i+1])|(eoeoe2$Issue[i]!=eoeoe2$Issue[i+1] & eoeoe2$Page[i]==eoeoe2$Page[i+1]))){
  eoeoe2$kalro[i]<-16546546546}else{
    eoeoe2$kalro[i]<-0}}

# On marque la ligne suivante pour pouvoir ensuite comparer i et i +1

for(i in 2:(nrow(eoeoe2)-1)){if(eoeoe2$kalro[i]==16546546546){
  eoeoe2$kalro[i+1]<-888}else{
    eoeoe2$kalro[i+1]<-eoeoe2$kalro[i+1]}}


# On ne garde que les i et les i+1

prto<-eoeoe2[eoeoe2$kalro==16546546546|eoeoe2$kalro==888,]
prto<-prto[,c(1,7,8,9)]

# On calcule la distance entre la ref biblio complète de i et de i+1

library(DescTools)

prto$Recl<-0
for(i in 1:(nrow(prto)-1)){
  prto$Recl[i]<-StrDist(prto$V1[i], prto$V1[i+1], method = "normlevenshtein", mismatch = 1, gap = 1, ignore.case = FALSE)
}


# Si la proximité est supérieur à 0.7 et que la fréquence des identifiants est différente, on associe i et i+1
# à l'identifiant le plus cité des deux dans une nouvelle colonne

prto$trop<-"AB"
for(i in 1:(nrow(prto)-1)){
  if(prto$Recl[i]>0.70){
    if(prto$frequence[i]!= prto$frequence[i+1]){
      j<-which.max(c(prto$frequence[i],prto$frequence[i+1]))
      prto$trop[i]<-prto$identifiant[i+j-1]
      prto$trop[i+1]<-prto$identifiant[i+j-1]}else{
        prto$trop[i]<-prto$trop[i]}}
  else{prto$trop[i]<-prto$trop[i]}}


## On regarde qualitativement s'il n'y a pas eu d'erreur, et au besoin on corrige


prto$trop[prto$identifiant=="BILS (2001) 91 274"]<-"BILS (2001) 91 274"
prto$trop[prto$identifiant=="NEWMAN (1988) 24 14"]<-"NEWMAN (1988) 24 14"
prto$trop[prto$identifiant=="POWELL (1992) 13 551"]<-"POWELL (1992) 13 551"


## Dans notre base générale, on remplace les i et les i+1 concernés par leur identifiant corrigé

for(i in 1:nrow(prto)){
  if(prto$identifiant[i]!=prto$trop[i] & prto$trop[i] !="AB"){
    eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]<- prto$trop[i]
  }else{eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]<-eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]}
}


############################################################################################################################
############################################################################################################################
# Deuxième itération (seuil moins élevé) : OK
############################################################################################################################
############################################################################################################################


# On calcule la fréquence de chaque identifiant

freq<-table(eoeoe$identifiant)
freq<-as.data.frame(freq)

# On stocke la fréquence comme nouvelle variable, ce qui nous servira à renseigner la taille des noeuds dans les graphes

eoeoe$frequence<-0
for(i in 1:nrow(eoeoe)){
  j<-which(eoeoe$identifiant[i] == freq$Var1)
  eoeoe$frequence[i]<-freq$Freq[j]
}


eoeoe2<-eoeoe[order(eoeoe$identifiant),]



eoeoe2$kalro<-0
for(i in 2:(nrow(eoeoe2)-1)){if(eoeoe2$Firstauthor[i]==eoeoe2$Firstauthor[i+1] & eoeoe2$Datepubli[i]==eoeoe2$Datepubli[i+1] & eoeoe2$identifiant[i]!= eoeoe2$identifiant[i+1] & ((eoeoe2$Issue[i]==eoeoe2$Issue[i+1] & eoeoe2$Page[i]!=eoeoe2$Page[i+1])|(eoeoe2$Issue[i]!=eoeoe2$Issue[i+1] & eoeoe2$Page[i]==eoeoe2$Page[i+1]))){
  eoeoe2$kalro[i]<-16546546546}else{
    eoeoe2$kalro[i]<-0}}


for(i in 2:(nrow(eoeoe2)-1)){if(eoeoe2$kalro[i]==16546546546){
  eoeoe2$kalro[i+1]<-888}else{
    eoeoe2$kalro[i+1]<-eoeoe2$kalro[i+1]}}


prto<-eoeoe2[eoeoe2$kalro==16546546546|eoeoe2$kalro==888,]
prto<-prto[,c(1,7,8,9)]

prto$Recl<-0
for(i in 1:(nrow(prto)-1)){
  prto$Recl[i]<-StrDist(prto$V1[i], prto$V1[i+1], method = "normlevenshtein", mismatch = 1, gap = 1, ignore.case = FALSE)
}


prto$trop<-"AB"
for(i in 1:(nrow(prto)-1)){
  if(prto$Recl[i]>0.6){
    if(prto$frequence[i]!= prto$frequence[i+1]){
      j<-which.max(c(prto$frequence[i],prto$frequence[i+1]))
      prto$trop[i]<-prto$identifiant[i+j-1]
      prto$trop[i+1]<-prto$identifiant[i+j-1]}else{
        prto$trop[i]<-prto$trop[i]}}
  else{prto$trop[i]<-prto$trop[i]}}



prto$trop[prto$identifiant=="BILS (2001) 91 274"]<-"BILS (2001) 91 274"
prto$trop[prto$identifiant=="CAPLIN (1991) 59 1"]<-"CAPLIN (1991) 59 1"
prto$trop[prto$identifiant=="CASWELL (1992) 74 1196"]<-"CASWELL (1992) 74 1196"
prto$trop[prto$identifiant=="ESKILDSEN (2000) 11 581"]<-"ESKILDSEN (2000) 11 581"
prto$trop[prto$identifiant=="FUCHSBERG (1992) 10 B1"]<-"FUCHSBERG (1992) 10 B1"
prto$trop[prto$identifiant=="HARARI (1993) 82 39"]<-"HARARI (1993) 82 39"
prto$trop[prto$identifiant=="JEPPERSON (1991) AB 204"]<-"JEPPERSON (1991) AB 204"
prto$trop[prto$identifiant=="JORESKOG (1971) 36 409"]<-"JORESKOG (1971) 36 409"
prto$trop[prto$identifiant=="KARAPETROVIC (1998) 15 694"]<-"KARAPETROVIC (1998) 15 694"
prto$trop[prto$identifiant=="KLEINDORFER (2005) 14 53"]<-"KLEINDORFER (2005) 14 53"
prto$trop[prto$identifiant=="LAPORTA (1998) 15 1113"]<-"LAPORTA (1998) 15 1113"
prto$trop[prto$identifiant=="MAJUMDAR (1998) 19 809"]<-"MAJUMDAR (1998) 19 809"
prto$trop[prto$identifiant=="MURPHY (2005) 30 327"]<-"MURPHY (2005) 30 327"
prto$trop[prto$identifiant=="NEWMAN (1988) 24 14"]<-"NEWMAN (1988) 24 14"
prto$trop[prto$identifiant=="NIMON (1999) 81 1078"]<-"NIMON (1999) 81 1078"
prto$trop[prto$identifiant=="PLAMBECK (2007) 53 1872"]<-"PLAMBECK (2007) 53 1872"
prto$trop[prto$identifiant=="POWELL (1992) 13 551"]<-"POWELL (1992) 13 551"
prto$trop[prto$identifiant=="RUBEN (2008) AB 155"]<-"RUBEN (2008) AB 155"
prto$trop[prto$identifiant=="SAPPINGTON (2005) 27 155"]<-"SAPPINGTON (2005) 27 155"
prto$trop[prto$identifiant=="SANDERSON (1995) 24 583"]<-"SANDERSON (1995) 24 583"
prto$trop[prto$identifiant=="SHAPIRO (1985) 75 424"]<-"SHAPIRO (1985) 75 424"
prto$trop[prto$identifiant=="SKINNER (1996) 5 15"]<-"SKINNER (1996) 5 15"
prto$trop[prto$identifiant=="STONEMAN (1987) AB 126"]<-"STONEMAN (1987) AB 126"
prto$trop[prto$identifiant=="SUN (2000) 17 636"]<-"SUN (2000) 17 636"
prto$trop[prto$identifiant=="TAYLOR (1995) 12 10"]<-"TAYLOR (1995) 12 10"
prto$trop[prto$identifiant=="TOWN (2001) 20 967"]<-"TOWN (2001) 20 967"
prto$trop[prto$identifiant=="VENKATRAMAN (1986) 11 71"]<-"VENKATRAMAN (1986) 11 71"
prto$trop[prto$identifiant=="VENKATRAMAN (1986) 1 71"]<-"VENKATRAMAN (1986) 1 71"
prto$trop[prto$identifiant=="WALTON (1998) 34 2"]<-"WALTON (1998) 34 2"






for(i in 1:nrow(prto)){
  if(prto$identifiant[i]!=prto$trop[i] & prto$trop[i] !="AB"){
    eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]<- prto$trop[i]
  }else{eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]<-eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]}
}




############################################################################################################################
############################################################################################################################
# Troisième itération: i et i-1 : OK
############################################################################################################################
############################################################################################################################


# On calcule la fréquence de chaque identifiant

freq<-table(eoeoe$identifiant)
freq<-as.data.frame(freq)

# On stocke la fréquence comme nouvelle variable, ce qui nous servira à renseigner la taille des noeuds dans les graphes

eoeoe$frequence<-0
for(i in 1:nrow(eoeoe)){
  j<-which(eoeoe$identifiant[i] == freq$Var1)
  eoeoe$frequence[i]<-freq$Freq[j]
}


eoeoe2<-eoeoe[order(eoeoe$identifiant),]



eoeoe2$kalro<-0
for(i in 2:(nrow(eoeoe2)-1)){if(eoeoe2$Firstauthor[i]==eoeoe2$Firstauthor[i-1] & eoeoe2$Datepubli[i]==eoeoe2$Datepubli[i-1] & eoeoe2$identifiant[i]!= eoeoe2$identifiant[i-1] & ((eoeoe2$Issue[i]==eoeoe2$Issue[i-1] & eoeoe2$Page[i]!=eoeoe2$Page[i-1])|(eoeoe2$Issue[i]!=eoeoe2$Issue[i-1] & eoeoe2$Page[i]==eoeoe2$Page[i-1]))){
  eoeoe2$kalro[i-1]<-16546546546}else{
    eoeoe2$kalro[i-1]<-0}}


for(i in 2:(nrow(eoeoe2)-1)){if(eoeoe2$kalro[i]==16546546546){
  eoeoe2$kalro[i+1]<-888}else{
    eoeoe2$kalro[i+1]<-eoeoe2$kalro[i+1]}}


prto<-eoeoe2[eoeoe2$kalro==16546546546|eoeoe2$kalro==888,]
prto<-prto[,c(1,7,8,9)]

prto$Recl<-0
for(i in 1:(nrow(prto)-1)){
  prto$Recl[i]<-StrDist(prto$V1[i], prto$V1[i+1], method = "normlevenshtein", mismatch = 1, gap = 1, ignore.case = FALSE)
}


prto$trop<-"AB"
for(i in 1:(nrow(prto)-1)){
  if(prto$Recl[i]>0.60){
    if(prto$frequence[i]!= prto$frequence[i+1]){
      j<-which.max(c(prto$frequence[i],prto$frequence[i+1]))
      prto$trop[i]<-prto$identifiant[i+j-1]
      prto$trop[i+1]<-prto$identifiant[i+j-1]}else{
        prto$trop[i]<-prto$trop[i]}}
  else{prto$trop[i]<-prto$trop[i]}}




prto$trop[prto$identifiant=="BILS (2001) 91 274"]<-"BILS (2001) 91 274"
prto$trop[prto$identifiant=="CAPLIN (1991) 59 1"]<-"CAPLIN (1991) 59 1"
prto$trop[prto$identifiant=="CASWELL (1992) 74 1196"]<-"CASWELL (1992) 74 1196"
prto$trop[prto$identifiant=="ESKILDSEN (2000) 11 581"]<-"ESKILDSEN (2000) 11 581"
prto$trop[prto$identifiant=="HARARI (1993) 82 39"]<-"HARARI (1993) 82 39"
prto$trop[prto$identifiant=="JEPPERSON (1991) AB 204"]<-"JEPPERSON (1991) AB 204"
prto$trop[prto$identifiant=="JORESKOG (1971) 36 409"]<-"JORESKOG (1971) 36 409"
prto$trop[prto$identifiant=="KARAPETROVIC (1998) 15 694"]<-"KARAPETROVIC (1998) 15 694"
prto$trop[prto$identifiant=="KLEINDORFER (2005) 14 53"]<-"KLEINDORFER (2005) 14 53"
prto$trop[prto$identifiant=="LAPORTA (1998) 15 1113"]<-"LAPORTA (1998) 15 1113"
prto$trop[prto$identifiant=="MAJUMDAR (1998) 19 809"]<-"MAJUMDAR (1998) 19 809"
prto$trop[prto$identifiant=="MILGROM (1981) 49 921"]<-"MILGROM (1981) 49 921"
prto$trop[prto$identifiant=="MURPHY (2005) 30 327"]<-"MURPHY (2005) 30 327"
prto$trop[prto$identifiant=="NEWMAN (1988) 24 14"]<-"NEWMAN (1988) 24 14"
prto$trop[prto$identifiant=="NIMON (1999) 81 1078"]<-"NIMON (1999) 81 1078"
prto$trop[prto$identifiant=="PLAMBECK (2007) 53 1872"]<-"PLAMBECK (2007) 53 1872"
prto$trop[prto$identifiant=="POWELL (1992) 13 551"]<-"POWELL (1992) 13 551"
prto$trop[prto$identifiant=="RUBEN (2008) AB 155"]<-"RUBEN (2008) AB 155"
prto$trop[prto$identifiant=="SANDERSON (1995) 24 583"]<-"SANDERSON (1995) 24 583"
prto$trop[prto$identifiant=="SAPPINGTON (2005) 27 155"]<-"SAPPINGTON (2005) 27 155"
prto$trop[prto$identifiant=="SHAPIRO (1985) 75 424"]<-"SHAPIRO (1985) 75 424"
prto$trop[prto$identifiant=="SKINNER (1996) 5 15"]<-"SKINNER (1996) 5 15"
prto$trop[prto$identifiant=="STONEMAN (1987) AB 126"]<-"STONEMAN (1987) AB 126"
prto$trop[prto$identifiant=="SUN (2000) 17 636"]<-"SUN (2000) 17 636"
prto$trop[prto$identifiant=="TAYLOR (1995) 12 10"]<-"TAYLOR (1995) 12 10"
prto$trop[prto$identifiant=="TOWN (2001) 20 967"]<-"TOWN (2001) 20 967"


for(i in 1:nrow(prto)){
  if(prto$identifiant[i]!=prto$trop[i] & prto$trop[i] !="AB"){
    eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]<- prto$trop[i]
  }else{eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]<-eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]}
}



############################################################################################################################
############################################################################################################################
# Quatrième itération: On change le type d'identifiant : OK
############################################################################################################################
############################################################################################################################



##### On change l'ordre des 4 informations de sorte à voir si ça nous permettra de matcher d'autres identifiants

eoeoe$identifiant2<-"AB"
for(i in 1:nrow(eoeoe)){
  eoeoe$identifiant2[i]<-paste(eoeoe$Firstauthor[i],eoeoe$Issue[i],eoeoe$Page[i],eoeoe$Datepubli[i])}


# On calcule la fréquence de chaque identifiant

freq<-table(eoeoe$identifiant)
freq<-as.data.frame(freq)

# On stocke la fréquence comme nouvelle variable, ce qui nous servira à renseigner la taille des noeuds dans les graphes

eoeoe$frequence<-0
for(i in 1:nrow(eoeoe)){
  j<-which(eoeoe$identifiant[i] == freq$Var1)
  eoeoe$frequence[i]<-freq$Freq[j]
}



eoeoe2<-eoeoe[order(eoeoe$identifiant2),]

####################################################################################
# Dans cette boucle, identifiant tout court et pas identifiant 2
#####################################################################################

eoeoe2$kalro<-0
for(i in 2:(nrow(eoeoe2)-1)){if(eoeoe2$Firstauthor[i]==eoeoe2$Firstauthor[i+1] & eoeoe2$Issue[i]==eoeoe2$Issue[i+1] & eoeoe2$Issue[i]!="AB" & eoeoe2$identifiant[i]!= eoeoe2$identifiant[i+1] & ((eoeoe2$Datepubli[i]==eoeoe2$Datepubli[i+1] & eoeoe2$Page[i]!=eoeoe2$Page[i+1])|(eoeoe2$Datepubli[i]!=eoeoe2$Datepubli[i+1] & eoeoe2$Page[i]==eoeoe2$Page[i+1]))){
  eoeoe2$kalro[i]<-16546546546}else{
    eoeoe2$kalro[i]<-0}}


for(i in 2:(nrow(eoeoe2)-1)){if(eoeoe2$kalro[i]==16546546546){
  eoeoe2$kalro[i+1]<-888}else{
    eoeoe2$kalro[i+1]<-eoeoe2$kalro[i+1]}}


prto<-eoeoe2[eoeoe2$kalro==16546546546|eoeoe2$kalro==888,]
prto<-prto[,c(1,7,8,9)]


library(DescTools)

prto$Recl<-0
for(i in 1:(nrow(prto)-1)){
  prto$Recl[i]<-StrDist(prto$V1[i], prto$V1[i+1], method = "normlevenshtein", mismatch = 1, gap = 1, ignore.case = FALSE)
}



prto$trop<-"AB"
for(i in 1:(nrow(prto)-1)){
  if(prto$Recl[i]>0.6){
    if(prto$frequence[i]!= prto$frequence[i+1]){
      j<-which.max(c(prto$frequence[i],prto$frequence[i+1]))
      prto$trop[i]<-prto$identifiant[i+j-1]
      prto$trop[i+1]<-prto$identifiant[i+j-1]}else{
        prto$trop[i]<-prto$trop[i]}}
  else{prto$trop[i]<-prto$trop[i]}}





prto$trop[prto$identifiant=="ANDREWS (1992) 60 953"]<-"ANDREWS (1992) 60 953"
prto$trop[prto$identifiant=="BILS (2001) 91 274"]<-"BILS (2001) 91 274"
prto$trop[prto$identifiant=="CAPLIN (1991) 59 1"]<-"CAPLIN (1991) 59 1"
prto$trop[prto$identifiant=="CASWELL (1992) 74 1196"]<-"CASWELL (1992) 74 1196"
prto$trop[prto$identifiant=="ESKILDSEN (2000) 11 581"]<-"ESKILDSEN (2000) 11 581"
prto$trop[prto$identifiant=="FARRELL (1985) 76 940"]<-"FARRELL (1985) 76 940"
prto$trop[prto$identifiant=="JORESKOG (1971) 36 409"]<-"JORESKOG (1971) 36 409"
prto$trop[prto$identifiant=="KARAPETROVIC (1998) 15 694"]<-"KARAPETROVIC (1998) 15 694"
prto$trop[prto$identifiant=="KLEINDORFER (2005) 14 53"]<-"KLEINDORFER (2005) 14 53"
prto$trop[prto$identifiant=="MAANI (1989) 11 19"]<-"MAANI (1989) 11 19"
prto$trop[prto$identifiant=="MAJUMDAR (1998) 19 809"]<-"MAJUMDAR (1998) 19 809"
prto$trop[prto$identifiant=="MILGROM (1981) 49 921"]<-"MILGROM (1981) 49 921"
prto$trop[prto$identifiant=="MURPHY (2005) 30 327"]<-"MURPHY (2005) 30 327"
prto$trop[prto$identifiant=="NELSON (1974) 78 311"]<-"NELSON (1974) 82 729"
prto$trop[prto$identifiant=="NEWMAN (1988) 24 14"]<-"NEWMAN (1988) 24 14"
prto$trop[prto$identifiant=="NIMON (1999) 81 1078"]<-"NIMON (1999) 81 1078"
prto$trop[prto$identifiant=="PLAMBECK (2007) 53 1872"]<-"PLAMBECK (2007) 53 1872"
prto$trop[prto$identifiant=="SANDERSON (1995) 24 583"]<-"SANDERSON (1995) 24 583"
prto$trop[prto$identifiant=="SAPPINGTON (2005) 27 155"]<-"SAPPINGTON (2005) 27 155"
prto$trop[prto$identifiant=="SHAPIRO (1985) 75 424"]<-"SHAPIRO (1985) 75 424"
prto$trop[prto$identifiant=="SKINNER (1996) 5 15"]<-"SKINNER (1996) 5 15"
prto$trop[prto$identifiant=="STIGLER (1964) 72 44"]<-"STIGLER (1964) 72 44"
prto$trop[prto$identifiant=="STIGLER (1961) 72 44"]<-"STIGLER (1961) 72 44"
prto$trop[prto$identifiant=="SUN (2000) 17 636"]<-"SUN (2000) 17 636"
prto$trop[prto$identifiant=="TOWN (2001) 20 967"]<-"TOWN (2001) 20 967"
prto$trop[prto$identifiant=="VENKATRAMAN (1986) 1 71"]<-"VENKATRAMAN (1986) 1 71"



for(i in 1:nrow(prto)){
  if(prto$identifiant[i]!=prto$trop[i] & prto$trop[i] !="AB"){
    eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]<- prto$trop[i]
  }else{eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]<-eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]}
}




############################################################################################################################
############################################################################################################################
# Cinquième itération: On change le type d'identifiant : OK
############################################################################################################################
############################################################################################################################


##### Pareil qu'avant, mais on change encore l'ordre

eoeoe$identifiant3<-"AB"
for(i in 1:nrow(eoeoe)){
  eoeoe$identifiant3[i]<-paste(eoeoe$Firstauthor[i],eoeoe$Page[i],eoeoe$Issue[i],eoeoe$Datepubli[i])}



# On calcule la fréquence de chaque identifiant

freq<-table(eoeoe$identifiant)
freq<-as.data.frame(freq)

# On stocke la fréquence comme nouvelle variable, ce qui nous servira à renseigner la taille des noeuds dans les graphes

eoeoe$frequence<-0
for(i in 1:nrow(eoeoe)){
  j<-which(eoeoe$identifiant[i] == freq$Var1)
  eoeoe$frequence[i]<-freq$Freq[j]
}



eoeoe2<-eoeoe[order(eoeoe$identifiant3),]



eoeoe2$kalro<-0
for(i in 2:(nrow(eoeoe2)-1)){if(eoeoe2$Firstauthor[i]==eoeoe2$Firstauthor[i+1] & eoeoe2$Page[i]==eoeoe2$Page[i+1] & eoeoe2$Page[i]!="AB" &  eoeoe2$identifiant[i]!= eoeoe2$identifiant[i+1] & ((eoeoe2$Datepubli[i]==eoeoe2$Datepubli[i+1] & eoeoe2$Issue[i]!=eoeoe2$Issue[i+1])|(eoeoe2$Datepubli[i]!=eoeoe2$Datepubli[i+1] & eoeoe2$Issue[i]==eoeoe2$Issue[i+1]))){
  eoeoe2$kalro[i]<-16546546546}else{
    eoeoe2$kalro[i]<-0}}


for(i in 2:(nrow(eoeoe2)-1)){if(eoeoe2$kalro[i]==16546546546){
  eoeoe2$kalro[i+1]<-888}else{
    eoeoe2$kalro[i+1]<-eoeoe2$kalro[i+1]}}


prto<-eoeoe2[eoeoe2$kalro==16546546546|eoeoe2$kalro==888,]
prto<-prto[,c(1,7,8,9,10)]


library(DescTools)

prto$Recl<-0
for(i in 1:(nrow(prto)-1)){
  prto$Recl[i]<-StrDist(prto$V1[i], prto$V1[i+1], method = "normlevenshtein", mismatch = 1, gap = 1, ignore.case = FALSE)
}



prto$trop<-"AB"
for(i in 1:(nrow(prto)-1)){
  if(prto$Recl[i]>0.6){
    if(prto$frequence[i]!= prto$frequence[i+1]){
      j<-which.max(c(prto$frequence[i],prto$frequence[i+1]))
      prto$trop[i]<-prto$identifiant[i+j-1]
      prto$trop[i+1]<-prto$identifiant[i+j-1]}else{
        prto$trop[i]<-prto$trop[i]}}
  else{prto$trop[i]<-prto$trop[i]}}





prto$trop[prto$identifiant=="FUCHSBERG (1992) 10 B1"]<-"FUCHSBERG (1992) 10 B1"
prto$trop[prto$identifiant=="FUCHSBERG (1993) AB B1"]<-"FUCHSBERG (1993) AB B1"
prto$trop[prto$identifiant=="GROSSMAN (2005) 72 135"]<-"GROSSMAN (2005) 72 135"
prto$trop[prto$identifiant=="LAPORTA (1998) 15 1113"]<-"LAPORTA (1998) 15 1113"
prto$trop[prto$identifiant=="MAANI (1989) 11 19"]<-"MAANI (1989) 11 19"
prto$trop[prto$identifiant=="NELSON (1974) 82 729"]<-"NELSON (1974) 82 729"
prto$trop[prto$identifiant=="ROBINSON (1988) 259 2676"]<-"ROBINSON (1988) 259 2676"
prto$trop[prto$identifiant=="ROBINSON (1988) 259 696"]<-"ROBINSON (1988) 259 696"
prto$trop[prto$identifiant=="STIGLER (1961) 72 44"]<-"STIGLER (1961) 69 213"



for(i in 1:nrow(prto)){
  if(prto$identifiant[i]!=prto$trop[i] & prto$trop[i] !="AB"){
    eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]<- prto$trop[i]
  }else{eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]<-eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]}
}





############################################################################################################################
############################################################################################################################
# Sixième itération: On change le type d'identifiant : OK
############################################################################################################################
############################################################################################################################



##### Pareil mais on change une dernière fois l'ordre

eoeoe$identifiant4<-"AB"
for(i in 1:nrow(eoeoe)){
  eoeoe$identifiant4[i]<-paste(eoeoe$Datepubli[i],eoeoe$Issue[i],eoeoe$Page[i],eoeoe$Firstauthor[i])}



# On calcule la fréquence de chaque identifiant

freq<-table(eoeoe$identifiant)
freq<-as.data.frame(freq)

# On stocke la fréquence comme nouvelle variable, ce qui nous servira à renseigner la taille des noeuds dans les graphes

eoeoe$frequence<-0
for(i in 1:nrow(eoeoe)){
  j<-which(eoeoe$identifiant[i] == freq$Var1)
  eoeoe$frequence[i]<-freq$Freq[j]
}



eoeoe2<-eoeoe[order(eoeoe$identifiant4),]



eoeoe2$kalro<-0
for(i in 2:(nrow(eoeoe2)-1)){if(eoeoe2$Datepubli[i]==eoeoe2$Datepubli[i+1] & eoeoe2$Issue[i]==eoeoe2$Issue[i+1] & eoeoe2$Issue[i]!="AB" & eoeoe2$identifiant[i]!= eoeoe2$identifiant[i+1] & ((eoeoe2$Firstauthor[i]==eoeoe2$Firstauthor[i+1] & eoeoe2$Page[i]!=eoeoe2$Page[i+1])|(eoeoe2$Firstauthor[i]!=eoeoe2$Firstauthor[i+1] & eoeoe2$Page[i]==eoeoe2$Page[i+1]))){
  eoeoe2$kalro[i]<-16546546546}else{
    eoeoe2$kalro[i]<-0}}


for(i in 2:(nrow(eoeoe2)-1)){if(eoeoe2$kalro[i]==16546546546){
  eoeoe2$kalro[i+1]<-888}else{
    eoeoe2$kalro[i+1]<-eoeoe2$kalro[i+1]}}


prto<-eoeoe2[eoeoe2$kalro==16546546546|eoeoe2$kalro==888,]
prto<-prto[,c(1,7,8,9,10,11)]


library(DescTools)

prto$Recl<-0
for(i in 1:(nrow(prto)-1)){
  prto$Recl[i]<-StrDist(prto$V1[i], prto$V1[i+1], method = "normlevenshtein", mismatch = 1, gap = 1, ignore.case = FALSE)
}



prto$trop<-"AB"
for(i in 1:(nrow(prto)-1)){
  if(prto$Recl[i]>0.6){
    if(prto$frequence[i]!= prto$frequence[i+1]){
      j<-which.max(c(prto$frequence[i],prto$frequence[i+1]))
      prto$trop[i]<-prto$identifiant[i+j-1]
      prto$trop[i+1]<-prto$identifiant[i+j-1]}else{
        prto$trop[i]<-prto$trop[i]}}
  else{prto$trop[i]<-prto$trop[i]}}



prto$trop[prto$identifiant=="NEWMAN (1988) 24 14"]<-"NEWMAN (1988) 24 14"
prto$trop[prto$identifiant=="ROSS (1991) 32 22"]<-"ROSS (1991) 32 22"
prto$trop[prto$identifiant=="FORD (1991) 8 7"]<-"FORD (1991) 8 7"
prto$trop[prto$identifiant=="HOUGHTON (1993) 65 74"]<-"HOUGHTON (1993) 65 74"
prto$trop[prto$identifiant=="COOK (1995) 77 1144"]<-"COOK (1995) 77 1144"
prto$trop[prto$identifiant=="SEGERSEN (1998) 36 109"]<-"SEGERSEN (1998) 36 109"
prto$trop[prto$identifiant=="PLAMBECK (2007) 53 1872"]<-"PLAMBECK (2007) 53 1872"
prto$trop[prto$identifiant=="HAJIME (2009) 27 403"]<-"HAJIME (2009) 27 403"



for(i in 1:nrow(prto)){
  if(prto$identifiant[i]!=prto$trop[i] & prto$trop[i] !="AB"){
    eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]<- prto$trop[i]
  }else{eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]<-eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]}
}




############################################################################################################################
############################################################################################################################
# Septième itération: on traite le cas du nombre de citation identique : Ok
############################################################################################################################
############################################################################################################################


# On calcule la fréquence de chaque identifiant

freq<-table(eoeoe$identifiant)
freq<-as.data.frame(freq)

# On stocke la fréquence comme nouvelle variable, ce qui nous servira à renseigner la taille des noeuds dans les graphes

eoeoe$frequence<-0
for(i in 1:nrow(eoeoe)){
  j<-which(eoeoe$identifiant[i] == freq$Var1)
  eoeoe$frequence[i]<-freq$Freq[j]
}


eoeoe2<-eoeoe[order(eoeoe$identifiant),]



eoeoe2$kalro<-0
for(i in 2:(nrow(eoeoe2)-1)){if(eoeoe2$Firstauthor[i]==eoeoe2$Firstauthor[i+1] & eoeoe2$Datepubli[i]==eoeoe2$Datepubli[i+1] & eoeoe2$identifiant[i]!= eoeoe2$identifiant[i+1] & ((eoeoe2$Issue[i]==eoeoe2$Issue[i+1] & eoeoe2$Page[i]!=eoeoe2$Page[i+1])|(eoeoe2$Issue[i]!=eoeoe2$Issue[i+1] & eoeoe2$Page[i]==eoeoe2$Page[i+1]))){
  eoeoe2$kalro[i]<-16546546546}else{
    eoeoe2$kalro[i]<-0}}


for(i in 2:(nrow(eoeoe2)-1)){if(eoeoe2$kalro[i]==16546546546){
  eoeoe2$kalro[i+1]<-888}else{
    eoeoe2$kalro[i+1]<-eoeoe2$kalro[i+1]}}


prto<-eoeoe2[eoeoe2$kalro==16546546546|eoeoe2$kalro==888,]
prto<-prto[,c(1,7,8,9)]

prto$Recl<-0
for(i in 1:(nrow(prto)-1)){
  prto$Recl[i]<-StrDist(prto$V1[i], prto$V1[i+1], method = "normlevenshtein", mismatch = 1, gap = 1, ignore.case = FALSE)
}



####### Par rapport aux autres situations, cette boucle ne recquière plus que les occurrences soient différentes


prto$trop<-"AB"
for(i in 1:(nrow(prto)-1)){
  if(prto$Recl[i]>0.6){
    j<-which.max(c(prto$frequence[i],prto$frequence[i+1]))
    prto$trop[i]<-prto$identifiant[i+j-1]
    prto$trop[i+1]<-prto$identifiant[i+j-1]}
  else{prto$trop[i]<-prto$trop[i]}}




prto$trop[prto$identifiant=="ANDERSON (2001) 81 231"]<-"ANDERSON (2001) 81 231"
prto$trop[prto$identifiant=="BARON (2007) 16 683"]<-"BARON (2007) 16 683"
prto$trop[prto$identifiant=="BARRIENTOS (2011) 150 319"]<-"BARRIENTOS (2011) 150 319"
prto$trop[prto$identifiant=="BAUM (1994) AB 76"]<-"BAUM (1994) AB 76"
prto$trop[prto$identifiant=="DALGLEISH (2003) 42 18"]<-"DALGLEISH (2003) 42 18"
prto$trop[prto$identifiant=="DEANGELO (1981) 3 183"]<-"DEANGELO (1981) 3 183"
prto$trop[prto$identifiant=="DECOSTER (1996) 68 80"]<-"DECOSTER (1996) 68 80"
prto$trop[prto$identifiant=="FOTOPOULOS (2002) 104 730"]<-"FOTOPOULOS (2002) 104 730"
prto$trop[prto$identifiant=="GHATAK (1999) 60 27"]<-"GHATAK (1999) 60 27"
prto$trop[prto$identifiant=="GOME (1995) AB 58"]<-"GOME (1995) AB 58"
prto$trop[prto$identifiant=="GRILICHES (1971) AB 3"]<-"GRILICHES (1971) AB 3"
prto$trop[prto$identifiant=="HAMILTON (1999) AB AB"]<-"HAMILTON (1999) AB AB"
prto$trop[prto$identifiant=="HAUG (2002) AB 15"]<-"HAUG (2002) AB 15"
prto$trop[prto$identifiant=="JAHN (2004) AB AB"]<-"JAHN (2004) AB AB"
prto$trop[prto$identifiant=="LIAO (2007) 31 393"]<-"LIAO (2007) 31 393"
prto$trop[prto$identifiant=="LIEBOWITZ (1998) AB 671"]<-"LIEBOWITZ (1998) AB 671"
prto$trop[prto$identifiant=="MAHAJAN (2001) 49 646"]<-"MAHAJAN (2001) 49 646"
prto$trop[prto$identifiant=="MEYER (2001) 32 575"]<-"MEYER (2001) 32 575"
prto$trop[prto$identifiant=="MILGROM (1982) 50 1089"]<-"MILGROM (1982) 50 1089"
prto$trop[prto$identifiant=="MOORE (1993) 71 82"]<-"MOORE (1993) 71 82"
prto$trop[prto$identifiant=="PRIEM (2001) 26 57"]<-"PRIEM (2001) 26 57"
prto$trop[prto$identifiant=="QUINN (2000) 32 41"]<-"QUINN (2000) 32 41"
prto$trop[prto$identifiant=="RAMEY (1997) AB 13"]<-"RAMEY (1997) AB 13"
prto$trop[prto$identifiant=="SCOTT (1991) AB 164"]<-"SCOTT (1991) AB 164"
prto$trop[prto$identifiant=="SMITH (2001) 44 376"]<-"SMITH (2001) 44 376"
prto$trop[prto$identifiant=="STAATZ (1987) AB 87"]<-"STAATZ (1987) AB 87"
prto$trop[prto$identifiant=="STOLL (1978) 33 1153"]<-"STOLL (1978) 33 1153"
prto$trop[prto$identifiant=="STURGEON (2011) 4 181"]<-"STURGEON (2011) 4 181"
prto$trop[prto$identifiant=="SULLIVAN (1986) 19 77"]<-"SULLIVAN (1986) 19 77"
prto$trop[prto$identifiant=="SURESHCHANDAR (2001) 12 378"]<-"SURESHCHANDAR (2001) 12 378"
prto$trop[prto$identifiant=="TAKAHASHI (1999) AB B7"]<-"TAKAHASHI (1999) AB B7"
prto$trop[prto$identifiant=="TAKAHASHI (1999) AB B8"]<-"TAKAHASHI (1999) AB B8"
prto$trop[prto$identifiant=="TROCHIM (1989) 12 355"]<-"TROCHIM (1989) 12 355"
prto$trop[prto$identifiant=="UNDERDAL (2004) AB 361"]<-"UNDERDAL (2004) AB 361"
prto$trop[prto$identifiant=="VANEGAS (2001) 39 99"]<-"VANEGAS (2001) 39 99"
prto$trop[prto$identifiant=="VANHOEK (2001) 21 15"]<-"VANHOEK (2001) 21 15"
prto$trop[prto$identifiant=="WOLFE (1991) 12 281"]<-"WOLFE (1991) 12 281"
prto$trop[prto$identifiant=="BILS (2001) 91 274"]<-"BILS (2001) 91 274"
prto$trop[prto$identifiant=="CAPLIN (1991) 59 1"]<-"CAPLIN (1991) 59 1"
prto$trop[prto$identifiant=="CASWELL (1992) 74 1196"]<-"CASWELL (1992) 74 1196"
prto$trop[prto$identifiant=="ESKILDSEN (2000) 11 581"]<-"ESKILDSEN (2000) 11 581"
prto$trop[prto$identifiant=="HARARI (1993) 82 39"]<-"HARARI (1993) 82 39"
prto$trop[prto$identifiant=="JEPPERSON (1991) AB 204"]<-"JEPPERSON (1991) AB 204"
prto$trop[prto$identifiant=="JORESKOG (1971) 36 409"]<-"JORESKOG (1971) 36 409"
prto$trop[prto$identifiant=="KARAPETROVIC (1998) 15 694"]<-"KARAPETROVIC (1998) 15 694"
prto$trop[prto$identifiant=="KLEINDORFER (2005) 14 53"]<-"KLEINDORFER (2005) 14 53"
prto$trop[prto$identifiant=="LAPORTA (1998) 15 1113"]<-"LAPORTA (1998) 15 1113"
prto$trop[prto$identifiant=="MAJUMDAR (1998) 19 809"]<-"MAJUMDAR (1998) 19 809"
prto$trop[prto$identifiant=="MURPHY (2005) 30 327"]<-"MURPHY (2005) 30 327"
prto$trop[prto$identifiant=="NEWMAN (1988) 24 14"]<-"NEWMAN (1988) 24 14"
prto$trop[prto$identifiant=="NIMON (1999) 81 1078"]<-"NIMON (1999) 81 1078"
prto$trop[prto$identifiant=="PLAMBECK (2007) 53 1872"]<-"PLAMBECK (2007) 53 1872"
prto$trop[prto$identifiant=="POWELL (1992) 13 551"]<-"POWELL (1992) 13 551"
prto$trop[prto$identifiant=="RUBEN (2008) AB 155"]<-"RUBEN (2008) AB 155"
prto$trop[prto$identifiant=="SAPPINGTON (2005) 27 155"]<-"SAPPINGTON (2005) 27 155"
prto$trop[prto$identifiant=="SANDERSON (1995) 24 583"]<-"SANDERSON (1995) 24 583"
prto$trop[prto$identifiant=="SKINNER (1996) 5 15"]<-"SKINNER (1996) 5 15"
prto$trop[prto$identifiant=="STONEMAN (1987) AB 126"]<-"STONEMAN (1987) AB 126"
prto$trop[prto$identifiant=="SUN (2000) 17 636"]<-"SUN (2000) 17 636"
prto$trop[prto$identifiant=="TAYLOR (1995) 12 10"]<-"TAYLOR (1995) 12 10"
prto$trop[prto$identifiant=="TOWN (2001) 20 967"]<-"TOWN (2001) 20 967"







for(i in 1:nrow(prto)){
  if(prto$identifiant[i]!=prto$trop[i] & prto$trop[i] !="AB"){
    eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]<- prto$trop[i]
  }else{eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]<-eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]}
}





##########################################################################################################################
##########################################################################################################################
# Huitième itération: Approche alternative pour travailler sur les identifiants
##########################################################################################################################
##########################################################################################################################


# On calcule la fréquence de chaque identifiant

freq<-table(eoeoe$identifiant)
freq<-as.data.frame(freq)

# On stocke la fréquence comme nouvelle variable, ce qui nous servira à renseigner la taille des noeuds dans les graphes

eoeoe$frequence<-0
for(i in 1:nrow(eoeoe)){
  j<-which(eoeoe$identifiant[i] == freq$Var1)
  eoeoe$frequence[i]<-freq$Freq[j]
}


eoeoe2<-eoeoe[order(eoeoe$identifiant),]


#### L'idée est la suivante: les pseudos sont classés par auteur date issue page. Du coup, dans le cas où un auteur et une 
#### date sont identique entre i et i+1 et que l'identifiant de i est différent de celui de i+1, alors on mesure la distance
#### entre la ref complète de i et de i+1, et si cette distance est suffisamment élevée, alors on note i.
#### Les lignes marquées sont ainsi susceptibles de correspondre à des erreurs restantes. 


eoeoe2$kalro<-0
for(i in 2:(nrow(eoeoe2)-1)){if(eoeoe2$Firstauthor[i]==eoeoe2$Firstauthor[i+1] & eoeoe2$Datepubli[i]==eoeoe2$Datepubli[i+1] & eoeoe2$identifiant[i]!= eoeoe2$identifiant[i+1]){
  if(StrDist(eoeoe2$V1[i], eoeoe2$V1[i+1], method = "normlevenshtein", mismatch = 1, gap = 1, ignore.case = FALSE)>0.75){eoeoe2$kalro[i]<-16546546546}
  else{eoeoe2$kalro[i]<-0}}else{
    eoeoe2$kalro[i]<-0}}



for(i in 2:(nrow(eoeoe2)-1)){if(eoeoe2$kalro[i]==16546546546){
  eoeoe2$kalro[i+1]<-888}else{
    eoeoe2$kalro[i+1]<-eoeoe2$kalro[i+1]}}



prto<-eoeoe2[eoeoe2$kalro==16546546546|eoeoe2$kalro==888,]
prto<-prto[,c(1,7,8,12)]




prto$trop<-"AB"
for(i in 1:(nrow(prto)-1)){
  if(prto$kalro[i]==16546546546){
    j<-which.max(c(prto$frequence[i],prto$frequence[i+1]))
    prto$trop[i]<-prto$identifiant[i+j-1]
    prto$trop[i+1]<-prto$identifiant[i+j-1]}
  else{prto$trop[i]<-prto$trop[i]}}



prto$trop[prto$identifiant=="SANA (2010) 200 451"]<-"SANA (2010) 200 451"




for(i in 1:nrow(prto)){
  if(prto$identifiant[i]!=prto$trop[i] & prto$trop[i] !="AB"){
    eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]<- prto$trop[i]
  }else{eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]<-eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]}
}





##########################################################################################################################
##########################################################################################################################
# Neuvième itération : on refait tourner la boucle précédente 
##########################################################################################################################
##########################################################################################################################


# On calcule la fréquence de chaque identifiant

freq<-table(eoeoe$identifiant)
freq<-as.data.frame(freq)

# On stocke la fréquence comme nouvelle variable, ce qui nous servira à renseigner la taille des noeuds dans les graphes

eoeoe$frequence<-0
for(i in 1:nrow(eoeoe)){
  j<-which(eoeoe$identifiant[i] == freq$Var1)
  eoeoe$frequence[i]<-freq$Freq[j]
}


eoeoe2<-eoeoe[order(eoeoe$identifiant),]



eoeoe2$kalro<-0
for(i in 2:(nrow(eoeoe2)-1)){if(eoeoe2$Firstauthor[i]==eoeoe2$Firstauthor[i+1] & eoeoe2$Datepubli[i]==eoeoe2$Datepubli[i+1] & eoeoe2$identifiant[i]!= eoeoe2$identifiant[i+1]){
  if(StrDist(eoeoe2$V1[i], eoeoe2$V1[i+1], method = "normlevenshtein", mismatch = 1, gap = 1, ignore.case = FALSE)>0.65){eoeoe2$kalro[i]<-16546546546}
  else{eoeoe2$kalro[i]<-0}}else{
    eoeoe2$kalro[i]<-0}}


for(i in 2:(nrow(eoeoe2)-1)){if(eoeoe2$kalro[i]==16546546546){
  eoeoe2$kalro[i+1]<-888}else{
    eoeoe2$kalro[i+1]<-eoeoe2$kalro[i+1]}}



prto<-eoeoe2[eoeoe2$kalro==16546546546|eoeoe2$kalro==888,]
prto<-prto[,c(1,7,8,12)]

prto$trop<-"AB"
for(i in 1:(nrow(prto)-1)){
  if(prto$kalro[i]==16546546546){
    j<-which.max(c(prto$frequence[i],prto$frequence[i+1]))
    prto$trop[i]<-prto$identifiant[i+j-1]
    prto$trop[i+1]<-prto$identifiant[i+j-1]}
  else{prto$trop[i]<-prto$trop[i]}}



prto$trop[prto$identifiant=="ABERNATHY (1988) 2 55"]<-"ABERNATHY (1988) 2 55"
prto$trop[prto$identifiant=="ANDERSON (2001) 81 231"]<-"ANDERSON (2001) 81 231"
prto$trop[prto$identifiant=="BILS (2001) 91 274"]<-"BILS (2001) 91 274"
prto$trop[prto$identifiant=="CASTKA (2008) 111 274"]<-"CASTKA (2008) 111 274"
prto$trop[prto$identifiant=="CASWELL (1992) 74 460"]<-"CASWELL (1992) 74 460"
prto$trop[prto$identifiant=="CHENG (1989) 40 252"]<-"CHENG (1989) 40 252"
prto$trop[prto$identifiant=="DEANGELO (1981) 3 183"]<-"DEANGELO (1981) 3 183"
prto$trop[prto$identifiant=="ESKILDSEN (2000) 11 581"]<-"ESKILDSEN (2000) 11 581"
prto$trop[prto$identifiant=="FARRELL (1986) 21 73"]<-"FARRELL (1986) 21 73"
prto$trop[prto$identifiant=="GHATAK (1999) 60 27"]<-"GHATAK (1999) 60 27"
prto$trop[prto$identifiant=="HARARI (1993) 82 39"]<-"HARARI (1993) 82 39"
prto$trop[prto$identifiant=="HART (1985) 95 889"]<-"HART (1985) 95 889"
prto$trop[prto$identifiant=="HILLMAN AB 78 729"]<-"HILLMAN AB 78 729"
prto$trop[prto$identifiant=="JAHN (2004) AB AB"]<-"JAHN (2004) AB AB"
prto$trop[prto$identifiant=="JEPPERSON (1991) AB 204"]<-"JEPPERSON (1991) AB 204"
prto$trop[prto$identifiant=="KOGUT (1985) 26 15"]<-"KOGUT (1985) 26 15"
prto$trop[prto$identifiant=="LAPORTA (1998) 15 1113"]<-"LAPORTA (1998) 15 1113"
prto$trop[prto$identifiant=="LIAO (2007) 31 393"]<-"LIAO (2007) 31 393"
prto$trop[prto$identifiant=="LILIEN (1974) 20 1027"]<-"LILIEN (1974) 20 1027"
prto$trop[prto$identifiant=="MORGAN (2000) AB AB"]<-"MORGAN (2000) AB AB"
prto$trop[prto$identifiant=="NEWMAN (1988) 24 14"]<-"NEWMAN (1988) 24 14"
prto$trop[prto$identifiant=="OLORUNNIWO (2006) 20 59"]<-"OLORUNNIWO (2006) 20 59"
prto$trop[prto$identifiant=="POWELL (1992) 13 551"]<-"POWELL (1992) 13 551"
prto$trop[prto$identifiant=="QUINN (2000) 32 41"]<-"QUINN (2000) 32 41"
prto$trop[prto$identifiant=="RAGHUNATHAN (2000) 17 87"]<-"RAGHUNATHAN (2000) 17 87"
prto$trop[prto$identifiant=="RETCHIN (1990) 80 411"]<-"RETCHIN (1990) 80 411"
prto$trop[prto$identifiant=="RUBEN (2008) AB 155"]<-"RUBEN (2008) AB 155"
prto$trop[prto$identifiant=="SANA (2010) 200 451"]<-"SANA (2010) 200 451"
prto$trop[prto$identifiant=="SCOTT (1983) AB AB"]<-"SCOTT (1983) AB AB"
prto$trop[prto$identifiant=="SCOTT (1991) AB 164"]<-"SCOTT (1991) AB 164"
prto$trop[prto$identifiant=="SHIMBUNSHA AB AB 21"]<-"SHIMBUNSHA AB AB 21"
prto$trop[prto$identifiant=="SHIMBUNSHA AB AB 54"]<-"SHIMBUNSHA AB AB 54"
prto$trop[prto$identifiant=="STOLL (1978) 33 1153"]<-"STOLL (1978) 33 1153"
prto$trop[prto$identifiant=="STURGEON (2011) 4 181"]<-"STURGEON (2011) 4 181"
prto$trop[prto$identifiant=="SUN (2000) 17 636"]<-"SUN (2000) 17 636"
prto$trop[prto$identifiant=="TAYLOR (1995) 12 10"]<-"TAYLOR (1995) 12 10"
prto$trop[prto$identifiant=="UNDERDAL (2004) AB 361"]<-"UNDERDAL (2004) AB 361"



for(i in 1:nrow(prto)){
  if(prto$identifiant[i]!=prto$trop[i] & prto$trop[i] !="AB"){
    eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]<- prto$trop[i]
  }else{eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]<-eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]}
}




##########################################################################################################################
##########################################################################################################################
# Dixième itération : on change le type de pseudo auquel on soumet cette boucle 
##########################################################################################################################
##########################################################################################################################


# On calcule la fréquence de chaque identifiant

freq<-table(eoeoe$identifiant)
freq<-as.data.frame(freq)

# On stocke la fréquence comme nouvelle variable, ce qui nous servira à renseigner la taille des noeuds dans les graphes

eoeoe$frequence<-0
for(i in 1:nrow(eoeoe)){
  j<-which(eoeoe$identifiant[i] == freq$Var1)
  eoeoe$frequence[i]<-freq$Freq[j]
}


eoeoe2<-eoeoe[order(eoeoe$identifiant2),]



eoeoe2$kalro<-0
for(i in 2:(nrow(eoeoe2)-1)){if(eoeoe2$Firstauthor[i]==eoeoe2$Firstauthor[i+1] & eoeoe2$Issue[i]==eoeoe2$Issue[i+1] & eoeoe2$identifiant[i]!= eoeoe2$identifiant[i+1]){
  if(StrDist(eoeoe2$V1[i], eoeoe2$V1[i+1], method = "normlevenshtein", mismatch = 1, gap = 1, ignore.case = FALSE)>0.75){eoeoe2$kalro[i]<-16546546546}
  else{eoeoe2$kalro[i]<-0}}else{
    eoeoe2$kalro[i]<-0}}





for(i in 2:(nrow(eoeoe2)-1)){if(eoeoe2$kalro[i]==16546546546){
  eoeoe2$kalro[i+1]<-888}else{
    eoeoe2$kalro[i+1]<-eoeoe2$kalro[i+1]}}



prto<-eoeoe2[eoeoe2$kalro==16546546546|eoeoe2$kalro==888,]
prto<-prto[,c(1,7,8,12)]

prto$trop<-"AB"
for(i in 1:(nrow(prto)-1)){
  if(prto$kalro[i]==16546546546){
    j<-which.max(c(prto$frequence[i],prto$frequence[i+1]))
    prto$trop[i]<-prto$identifiant[i+j-1]
    prto$trop[i+1]<-prto$identifiant[i+j-1]}
  else{prto$trop[i]<-prto$trop[i]}}





prto$trop[prto$identifiant=="BEFFERT (2005) AB AB"]<-"BEFFERT (2005) AB AB"
prto$trop[prto$identifiant=="EATON (2001) AB AB"]<-"EATON (2001) AB AB"
prto$trop[prto$identifiant=="FOUCAULT (2008) AB AB"]<-"FOUCAULT (2008) AB AB"
prto$trop[prto$identifiant=="HANEMANN (1996) AB AB"]<-"HANEMANN (1996) AB AB"
prto$trop[prto$identifiant=="HARRIGAN (1986) AB AB"]<-"HARRIGAN (1986) AB AB"
prto$trop[prto$identifiant=="HUCK (2008) AB AB"]<-"HUCK (2008) AB AB"
prto$trop[prto$identifiant=="KAMIEN (1985) AB AB"]<-"KAMIEN (1985) AB AB"
prto$trop[prto$identifiant=="PHLIPS (1988) AB AB"]<-"PHLIPS (1988) AB AB"
prto$trop[prto$identifiant=="SHIMBUNSHA AB AB 21"]<-"SHIMBUNSHA AB AB 21"
prto$trop[prto$identifiant=="ULPH (2001) AB AB"]<-"ULPH (2001) AB AB"
prto$trop[prto$identifiant=="VARIYAM (1997) AB AB"]<-"VARIYAM (1997) AB AB"



for(i in 1:nrow(prto)){
  if(prto$identifiant[i]!=prto$trop[i] & prto$trop[i] !="AB"){
    eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]<- prto$trop[i]
  }else{eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]<-eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]}
}



##########################################################################################################################
##########################################################################################################################
# Onzième itération : on change le type de pseudo auquel on soumet cette boucle 
##########################################################################################################################
##########################################################################################################################


# On calcule la fréquence de chaque identifiant

freq<-table(eoeoe$identifiant)
freq<-as.data.frame(freq)

# On stocke la fréquence comme nouvelle variable, ce qui nous servira à renseigner la taille des noeuds dans les graphes

eoeoe$frequence<-0
for(i in 1:nrow(eoeoe)){
  j<-which(eoeoe$identifiant[i] == freq$Var1)
  eoeoe$frequence[i]<-freq$Freq[j]
}


eoeoe2<-eoeoe[order(eoeoe$identifiant3),]



eoeoe2$kalro<-0
for(i in 2:(nrow(eoeoe2)-1)){if(eoeoe2$Firstauthor[i]==eoeoe2$Firstauthor[i+1] & eoeoe2$Page[i]==eoeoe2$Page[i+1] & eoeoe2$identifiant[i]!= eoeoe2$identifiant[i+1]){
  if(StrDist(eoeoe2$V1[i], eoeoe2$V1[i+1], method = "normlevenshtein", mismatch = 1, gap = 1, ignore.case = FALSE)>0.75){eoeoe2$kalro[i]<-16546546546}
  else{eoeoe2$kalro[i]<-0}}else{
    eoeoe2$kalro[i]<-0}}





for(i in 2:(nrow(eoeoe2)-1)){if(eoeoe2$kalro[i]==16546546546){
  eoeoe2$kalro[i+1]<-888}else{
    eoeoe2$kalro[i+1]<-eoeoe2$kalro[i+1]}}



prto<-eoeoe2[eoeoe2$kalro==16546546546|eoeoe2$kalro==888,]
prto<-prto[,c(1,7,8,12)]




prto$trop<-"AB"
for(i in 1:(nrow(prto)-1)){
  if(prto$kalro[i]==16546546546){
    j<-which.max(c(prto$frequence[i],prto$frequence[i+1]))
    prto$trop[i]<-prto$identifiant[i+j-1]
    prto$trop[i+1]<-prto$identifiant[i+j-1]}
  else{prto$trop[i]<-prto$trop[i]}}






prto$trop[prto$identifiant=="BEFFERT (2005) AB AB"]<-"BEFFERT (2005) AB AB"
prto$trop[prto$identifiant=="EATON (2001) AB AB"]<-"EATON (2001) AB AB"
prto$trop[prto$identifiant=="FOUCAULT (2008) AB AB"]<-"FOUCAULT (2008) AB AB"
prto$trop[prto$identifiant=="HANEMANN (1996) AB AB"]<-"HANEMANN (1996) AB AB"
prto$trop[prto$identifiant=="HARRIGAN (1986) AB AB"]<-"HARRIGAN (1986) AB AB"
prto$trop[prto$identifiant=="HUCK (2008) AB AB"]<-"HUCK (2008) AB AB"
prto$trop[prto$identifiant=="KAMIEN (1985) AB AB"]<-"KAMIEN (1985) AB AB"
prto$trop[prto$identifiant=="PHLIPS (1988) AB AB"]<-"PHLIPS (1988) AB AB"
prto$trop[prto$identifiant=="ULPH (2001) AB AB"]<-"ULPH (2001) AB AB"
prto$trop[prto$identifiant=="UPDEGROVE (2007) AB AB"]<-"UPDEGROVE (2007) AB AB"
prto$trop[prto$identifiant=="VARIYAM (1997) AB AB"]<-"VARIYAM (1997) AB AB"



for(i in 1:nrow(prto)){
  if(prto$identifiant[i]!=prto$trop[i] & prto$trop[i] !="AB"){
    eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]<- prto$trop[i]
  }else{eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]<-eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]}
}





##########################################################################################################################
##########################################################################################################################
# Douzième itération : on change le type de pseudo auquel on soumet cette boucle 
##########################################################################################################################
##########################################################################################################################


# On calcule la fréquence de chaque identifiant

freq<-table(eoeoe$identifiant)
freq<-as.data.frame(freq)

# On stocke la fréquence comme nouvelle variable, ce qui nous servira à renseigner la taille des noeuds dans les graphes

eoeoe$frequence<-0
for(i in 1:nrow(eoeoe)){
  j<-which(eoeoe$identifiant[i] == freq$Var1)
  eoeoe$frequence[i]<-freq$Freq[j]
}


eoeoe2<-eoeoe[order(eoeoe$identifiant4),]



eoeoe2$kalro<-0
for(i in 2:(nrow(eoeoe2)-1)){if(eoeoe2$Datepubli[i]==eoeoe2$Datepubli[i+1] & eoeoe2$Issue[i]==eoeoe2$Issue[i+1] & eoeoe2$identifiant[i]!= eoeoe2$identifiant[i+1]){
  if(StrDist(eoeoe2$V1[i], eoeoe2$V1[i+1], method = "normlevenshtein", mismatch = 1, gap = 1, ignore.case = FALSE)>0.75){eoeoe2$kalro[i]<-16546546546}
  else{eoeoe2$kalro[i]<-0}}else{
    eoeoe2$kalro[i]<-0}}





for(i in 2:(nrow(eoeoe2)-1)){if(eoeoe2$kalro[i]==16546546546){
  eoeoe2$kalro[i+1]<-888}else{
    eoeoe2$kalro[i+1]<-eoeoe2$kalro[i+1]}}



prto<-eoeoe2[eoeoe2$kalro==16546546546|eoeoe2$kalro==888,]
prto<-prto[,c(1,7,8,12)]




prto$trop<-"AB"
for(i in 1:(nrow(prto)-1)){
  if(prto$kalro[i]==16546546546){
    j<-which.max(c(prto$frequence[i],prto$frequence[i+1]))
    prto$trop[i]<-prto$identifiant[i+j-1]
    prto$trop[i+1]<-prto$identifiant[i+j-1]}
  else{prto$trop[i]<-prto$trop[i]}}




prto$trop[prto$identifiant=="OISON (1965) AB AB"]<-"OLSON (1965) AB AB"
prto$trop[prto$identifiant=="OLSEN (1965) AB AB"]<-"OLSON (1965) AB AB"
prto$trop[prto$identifiant=="MARQUARDL (1975) 4 27"]<-"MARQUARDT (1975) 4 27"
prto$trop[prto$identifiant=="ENTRY (1977) 10 1"]<-"SPENCE (1977) 10 1 Spence"
prto$trop[prto$identifiant=="MCCARTHY (1980) 3 149"]<-"MCCARTHY (1980) 3 149"
prto$trop[prto$identifiant=="BUFFA (1981) 12 572"]<-"BUFFA (1981) 12 572"
prto$trop[prto$identifiant=="HAX (1981) 12 574"]<-"HAX (1981) 12 574"
prto$trop[prto$identifiant=="REINCHELD (1990) AB 105"]<-"REICHHELD (1990) 68 105"
prto$trop[prto$identifiant=="SASSER (1990) AB 105"]<-"REICHHELD (1990) 68 105"
prto$trop[prto$identifiant=="WOODRUFF (1992) AB 209"]<-"WOODRUFF (1992) AB 209"
prto$trop[prto$identifiant=="FULTON (1995) 77 1144"]<-"FULTON (1995) 77 1144"
prto$trop[prto$identifiant=="CASABIANCA (2000) 2  1999) 269"]<-"CASABIANCA (2000) 2  1999) 269"
prto$trop[prto$identifiant=="GREENAWAY (2004) 20 358"]<-"GREENAWAY (2004) 20 358"
prto$trop[prto$identifiant=="MARETTE (2011) AB 499"]<-"MARETTE (2011) AB 499"




for(i in 1:nrow(prto)){
  if(prto$identifiant[i]!=prto$trop[i] & prto$trop[i] !="AB"){
    eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]<- prto$trop[i]
  }else{eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]<-eoeoe$identifiant[which(eoeoe$identifiant==prto$identifiant[i])]}
}





save(eoeoe, file = "finalbase.rda")





