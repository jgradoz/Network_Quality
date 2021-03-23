###### Import packages

library(readr)
library(stringr)
library(tidyverse)
library(biblionetwork)

##### Set working directory

setwd("C:/Users/jgrad/Desktop/Network_Quality/Network_Quality")

##### Import file used

zal <- read_delim("Data_3_Core.txt", "\t", escape_double = FALSE, trim_ws = TRUE)


######################################################################################################################################
###################################################################################################################################
# NB: Beaucoup de commandes sont similaire au traitement SCOPUS précédent, du coup plus de commentaires sur le précédent script
# et on pourra s'y reporter.
######################################################################################################################################
#####################################################################################################################################


#### Vérifier qu'on a bien le bon nombre de références. Comment faire?
#### Les infos sur les references sont définies avec des balises, donc on identifie la balise
#### annonçant le début de la bibliographie ("REFERENCES:"), de longueur 11 caractères
#### Puis on comptera le nombe de balises identifiées
#### Cette démarche nous permettra aussi de débuter le travail sur la bibliographie


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
  if(str_sub (zalerit[i,1], 1,9)=="ABSTRACT:"){zalerit$topor[i]<-T}else{zalerit$topor[i]<-F}
}


zalerit<-zalerit[zalerit$topor==1,]
zalerit<-zalerit[,c(1,8)]
zalerit$V1<-str_replace_all(string = zalerit$V1, pattern = "ABSTRACT:", replacement = "")



save(zalerit, file = "Abstracts.rda")



##### idsbis vise à comparer si on a la même liste d'identifiants que pour ids
##### la seule différence est 18888, qui correspond aux articles n'ayant pas de biblios

idsbis<-unique(zaler$Cartel)
idsbis<-as.data.frame(idsbis)
ids<-as.data.frame(ids)


##### On supprime les refs biblios de notre base

zaler<-zaler[zal[,4]!=1,]

##### On ne garde que les colonnes d'intéret

zaler<-zaler[,c(1,8)]


##### On extrait l'info sur le premier auteur grace à une REGULAR EXPRESSION (REGEX)

zaler$Firstauthor<-"AB"
zaler$Firstauthor<-gsub("([A-z0-9]\\.\\,.*|([A-z0-9]\\.[A-z0-9]?\\.{1}$| [A-z0-9]?\\.{1}$))", "",zaler$V1)

##### On créée une colonne qui dit si la nouvelle colonne créée contient la même info que la précédente
##### (ce qui veut implicitement dire que que ce n'est pas une ligne d'auteur)

zaler$comp<-zaler$Firstauthor==zaler$V1


##### A partir de cette info, on ne garde que les infos que les auteurs

for(i in 1:nrow(zaler)){if(zaler$comp[i]==T){zaler$Firstauthor[i]<-"AB"}else{zaler$Firstauthor[i]<-zaler$Firstauthor[i]}}


##### Malgré la relative efficacité de la REGEX, cela n'empeche pas quelques autres infos 
##### de s'être glissées dans la colonne premier auteur. Comme beaucoup de ses infos se 
##### remarquent avec une balise, alors on peut les effacer

zaler$comp<-str_detect (string =zaler$Firstauthor, pattern = "ABSTRACT:")


for(i in 1:nrow(zaler)){if(zaler$comp[i]==T){zaler$Firstauthor[i]<-"AB"}else{zaler$Firstauthor[i]<-zaler$Firstauthor[i]}}


zaler$comp<-str_detect (string =zaler$Firstauthor, pattern = "PUBLISHER:")

for(i in 1:nrow(zaler)){if(zaler$comp[i]==T){zaler$Firstauthor[i]<-"AB"}else{zaler$Firstauthor[i]<-zaler$Firstauthor[i]}}


zaler$comp<-str_detect (string =zaler$Firstauthor, pattern = "ABBREVIATED SOURCE TITLE:")

for(i in 1:nrow(zaler)){if(zaler$comp[i]==T){zaler$Firstauthor[i]<-"AB"}else{zaler$Firstauthor[i]<-zaler$Firstauthor[i]}}


zaler$comp<-str_detect (string =zaler$Firstauthor, pattern = "AFFILIATIONS:")

for(i in 1:nrow(zaler)){if(zaler$comp[i]==T){zaler$Firstauthor[i]<-"AB"}else{zaler$Firstauthor[i]<-zaler$Firstauthor[i]}}


zaler$comp<-str_detect (string =zaler$Firstauthor, pattern = "CORRESPONDENCE ADDRESS:")

for(i in 1:nrow(zaler)){if(zaler$comp[i]==T){zaler$Firstauthor[i]<-"AB"}else{zaler$Firstauthor[i]<-zaler$Firstauthor[i]}}

zaler$comp<-str_detect (string =zaler$Firstauthor, pattern = "FUNDING TEXT")

for(i in 1:nrow(zaler)){if(zaler$comp[i]==T){zaler$Firstauthor[i]<-"AB"}else{zaler$Firstauthor[i]<-zaler$Firstauthor[i]}}


##### Comme ça ne suffit pas à tout purger, on regarde aussi les chaines de 
##### caractères trop longues pour être des noms

zaler$char<-0
for(i in 1:nrow(zaler)){zaler$char[i]<-nchar(zaler$Firstauthor[i])}


##### Qualitativement, on voit que aucune info au dessus de 27 caractères correspond à un 
##### auteur, donc on purge

for(i in 1:nrow(zaler)){if(zaler$char[i]>=27){zaler$Firstauthor[i]<-"AB"}else{zaler$Firstauthor[i]<-zaler$Firstauthor[i]}}



##### On passe maintenant à l'extraction des infos sur la publication, qui 
##### s'identifient par le fait qu'elles commencent par une année entre parenthèses
##### de plus, même technique de comparaison que pour les premier auteur plus haut


zaler$publi<-"AB"
zaler$publi<-gsub("^\\([[:digit:]][[:digit:]][[:digit:]][[:digit:]]\\)", "",zaler$V1)
zaler$comp<-zaler$publi==zaler$V1
for(i in 1:nrow(zaler)){if(zaler$comp[i]==T){zaler$publi[i]<-"AB"}else{zaler$publi[i]<-zaler$publi[i]}}



####### On extrait le titre, qui est toujours une ligne au-dessus des infos de publication

zaler$titre<-"AB"
for(i in 2:nrow(zaler)){if(zaler$comp[i]==F){zaler$titre[i]<-zaler$V1[i-1]}else{zaler$titre[i-1]<-zaler$titre[i-1]}}


###### On extrait aussi l'année de publication


zaler$Datepubli<- str_extract_all(zaler$V1, "^\\([[:digit:]][[:digit:]][[:digit:]][[:digit:]]\\)")

zaler$Datepubli[zaler$Datepubli == "character(0)"] <- "AB"


####### On extrait la position des lignes où l'on passe d'un article à un autre
####### ce qui s'observe par le changement d'identifiant unique dans la colonne Cartel


idsbis<- which(zaler$Cartel != dplyr::lag(zaler$Cartel))

idsbis<-as.data.frame(idsbis)

idsbis<-rbind(1,idsbis)



##### Boucle ingénieuse, qui fait un peu pareil que des boucles plus longues que l'on retrouvera 
##### plus tard. L'idée est la suivante: "entre le début de l'utilisation d'un nouvel identifiant
##### unique et la fin de son utilisation, capte la position du premier element qui n'est pas un 
##### AB dans la colonne du premier auteur (le [1] à la fin permet de selectionner le premier element
##### ce qui est super car cela evacue les affiliations ou les abstract qui auraient pu se glisser),
##### capte aussi la position de la ligne dans laquelle toutes les autres infos ont été regroupées
##### identifiées par la position de la date, puis met l'info sur les auteurs sur la même ligne que
##### les autres infos. 
##### Ce qui est pas mal avec cette boucle, c'est qu'on est sur que les infos d'un article ne vont 
##### pas se retrouver dans un autre, car on définit des bornes strictes.  


for(i in 1:(nrow(idsbis)-1)){
  j<-which(zaler$Firstauthor[idsbis[i,1]:(idsbis[i+1,1])]!="AB")[1]
  k<-which(zaler$Datepubli[idsbis[i,1]:(idsbis[i+1,1]+1)]!="AB")[1]
  zaler$Firstauthor[(idsbis[i,1]+k-1)]<-zaler$Firstauthor[(idsbis[i,1]+j-1)]}



##### On ne garde que les lignes avec une date de publi

desco<-zaler[zaler$Datepubli!="AB",]


##### On regarde s'il y a des éléments en double dans les identifiants uniques
##### (ce qui serait tragiques pour le package)

jki<-desco[duplicated(desco$Cartel)|duplicated(desco$Cartel, fromLast=TRUE),]

##### On remarque que les seuls doublons sont des 18888, c'est-à-dire les identifiants 
##### d'articles sans références bibliographiques. On peut donc les supprimer

desco<-desco[desco$Cartel!=18888,]


##### On compare les identifiants uniques de notre base de noeuds avec notre base complete

carlito<-desco$Cartel
carlito<-as.data.frame(carlito)
#comp<-cbind(carlito,ids)


#setdiff(carlito[,1],ids[,1])

########## Les deux colonnes sont pas identiques, problème, on supprime donc un peu plus loin les 4 articles de différence


desco<-desco[,c(2,3,6,7,8)]


##### On sauvegarde la base des noeuds


save(desco, file = "nodes.rda")


##############################################################################
##############################################################################
# Maintenant on fait le traitement des références bibliographiques --------------------
##############################################################################
##############################################################################

#### On ne conserve que les refs biblios, et l'identifiant de l'article original

zal<-zal[zal[,4]==1,]

zal<-zal[,c(1,5)]

#### On supprime les 4 articles qui empêchent de matcher avec les noeuds

zal<-zal[zal$...5!=131191,]
zal<-zal[zal$...5!=148413,]
zal<-zal[zal$...5!=148866,]
zal<-zal[zal$...5!=151086,]


#### La commande ci-dessous détermine le nombre d'identifiant unique des articles originaux
#### Donc en gros, ça permet de vérifier de combien d'articles de la base originale ont à pu 
#### extraire grace à la biblio. 

zera<-unique(zal[,2])

comp<-cbind(carlito,zera)


##############################################################################################
##############################################################################################
# Premier problème: certains articles sont collés sur la même ligne et séparés par un point-virgule
# Ceci-étant, on ne peut pas se baser que sur la présence du point-virgule, car cela casserait
# d'autres articles. 
# Sur regex il a donc fallu faire un truc un peu plus complexe. 
# La structure ci-desssous fonctionne de la façon suivante: "si tu détecte un point-virgule,
# puis du texte (nom auteur), puis une virgule, puis du texte (prenom) puis le combo point + virgule,
# alors tu met un True (c'est en fait que deux articles sont collés)
#
# NB: pour vérifier la validité de la syntaxe, on peut utiliser https://spannbaueradam.shinyapps.io/r_regex_tester/
##############################################################################################
##############################################################################################


zal$Cace<-str_detect (string =zal$V1, pattern = "(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)")


### Lorsque c'est le cas, nouvelle colonne dans laquelle on met le nouvel article

zal$troupinar<-"ABC"

for(i in 1:nrow(zal)){if(zal[i,3]==T){zal[i,4]<-str_extract(string = zal[i,1], pattern ="(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)")
}else{zal[i,4]<-"AB"}}




###### On a donc un truc avec cette tête |(article 1 + 1 bis) collés|ID1|article 1 bis|
######                                   |article 2                 |ID2|"AB"         |

###### Ce qu'on veut
######                                   |article 1                 |ID1|"AB"         |
######                                   |article 1bis              |ID1|"AB"         |
######                                   |article 2                 |ID2|"AB"         |

###### Du coup on utilise la fonction de replication lorsqu'on est dans la situation où 
###### deux articles étaient collés: dès que c'est le cas repetition de la ligne 2 fois et 
###### sinon on garde l'exemplaire unique. 

zal <- zal[rep(row.names(zal), ifelse(zal$troupinar!="AB",2,1)),]

for(i in 1:(nrow(zal)-1)){if(zal[i,3]==T&zal[i+1,3]==T){
  zal[i+1,3]<-0
}else{zal[i+1,3]<-zal[i+1,3]}}



###### On supprime le "1 bis collés" de "article 1 + 1 bis collés" dans la première colonne 

zal$V1<-gsub("(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)", "",zal$V1)

###### On transfere les infos dans les nouvelles lignes créées

for(i in 1:(nrow(zal)-1)){if(zal[i,3]==T&zal[i+1,3]==F){
  zal[i+1,1]<-zal[i+1,4]
}else{zal[i+1,1]<-zal[i+1,1]}}


###### On garde que les colonnes qui nous interesse

zal<-zal[,c(1,2)]

###### On nettoie les point-virgule


for(i in 1:nrow(zal)){
  if(str_sub (zal[i,1], 1,1)==";"){str_sub (zal[i,1], 1,1)<-""}else{zal[i,1]<-zal[i,1]}
}

#####################################################################################
#####################################################################################
# Comme certaines refs étaient collées en bloc, on réitère la boucle pour les séparer 
# au fur et à mesure
#####################################################################################
#####################################################################################


zal$Cace<-str_detect (string =zal$V1, pattern = "(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)")
zal$troupinar<-"ABC"

for(i in 1:nrow(zal)){if(zal[i,3]==T){zal[i,4]<-str_extract(string = zal[i,1], pattern ="(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)")
}else{zal[i,4]<-"AB"}}

zal <- zal[rep(row.names(zal), ifelse(zal$troupinar!="AB",2,1)),]

for(i in 1:(nrow(zal)-1)){if(zal[i,3]==T&zal[i+1,3]==T){
  zal[i+1,3]<-0
}else{zal[i+1,3]<-zal[i+1,3]}}



zal$V1<-gsub("(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)", "",zal$V1)


for(i in 1:(nrow(zal)-1)){if(zal[i,3]==T&zal[i+1,3]==F){
  zal[i+1,1]<-zal[i+1,4]
}else{zal[i+1,1]<-zal[i+1,1]}}


zal<-zal[,c(1,2)]

for(i in 1:nrow(zal)){
  if(str_sub (zal[i,1], 1,1)==";"){str_sub (zal[i,1], 1,1)<-""}else{zal[i,1]<-zal[i,1]}
}

zal$Cace<-str_detect (string =zal$V1, pattern = "(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)")
zal$troupinar<-"ABC"

for(i in 1:nrow(zal)){if(zal[i,3]==T){zal[i,4]<-str_extract(string = zal[i,1], pattern ="(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)")
}else{zal[i,4]<-"AB"}}


zal <- zal[rep(row.names(zal), ifelse(zal$troupinar!="AB",2,1)),]

for(i in 1:(nrow(zal)-1)){if(zal[i,3]==T&zal[i+1,3]==T){
  zal[i+1,3]<-0
}else{zal[i+1,3]<-zal[i+1,3]}}

zal$V1<-gsub("(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)", "",zal$V1)


for(i in 1:(nrow(zal)-1)){if(zal[i,3]==T & zal[i+1,3]==F){
  zal[i+1,1]<-zal[i+1,4]
}else{zal[i+1,1]<-zal[i+1,1]}}


zal<-zal[,c(1,2)]

for(i in 1:nrow(zal)){
  if(str_sub (zal[i,1], 1,1)==";"){str_sub (zal[i,1], 1,1)<-""}else{zal[i,1]<-zal[i,1]}
}


zal$Cace<-str_detect (string =zal$V1, pattern = "(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)")
zal$troupinar<-"ABC"

for(i in 1:nrow(zal)){if(zal[i,3]==T){zal[i,4]<-str_extract(string = zal[i,1], pattern ="(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)")
}else{zal[i,4]<-"AB"}}


zal <- zal[rep(row.names(zal), ifelse(zal$troupinar!="AB",2,1)),]

for(i in 1:(nrow(zal)-1)){if(zal[i,3]==T&zal[i+1,3]==T){
  zal[i+1,3]<-0
}else{zal[i+1,3]<-zal[i+1,3]}}

zal$V1<-gsub("(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)", "",zal$V1)


for(i in 1:(nrow(zal)-1)){if(zal[i,3]==T & zal[i+1,3]==F){
  zal[i+1,1]<-zal[i+1,4]
}else{zal[i+1,1]<-zal[i+1,1]}}


zal<-zal[,c(1,2)]

for(i in 1:nrow(zal)){
  if(str_sub (zal[i,1], 1,1)==";"){str_sub (zal[i,1], 1,1)<-""}else{zal[i,1]<-zal[i,1]}
}

zal$Cace<-str_detect (string =zal$V1, pattern = "(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)")
zal$troupinar<-"ABC"

for(i in 1:nrow(zal)){if(zal[i,3]==T){zal[i,4]<-str_extract(string = zal[i,1], pattern ="(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)")
}else{zal[i,4]<-"AB"}}


zal <- zal[rep(row.names(zal), ifelse(zal$troupinar!="AB",2,1)),]

for(i in 1:(nrow(zal)-1)){if(zal[i,3]==T&zal[i+1,3]==T){
  zal[i+1,3]<-0
}else{zal[i+1,3]<-zal[i+1,3]}}

zal$V1<-gsub("(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)", "",zal$V1)


for(i in 1:(nrow(zal)-1)){if(zal[i,3]==T & zal[i+1,3]==F){
  zal[i+1,1]<-zal[i+1,4]
}else{zal[i+1,1]<-zal[i+1,1]}}


zal<-zal[,c(1,2)]

for(i in 1:nrow(zal)){
  if(str_sub (zal[i,1], 1,1)==";"){str_sub (zal[i,1], 1,1)<-""}else{zal[i,1]<-zal[i,1]}
}

zal$Cace<-str_detect (string =zal$V1, pattern = "(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)")



####### Maintenant on créé eoeoe, sur laquelle on va travailleur par la suite

eoeoe<-zal[,c(1,2,3)]

names(eoeoe)[names(eoeoe) == "...5"] <- "V2"




###### Pour déterminer le premier auteur, on enlève tout ce qu'il y a après la premiere ","

eoeoe<-eoeoe[,c(1,2)]
eoeoe$Firstauthor<-gsub("[A-z0-9]\\.\\,.*", "",eoeoe$V1)


##### On créée une colonne qui dit si la nouvelle colonne créée contient la même info que la précédente

eoeoe$comp<-eoeoe$Firstauthor==eoeoe$V1


###### On enlève aussi la balise de référence


eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "REFERENCES: ", replacement = "")


eoeoe$V1<-str_replace_all(string = eoeoe$V1, pattern = "REFERENCES:", replacement = "")


##### trimws supprime les espaces avant et après

eoeoe$V1<-trimws(eoeoe$V1)

#### On compte les caractère de FirstAuthor pour détecter les anomalies

eoeoe$crapaud<-0
for(i in 1:nrow(eoeoe)){eoeoe$crapaud[i]<-nchar(eoeoe$Firstauthor[i])}

eoeoe<-eoeoe[eoeoe$crapaud!=2 & eoeoe$crapaud!=1,]


#### Certaines refs sont construites différement du coup on adpate le REGEX sur les infos pas traitées avec
#### le REGEX précédent.

eoeoe$stabilo<-"AB"
eoeoe$stabilo[eoeoe$crapaud > 29]<-str_detect (string =eoeoe$V1[eoeoe$crapaud > 29], pattern = "(?:^)(\\w+)\\,")


eoeoe$Firstauthor[eoeoe$stabilo==T]<- str_extract_all(eoeoe$V1[eoeoe$stabilo==T], "(?:^)(\\w+)\\,")


eoeoe$crapaud<-0
for(i in 1:nrow(eoeoe)){eoeoe$crapaud[i]<-nchar(eoeoe$Firstauthor[i])}


#############################################################################################
#############################################################################################
# Attention: Certaines refs construite différement peuvent être collées à d'autres, du coup 
# j'en ai pas tenu compte, mais on pourrait revenir dessus
#############################################################################################
#############################################################################################


######################################################################################
# Enorme souci: je pensais pouvoir détecter les auteurs tranquilement en mode:
# Hendel, L., Lizzeri, A., Interfering with secondary markets (1999)
# on récupère ce qu'il y a avant la première virgule
# seulement, si les auteurs ont écrit plusieurs articles et que les articles sont écrits
# ensemble, alors on a quelque chose comme ça: 
#Hendel, L., Lizzeri, A., Interfering with secondary markets (1999) Rand Journal of Economics, 30, pp. 1-21; 
#Adverse selection in durable goods markets (1999) American Economic Review, 89, pp. 1097-1115; 
#
# Du coup, nécessité de trouver une solution.
######################################################################################

######################################################################################
# Ce que j'ai fait: Les articles qui foirent échouent également à récupérer le nom
# du premier auteur, et récupèrent en général tout le titre. Du coup j'ai créé une colonne
# dans laquelle on compte le nombre de caractères de l'identifiant du premier auteur. 
# Ensuite, on filtre les identifiants à plus de 30 caractères, et on classe la première colonne
# par ordre alphabétique, ce qui permet de corriger à la main les identifiants
######################################################################################

##### Ce nettoyage est stocké sur un autre script

source("Functions_and_cleaning/Cleaningdata.R")


##### Detecte toutes les refs qui commencent par une année, qui correspond aux articles de 
##### journaux et aux rapports. 

eoeoe$annee<-4
for(i in 1:nrow(eoeoe)){if(grepl("^\\([[:digit:]][[:digit:]][[:digit:]][[:digit:]]\\)",str_sub (eoeoe$Firstauthor[i], 1,7))==T){eoeoe$annee[i]<-1}else{eoeoe$annee[i]<-0}}

#### On supprime ces références

eoeoe<-eoeoe[eoeoe$annee==0,]

##### On convertit Frist author au bon format

eoeoe$Firstauthor<-as.character(eoeoe$Firstauthor)

eoeoe$Firstauthor<-trimws(eoeoe$Firstauthor)

##### On transforme les caractères spéciaux (comme les accents) en caractères neutres

eoeoe$Firstauthor<-iconv(eoeoe$Firstauthor,from="UTF-8",to="ASCII//TRANSLIT")

##### On passe les noms en majuscule

eoeoe$Firstauthor<-toupper(eoeoe$Firstauthor)


##### Les auteurs avec des dates sont des erreur, on supprime. Pareil s'ils contiennent des parenthèses


eoeoe$Firstauthor2<- str_detect (string =eoeoe$Firstauthor, pattern = "20")
eoeoe<-eoeoe[eoeoe$Firstauthor2==F,]
eoeoe$Firstauthor2<- str_detect (string =eoeoe$Firstauthor, pattern = "19")
eoeoe<-eoeoe[eoeoe$Firstauthor2==F,]
eoeoe$Firstauthor2<- str_detect (string =eoeoe$Firstauthor, pattern = "\\(")
eoeoe<-eoeoe[eoeoe$Firstauthor2==F,]
eoeoe$Firstauthor2<- str_detect (string =eoeoe$Firstauthor, pattern = "\\)")
eoeoe<-eoeoe[eoeoe$Firstauthor2==F,]

##### On ne garde que le premier auteur

eoeoe$Firstauthor<-gsub("\\,.*", "",eoeoe$Firstauthor)

##### On supprime les espaces avant et après

eoeoe$Firstauthor<-trimws(eoeoe$Firstauthor)

#### On nettoie certaines infos de premier auteur

eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " A\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " B\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " C\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " D\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " E\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " F\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " G\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " H\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " I\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " J\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " K\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " L\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " M\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " N\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " O\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " P\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " Q\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " R\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " S\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " T\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " U\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " V\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " W\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " X\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " Y\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " Z\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " A\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " B\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " C\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " D\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " E\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " F\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " G\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " H\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " I\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " J\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " K\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " L\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " M\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " N\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " O\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " P\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " Q\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " R\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " S\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " T\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " U\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " V\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " W\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " X\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " Y\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " Z\\.", replacement = " ")

eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " III", replacement = " ")


eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " \\. Y", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " \\. W", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " \\. S", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " \\. R", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " \\. M", replacement = " ")

eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " J$", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " II$", replacement = " ")

eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "^A\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "^B\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "^C\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "^D\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "^E\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "^F\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "^G\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "^H\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "^I\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "^J\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "^K\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "^L\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "^M\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "^N\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "^O\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "^P\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "^Q\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "^R\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "^S\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "^T\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "^U\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "^V\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "^W\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "^X\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "^Y\\.", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "^Z\\.", replacement = " ")

eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " JR", replacement = " ")

eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " \\.", replacement = " ")

##### On compte le nombre de mots dans Firstauthor (en comptant les espaces en l'occurrence)

eoeoe$decat<-sapply(strsplit(eoeoe$Firstauthor, " "), length)

##### Plus de quatre mot est nécessairement une erreur

eoeoe<-eoeoe[eoeoe$decat<4,]

##### Deuxième vague de nettoyage de Firstauthor

source("Functions_and_cleaning/Cleaningbis.R")

##### On remplace les tirets par des espaces, et on supprime tous les espaces

eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "\\-", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " ", replacement = "")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "  ", replacement = "")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " ", replacement = "")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "\\-", replacement = " ")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " ", replacement = "")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "  ", replacement = "")
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = " ", replacement = "")

##### On recompte le nombre de mots

eoeoe$decat<-sapply(strsplit(eoeoe$Firstauthor, " "), length)

#### On ne garde que s'il y a plus de 1 mot
eoeoe<-eoeoe[eoeoe$decat>0,]


##### On supprime les espaces avant et après

eoeoe$Firstauthor<-trimws(eoeoe$Firstauthor)

##### On extrait l'information sur le volume (qui est soit un nombre entre deux virgules, soit de la forme ", 5(6),")

eoeoe$Issue<- str_extract_all(eoeoe$V1, "(, \\d{1,9})(,| \\(.*\\),)")
eoeoe$Issue<-as.character(eoeoe$Issue)
eoeoe$Issue[eoeoe$Issue == "character(0)"] <- "AB"
eoeoe<-na.omit(eoeoe)

##### Ceci dit, pas tous ce format, du coup on extrait sur les restants avec un autre REGEX

eoeoe$Issue[eoeoe$Issue == "AB"]<- str_extract_all(eoeoe$V1[eoeoe$Issue == "AB"], "(, \\d{1,9})(,| \\(.*\\)\\.,)")
eoeoe$Issue<-as.character(eoeoe$Issue)
eoeoe$Issue[eoeoe$Issue == "character(0)"] <- "AB"

#### On enlève un problème bien particulier 


eoeoe$Issuent<- str_detect (string =eoeoe$Issue, pattern = "(19\\d{2}|20\\d{2})")
eoeoe$Issuent2<- str_detect (string =eoeoe$V1, pattern = "Brookings")

##### Si mutliple issue on supprime

eoeoe$Issue<-str_replace_all(string = eoeoe$Issue, pattern = "c\\(", replacement = "")
eoeoe$Issue<-str_replace_all(string = eoeoe$Issue, pattern = "\\(.*\\)", replacement = "")
eoeoe$Issue<-str_replace_all(string = eoeoe$Issue, pattern = "\\,", replacement = "")



###### On extrait l'année en détectant les parenthèses qui contiennent quatre chiffres dedans

eoeoe$Datepubli<- str_extract_all(eoeoe$V1, "\\((17|18|19|20)\\d{2}\\)")
eoeoe$Datepubli<-as.character(eoeoe$Datepubli)
eoeoe$Datepubli[eoeoe$Datepubli == "character(0)"] <- "AB"

##### Pas tous ce format, du coup on extrait sur les restants avec un autre REGEX

eoeoe$Datepubli[eoeoe$Datepubli == "AB"]<- str_extract_all(eoeoe$V1[eoeoe$Datepubli == "AB"], "\\, (17|18|19|20)\\d{2}\\)")
eoeoe$Datepubli<-as.character(eoeoe$Datepubli)
eoeoe$Datepubli[eoeoe$Datepubli == "character(0)"] <- "AB"
eoeoe$Datepubli[eoeoe$Datepubli == "AB"]<- str_extract_all(eoeoe$V1[eoeoe$Datepubli == "AB"], "\\. (17|18|19|20)\\d{2}\\.")
eoeoe$Datepubli<-as.character(eoeoe$Datepubli)
eoeoe$Datepubli[eoeoe$Datepubli == "character(0)"] <- "AB"

##### Pas tous ce format, du coup on extrait sur les restants avec un autre REGEX

eoeoe$Datepubli[eoeoe$Datepubli == "AB"]<- str_extract_all(eoeoe$V1[eoeoe$Datepubli == "AB"], "\\, (17|18|19|20)\\d{2}\\.")
eoeoe$Datepubli<-as.character(eoeoe$Datepubli)
eoeoe$Datepubli[eoeoe$Datepubli == "character(0)"] <- "AB"

##### Pas tous ce format, du coup on extrait sur les restants avec un autre REGEX

eoeoe$Datepubli[eoeoe$Datepubli == "AB"]<- str_extract_all(eoeoe$V1[eoeoe$Datepubli == "AB"], "\\, (17|18|19|20)\\d{2}\\,")
eoeoe$Datepubli<-as.character(eoeoe$Datepubli)
eoeoe$Datepubli[eoeoe$Datepubli == "character(0)"] <- "AB"

##### Pas tous ce format, du coup on extrait sur les restants avec un autre REGEX

eoeoe$Datepubli[eoeoe$Datepubli == "AB"]<- str_extract_all(eoeoe$V1[eoeoe$Datepubli == "AB"], "\\, (17|18|19|20)\\d{2}\\;")
eoeoe$Datepubli<-as.character(eoeoe$Datepubli)
eoeoe$Datepubli[eoeoe$Datepubli == "character(0)"] <- "AB"

##### Pas tous ce format, du coup on extrait sur les restants avec un autre REGEX

eoeoe$Datepubli[eoeoe$Datepubli == "AB"]<- str_extract_all(eoeoe$V1[eoeoe$Datepubli == "AB"], "(17|18|19|20)\\d{2}")
eoeoe$Datepubli<-as.character(eoeoe$Datepubli)
eoeoe$Datepubli[eoeoe$Datepubli == "character(0)"] <- "AB"


###### Si plusieurs années extraites, on ne garde que si les différentes années sont identiques. 
###### Du coup on splite dans plusieurs colonnes et on compare les différentes années

eoeoe$Datepubli2<-gsub("\\,.*", "",eoeoe$Datepubli)
eoeoe$Datepubli3<- str_extract_all(eoeoe$Datepubli, '\\,.*')
eoeoe$Datepubli2[eoeoe$Datepubli2 == "character(0)"] <- "AB"
eoeoe$Datepubli3[eoeoe$Datepubli3 == "character(0)"] <- "AB"

eoeoe$Datepubli2<-str_replace_all(string = eoeoe$Datepubli2, pattern = "\\(", replacement = "")
eoeoe$Datepubli2<-str_replace_all(string = eoeoe$Datepubli2, pattern = "\\)", replacement = "")
eoeoe$Datepubli2<-str_replace_all(string = eoeoe$Datepubli2, pattern = "c", replacement = "")
eoeoe$Datepubli2<-str_replace_all(string = eoeoe$Datepubli2, pattern = '\\"', replacement = "")

eoeoe$Datepubli3<-str_replace_all(string = eoeoe$Datepubli3, pattern = "\\(", replacement = "")
eoeoe$Datepubli3<-str_replace_all(string = eoeoe$Datepubli3, pattern = "\\)", replacement = "")
eoeoe$Datepubli3<-str_replace_all(string = eoeoe$Datepubli3, pattern = "c", replacement = "")
eoeoe$Datepubli3<-str_replace_all(string = eoeoe$Datepubli3, pattern = '\\"', replacement = "")
eoeoe$Datepubli3<-str_replace_all(string = eoeoe$Datepubli3, pattern = ',', replacement = "")


eoeoe$Datepubli3<-trimws(eoeoe$Datepubli3)
eoeoe$Datepubli2<-trimws(eoeoe$Datepubli2)

#### Si ce sont les mêmes années, alors on remplace les années multiples par une unique année


for(i in 1:nrow(eoeoe)){
  if (eoeoe$Datepubli3[i]==eoeoe$Datepubli2[i] & eoeoe$Datepubli3[i]!="AB" & eoeoe$Datepubli2[i]!= "AB")
  {eoeoe$Datepubli[i]<-eoeoe$Datepubli3[i]}
  else{eoeoe$Datepubli[i]<-eoeoe$Datepubli[i]}}




#################################################################################################
# Je supprime les observations avec des dates mutliples restantes (pas optimal)
#################################################################################################

eoeoe$Datepubli<-gsub("c\\(.*", "AB",eoeoe$Datepubli)


###### On détecte les pages, soit au format "pp." ou au format "p.", et on garde ce qu'il y a apres.
###### "\\d" correspond à des informations numériques


eoeoe$Page<- str_extract_all(eoeoe$V1, " pp.+\\d")
eoeoe$Page2<- str_extract_all(eoeoe$V1, ", p\\..*")


#### L'existence de chaîne de caractères nulles pose des pbs, du coup on les remplace

eoeoe$Page<-as.character(eoeoe$Page)
eoeoe$Page[eoeoe$Page == "character(0)"] <- "AB"
eoeoe$Page2[eoeoe$Page2 == "character(0)"] <- "AB"



###### On fusionne les deux formats de pages (pp. et p.) stockées dans deux colonnes
###### au sein d'une même colonne


for(i in 1:nrow(eoeoe)){
  if(str_sub (eoeoe$Page2[i], 1,4)==", p."){eoeoe$Page[i]<-eoeoe$Page2[i]}else{eoeoe$Page[i]<-eoeoe$Page[i]}
}


##### On ne garde que l'information sur la première page en supprimant ce qu'il y a après

eoeoe$Page<-gsub("-.*", "",eoeoe$Page)
eoeoe$Page<-gsub(".,.*", "",eoeoe$Page)


##### On supprime aussi ce qu'il y a avant

eoeoe$Page<-str_replace(string = eoeoe$Page, pattern = "pp. ", replacement = "")
eoeoe$Page<-str_replace(string = eoeoe$Page, pattern = ", p. ", replacement = "")


###### On enlève les guillemets des issues (parfois plein, donc répétition de l'opération)

eoeoe$Issue<-str_replace(string = eoeoe$Issue, pattern = '\\"', replacement = "")
eoeoe$Issue<-str_replace(string = eoeoe$Issue, pattern = '\\"', replacement = "")
eoeoe$Issue<-str_replace(string = eoeoe$Issue, pattern = '\\"', replacement = "")
eoeoe$Issue<-str_replace(string = eoeoe$Issue, pattern = '\\"', replacement = "")
eoeoe$Issue<-str_replace(string = eoeoe$Issue, pattern = '\\"', replacement = "")
eoeoe$Issue<-str_replace(string = eoeoe$Issue, pattern = '\\"', replacement = "")


#### trimws supprime les espace au début et à la fin des chaines de caractères


eoeoe$Firstauthor<-trimws(eoeoe$Firstauthor)
eoeoe$Datepubli<-trimws(eoeoe$Datepubli)
eoeoe$Issue<-trimws(eoeoe$Issue)
eoeoe$Page<-trimws(eoeoe$Page)



##### On garde uniquement les colonnes d'intérêt

eoeoe<-eoeoe[,c(1,2,3,11,14,17)]

# On nettoie les points-virgules

eoeoe$Page<-str_replace(string = eoeoe$Page, pattern = ";", replacement = "")


eoeoe$Firstauthor<-trimws(eoeoe$Firstauthor)
eoeoe$Datepubli<-trimws(eoeoe$Datepubli)
eoeoe$Issue<-trimws(eoeoe$Issue)
eoeoe$Page<-trimws(eoeoe$Page)


##### On créer une colonne qui fusionne les différentes informations, permettant de créer 
##### un identifiant unique pour les refs biblio

eoeoe$identifiant<-"AB"
for(i in 1:nrow(eoeoe)){
  eoeoe$identifiant[i]<-paste(eoeoe$Firstauthor[i],eoeoe$Datepubli[i],eoeoe$Issue[i],eoeoe$Page[i])}

# On nettoie les points-virgules

eoeoe$identifiant<-str_replace(string = eoeoe$identifiant, pattern = ";", replacement = "")

eoeoe$V1<-str_replace(string = eoeoe$V1, pattern = "REFERENCES: ", replacement = "")



eoeoe$identifiant<-trimws(eoeoe$identifiant)

# On supprime un identifiant à la fois très présent et ne représentant rien

eoeoe<-eoeoe[eoeoe$identifiant!="NOTE AB AB AB",]



##### We save the file before the long cleaning of the ID


save(eoeoe, file = "Core.rda")





