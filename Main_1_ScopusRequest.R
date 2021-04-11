# Package used

library(readr)
library(stringr)
library(tidyverse)
library(biblionetwork)


# Set Working Directory

setwd("C:/Users/jgrad/Desktop/Network_Quality/Network_Quality/Data")

#Importer le fichier issu de EconLit

z <- read_delim("C:/Users/jgrad/Desktop/Network_Quality/Network_Quality/Data/Data_1_EconLit.txt", "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)


# Détecter la balise délimitant le titre des articles
# La fonction str_sub extrait les caractères d'une chaine en commençant par un point de départ (le premier ici)
# et un point d'arrivée (le troisième). 
# Ici, on teste donc si les trois premiers caractères d'une chaine correspondent à une balise, et on applique une condition

z[,2]<-"AB"
for(i in 1:nrow(z)){
  if(str_sub (z[i,1], 1,3)=="TI-"){z[i,2]<-"OK"}else{z[i,2]<-"NOK"}}


### Compter le nombre d'articles dans notre base (si 5023 c'est gagné)

table(z[,2])

# Détecter la balise délimitant les DOI

z[,3]<-"AB"
for(i in 1:nrow(z)){
  if(str_sub (z[i,1], 1,3)=="DI-"){z[i,3]<-"OK"}else{z[i,3]<-"NOK"}}


# Détecter la balise délimitant les informations sur la publication du document

z[,4]<-"AB"
for(i in 1:nrow(z)){
  if(str_sub (z[i,1], 1,3)=="SO-"){z[i,4]<-"OK"}else{z[i,4]<-"NOK"}}


# Détecter la balise délimitant les auteurs


z[,5]<-"AB"
for(i in 1:nrow(z)){
  if(str_sub (z[i,1], 1,3)=="AU-"){z[i,5]<-"OK"}else{z[i,5]<-"NOK"}}


# Détecter la balise détérminant le type de publication

z[,6]<-"AB"
for(i in 1:nrow(z)){
  if(str_sub (z[i,1], 1,3)=="PT-"){z[i,6]<-z[i,1]}else{z[i,6]<-"NOK"}}

#### Pour garder uniquement le nom du premier auteur, on supprime ce qu'il y a après la 
#### première virgule rencontrée : "," reconnait la virgule et ".*" dit de considérer tout
#### ce qu'il y a après, en remplaçant par une virgule).

#### NB: Cette synthaxe correspond à une REGULAR EXPRESSION (REGEX)


z[,7]<-"AB"
for(i in 1:nrow(z)){
  if(z[i,5]=="OK"){
    z[i,7]<-gsub(",.*", ",", z[i,1])} else{z[i,7]<-"AB"}}


#### Regarder la tête du résultat

Zelote<-z[z[,7]!="AB",7]

#### NB: La longueur de Zelote nous permet de savoir combien d'articles ont un auteur (ici 5019)

#### NB: Les tirets dans les noms ne posent pas de soucis sur Scopus (Bar-Isaac)
#### NB: pareil pour "de" (de palma) et "von" (von Beer)
#### NB: comme scopus passe tout en minsucule, pas de pb non plus sur toutes les lettres sont en majs



#### Intégrer le premier auteur dans la ligne sur les informations bibliographiques
#### (qui est en général deux lignes plus loin que l'info sur les auteurs)


#### Idée de la boucle: Dès que tu croises un "ok" sur la cinquième colonne (indiquant un premier
#### auteur), tu commences à regarder dans la quatrième colonne jusqu'à ce que tu repère un "ok"
#### (indiquant les informations sur la publication, généralement deux lignes en dessous), puis
#### tu récupères cette information que tu remplaces par le premier auteur dans une nouvelle colonne
#### et sur la même ligne que l'information initiale sur la publication. 
#### Sinon, tu marques "AB" ou "ABC" en fonction des cas.
#### NB: "1:min(20,nrow(z)-i)" permet de ne pas avoir d'erreurs sur les 20 dernières lignes, car il  
#### y a des valeurs manquantes sinon. Par ailleurs, 20 est choisit arbitrairement


z[,8]<-"AB"
for(i in 1: (nrow(z))){
  j<-0
  if(z[i,5]=="OK"){
    for(j in 1:min(20,nrow(z)-i)){
      if(z[i+j,4]=="NOK"){z[i+j,8]<-"ABC"}else{
        z[i+j,8]<-paste(z[i,7],z[i+j,1])}}
  }else{z[i+j,8]<-z[i+j,8]}
}


#### Voir la tête du résultat

zelote3<-z[z[,8]!="ABC"&z[,8]!="AB",]
zelote3<-zelote3[,c(8)]


#### Même idée mais pour mettre le type de publication au même niveau que les autres infos

for(i in 1:nrow(z)){if(z[i,6]=="NOK"){z[i,6]<-"PT- NOK"} else{z[i,6]<-z[i,6]}}


z[,9]<-"AB"
for(i in 1: (nrow(z))){
  j<-0
  if(z[i,4]=="OK"){
    for(j in 1:min(20,nrow(z)-i)){
      if(z[i+j,6]=="PT- NOK"){z[i+j,9]<-"ABC"}else{
        z[i,9]<-z[i+j,6]}}
  }else{z[i+j,9]<-z[i+j,9]}
}


#### Voir la tête du résultat

zelote2<-z[z[,4]=="OK",]
zelote2<-zelote2[,c(8,9)]



# Garder uniquement les informations sur la publication

z4<-z[z[,4]=="OK",]
z4<-z4[,c(8,9)]


# Voir comment se repartissent le type de publication 

table(z4[,2])


#### NB pas de possibilité de chercher les working papers ou les dissertation sur SCOPUS,
#### on se passe donc d'environ 600 informations. On va aussi enlever les book review (4), 
#### les non-étiquetés (6), et garder uniquement les articles, les chapitres et les livres



#################################################################################################
#################################################################################################
# Travail sur les articles ----------------------------------------------------------
#################################################################################################
#################################################################################################

##### On ne garde que les items avec la balise d'article

z4Article <- z4[z4[,2]=="PT- Journal Article",1]

#### NB: 3239 articles


#### On supprime une revue croate qui ne nous servira pas (et qui avait une virgule dans son titre
#### ce qui posait problème au moment de spliter)

z4Article[,2]<-234
z4Article[,2]<-str_detect (string = z4Article$...8, pattern = "Croatia")
table(z4Article[,2])


z4Article<-z4Article[z4Article[,2]==F,]
z4Article<-z4Article[,1]

z4Article<-as.data.frame(z4Article)


#### Pareil pour la revue "OR spectrum", dont le "OR" posait des pbs de requete sur SCOPUS

z4Article[,2]<-234
z4Article[,2]<-str_detect (string = z4Article$...8, pattern = "OR Spectrum")
table(z4Article[,2])


z4Article<-z4Article[z4Article[,2]==F,]
z4Article<-z4Article[,1]


#### On splite la ligne contenant les informations sur la publication, séparées par des virgules,
#### ce qui permet d'isoler les différents éléments. 
#### DIt autrement, la fonction ci-dessous met le texte dans une nouvelle ligne dès qu'elle croise une virgule

s <- strsplit(z4Article, split = ",")

eoeoe<-unlist(s)

eoeoe<-as.data.frame(eoeoe)

#####################################################################
###### Transformer ces informations en requêtes pour SCOPUS
#####################################################################
#####################################################################
#####  Type de requête attendue: ( SRCTITLE( European Review) AND PUBYEAR = 2020 AND VOLUME(47) 
#####  AND PAGEFIRST( 1861 ) ) OR ( SRCTITLE( Comparative Economic Studies ) AND PUBYEAR = 2020 
#####  AND VOLUME( 62 ) AND PAGEFIRST( 632 ) )
#####  OU
#####  Type de requête attendue: ( AUTH(Akerlof) AND PUBYEAR = 2020 AND VOLUME(47) 
#####  AND PAGEFIRST( 1861 ) ) OR ( AUTH(Klein) AND PUBYEAR = 2020 
#####  AND VOLUME( 62 ) AND PAGEFIRST( 632 ) )
#####  Et ce avec le maximum d'articles possibles
#####################################################################


##### On détecte les informations sur les volumes de publication (" v. ")

eoeoe[,2]<-"AB"
for(i in 1:nrow(eoeoe)){
  if(str_sub (eoeoe[i,1], 1,4)==" v. "){eoeoe[i,2]<-4}else{eoeoe[i,2]<-eoeoe[i,2]}
}


##### On détecte les informations sur les issues (" iis. ")

for(i in 1:nrow(eoeoe)){
  if(str_sub (eoeoe[i,1], 1,6)==" iss. "){eoeoe[i,2]<-5}else{eoeoe[i,2]<-eoeoe[i,2]}
}


#### On détecte les informations sur les pages " pp. "

for(i in 1:nrow(eoeoe)){
  if(str_sub (eoeoe[i,1], 1,5)==" pp. "){eoeoe[i,2]<-6}else{eoeoe[i,2]<-eoeoe[i,2]}
}


#### On detecte les informations sur les années de publication (mais plein de formats différents,
#### donc besoin d'harmoniser en supprimant les saisons/mois/"Special Issue")

for(i in 1:nrow(eoeoe)){
  if(str_sub (eoeoe[i,1], 1,4)==" Jan" |str_sub (eoeoe[i,1], 1,4)==" 3rd"|str_sub (eoeoe[i,1], 1,6)==" Suppl" |str_sub (eoeoe[i,1], 1,5)==" Part" |str_sub (eoeoe[i,1], 1,4)==" 4th" |str_sub (eoeoe[i,1], 1,4)==" Spe" |str_sub (eoeoe[i,1], 1,4)==" 1st" |str_sub (eoeoe[i,1], 1,4)==" 2nd" |str_sub (eoeoe[i,1], 1,4)==" Aut" |str_sub (eoeoe[i,1], 1,5)==" Spe" |str_sub (eoeoe[i,1], 1,4)==" Sum"|str_sub (eoeoe[i,1], 1,4)==" Win"|str_sub (eoeoe[i,1], 1,4)==" Fal"|str_sub (eoeoe[i,1], 1,4)==" Spr" |str_sub (eoeoe[i,1], 1,4)==" Feb" |str_sub (eoeoe[i,1], 1,4)==" Mar" |str_sub (eoeoe[i,1], 1,4)==" Apr" |str_sub (eoeoe[i,1], 1,4)==" May" |str_sub (eoeoe[i,1], 1,4)==" Jun" |str_sub (eoeoe[i,1], 1,4)==" Jul" |str_sub (eoeoe[i,1], 1,4)==" Aug" |str_sub (eoeoe[i,1], 1,4)==" Sep" |str_sub (eoeoe[i,1], 1,4)==" Oct" |str_sub (eoeoe[i,1], 1,4)==" Nov" |str_sub (eoeoe[i,1], 1,4)==" Dec" |str_sub (eoeoe[i,1], 1,4)==" 200" |str_sub (eoeoe[i,1], 1,4)==" 201" |str_sub (eoeoe[i,1], 1,4)==" 199"|str_sub (eoeoe[i,1], 1,4)==" 202"){eoeoe[i,2]<-3}else{eoeoe[i,2]<-eoeoe[i,2]}
}


#### Nom des revues 

for(i in 1:nrow(eoeoe)){
  if(str_sub (eoeoe[i,1], 1,5)==" SO- "){eoeoe[i,2]<-2}else{eoeoe[i,2]<-eoeoe[i,2]}
}


#### Information sur le premier auteur

for(i in 1:nrow(eoeoe)){
  if(str_sub (eoeoe[i,1], 1,4)=="AU- "){eoeoe[i,2]<-1}else{eoeoe[i,2]<-eoeoe[i,2]}
}


##### Pour savoir combien d'éléments de la base d'origine on a pu traiter

table(eoeoe[,2])

##### Pour l'info des auteurs, aucune perte dans la base, un peu pour les autres infos.
##### Comme on va faire une recherche à partir des colonnes 1,3,4,6 et que min(1,3,4,6))=3058, on
##### peut espérer soumettre 3058 requetes.


#############################################################################################
#############################################################################################
# Attention: Il y aurait toujours du travail à faire pour nettoyer les "AB" restants, mais il 
# reste quasiment plus rien, c'est négligeable et on peut poursuivre
#############################################################################################
#############################################################################################


#### On nettoie les infos sur les revues en supprimant la balise

eoeoe[eoeoe$V2==2,1]<-str_replace(string = eoeoe[eoeoe$V2==2,1], pattern = " SO- ", replacement = "")


#### Pareil pour les auteurs

eoeoe[eoeoe$V2==1,1]<-str_replace(string = eoeoe[eoeoe$V2==1,1], pattern = "AU- ", replacement = "")


#### De plus, pour les requetes SCOPUS, il ne faut pas que le titre de la revue possède de parenthèses
#### Pour mettre des parenthèses dans les requêtes textuelles, il faut mettre un \\ avant
#### Gsub cherche et remplace

eoeoe[eoeoe$V2==2,1]<-gsub("\\(", "", eoeoe[eoeoe$V2==2,1])
eoeoe[eoeoe$V2==2,1]<-gsub("\\)", "", eoeoe[eoeoe$V2==2,1])


#### Pareil pour les auteurs

eoeoe[eoeoe$V2==1,1]<-gsub("\\(", "", eoeoe[eoeoe$V2==1,1])
eoeoe[eoeoe$V2==1,1]<-gsub("\\)", "", eoeoe[eoeoe$V2==1,1])


#### On enlève les balises restantes pour les autres colonnes

eoeoe[eoeoe$V2==4,1]<-str_replace(string = eoeoe[eoeoe$V2==4,1], pattern = " v. ", replacement = "")
eoeoe[eoeoe$V2==5,1]<-str_replace(string = eoeoe[eoeoe$V2==5,1], pattern = " iss. ", replacement = "")
eoeoe[eoeoe$V2==6,1]<-str_replace(string = eoeoe[eoeoe$V2==6,1], pattern = " pp. ", replacement = "")


##### On fait un point sur le nombre d'éléments conservés

table(eoeoe[,2])


##### min(1,3,4,6) est toujours à 3058, rien n'a bougé du coup


#### Comme on ne garde que l'information sur première page pour faire la requete SCOPUS, 
#### on supprime ce qu'il y a après le tiret ( detecte "-" et prend en compte tout ce qu'il
#### y a après grace au ".*", pour le remplacer par du vide.

eoeoe[eoeoe$V2==6,1]<-gsub("-.*", "",eoeoe[eoeoe$V2==6,1])


#### Comme certaines dates commencent par un mois, une saison, et que toutes se finissent
#### par une année, on garde uniquement les quatre derniers caractères des lignes de dates.


eoeoe[eoeoe$V2==3,1]<- str_sub(eoeoe[eoeoe$V2==3,1], start = -4, end = -1)


#### Pour regarder que cette technique fonctionne

table(eoeoe[eoeoe$V2==3,1])


#### On voit que seuls cinq items posent problèmes, donc pas de soucis


###### On entoure les lignes des balises nécessaires pour faire les requetes sur SCOPUS
###### En adaptant évidemment la balise à la nature de l'information demandée.


eoeoe[eoeoe$V2==1,1]<-paste("AUTHLASTNAME(",eoeoe[eoeoe$V2==1,1],")") 
eoeoe[eoeoe$V2==2,1]<-paste("SRCTITLE(",eoeoe[eoeoe$V2==2,1],")") 
eoeoe[eoeoe$V2==3,1]<-paste("PUBYEAR =",eoeoe[eoeoe$V2==3,1]) 
eoeoe[eoeoe$V2==4,1]<-paste("VOLUME(",eoeoe[eoeoe$V2==4,1],")") 
eoeoe[eoeoe$V2==6,1]<-paste("PAGEFIRST(",eoeoe[eoeoe$V2==6,1],")")


#### Désormais, on garde uniquement les colonnes qui vont nous intéresser pour la requête

#### premier auteur/ année/ volume/page

Timeo<-eoeoe[eoeoe$V2==1|eoeoe$V2==3|eoeoe$V2==4|eoeoe$V2==6,]


##### On fait un point sur le nombre d'éléments conservés

table(Timeo[,2])


##### Aucune perte


##### On fait une boucle pour que lorsqu'on dispose des 4 infos, on génère une chaine de
##### caractère compatible avec Scopus et associant les 4 infos
##### exemple:  (AUTH(Akerlof) AND PUBYEAR = 2020 AND VOLUME(47) 
##### AND PAGEFIRST( 1861 ) ) OR ( AUTH(Klein) AND PUBYEAR = 2020 
##### AND VOLUME( 62 ) AND PAGEFIRST( 632 ) )

for(i in 1:nrow(Timeo)){
  if(Timeo[i,2]==1 & Timeo[i+1,2]==3 &Timeo[i+2,2]==4 &Timeo[i+3,2]==6){
    Timeo[i+3,1]<-paste("(",Timeo[i,1],"AND", Timeo[i+1,1],"AND", Timeo[i+2,1],"AND", Timeo[i+3,1],")")
    Timeo[i+3,2]<-9
  }else{Timeo[i,1]<-Timeo[i,1]}}


##### On fait un point sur le nombre d'éléments conservés

table(Timeo[,2])

##### On voit que nous avons une requête complète pour 2964 articles


#### On ne garde que les articles pour lesquels nous disposons des quatre informations

Timeo<-Timeo[Timeo[,2]==9,]
Timeo<-Timeo[,1]
Timeo<-as.data.frame(Timeo)


##### On aggrège maintenant l'ensemble de ces chaines de caractères pour 
##### générer la requete sur SCOPUS

for(i in 1:nrow(Timeo)){
  Timeo[i+1,]<- paste(Timeo[i,],"OR", Timeo[i+1,])
}


##### On stocke cette chaîne de caractères 

mot<-Timeo[nrow(Timeo),]


#On enregistre la requête sur le PC


write.table(mot,file="SCOPUSrequest.txt", sep = ";", row.names=FALSE) 


#####################################################################
# Bilan: 3239 articles à la base, 2346 matchs sur SCOPUS (72%)
# Attention: SCOPUS ne laisse exporter que 2000 résultats max
# Du coup, j'ai coupé à "après 2003" (1968 résultats), "2003" (44 résultats)
# et "avant 2003" (334). Immédiatemment après l'exportation, j'ai réassemblé les trois bases
# sur un même fichier txt. 
#####################################################################



#######################################################################################
#######################################################################################
# Travail sur les chapitres d'ouvrage ----------------------------------------------------
#######################################################################################
#######################################################################################


#1138 Chapitres d'ouvrages dans la base initiallement 


# Détecter la balise délimitant les éditeurs des ouvrages

z[,10]<-"AB"
for(i in 1:nrow(z)){
  if(str_sub (z[i,1], 1,3)=="ED-"){z[i,10]<-z[i,1]}else{z[i,10]<-"NOK"}}


# Pour mettre l'éditeur au même niveau que les autres infos.

z[,11]<-"AB"
for(i in 1: (nrow(z))){
  j<-0
  if(z[i,4]=="OK"){
    for(j in 1:min(20,nrow(z)-i)){
      if(z[i+j,10]=="NOK"){z[i+j,11]<-"ABC"}else{
        z[i,11]<-z[i+j,10]}}
  }else{z[i+j,11]<-z[i+j,11]}
}



##### On ne garde que la ligne avec l'ensemble des infos, et les colonnes d'interet

z4<-z[z[,4]=="OK",]
z4<-z4[,c(8,9,11)]


##### On se concentre sur les chapitres d'ouvrage

z4Chapter <- z4[z4[,2]=="PT- Collective Volume Article",c(1,3)]


##### La longueur de la base est 1138, tout est bon


##### Boucle pour ajouter la balise editeur quand elle n'y est pas (pour faciliter la 
##### séparation ensuite)

for(i in 1:nrow(z4Chapter)){if(str_sub (z4Chapter[i,2], 1,3)!="ED-"){z4Chapter[i,2]<-paste("ED-",z4Chapter[i,2])} else{z4Chapter[i,2]<-z4Chapter[i,2]}}


##### Boucle pour garder le premier nom de l'éditeur

z4Chapter[,3]<-"AB"
for(i in 1:nrow(z4Chapter)){z4Chapter[i,3]<-gsub(",.*", "", z4Chapter[i,2])}


##### Combiner toutes les infos dans une même colonne

for(i in 1:nrow(z4Chapter)){z4Chapter[i,1]<-paste(z4Chapter[i,3], ",",z4Chapter[i,1])} 

z4Chapter<-z4Chapter[,1]


#### On splite la ligne contenant les informations sur la publication, séparées par des virgules,
#### ce qui permet d'isoler les différents éléments. 

z4Chapter<-as.character(z4Chapter)

s <- strsplit(z4Chapter, split = ",")

eoeoe<-unlist(s)

eoeoe<-as.data.frame(eoeoe)


#### On fait le point pour voir si on a perdu des infos

table(str_detect (string = eoeoe[,1], pattern = "ED-"))


#### On détecte les informations sur les pages " pp. "

eoeoe[,2]<-"AB"
for(i in 1:nrow(eoeoe)){
  if(str_sub (eoeoe[i,1], 1,5)==" pp. "){eoeoe[i,2]<-5}else{eoeoe[i,2]<-eoeoe[i,2]}
}


#### Pareil pour les années

for(i in 1:nrow(eoeoe)){
  if(str_sub (eoeoe[i,1], 1,4)==" 200" |str_sub (eoeoe[i,1], 1,4)==" 201" |str_sub (eoeoe[i,1], 1,4)==" 199"|str_sub (eoeoe[i,1], 1,4)==" 202"){eoeoe[i,2]<-4}else{eoeoe[i,2]<-eoeoe[i,2]}
}



#### Pareil auteurs

for(i in 1:nrow(eoeoe)){
  if(str_sub (eoeoe[i,1], 1,4)==" AU-"){eoeoe[i,2]<-2}else{eoeoe[i,2]<-eoeoe[i,2]}
}


#### Pareil éditeurs

for(i in 1:nrow(eoeoe)){
  if(str_sub (eoeoe[i,1], 1,6)==' \"ED- '|str_sub (eoeoe[i,1], 1,7)==' \n\"ED- '){eoeoe[i,2]<-1}else{eoeoe[i,2]<-eoeoe[i,2]}
}


#### Pareil titre

for(i in 1:nrow(eoeoe)){
  if(str_sub (eoeoe[i,1], 1,4)==" SO-"){eoeoe[i,2]<-3}else{eoeoe[i,2]<-eoeoe[i,2]}
}


##### Modif manuelle pour la première ligne

eoeoe[1,1]<-str_replace(string = eoeoe[1,1], pattern = "c\\(", replacement = "")
eoeoe[1,2]<-1


#### On fait le point pour voir si on a perdu des infos

table(str_detect (string = eoeoe[,1], pattern = "ED-"))



#### On ne garde que les infos qu'on a pu matcher (pas mal de pertes, du aux titres de livres
#### qui contenaient des virgules)


eoeoe<-eoeoe[eoeoe[,2]==1|eoeoe[,2]==2|eoeoe[,2]==3|eoeoe[,2]==4|eoeoe[,2]==5,]


##### On nettoie les pages et les autres informations

eoeoe[eoeoe$V2==5,1]<-gsub("-.*", "",eoeoe[eoeoe$V2==5,1])

eoeoe[eoeoe$V2==5,1]<-str_replace(string = eoeoe[eoeoe$V2==5,1], pattern = " pp. ", replacement = "")

eoeoe[eoeoe$V2==2,1]<-str_replace(string = eoeoe[eoeoe$V2==2,1], pattern = "AU- ", replacement = "")

eoeoe[eoeoe$V2==1,1]<-str_replace(string = eoeoe[eoeoe$V2==1,1], pattern = ' \"ED- ', replacement = "")

eoeoe[eoeoe$V2==1,1]<-str_replace(string = eoeoe[eoeoe$V2==1,1], pattern = ' \n\"ED- ', replacement = "")

eoeoe[eoeoe$V2==3,1]<-str_replace(string = eoeoe[eoeoe$V2==3,1], pattern = "SO- ", replacement = "")



eoeoe[1,1]<-"Tremblay"

#### On fait un point

table(eoeoe[,2])


#### On enlève les parenthèses de toutes les infos, qui posent des problèmes de requête sur SCOPUS 

eoeoe[,3]<-"AB"
eoeoe[eoeoe[,2]==4,3]<-str_detect (string = eoeoe[eoeoe[,2]==4,1], pattern = "\\)")

eoeoe[eoeoe[,3]==T,1]<-str_replace(string = eoeoe[eoeoe[,3]==T,1], pattern = "\\)", replacement = "")

eoeoe[eoeoe[,3]==T,1]<-str_replace(string = eoeoe[eoeoe[,3]==T,1], pattern = "2013.", replacement = "2013")

eoeoe[eoeoe$V2==3,1]<-gsub("\\(", "", eoeoe[eoeoe$V2==3,1])
eoeoe[eoeoe$V2==3,1]<-gsub("\\)", "", eoeoe[eoeoe$V2==3,1])

eoeoe<-eoeoe[,c(1,2)]


#### Modification manuelle de la dernière ligne 

eoeoe[5826,1]<-69


#### Travail sur les titres de bouquins
##### On enleve la suite lorsqu'il y a des points ou des doubles-point

eoeoe[eoeoe[,2]==3,1]<-gsub("\\..*", "",eoeoe[eoeoe[,2]==3,1])
eoeoe[eoeoe[,2]==3,1]<-gsub(":.*", "",eoeoe[eoeoe[,2]==3,1])

#### On ne garde que les titres de bouquins pour les regarder de plus près

zerak <- eoeoe[eoeoe[,2]==3,1]
zerak<-as.data.frame(zerak)


###### On entoure les lignes des balises nécessaires pour faire les requetes sur SCOPUS
###### En adaptant évidemment la balise à la nature de l'information demandée.

eoeoe[eoeoe$V2==1,1]<-paste("EDLASTNAME(",eoeoe[eoeoe$V2==1,1],")") 
eoeoe[eoeoe$V2==2,1]<-paste("AUTHLASTNAME(",eoeoe[eoeoe$V2==2,1],")") 
eoeoe[eoeoe$V2==3,1]<-paste("SRCTITLE(",eoeoe[eoeoe$V2==3,1],")")
eoeoe[eoeoe$V2==4,1]<-paste("PUBYEAR =",eoeoe[eoeoe$V2==4,1]) 
eoeoe[eoeoe$V2==5,1]<-paste("PAGEFIRST(",eoeoe[eoeoe$V2==5,1],")") 


table(eoeoe[,2])



#### Désormais, on garde uniquement les colonnes qui vont nous intéresser pour la requête

#### Si premier auteur/ titre livre/ année/first page

Timeo<-eoeoe[eoeoe$V2==2|eoeoe$V2==3|eoeoe$V2==4|eoeoe$V2==5,]


##### On fait un point sur le nombre d'éléments conservés

table(Timeo[,2])

##### Au max 857 requete (les éditeurs tirent vers le bas), ce qui fait pas mal de perte
##### Peut-être privilégier les premières pages aux éditeurs



##### On fait une boucle pour que lorsqu'on dispose des 4 infos, on génère une chaine de
##### caractère compatible avec Scopus et associant les 4 infos
##### exemple:  (AUTH(Akerlof) AND PUBYEAR = 2020 AND VOLUME(47) 
##### AND PAGEFIRST( 1861 ) ) OR ( AUTH(Klein) AND PUBYEAR = 2020 
##### AND VOLUME( 62 ) AND PAGEFIRST( 632 ) )

for(i in 1:nrow(Timeo)){
  if(Timeo[i,2]==2 & Timeo[i+1,2]==3 &Timeo[i+2,2]==4&Timeo[i+3,2]==5){
    Timeo[i+3,1]<-paste("(",Timeo[i,1],"AND", Timeo[i+1,1],"AND", Timeo[i+2,1],"AND", Timeo[i+3,1],")")
    Timeo[i+3,2]<-9
  }else{Timeo[i,1]<-Timeo[i,1]}}


##### On fait un point sur le nombre d'éléments conservés

table(Timeo[,2])

##### On voit que nous avons une requête complète pour 974 articles


#### On ne garde que les quatre informations

Timeo<-Timeo[Timeo[,2]==9,]
Timeo<-Timeo[,1]
Timeo<-as.data.frame(Timeo)


##### On aggrège maintenant l'ensemble de ces chaines de caractères pour 
##### générer la requete sur SCOPUS

for(i in 1:nrow(Timeo)){
  Timeo[i+1,]<- paste(Timeo[i,],"OR", Timeo[i+1,])
}



##### On stocke cette chaîne de caractères 

mot<-Timeo[nrow(Timeo),]


###### il y a deux pages qui contiennent un "\ que j'ai suppr manuellement, mais à faire sinon
###### scopus bloque. Pareil, tout à la fin, une parenthèse en trop


#On enregistre la requête sur le PC


write.table(mot,file="SCOPUSrequestchapter.txt", sep = ";", row.names=FALSE) 



#####################################################################
# Bilan 975 requetes, mais seulement 226 résultats
#####################################################################

