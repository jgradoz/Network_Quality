#####################################################################################################################
#####################################################################################################################
# Coupling Co-citation 2010-2019 -------------------
#####################################################################################################################
#####################################################################################################################


#####################################################################################################################
#####################################################################################################################
# Bibliographic Coupling 2010-2019----------------------------------------------------
#####################################################################################################################
#####################################################################################################################


##### On a une colonne d'identifiant unique des articles et une colonne avec les refs biblios
##### recodées, on peut donc soumettre ça au package d'Aurelien

set.seed(12)


setwd("C:/Users/jgrad/Desktop/Network_Quality/Network_Quality/Data")

load("Data_5_nodes.rda")
load("Data_7_finalbase.rda")

library(readr)
library(stringr)
#library(bibliometrix)
library(tidyverse)
library(biblionetwork)
#library(devtools)
#devtools::install_github("agoutsmedt/biblionetwork")


eoeoe2<-eoeoe

simple<-unique(eoeoe2$V2)
simple<-as.data.frame(simple)

setdiff(desco$Cartel,simple[,1])

desco<-desco[desco$Cartel!=108237,]


comp<-cbind(simple,desco$Cartel)




desco$sloubi<-str_detect (string = desco$Datepubli, pattern = "2010|2011|2012|2013|2014|2015|2016|2017|2018|2019")
#desco$sloubi<-str_detect (string = desco$Datepubli, pattern = "1970|1971|1972|1973|1974|1975|1976|1977|1978|1979|1980|1981|1982|1983|1984|1985|1986|1987|1988|1989|1990|1991|1992|1993|1994|1995|1996|1997|1998|1990|2000")

desco<-desco[desco$sloubi==T,]


Cartel<-desco$Cartel

eoeoe2$toofar<-"AB"
for(i in 1:nrow(eoeoe2)){
  if(eoeoe2$V2[i]%in%Cartel){eoeoe2$toofar[i]<-T}else{eoeoe2$toofar[i]<-F}}

eoeoe2<-eoeoe2[eoeoe2$toofar==T,]

Carlito<-unique(eoeoe2$V2)

Carlito<-as.data.frame(Carlito)
Cartel<-as.data.frame(Cartel)


Cabal<-cbind(Carlito,Cartel)


eoeoe2$V2<-as.character(eoeoe2$V2)
eoeoe2$identifiant<-as.character(eoeoe2$identifiant)
desco$Cartel<-as.character(desco$Cartel)



Basefinale<-biblio_coupling(eoeoe2, source = "V2", ref = "identifiant", normalized_weight_only = FALSE, weight_threshold = 2)



#devtools::install_github("agoutsmedt/networkflow")

library(biblionetwork)
library(magrittr)
library(dplyr)
library(tidygraph)
library(networkflow)
library(tidygraph)
#devtools::install_github("ParkerICI/vite")
library(vite)




Edges_coupling2<-Basefinale[,-c(4)]


Nodes_coupling2<-desco

Nodes_coupling2$frequence<-1

Nodes_coupling2$Datepubli<-as.character(Nodes_coupling2$Datepubli)

Nodes_coupling2$author_date<-paste(Nodes_coupling2$Cartel,Nodes_coupling2$Firstauthor,Nodes_coupling2$Datepubli)

palette <- c("#1969B3","#01A5D8","#DA3E61","#3CB95F","#E0AF0C","#E25920","#6C7FC9","#DE9493","#CD242E","#6F4288","#B2EEF8","#7FF6FD","#FDB8D6","#8BF9A9","#FEF34A","#FEC57D","#DAEFFB","#FEE3E1","#FBB2A7","#EFD7F2","#5CAADA","#37D4F5","#F5779B","#62E186","#FBDA28","#FB8F4A","#A4B9EA","#FAC2C0","#EB6466","#AD87BC","#0B3074","#00517C","#871B2A","#1A6029","#7C4B05","#8A260E","#2E3679","#793F3F","#840F14","#401C56","#003C65","#741A09","#602A2A","#34134A","#114A1B","#27DDD1","#27DD8D","#4ADD27","#D3DD27","#DDA427","#DF2935","#DD27BC","#BA27DD","#3227DD","#2761DD","#27DDD1")

graph <- tbl_main_component(nodes = Nodes_coupling2, edges = Edges_coupling2, directed = FALSE, node_key = "Cartel", nb_components = 1)

graph <- leiden_workflow(graph, res_1 = 1)

graph <- community_colors(graph, palette, community_column = "Com_ID")


graph <- graph %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree())



graph <- community_names(graph, ordering_column = "degree", naming = "author_date", community_column = "Com_ID")


graph <- graph %>%
  activate(nodes) %>% 
  mutate(size = degree) # use here what you will use for the size of the nodes in the graph. Force Atlas will take care of avoiding overlapping

graph <- vite::complete_forceatlas2(graph, first.iter = 10000, overlap.method = "repel", overlap.iter = 1000)



graph123<-as.data.frame(graph)



#################################################################################################################
#################################################################################################################
# On extrait les abstracts pour faire de l'analyse textuelle ---------------------------------
#################################################################################################################
#################################################################################################################


load("Data_4_Abstracts.rda")



zalerit$comcitant<-0
for(i in 1:nrow(graph123)){
  j<-which(zalerit$Cartel == graph123$Id[i])
  zalerit$comcitant[j]<-graph123$Com_ID[i]
}


library(dplyr)
library(janeaustenr) #Inutile je pense
library(tidytext)
library(gutenbergr)  #Idem
library(pdftools)
library(tidyverse)
library(qdapRegex)


zaler2<-zalerit[,c(1,3)]

zaler2<-zaler2[zaler2$comcitant!=0,]

zaler2$V1<-as.character(zaler2$V1)
zaler2$comcitant<-as.character(zaler2$comcitant)



paly<-length(table(zaler2$comcitant))


Basetotale_words <- zaler2 %>%
  unnest_tokens(word, V1) %>%
  count(comcitant, word, sort = TRUE)



# On supprime les mots de deux lettres et de une lettre
Basetotale_words$word<-rm_nchar_words(Basetotale_words$word, 2)
Basetotale_words$word<-rm_nchar_words(Basetotale_words$word, 1)


# On remplace le vide que cree l'etape precedente par des NA et on supprime les NA
Basetotale_words$word[Basetotale_words$word==""] <- NA
Basetotale_words <- na.omit(Basetotale_words)


mystopwords <- tibble(word = c("qmi","consumerswho"))


Basetotale_words <- anti_join(Basetotale_words, mystopwords,
                              by = "word")


plot_Basetotale <- Basetotale_words %>%
  bind_tf_idf(word, comcitant, n) %>%
  mutate(word = str_remove_all(word, "_")) %>%
  group_by(comcitant) %>%
  slice_max(tf_idf, n = 25) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, comcitant)) %>%
  mutate(comcitant = factor(comcitant))



palette <- c("#1969B3","#01A5D8","#DA3E61","#3CB95F","#E0AF0C","#E25920","#6C7FC9","#DE9493","#CD242E","#6F4288","#B2EEF8","#7FF6FD","#FDB8D6","#8BF9A9","#FEF34A","#FEC57D","#DAEFFB","#FEE3E1","#FBB2A7","#EFD7F2","#5CAADA","#37D4F5","#F5779B","#62E186","#FBDA28","#FB8F4A","#A4B9EA","#FAC2C0","#EB6466","#AD87BC","#0B3074","#00517C","#871B2A","#1A6029","#7C4B05","#8A260E","#2E3679","#793F3F","#840F14","#401C56","#003C65","#741A09","#602A2A","#34134A","#114A1B","#27DDD1","#27DD8D","#4ADD27","#D3DD27","#DDA427","#DF2935","#DD27BC","#BA27DD","#3227DD","#2761DD","#27DDD1")

palette <- palette[1:paly]

ggplot(plot_Basetotale, aes(word, tf_idf, fill = comcitant)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~comcitant, ncol = 2, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  scale_fill_manual(values=c(palette))


plot_Basetotale2<-plot_Basetotale[,c(1,2)]


plot_Basetotale2$word<-str_replace(string = plot_Basetotale2$word, pattern = "___.*", replacement = "")


plot_Basetotale2$word<-toupper(plot_Basetotale2$word)

plot_Basetotale2$word<-trimws(plot_Basetotale2$word)


plot_Basetotale2$plural <- gsub('.{1}$','', plot_Basetotale2$word)


j<-unique(plot_Basetotale2$comcitant)

plot_Basetotale2$dede <- 0
for(i in 1: length(unique(plot_Basetotale2$comcitant))){
  k<-which(plot_Basetotale2$comcitant==j[i])
  for(m in k){if(any(plot_Basetotale2$plural[m]==plot_Basetotale2$word[k])){
    z<-which(plot_Basetotale2$plural[m]==plot_Basetotale2$word[k])
    if(k[z]<m){
      plot_Basetotale2$dede[k[z]]<-10
      plot_Basetotale2$dede[m]<-28}else{
        plot_Basetotale2$dede[k[z]]<-28
        plot_Basetotale2$dede[m]<-10}
  }else{
    plot_Basetotale2$dede[m]<-plot_Basetotale2$dede[m]}}
}



plot_Basetotale2<-plot_Basetotale2[plot_Basetotale2$dede!=28,]
plot_Basetotale2<-plot_Basetotale2[,c(1,2)]



j<-unique(plot_Basetotale2$comcitant)
plot_Basetotale2$comp<-0

for(i in 1: length(unique(plot_Basetotale2$comcitant))){
  k<-which(plot_Basetotale2$comcitant==j[i])[1:4]
  plot_Basetotale2$comp[k]<-1
}

plot_Basetotale2<-plot_Basetotale2[plot_Basetotale2$comp==T,]





for(i in 1: length(unique(plot_Basetotale2$comcitant))){
  k<-which(plot_Basetotale2$comcitant==j[i])[1]
  plot_Basetotale2$comp[k]<-paste(plot_Basetotale2$word[k],";",plot_Basetotale2$word[k+1],";",plot_Basetotale2$word[k+2],";",plot_Basetotale2$word[k+3])
}


plot_Basetotale2<-plot_Basetotale2[,c(1,3)]
plot_Basetotale2<-plot_Basetotale2[plot_Basetotale2$comp!=1,]



names(plot_Basetotale2)[names(plot_Basetotale2) == "comcitant"] <- "Com_ID"


#################################################################################################################################
#################################################################################################################################
# Fin de l'extraction des mots-clés --------------------------------------------------------------
#################################################################################################################################
#################################################################################################################################



top_nodes  <- top_nodes(graph, ordering_column = "degree", top_n = 10, top_n_per_com = 1)


community_labels <- community_labels(graph, community_name_column = "Community_name", community_size_column = "Size_com")

community_labels$identifiant<-str_sub (community_labels$Community_name, 1,2)

for(i in 1:nrow(community_labels)){
  j<-which(community_labels$identifiant[i] == plot_Basetotale2)
  community_labels$Community_name[i]<- plot_Basetotale2$comp[j]
}



library(ggraph)
library(ggrepel) 
library(ggnewscale)


dev.new()
ggraph(graph, "manual", x = x, y = y) + 
  geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.4, strength = 0.2, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.1,2)) +
  scale_edge_colour_identity() +
  geom_node_point(aes(x=x, y=y, size = degree, fill = color), pch = 21, alpha = 0.9, show.legend = FALSE) +
  scale_size_continuous(range = c(0.2,13)) +
  scale_fill_identity() +
  new_scale("size") +
  geom_text_repel(data=top_nodes, aes(x=x, y=y, label = Label), size = 2, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  geom_label_repel(data=community_labels, aes(x=x, y=y, label = Community_name, fill = color, size = Size_com), fontface="bold", alpha = 0.9, point.padding=NA, show.legend = FALSE) +
  scale_size_continuous(range = c(0.5,5)) +
  theme_void()



#################################################################################################################################
#################################################################################################################################
# Co-citation 2010-2019-----------------------------------------
#################################################################################################################################
#################################################################################################################################


##### On a une colonne d'identifiant unique des articles et une colonne avec les refs biblios
##### recodées, on peut donc soumettre ça au package d'Aurelien

set.seed(16)


load("Data_5_nodes.rda")
load("Data_7_finalbase.rda")




library(readr)
library(stringr)
#library(bibliometrix)
library(tidyverse)
library(biblionetwork)
#library(devtools)
#devtools::install_github("agoutsmedt/biblionetwork")




eoeoe2<-eoeoe

simple<-unique(eoeoe2$V2)
simple<-as.data.frame(simple)

setdiff(desco$Cartel,simple[,1])

desco<-desco[desco$Cartel!=108237,]




comp<-cbind(simple,desco$Cartel)




desco$sloubi<-str_detect (string = desco$Datepubli, pattern = "2010|2011|2012|2013|2014|2015|2016|2017|2018|2019")
#desco$sloubi<-str_detect (string = desco$Datepubli, pattern = "1970|1971|1972|1973|1974|1975|1976|1977|1978|1979")
#desco$sloubi<-str_detect (string = desco$Datepubli, pattern = "1980|1981|1982|1983|1984|1985|1986|1987|1988|1989")
#desco$sloubi<-str_detect (string = desco$Datepubli, pattern = "1990|1991|1992|1993|1994|1995|1996|1997|1998|1999")
#desco$sloubi<-str_detect (string = desco$Datepubli, pattern = "2000|2001|2002|2003|2004|2005|2006|2007|2008|2009")



desco<-desco[desco$sloubi==T,]


Cartel<-desco$Cartel

eoeoe2$toofar<-"AB"
for(i in 1:nrow(eoeoe2)){
  if(eoeoe2$V2[i]%in%Cartel){eoeoe2$toofar[i]<-T}else{eoeoe2$toofar[i]<-F}}

eoeoe2<-eoeoe2[eoeoe2$toofar==T,]

Carlito<-unique(eoeoe2$V2)

Carlito<-as.data.frame(Carlito)
Cartel<-as.data.frame(Cartel)


Cabal<-cbind(Carlito,Cartel)


eoeoe2$V2<-as.character(eoeoe2$V2)
eoeoe2$identifiant<-as.character(eoeoe2$identifiant)
desco$Cartel<-as.character(desco$Cartel)


Basefinale<-biblio_coupling(eoeoe2, source = "identifiant", ref = "V2", normalized_weight_only = FALSE, weight_threshold = 2)



#devtools::install_github("agoutsmedt/networkflow")

library(biblionetwork)
library(magrittr)
library(dplyr)
library(tidygraph)
library(networkflow)
#devtools::install_github("agoutsmedt/networkflow")

library(tidygraph)
#devtools::install_github("ParkerICI/vite")
library(vite)


Edges_coupling2<-Basefinale[,-c(4)]


Nodes_coupling2<-unique(eoeoe2$identifiant)

Nodes_coupling2<-as.data.frame(Nodes_coupling2)

Nodes_coupling2$frequence<-0
for(i in 1:nrow(Nodes_coupling2)){
  j<-which(Nodes_coupling2$Nodes_coupling2[i] == eoeoe2$identifiant)[1]
  Nodes_coupling2$frequence[i]<-eoeoe2$frequence[j]
}


Nodes_coupling2$ids<-Nodes_coupling2$Nodes_coupling2


palette <- c("#1969B3","#01A5D8","#DA3E61","#3CB95F","#E0AF0C","#E25920","#6C7FC9","#DE9493","#CD242E","#6F4288","#B2EEF8","#7FF6FD","#FDB8D6","#8BF9A9","#FEF34A","#FEC57D","#DAEFFB","#FEE3E1","#FBB2A7","#EFD7F2","#5CAADA","#37D4F5","#F5779B","#62E186","#FBDA28","#FB8F4A","#A4B9EA","#FAC2C0","#EB6466","#AD87BC","#0B3074","#00517C","#871B2A","#1A6029","#7C4B05","#8A260E","#2E3679","#793F3F","#840F14","#401C56","#003C65","#741A09","#602A2A","#34134A","#114A1B","#27DDD1","#27DD8D","#4ADD27","#D3DD27","#DDA427","#DF2935","#DD27BC","#BA27DD","#3227DD","#2761DD","#27DDD1")

graph <- tbl_main_component(nodes = Nodes_coupling2, edges = Edges_coupling2, directed = FALSE, node_key = "Cartel", nb_components = 1)


## Varying the resolution of the algorithm results in a different partition and 
## different number of communities. A lower resolution means less communities, and conversely.
## The basic resolution of the leiden_workflow() is set by res_1 and equals 1 by default. 
## You can vary this parameter, but also try a second resolution with res_2 and a third one 
## with res_3
## On peut l'enlever avec un argument par défaut de 1


graph <- leiden_workflow(graph, res_1 = 0.5)

graph <- community_colors(graph, palette, community_column = "Com_ID")


graph <- graph %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree())



graph <- community_names(graph, ordering_column = "degree", naming = "ids", community_column = "Com_ID")


graph <- graph %>%
  activate(nodes) %>% 
  mutate(size = frequence) # use here what you will use for the size of the nodes in the graph. Force Atlas will take care of avoiding overlapping


######## First.iter initiallement 5000, j'ai baissé pour arriver à le faire tourner


graph <- vite::complete_forceatlas2(graph, first.iter = 10000, overlap.method = "repel", overlap.iter = 1500)


top_nodes  <- top_nodes(graph, ordering_column = "degree", top_n = 10, top_n_per_com = 1)


community_labels2 <- community_labels(graph, community_name_column = "Community_name", community_size_column = "Size_com")

community_labels2$Community_name<-str_extract_all(community_labels2$Community_name, ".*\\)")



library(ggraph)
library(ggrepel) 
library(ggnewscale)


graph456<-as.data.frame(graph)


############################################################################################################################
############################################################################################################################
# we extract the keywords form the title
############################################################################################################################
############################################################################################################################



eoeoe$comcitant<-0
for(i in 1:nrow(graph456)){
  j<-which(eoeoe$identifiant == graph456$Id[i])
  eoeoe$comcitant[j]<-graph456$Com_ID[i]
}


comparaison<-str_extract_all(community_labels2$Community_name, "-.*\\(")

comparaison<-str_replace(string = comparaison, pattern = " \\(", replacement = "")
comparaison<-str_replace(string = comparaison, pattern = "-", replacement = "")



library(dplyr)
library(janeaustenr) #Inutile je pense
library(tidytext)
library(gutenbergr)  #Idem
library(pdftools)
library(tidyverse)
library(qdapRegex)


eoeoe2<-eoeoe[,c(1,12)]

eoeoe2<-eoeoe2[eoeoe2$comcitant!=0,]

eoeoe2$V1<-as.character(eoeoe2$V1)
eoeoe2$comcitant<-as.character(eoeoe2$comcitant)



paly<-length(table(eoeoe2$comcitant))


Basetotale_words <- eoeoe2 %>%
  unnest_tokens(word, V1) %>%
  count(comcitant, word, sort = TRUE)



# On supprime les mots de deux lettres et de une lettre
Basetotale_words$word<-rm_nchar_words(Basetotale_words$word, 2)
Basetotale_words$word<-rm_nchar_words(Basetotale_words$word, 1)


# On remplace le vide que cree l'etape precedente par des NA et on supprime les NA
Basetotale_words$word[Basetotale_words$word==""] <- NA
Basetotale_words <- na.omit(Basetotale_words)



Basetotale_words$word<-toupper(Basetotale_words$word)

Basetotale_words$word<-trimws(Basetotale_words$word)



mystopwords <- tibble(word = c(comparaison,"890","1929","841","GAL","VON","ECONOMETRICA","FRONTIERS","GARVIN","SETHI","OUARDIGHI","LEVINSOHN","COURSE","SOURCES","ZAREMBKA","ROHEIM","ASCHE","1212","BLIND"))


Basetotale_words <- anti_join(Basetotale_words, mystopwords,
                              by = "word")


plot_Basetotale <- Basetotale_words %>%
  bind_tf_idf(word, comcitant, n) %>%
  mutate(word = str_remove_all(word, "_")) %>%
  group_by(comcitant) %>%
  slice_max(tf_idf, n = 25) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, comcitant)) %>%
  mutate(comcitant = factor(comcitant))



palette <- c("#1969B3","#01A5D8","#DA3E61","#3CB95F","#E0AF0C","#E25920","#6C7FC9","#DE9493","#CD242E","#6F4288","#B2EEF8","#7FF6FD","#FDB8D6","#8BF9A9","#FEF34A","#FEC57D","#DAEFFB","#FEE3E1","#FBB2A7","#EFD7F2","#5CAADA","#37D4F5","#F5779B","#62E186","#FBDA28","#FB8F4A","#A4B9EA","#FAC2C0","#EB6466","#AD87BC","#0B3074","#00517C","#871B2A","#1A6029","#7C4B05","#8A260E","#2E3679","#793F3F","#840F14","#401C56","#003C65","#741A09","#602A2A","#34134A","#114A1B","#27DDD1","#27DD8D","#4ADD27","#D3DD27","#DDA427","#DF2935","#DD27BC","#BA27DD","#3227DD","#2761DD","#27DDD1")

palette <- palette[1:paly]

ggplot(plot_Basetotale, aes(word, tf_idf, fill = comcitant)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~comcitant, ncol = 2, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  scale_fill_manual(values=c(palette))


plot_Basetotale2<-plot_Basetotale[,c(1,2)]


plot_Basetotale2$word<-str_replace(string = plot_Basetotale2$word, pattern = "___.*", replacement = "")


plot_Basetotale2$plural <- gsub('.{1}$','', plot_Basetotale2$word)


j<-unique(plot_Basetotale2$comcitant)

plot_Basetotale2$dede <- 0
for(i in 1: length(unique(plot_Basetotale2$comcitant))){
  k<-which(plot_Basetotale2$comcitant==j[i])
  for(m in k){if(any(plot_Basetotale2$plural[m]==plot_Basetotale2$word[k])){
    z<-which(plot_Basetotale2$plural[m]==plot_Basetotale2$word[k])
    if(k[z]<m){
      plot_Basetotale2$dede[k[z]]<-10
      plot_Basetotale2$dede[m]<-28}else{
        plot_Basetotale2$dede[k[z]]<-28
        plot_Basetotale2$dede[m]<-10}
  }else{
    plot_Basetotale2$dede[m]<-plot_Basetotale2$dede[m]}}
}



plot_Basetotale2<-plot_Basetotale2[plot_Basetotale2$dede!=28,]
plot_Basetotale2<-plot_Basetotale2[,c(1,2)]



j<-unique(plot_Basetotale2$comcitant)
plot_Basetotale2$comp<-0

for(i in 1: length(unique(plot_Basetotale2$comcitant))){
  k<-which(plot_Basetotale2$comcitant==j[i])[1:4]
  plot_Basetotale2$comp[k]<-1
}

plot_Basetotale2<-plot_Basetotale2[plot_Basetotale2$comp==T,]





for(i in 1: length(unique(plot_Basetotale2$comcitant))){
  k<-which(plot_Basetotale2$comcitant==j[i])[1]
  plot_Basetotale2$comp[k]<-paste(plot_Basetotale2$word[k],";",plot_Basetotale2$word[k+1],";",plot_Basetotale2$word[k+2],";",plot_Basetotale2$word[k+3])
}


plot_Basetotale2<-plot_Basetotale2[,c(1,3)]
plot_Basetotale2<-plot_Basetotale2[plot_Basetotale2$comp!=1,]



names(plot_Basetotale2)[names(plot_Basetotale2) == "comcitant"] <- "Com_ID"


community_labels2$Com_ID<-str_sub (community_labels2$Community_name, 1,2)



community_labels2<-merge(plot_Basetotale2,community_labels2,by="Com_ID")

community_labels2$Community_name<-paste(community_labels2$Community_name,community_labels2$comp)





###############################################################################################################
###############################################################################################################
# We create the graph
###############################################################################################################
###############################################################################################################



dev.new()

dev.new()
ggraph(graph, "manual", x = x, y = y) + 
  geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.4, strength = 0.2, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.1,2)) +
  scale_edge_colour_identity() +
  geom_node_point(aes(x=x, y=y, size = degree, fill = color), pch = 21, alpha = 0.9, show.legend = FALSE) +
  scale_size_continuous(range = c(0.2,13)) +
  scale_fill_identity() +
  new_scale("size") +
  geom_text_repel(data=top_nodes, aes(x=x, y=y, label = Label), size = 2, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  geom_label_repel(data=community_labels2[community_labels2$Size_com>0.009,], aes(x=x, y=y, label = Community_name, fill = color, size = Size_com), fontface="bold", alpha = 0.9, point.padding=NA, show.legend = FALSE) +
  scale_size_continuous(range = c(0.5,5)) +
  theme_void()




##############################################################################################################
##############################################################################################################
# Connecter les communautés -----------------------------
##############################################################################################################
##############################################################################################################


graph123<-graph123[,c(1,9)]
graph456<-graph456[,c(1,4)]



eoeoe$comcitant<-0
for(i in 1:nrow(graph123)){
  j<-which(eoeoe$V2 == graph123$Id[i])
  eoeoe$comcitant[j]<-graph123$Com_ID[i]
}


eoeoe$comcite<-0
for(i in 1:nrow(graph456)){
  j<-which(eoeoe$identifiant == graph456$Id[i])
  eoeoe$comcite[j]<-graph456$Com_ID[i]
}



eoeoe4<-eoeoe
eoeoe4<-eoeoe4[eoeoe4$comcitant!=0,]
eoeoe4<-eoeoe4[eoeoe4$comcite!=0,]


community_labels2$ID<-gsub("-.*", "",community_labels2$Community_name)

community_labels2$IDil<-gsub(".*-", "",community_labels2$Community_name)


community_labels2$IDcompl<-"AB"
for(i in 1:nrow(community_labels2)){
  j<-which(community_labels2$IDil[i] == eoeoe$identifiant)
  z<-which(paste(eoeoe$Firstauthor[j],eoeoe$Datepubli[j],eoeoe$Issue[j],eoeoe$Page[j])==eoeoe$identifiant[j])[1]
  community_labels2$IDcompl[i] <- eoeoe$V1[j[z]]
}


tableau<-as.data.frame.matrix(prop.table(table(eoeoe4$comcitant,eoeoe4$comcite),margin=1)*100)
library(dplyr)
tableau<-tableau %>% mutate_if(is.numeric, ~round(., 1))


community_labels<-community_labels[order(community_labels$identifiant),]
row.names(tableau) <- community_labels$Community_name

community_labels2<-community_labels2[order(community_labels2$Community_name),]
colnames(tableau)<- community_labels2$Community_name


for(i in 1:nrow(community_labels2)){
  community_labels2$IDcompl[i]<-paste(i,"-",community_labels2$IDcompl[i])
}

Legend<-community_labels2[,c(8)]

Legend<-as.data.frame(Legend)

names(Legend)[names(Legend) == "IDcompl"] <- "Complete reference of the clusters"

library(knitr)
library(kableExtra)


kable(Legend)







