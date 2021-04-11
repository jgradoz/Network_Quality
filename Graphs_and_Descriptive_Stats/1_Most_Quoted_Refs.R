
setwd("C:/Users/jgrad/Desktop/Network_Quality/Network_Quality")


load("Data/Data_7_finalbase.rda")


#####################################################################################################################
#####################################################################################################################
# Last errors observed by exploring the base-------------------
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


load("Data/Data_5_nodes.rda")


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

top19811990<-top19811990[1:50,]
top19912000<-top19912000[1:50,]
top20012010<-top20012010[1:50,]
top20112020<-top20112020[1:50,]

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


top19811990<-as.data.frame(top19811990)
top19912000<-as.data.frame(top19912000)
top20012010<-as.data.frame(top20012010)
top20112020<-as.data.frame(top20112020)


##### We change the name of the column

names(top19811990)[names(top19811990) == "top19811990"] <- "Most quoted references 1980-1989"
names(top19912000)[names(top19912000) == "top19912000"] <- "Most quoted references 1990-1999"
names(top20012010)[names(top20012010) == "top20012010"] <- "Most quoted references 2000-2009"
names(top20112020)[names(top20112020) == "top20112020"] <- "Most quoted references 2010-2019"




#############################################################################################################
#############################################################################################################
# Creation of the bumpchart
#############################################################################################################
#############################################################################################################

##### We rank according to frequency

top19811990<-freq19811990[rev(order(freq19811990$Freq)),]
top19912000<-freq19912000[rev(order(freq19912000$Freq)),]
top20012010<-freq20012010[rev(order(freq20012010$Freq)),]
top20112020<-freq20112020[rev(order(freq20112020$Freq)),]

##### We keep all rows

top19811990<-top19811990[1:nrow(top19811990),]
top19912000<-top19912000[1:nrow(top19912000),]
top20012010<-top20012010[1:nrow(top20012010),]
top20112020<-top20112020[1:nrow(top20112020),]


###### We create a variable indicating the rank


top19811990$Freq<-1:nrow(top19811990)
top19912000$Freq<-1:nrow(top19912000)
top20012010$Freq<-1:nrow(top20012010)
top20112020$Freq<-1:nrow(top20112020)


##### We create a variable indicating the year


top19811990$YEAR<-"1980-1989"
top19912000$YEAR<-"1990-1999"
top20012010$YEAR<-"2000-2009"
top20112020$YEAR<-"2010-2019"



###### We keeep the 25 first obs

top19811990<-top19811990[1:25,]
top19912000<-top19912000[1:25,]
top20012010<-top20012010[1:25,]
top20112020<-top20112020[1:25,]

###### We gather the dataframe

top<-rbind(top19811990,top19912000,top20012010,top20112020)

library(ggplot2)
library(dplyr)


##### We remove the ', but also the remaining number

top$Var1<-str_replace_all(string = top$Var1, pattern = "D'ASPREMONT", replacement = "DASPREMONT")

top$Var1<-str_replace_all(string = top$Var1, pattern = " [[:digit:]].*$", replacement = "")

top$Var1<-str_replace_all(string = top$Var1, pattern = " AB.*$", replacement = "")


###### Fuction to create the bumpchart


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
    # Les size 0 suppriment les titre en abscisees et en ordonnées
    # Size 5 controle la taille du titre
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



gg_point=ggplot(top,aes(x = as.factor(YEAR), y = Freq, group = Var1)) +
  # Taille des lignes
  geom_line_interactive(aes(color = Var1, alpha = 1,tooltip=Var1, data_id = Var1), size = 0.3) +
  # Taille des points
  geom_point_interactive(aes(x = as.factor(YEAR), y = Freq, group = Var1, color = Var1, alpha = 1, tooltip = Var1, data_id = Var1), size = 1) +
  geom_point_interactive(color = "#FFFFFF", size = 0.01) +
  scale_y_reverse(breaks = 1:table(top$YEAR)[[1]]) + 
  scale_x_discrete(breaks = 1:length(table(top$YEAR))) +
  theme(legend.position = 'none') +
  # le x designe le positionnement des auteurs à gauche
  geom_text(data = top %>% filter(YEAR == "1980-1989"),
            aes(label = Var1, x = 0.42) , hjust = 0.0,
            fontface = "bold", color = "#888888", size = 0.7) +
  # idem mais à droite
  geom_text(data = top %>% filter(YEAR == "2010-2019"),
            aes(label = Var1, x = 4.5) , hjust = 1.0,
            fontface = "bold", color = "#888888", size = 0.7) +
  labs(x = '', y = '', title = 'Most quoted references (1980-1989, 1990-1999, 2000-2009, 2010-2019)') +
  my_theme() 



##### Representation du graphique interactif


girafe(ggobj = gg_point, width_svg = 3, height_svg = 2,
       options = list(
         opts_hover_inv(css = "opacity:0.1;"),
         opts_hover(css = "stroke-width:2;")
       ))


