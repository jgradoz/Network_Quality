############################################################################################################################
############################################################################################################################
# Réseau dynamique
############################################################################################################################
############################################################################################################################


setwd("C:/Users/jgrad/Desktop/Network_Quality/Network_Quality/Data")

graph_data_path <- "C:/Users/jgrad/Desktop/Network_Quality/Network_Quality/Graphs_and_Descriptive_Stats/"


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################## LOADING PACKAGES, PATHS AND OBJECTS ####################################--------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

##################### Packages ############################################--------------

cran_list <- c(
  "data.table", "magrittr", "ggnewscale", "igraph","forcats",
  "tidytext", "ggraph", "tidygraph", "ggrepel", "leidenAlg", "reshape2", "scales", 
  "ggforce", "directlabels", "patchwork", "DescTools", "DT", "grid", "scico",
  "ggalluvial", "dplyr","gridExtra","readr","stringi","tm","stringr",
  "RColorBrewer","textstem","tidyr","tidytext","quanteda"
)
for (p in cran_list) {
  if (p %in% installed.packages() == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
  library(p, character.only = TRUE)
}

github_list <- c("ParkerICI/vite", "agoutsmedt/biblionetwork", "agoutsmedt/networkflow")
for (p in github_list) {
  if (gsub(".*/", "", p) %in% installed.packages() == FALSE) {
    devtools::install_github(p)
  }
  library(gsub(".*/", "", p), character.only = TRUE)
}

# py_install("python-igraph")
# py_install("leidenalg", forge = TRUE)



source("../Functions_and_cleaning/dyncoupfct.R")


mypalette <- c("#1969B3", "#01A5D8", "#DA3E61", "#3CB95F", "#E0AF0C", "#E25920", "#6C7FC9", "#DE9493", "#CD242E", "#6F4288", "#B2EEF8", "#7FF6FD", "#FDB8D6", "#8BF9A9", "#FEF34A", "#FEC57D", "#DAEFFB", "#FEE3E1", "#FBB2A7", "#EFD7F2", "#5CAADA", "#37D4F5", "#F5779B", "#62E186", "#FBDA28", "#FB8F4A", "#A4B9EA", "#FAC2C0", "#EB6466", "#AD87BC", "#0B3074", "#00517C", "#871B2A", "#1A6029", "#7C4B05", "#8A260E", "#2E3679", "#793F3F", "#840F14", "#401C56", "#003C65", "#741A09", "#602A2A", "#34134A", "#114A1B", "#27DDD1", "#27DD8D", "#4ADD27", "#D3DD27", "#DDA427", "#DF2935", "#DD27BC", "#BA27DD", "#3227DD", "#2761DD", "#27DDD1")
palette_com_1 <- c("#C7FFFF", "#0E3F84", "#2B8C44", "#FFF953", "#B12C45", "#E7FCFF", "#0190C1", "#956407", "#B64017", "#2E3679", "#925554", "#840F14", "#572F6F")
palette_com_2 <- c("#C7FFFF", "#72BBE1", "#77ED98", "#FFF953", "#FDB8D6", "#E7FCFF", "#25CBF3", "#C57F7E", "#FCAA64", "#A4B9EA", "#F9BAB8", "#EF7776", "#8C5FA1")


######################### Fixing the sub-periods and thresholds ##########################################------------


load("Data_5_nodes.rda")
load("Data_7_finalbase.rda")



desco$Datepubli2<-str_replace_all(string = desco$Datepubli, pattern = "\\(", replacement = "")
desco$Datepubli2<-str_replace_all(string = desco$Datepubli2, pattern = "\\)", replacement = "")
desco$Datepubli2<-as.numeric(desco$Datepubli2)





######################### Integrating community names and creating their colors #########################-----
community_names <- fread(paste0(graph_data_path, "Community_names.csv")) %>% data.table()
community_names <- community_names[, Com_ID := gsub("\"\"", "", Com_ID)]

# Adding "first rank" colors to the data
color_com <- data.table(
  "Titre_provisoire" = sort(unique(community_names$Titre_provisoire)),
  "Color_Com_1" = palette_com_1[1:length(unique(community_names$Titre_provisoire))],
  "Color_Com_2" = palette_com_2[1:length(unique(community_names$Titre_provisoire))]
)
community_names <- merge(community_names, color_com, by = "Titre_provisoire")

# Manipulating the data table to have "second rank" colors
color_com <- unique(community_names[order(Titre_provisoire, Titre_provisoire_long), c("Titre_provisoire", "Titre_provisoire_long", "Color_Com_1", "Color_Com_2")])
color_com <- color_com[, `:=`(Order = 1:.N, total_titles = .N), by = list(Titre_provisoire)][, alpha_index := Order / total_titles]
color_com <- color_com[, Color_Com := MixColor(color_com$Color_Com_1, color_com$Color_Com_2, amount1 = color_com$alpha_index)]

community_names <- merge(community_names, color_com[, c("Titre_provisoire_long", "Color_Com")], by = "Titre_provisoire_long")
community_names$Titre_provisoire_long <- gsub("\\\\n", "\\\n", community_names$Titre_provisoire_long)





##### On a une colonne d'identifiant unique des articles et une colonne avec les refs biblios
##### recodées, on peut donc soumettre ça au package d'Aurelien

set.seed(12)


eoeoe2<-eoeoe

simple<-unique(eoeoe2$V2)
simple<-as.data.frame(simple)

setdiff(desco$Cartel,simple[,1])

desco<-desco[desco$Cartel!=108237,]


comp<-cbind(simple,desco$Cartel)



Cartel2<-desco$Cartel

eoeoe2$toofar<-"AB"
for(i in 1:nrow(eoeoe2)){
  if(eoeoe2$V2[i]%in%Cartel2){eoeoe2$toofar[i]<-T}else{eoeoe2$toofar[i]<-F}}

eoeoe2<-eoeoe2[eoeoe2$toofar==T,]

Carlito<-unique(eoeoe2$V2)

Carlito<-as.data.frame(Carlito)
Cartel2<-as.data.frame(Cartel2)


Cabal<-cbind(Carlito,Cartel2)


eoeoe2$V2<-as.character(eoeoe2$V2)
eoeoe2$identifiant<-as.character(eoeoe2$identifiant)
desco$Cartel<-as.character(desco$Cartel)


eoeoe2$Datepubli<-str_replace_all(string = eoeoe2$Datepubli, pattern = "\\(", replacement = "")
eoeoe2$Datepubli<-str_replace_all(string = eoeoe2$Datepubli, pattern = "\\)", replacement = "")
eoeoe2$Datepubli<-as.numeric(eoeoe2$Datepubli)



eoeoe2<-eoeoe2[!is.na(eoeoe2$Datepubli),]


eoeoe2$Date<-0
for(i in 1:nrow(eoeoe2)){
  j<-which(eoeoe2$V2[i] == desco$Cartel)
  eoeoe2$Date[i]<-desco$Datepubli2[j]
}

eoeoe2$Date<-as.numeric(eoeoe2$Date)


##### On convertit Frist author au bon format

desco$Firstauthor<-as.character(desco$Firstauthor)

desco$Firstauthor<-trimws(desco$Firstauthor)

##### On transforme les caractères spéciaux (comme les accents) en caractères neutres

desco$Firstauthor<-iconv(desco$Firstauthor,from="UTF-8",to="ASCII//TRANSLIT")

##### On passe les noms en majuscule

desco$Firstauthor<-toupper(desco$Firstauthor)

desco$Firstauthor<-gsub("\\,.*", "",desco$Firstauthor)



desco$Firstauthor<-str_replace(string = desco$Firstauthor, pattern = "\\-", replacement = " ")
desco$Firstauthor<-str_replace(string = desco$Firstauthor, pattern = " ", replacement = "")
desco$Firstauthor<-str_replace(string = desco$Firstauthor, pattern = "  ", replacement = "")
desco$Firstauthor<-str_replace(string = desco$Firstauthor, pattern = " ", replacement = "")
desco$Firstauthor<-str_replace(string = desco$Firstauthor, pattern = "\\-", replacement = " ")
desco$Firstauthor<-str_replace(string = desco$Firstauthor, pattern = " ", replacement = "")
desco$Firstauthor<-str_replace(string = desco$Firstauthor, pattern = "  ", replacement = "")
desco$Firstauthor<-str_replace(string = desco$Firstauthor, pattern = " ", replacement = "")



desco$Issue<- str_extract_all(desco$publi, "(, \\d{1,9})(,| \\(.*\\),)")
desco$Issue<-as.character(desco$Issue)
desco$Issue[desco$Issue == "character(0)"] <- "AB"


##### Ceci dit, pas tous ce format, du coup on extrait sur les restants avec un autre REGEX

desco$Issue[desco$Issue == "AB"]<- str_extract_all(desco$publi[desco$Issue == "AB"], "(, \\d{1,9})(,| \\(.*\\)\\.,)")
desco$Issue<-as.character(desco$Issue)
desco$Issue[desco$Issue == "character(0)"] <- "AB"






desco$Issue<-str_replace_all(string = desco$Issue, pattern = "\\(.*\\)", replacement = "")
desco$Issue<-str_replace_all(string = desco$Issue, pattern = "\\,", replacement = "")




###### On détecte les pages, soit au format "pp." ou au format "p.", et on garde ce qu'il y a apres.
###### "\\d" correspond à des informations numériques


desco$Page<- str_extract_all(desco$publi, " pp.+\\d")
desco$Page2<- str_extract_all(desco$publi, ", p\\..*")


#### L'existence de chaîne de caractères nulles pose des pbs, du coup on les remplace

desco$Page<-as.character(desco$Page)
desco$Page[desco$Page == "character(0)"] <- "AB"
desco$Page2[desco$Page2 == "character(0)"] <- "AB"



###### On fusionne les deux formats de pages (pp. et p.) stockées dans deux colonnes
###### au sein d'une même colonne


for(i in 1:nrow(desco)){
  if(str_sub (desco$Page2[i], 1,4)==", p."){desco$Page[i]<-desco$Page2[i]}else{desco$Page[i]<-desco$Page[i]}
}


##### On ne garde que l'information sur la première page en supprimant ce qu'il y a après

desco$Page<-gsub("-.*", "",desco$Page)
desco$Page<-gsub(".,.*", "",desco$Page)


##### On supprime aussi ce qu'il y a avant

desco$Page<-str_replace(string = desco$Page, pattern = "pp. ", replacement = "")
desco$Page<-str_replace(string = desco$Page, pattern = ", p. ", replacement = "")


###### On enlève les guillemets des issues (parfois plein, donc répétition de l'opération)

desco$Issue<-str_replace(string = desco$Issue, pattern = '\\"', replacement = "")
desco$Issue<-str_replace(string = desco$Issue, pattern = '\\"', replacement = "")
desco$Issue<-str_replace(string = desco$Issue, pattern = '\\"', replacement = "")
desco$Issue<-str_replace(string = desco$Issue, pattern = '\\"', replacement = "")
desco$Issue<-str_replace(string = desco$Issue, pattern = '\\"', replacement = "")
desco$Issue<-str_replace(string = desco$Issue, pattern = '\\"', replacement = "")



# On nettoie les points-virgules

desco$Page<-str_replace(string = desco$Page, pattern = ";", replacement = "")


desco$Firstauthor<-trimws(desco$Firstauthor)
desco$Datepubli<-trimws(desco$Datepubli)
desco$Issue<-trimws(desco$Issue)
desco$Page<-trimws(desco$Page)


##### On créer une colonne qui fusionne les différentes informations, permettant de créer 
##### un identifiant unique pour les refs biblio

desco$identifiantcit<-"AB"
for(i in 1:nrow(desco)){
  desco$identifiantcit[i]<-paste(desco$Firstauthor[i],desco$Datepubli[i],desco$Issue[i],desco$Page[i])}


desco$identifiantcit<-as.character(desco$identifiantcit)

z<-intersect(desco$identifiantcit,eoeoe2$identifiant)

z<-as.data.frame(z)


n_occur <- data.frame(table(desco$identifiantcit))


for(i in 1:nrow(n_occur)){
  if(n_occur$Freq[i]>1){
    j<-which(desco$identifiantcit==n_occur$Var1[i])
    for(k in j){
      desco$identifiantcit[k]<-paste(desco$identifiantcit[k],k)
    }
  }
}

n_occur <- data.frame(table(desco$identifiantcit))


eoeoe2$identifiantcit<-"AB"
for(i in 1:nrow(eoeoe2)){
  j<-which(eoeoe2$V2[i] == desco$Cartel)
  eoeoe2$identifiantcit[i]<-desco$identifiantcit[j]
}




desco$Page2<-as.character(desco$Page2)



desco<-desco[desco$Datepubli2 > 1989,]
eoeoe2<-eoeoe2[eoeoe2$Date > 1989,]



tbl_coup_list <- dynamic_biblio_coupling(corpus = desco,
                                         direct_citation_dt = eoeoe2, 
                                         source = "identifiantcit",
                                         source_as_ref = "identifiantcit",
                                         ref = "identifiant", 
                                         time_variable = "Datepubli2",
                                         coupling_method = "coupling_strength",
                                         time_window_length = 10,
                                         time_window_move = 0,
                                         weight_treshold = 2,
                                         nodes_threshold = 0,
                                         controlling_nodes = FALSE,
                                         controlling_edges = TRUE,
                                         nodes_limit = 10000,
                                         edges_limit = 400000,
                                         distribution_pruning = FALSE,
                                         quantile_threshold = 1,
                                         quantile_move = 0)





time_window_length = 10


desco<-desco[order(desco$Datepubli2),]

first_year <- desco$Datepubli2[1]

desco<-desco[order(-desco$Datepubli2),]

last_year <- (desco$Datepubli2[1]- time_window_length + 1) - 1

all_years <- first_year:last_year



tbl_coup_list <- lapply(tbl_coup_list, leiden_workflow, niter = 10000)





list_graph_position <- list()


for (Year in all_years) {
  message(paste0("Running Force Atlas for the ", Year, "-", Year + time_window_length - 1, " window."))
  if (is.null(tbl_coup_list[[paste0(Year - 1)]])) {
    list_graph_position[[paste0(Year)]] <- force_atlas(tbl_coup_list[[paste0(Year)]], kgrav = 4, change_size = FALSE)
  }
  if (!is.null(tbl_coup_list[[paste0(Year - 1)]])) {
    past_position <- list_graph_position[[paste0(Year - 1)]] %>%
      activate(nodes) %>%
      as.data.table()
    past_position <- past_position[, .(identifiantcit, x, y)]
    tbl <- tbl_coup_list[[paste0(Year)]] %>%
      activate(nodes) %>%
      left_join(past_position)
    list_graph_position[[paste0(Year)]] <- force_atlas(tbl, kgrav = 4, change_size = FALSE)
  }
  # saveRDS(tbl_coup_list, paste0(graph_data_path,"coupling_graph_",Year,"-",Year+time_window-1,".rds"))
  saveRDS(list_graph_position[[paste0(Year)]], paste0(graph_data_path, "coupling_graph_", Year, "-", Year + time_window_length - 1, ".rds"))
  gc()
}



saveRDS(list_graph_position, paste0(graph_data_path, "list_graph_", first_year, "-", last_year + time_window_length - 1, ".rds"))


start_date<-all_years
end_date<-all_years+time_window_length-1


all_nodes <- data.table("Id" = c(), "Annee_Bibliographique" = c(), "Titre" = c(), "Label" = c(), "Community_name" = c(), "color" = c())
for (i in 1:length(start_date)) {
  graph <- readRDS(paste0(graph_data_path, "coupling_graph_", start_date[i], "-", end_date[i], ".rds"))
  graph <- graph %>%
    activate(nodes) %>%
    select(identifiantcit, time_variable, titre, source_as_ref, Com_ID) %>%
    as.data.table()
  tolo<-unique(graph$Com_ID)
  tolo<-as.data.frame(tolo)
  tolo$number<-as.numeric(tolo$tolo)
  tolo<-tolo[order(tolo$number),]
  tolo$color<-mypalette[1:nrow(tolo)]
  graph$color<-"AB"
  for(i in 1:nrow(graph)){
    j<-which(graph$Com_ID[i]==tolo$tolo)
    graph$color[i]<-tolo$color[j]
  }
  
  all_nodes <- rbind(all_nodes, graph)
}




for (Year in all_years) {
  nodes <- list_graph_position[[paste0(Year)]] %>%
    activate(nodes) %>%
    select(identifiantcit, time_variable, titre, source_as_ref, Com_ID) %>%
    as.data.table()
  communities <- all_nodes[between(time_variable, Year, Year + time_window_length)]
  
  communities <- merge(nodes, communities[, c("source_as_ref", "color", "Com_ID")], by.x = "identifiantcit", by.y = "source_as_ref")
  communities <- communities[, size_com := .N, by = "Com_ID.x"][, .N, by = c("Com_ID.x", "size_com", "Com_ID.y", "color")]
  
  communities <- communities %>%
    group_by(Com_ID.x) %>%
    arrange(-N) %>%
    mutate(share = N / size_com) %>%
    select(Com_ID.x, Com_ID.y, color, share) %>%
    slice(1)
  
  names(communities)[names(communities) == "Com_ID.x"] <- "Com_ID"
  
  list_graph_position[[paste0(Year)]] <- list_graph_position[[paste0(Year)]] %>%
    activate(nodes) %>%
    left_join(communities)
  
 #  Mix color for edges of different color
 list_graph_position[[paste0(Year)]] <- list_graph_position[[paste0(Year)]] %>%
   activate(edges) %>%
   mutate(color_com_ID_to = .N()$color[to], color_com_ID_from = .N()$color[from]) %>%
   mutate(color_edges = DescTools::MixColor(color_com_ID_to, color_com_ID_from, amount1 = 0.5))
  
  # Cleaning progressively
  gc()
}


saveRDS(list_graph_position, paste0(graph_data_path, "list_graph_", first_year, "-", last_year + time_window_length - 1, ".rds"))





# loading the data
list_graph_position <- readRDS(paste0(graph_data_path, "list_graph_", first_year, "-", last_year + time_window_length - 1, ".rds"))

# creating a table with the data for nodes and edges for each window

nodes_lf <- lapply(list_graph_position, function(tbl) (tbl %>% activate(nodes) %>% as.data.table()))
nodes_lf <- lapply(nodes_lf, function(dt) (dt[, .(identifiantcit, x, y, Size_com, Com_ID, color)]))
nodes_lf <- rbindlist(nodes_lf, idcol = "window")
nodes_lf <- nodes_lf[, window := paste0(window, "-", as.integer(window) + 4)][order(identifiantcit, window)]

edges_lf <- lapply(list_graph_position, function(tbl) (tbl %>% activate(edges) %>% as.data.table()))
edges_lf <- lapply(edges_lf, function(dt) (dt[, .(Source, Target, weight, Com_ID, color_edges)]))
edges_lf <- rbindlist(edges_lf, idcol = "window")
edges_lf <- edges_lf[, window := paste0(window, "-", as.integer(window) + 4)]

nodes_info <- desco[desco$identifiantcit %in% unique(nodes_lf$identifiantcit)][, c("identifiancit", "titre", "Datepubli2", "publi")]

write_csv(nodes_lf, "nodes_lf.csv")
write_csv(edges_lf, "edges_lf.csv")
write_csv(nodes_info, paste0(platform_data, "nodes_info.csv"))











Year<-1995
run = T

if(run == TRUE){
  list_graph_position <- readRDS(paste0(graph_data_path, "list_graph_", first_year, "-", last_year + time_window_length - 1, ".rds"))
  
  com_label <- list()
  
  for (Year in all_years) {
    com_label[[paste0(Year)]] <- label_com(list_graph_position[[paste0(Year)]], biggest_community = FALSE, community_threshold = 0.01, community_name_column = "Com_ID", community_size_column = "share")
  }
  
  list_ggplot <- list()
  for (Year in all_years) {
    list_ggplot[[as.character(Year)]] <- ggraph(list_graph_position[[paste0(Year)]], "manual", x = x, y = y) +
      geom_edge_link0(aes(color = color_edges, width = weight), alpha = 0.5) +
      geom_node_point(aes(fill = color, size = size), pch = 21) +
      scale_edge_width_continuous(range = c(0.1, 0.5)) +
      scale_size_continuous(range = c(0.1, 3)) +
      theme_void() +
      # new_scale("size") +
      geom_label_repel(data = com_label[[paste0(Year)]], aes(x = x, y = y, label = Com_ID, fill = color), size = 0.5, fontface = "bold", alpha = 0.9, point.padding = NA, show.legend = FALSE) +
      theme(legend.position = "none") +
      scale_fill_identity() +
      scale_edge_colour_identity() +
      labs(title = paste0(as.character(Year), "-", as.character(Year + time_window_length - 1)))
    #ggsave("Networks/coup_2000.png", width=30, height=20, units = "cm")
  }
  
  
  benchmark <- do.call(grid.arrange, list_ggplot[c(1, 5, 9, 13, 17, 22)])
  ggsave(paste0(picture_path, "benchmark_edge_threshold_3.png"), benchmark, width = 30, height = 30, unit = "cm")
  
  ggsave(plot = g, paste0("Graphs/", author, "_networks.png"), width = 30, height = 40, units = "cm")
  
  library(ggpubr)
  for (i in c(1, 9, 18, 27, 35)) {
    g <- ggarrange(plotlist = list_ggplot[i:(i + 7)], common.legend = TRUE, legend = "none")
    ggsave(plot = g, paste0(picture_path, "Graph_", i), width = 30, height = 40, units = "cm")
    gc()
  }
}



