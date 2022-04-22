## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----set up of packages, include=FALSE--------------------------------------------------------------------------------------------------------
# install.packages("statnet")
library("RColorBrewer")
library("statnet")
library("scales")
library("formatR")


## ----set up of edge list , tidy=TRUE----------------------------------------------------------------------------------------------------------
edge_list <- read.csv("data/Edge_List_Values.csv", sep = ";")

# Ertsellen der Kanten-Liste aufgrund der gesammelten Daten
netmat2 <- edge_list

# Netzwerk-Objekt erstellen
transfer_net <- network(netmat2, matrix.type = "edgelist", directed = TRUE)

# Knoten benennen
#network.vertex.names(transfer_net) <- c(
#    "BSC Young Boys", "FC Basel", "Servette FC", "FC Lugano", "FC Luzern", "Lausanne-Sport", "FC St. Gallen", "FC Zürich", "FC Sion", "Grasshoppers",
#    "FC Vaduz", "FC Thun", "Stade-Lausanne", "FC Schaffhausen", "FC Aarau", "FC Winterthur", "FC Wil 1900", "SC Kriens", "Neuchâtel Xamax", "Yverdon #Sport"
#)

# Knoten Acronyms 
network.vertex.names(transfer_net) <- 
  c(
    "YB",
    "BAS",
    "SFC",
    "LUG",
    "LUZ",
    "LS",
    "SG",
    "FCZ",
    "SION",
    "GC",
    "VAD",
    "THU",
    "SLO",
    "FCS",
    "AAR",
    "WIN",
    "WIL",
    "SCK",
    "XAM",
    "YS")




# Knoten-Attribut "league" definieren
set.vertex.attribute(transfer_net, "league", c(
    "SL", "SL", "SL", "SL", "SL",
    "SL", "SL", "SL", "SL", "SL",
    "ChL", "ChL", "ChL", "ChL", "ChL",
    "ChL", "ChL", "ChL", "ChL", "ChL"
))

#Kanten-Attribut "spendings" definieren: Transfersumme in Tsd.
spendings <- read.csv("data/Spendings.csv", sep = ";")
set.edge.attribute(transfer_net, "spendings", spendings)

#Kanten-Attribut "type" definieren: Spieler-Transaktionstyp (Ablöse, Leihe, unbekannt)
type <- read.csv("data/Type_ID.csv", sep = ";")
set.edge.attribute(transfer_net, "type", type)


# Knoten-Attribut "alldeg" definieren (abgekürzte Variante)
transfer_net %v% "alldeg" <- degree(transfer_net)

summary(transfer_net)




## ----first few plots, echo=TRUE , tidy=TRUE, tidy.opts = list(blank = FALSE, width.cutoff = 60)-----------------------------------------------

my_pal = c("#2ca25f","#de2d26")
league_cat = as.factor(get.vertex.attribute(transfer_net,"league"))

edge_pal = c("#a8ddb5")

par(mar= c(2,0,2,0),mfrow=c(2,3))
gplot(transfer_net, vertex.col = my_pal[league_cat], displaylabels = TRUE, label.cex = 0.75, edge.col = edge_pal, edge.lwd = 0.5, mode = "circle", main = "circle", label.col = my_pal[league_cat])
gplot(transfer_net, vertex.col = my_pal[league_cat], displaylabels = TRUE, label.cex = 0.75, edge.col = edge_pal, edge.lwd = 0.5, mode = "kamadakawai", main = "kamadakawai", label.col = my_pal[league_cat])
gplot(transfer_net, vertex.col = my_pal[league_cat], displaylabels = TRUE, label.cex = 0.75, edge.col = edge_pal, edge.lwd = 0.5, mode = "random", main = "random", label.col = my_pal[league_cat])
gplot(transfer_net, vertex.col = my_pal[league_cat], displaylabels = TRUE, label.cex = 0.75, edge.col = edge_pal, edge.lwd = 0.5, mode = "fruchtermanreingold", main = "fruchtermanreingold", label.col = my_pal[league_cat])
gplot(transfer_net, vertex.col = my_pal[league_cat], displaylabels = TRUE, label.cex = 0.75, edge.col = edge_pal, edge.lwd = 0.5, mode = "fruchtermanreingold", main = "spring", label.col = my_pal[league_cat])
gplot(transfer_net, vertex.col = my_pal[league_cat], displaylabels = TRUE, label.cex = 0.75, edge.col = edge_pal, edge.lwd = 0.5, mode = "fruchtermanreingold", main = "eigen", label.col = my_pal[league_cat])



## ----compute betweenness, degree and closeness------------------------------------------------------------------------------------------------

bet <-betweenness(transfer_net, gmode = "graph")
bet

deg <-degree(transfer_net, gmode = "graph")
deg

cls <-closeness(transfer_net, gmode = "graph")
cls




## ----include spendings as quantitatives Knotenattribut----------------------------------------------------------------------------------------

par(mar=c(0,0,0,0), mfrow=c(1,1))
vertex_col <- c("#7fcdbb","#1d91c0")
edge_col <- c("#9ebcda")



par(mar=c(1,0,1,0), mfrow=c(1,1))
gplot(transfer_net, vertex.col = vertex_col[league_cat],
      displaylabels = TRUE,
      vertex.cex = deg/1.95,
      edge.col= edge_col ,mode = "fruchtermanreingold", label.cex = 0.75, label.pos = 0
      )



## ----Knotengrösse Anpassen gemäss betweenness , tidy=TRUE, tidy.opts = list(blank = FALSE, width.cutoff = 60)---------------------------------

par(mar=c(1,0,1,0), mfrow=c(1,1))
linecol_pal <- c("#e0209d","#fa9fb5","#ffc74f")
league_pal = c("#aa36eec4", "#f353de")
type_cat <- as.factor(get.edge.attribute(transfer_net,"type"))

gplot(transfer_net, vertex.col = league_pal[league_cat],
      displaylabels = TRUE,
      vertex.cex = sqrt(bet+1)/1.85,
      vertex.border = "white",
      edge.col= linecol_pal[transfer_net %e% 'type'],
      edge.lwd = sqrt(transfer_net 
      %e% "spendings")*0.35,mode = "fruchtermanreingold",
      boxed.labels = TRUE,label.border = "white", label.pos = 0,
      label.bg = "white", label.col = league_pal[league_cat],
      label.cex = 0.85, usearrows = FALSE)
legend("bottomleft", legend= c("Abloese", "Leihe", "unbekannt"),
       col=linecol_pal, pch = 19, pt.cex = 1.5, bty = "n", title = "Transfer-Typ")





## ---------------------------------------------------------------------------------------------------------------------------------------------

get.vertex.attribute(transfer_net,"vertex.names")


class(transfer_net)




## ----convert markdown to R Script-------------------------------------------------------------------------------------------------------------

#knitr::purl("Heimarbeit2.rmd")



