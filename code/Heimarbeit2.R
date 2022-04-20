## ----setup, include=FALSE---------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----set up of packages, include=FALSE--------------------------------------------------------------------------------
# install.packages("statnet")
library("RColorBrewer")
library("statnet")


## ----set up of edge list----------------------------------------------------------------------------------------------
edge_list <- read.csv("data/Edge_List_Values.csv", sep = ";")

# Ertsellen der Kanten-Liste aufgrund der gesammelten Daten
netmat2 <- edge_list

# Netzwerk-Objekt erstellen
transfer_net <- network(netmat2, matrix.type = "edgelist", directed = TRUE)
# Knoten benennen
network.vertex.names(transfer_net) <- c(
    "BSC Young Boys", "FC Basel", "Servette FC", "FC Lugano", "FC Luzern", "Lausanne-Sport", "FC St. Gallen", "FC Zürich", "FC Sion", "Grasshoppers",
    "FC Vaduz", "FC Thun", "Stade-Lausanne", "FC Schaffhausen", "FC Aarau", "FC Winterthur", "FC Wil 1900", "SC Kriens", "Neuchâtel Xamax", "Yverdon Sport"
)
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

# Knoten-Attribut "alledg" definieren (abgekürzte Variante)
transfer_net %v% "alldeg" <- degree(transfer_net)

summary(transfer_net)




## ----first few plots, echo=TRUE---------------------------------------------------------------------------------------
par(mar= c(0,0,4,0),mfrow=c(2,2))
gplot(transfer_net, vertex.col = c("steelblue", "burlywood1"), displaylabels = TRUE, label.cex = 0.75, edge.col = "lightgreen", edge.lwd = 0.5, mode = "circle")
gplot(transfer_net, vertex.col = c("steelblue", "burlywood1"), displaylabels = TRUE, label.cex = 0.75, edge.col = "darkgrey", edge.lwd = 0.5, mode = "kamadakawai")
gplot(transfer_net, vertex.col = c("steelblue", "burlywood1"), displaylabels = TRUE, label.cex = 0.75, edge.col = "darkgrey", edge.lwd = 0.5, mode = "random")
gplot(transfer_net, vertex.col = c("steelblue", "burlywood1"), displaylabels = TRUE, label.cex = 0.75, edge.col = "darkgrey", edge.lwd = 0.5, mode = "fruchtermanreingold")


par(mfrow=c(1,1))
gplot(transfer_net, vertex.col = c("steelblue", "burlywood1"), displaylabels = TRUE, edge.col = "darkgrey", edge.lwd = transfer_net %e% "spendings"*0.05)


## ----include spendings as quantitatives Knotenattribut----------------------------------------------------------------

par(mfrow=c(1,1))
gplot(transfer_net, vertex.col = c("steelblue", "burlywood1"), displaylabels = TRUE, edge.col = "darkgrey", edge.lwd = transfer_net %e% "spendings"*0.05)




## ----compute betweenness----------------------------------------------------------------------------------------------


bet <-betweenness(transfer_net, gmode = "graph")






## ----Knotengrösse Anpassen gemäss betweenness-------------------------------------------------------------------------

par(mar=c(0,0,0,0), mfrow=c(1,1))
gplot(transfer_net, vertex.col = c("steelblue", "burlywood1"),
      displaylabels = TRUE, edge.col = "darkgrey",
      vertex.cex = sqrt(bet+1), edge.lwd = transfer_net 
      %e% "spendings"*0.05)





## ---------------------------------------------------------------------------------------------------------------------





