# install.packages("statnet")
library("RColorBrewer")
library("statnet")

edge_list <- read.csv("code/data/Edge_List_Values.csv", sep = ";")

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

# Knoten-Attribut "balace" definieren: Kategorien von 1-4
# repräsentieren verschiedene Kategorien des Kontostands
# set.vertex.attribute(transfer_net, "balance", c("1", "3", "3", "2", "4",
# "4", "1", "1", "2", "1",
# "2", "1", "1", "2", "2"))
# Knoten-Attribut "alledg" definieren (abgekürzte Variante)
transfer_net %v% "alldeg" <- degree(transfer_net)


# Beziehungsart der Kanten definieren
# relation <- c(rep("family", 3), rep("friends", 5), rep("colleague", 3), rep("friends", 4))
# Kanten-Attribut "relation" definieren
# set.edge.attribute(transfer_net, "relation", relation)

summary(transfer_net)

# Visualisierung
par(mfrow=c(1,2))
gplot(transfer_net, vertex.col = c("steelblue", "burlywood1"), displaylabels = TRUE, edge.col = "darkgrey", edge.lwd = 0.5)
gplot(transfer_net, vertex.col = c("steelblue", "burlywood1"), displaylabels = TRUE, edge.col = "darkgrey", edge.lwd = 0.5, mode = "circle")

par(mfrow=c(1,2))
gplot(transfer_net, vertex.col = c("steelblue", "burlywood1"), displaylabels = TRUE, edge.col = "darkgrey", edge.lwd = 0.5, mode = "kamadakawai")
gplot(transfer_net, vertex.col = c("steelblue", "burlywood1"), displaylabels = TRUE, edge.col = "darkgrey", edge.lwd = 0.5, mode = "random")

# my_pal <- brewer.pal(3, "Set1")
# league <- as.factor(get.vertex.attribute(transfer_net, "league"))
# plot(transfer_net, usearrows = FALSE, vertex.cex = 2.5, vertex.col = my_pal[league], displaylabels = T, edge.col = "darkgrey")



Kantenliste <- as.edgelist(transfer_net)
Soziomatrix <- as.sociomatrix(transfer_net)

