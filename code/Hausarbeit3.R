---
title: "Hausarbeit3"
output: pdf_document
---
# Hausarbeit 3
## Hoesli/Lattmann
### 1. Netzwerk erstellen
Wie bereits bei der letzten Arbeit beschrieben stellt unser Netzwerk die Transfers
zwischen allen Teams aus den beiden höchsten Fussballigen der Schweiz in der Saison
2021/22 dar. Die Knoten stellen die 20 Mannschaften dar (je 10 pro Liga), während
die Kanten jeweils einen Transfer, also einen Spieler, welcher von einem Team zu
einem anderen gewechselt ist, verkörpern. Weil die Richtung des Wechsels bestimmt
ist, handelt es sich um ein gerichtetes Netzwerk.
```{r}
## ----set up of packages, include=FALSE--------------------------------------------------------------------------------------------------------
# install.packages("statnet")
library("RColorBrewer")
library("statnet")
library("scales")
library("formatR")


## ----set up of edge list , tidy=TRUE----------------------------------------------------------------------------------------------------------
edge_list <- read.csv("/Users/julienlattmann/Desktop/Edge List Values.csv", sep = ";")

# Ertsellen der Kanten-Liste aufgrund der gesammelten Daten
netmat2 <- edge_list

# Netzwerk-Objekt erstellen
transfer_net <- network(netmat2, matrix.type = "edgelist", directed = TRUE)

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
spendings <- read.csv("/Users/julienlattmann/Desktop/Spendings.csv", sep = ";")
set.edge.attribute(transfer_net, "spendings", spendings)

#Kanten-Attribut "type" definieren: Spieler-Transaktionstyp (Ablöse, Leihe, unbekannt)
type <- read.csv("/Users/julienlattmann/Desktop/Types_ID.csv", sep = ";")
set.edge.attribute(transfer_net, "type", type)


# Knoten-Attribut "alldeg" definieren (abgekürzte Variante)
transfer_net %v% "alldeg" <- degree(transfer_net)

summary(transfer_net)
```


### 2. Masszahlen
Die Grösse des Netzwerks beträgt nicht überraschend 20, was der Anzahl der Knoten
entspricht. Eine grössere Aussagekraft zur Beschreibung des Netzwerkes hat der 
Wert für die Dichte, welcher 0.242 beträgt und somit auf eine ziemlich hohe 
Dichte hinweist, welche sich auch beim Betrachten des Graphen widerspiegelt. Die
hohe Dichte geht ausserdem darauf zurück, dass viele von allen möglichen 
Interaktionen zwischen den Teams ausgeschöpft werden. Passend zu diesem Ergebnis
besteht das Netzwerk nur aus einer einzigen Komponente, womit alle Knoten über
gewisse Pfade miteinander verbunden sind. Dies zeigt sich gewissermassen auch im
Durchmesser des Netzwerkes, welcher 5 beträgt und die Länge des kürzesten Pfades
zwischen den beiden am weitesten von einander entfernten Knoten ausdrückt. Anders
ausgedrückt: Bei einer Netzwerkgrösse von 20 scheint ein Durchmesser von 5 eher 
klein zu sein, was widerum für ein dichtes Netzwerk spricht. Mit 0.176 fällt 
zuletzt der Clustering-Koeffizient nicht allzu hoch aus. Dies kommt wohl daher,
da im Graphen nicht allzu viele "Dreicke" vorhanden sind bzw. gewisse Teams keine
Spieler untereinander austauschten. Die Masszahlen drücken schliesslich aus, dass
es sich beim vorliegenden Netzwerk um ein äusserst Dichtes Gebilde handelt, in 
welchem alle der 20 Akteure mit einigen der anderen Knoten interagieren, 
Interaktionspartner aber dennoch bewusst selektioniert werden (dafür spricht der
eher geringe Clustering-Koeffizient). 
```{r}
########## Start Hausarbeit 3 ########## 

#Grösse des Netzwerkes berechnen
Grösse <- network.size(transfer_net)
Grösse

#Dichte des Netzwerkes berechnen
Dichte <- gden(transfer_net)
Dichte
# oder von Hand --> m/(n(n-1))
Dichte_H <- 92/(20*(20-1))
Dichte_H

#Komponenten des Netzwerkes berechnen
Komp <- components(transfer_net)
Komp

#Durchmesser des Netzwerkes berechnen
Dm <- geodist(transfer_net)
Dm_max <- max(Dm$gdist)
Dm_max

#Clustering Coefficient des Netzwerkes berechnen
Clust <- gtrans(transfer_net)
Clust
```

### 3. Zentralitäts-Masse
Bei der Betrachtung der verschiedenen Zentralitäts-Masse offenbaren sich einige
Unterschiede bezüglich der Varianz der Zentralität zwischen den Knoten.
Bei der degree-basierten Zentralisierung variieren die Werte für alle Knoten in 
einem überschaubaren Bereich von 3-7. Dies drückt aus, wie viele direkte 
Verbindungen zu anderen Knoten bei jedem Knoten vorhanden sind (Jansen, 2006).
Gemäss Jansen weist das Mass auf die "mögliche Kommunikationsaktivität" hin, 
womit die Aktivität der Teams bezüglich dem Abgeben von Spielern nicht allzu fest
variiert und somit auch kein Team einen extrem "zentralen" Platz im Graphen
einnimmt. 
Die betweenness-basierte Zentralisierung offenbart grössere Unterschiede zwischen
den einzelnen Knoten, da die Unterschiede bezüglich kürzesten Pfaden, welche
durch den betrachteten Knoten führen, grösser sind als jene bezüglich direkten 
Verbindungen. Der Wertebereich reicht somit von 1.7 bis 28.8 und drückt gemäss
Jansen (2006) die "mögliche Kommunikationskontrolle" aus. Das "zentralste" Team
im Netzwerk wäre somit YB, welches über ein hohes Mass an "Kontrolle" darüber
verfügt, wie Spieler innerhalb der Liga transferiert werden. 
Zulezt kann noch die closeness-basierte Zentralisierung betrachtet werden, welche 
aufgrund der Werte. welche alle nahe bei 0.5 liegen und nur wenig variieren,
nahe legt dass alle Knoten ein ähnlich hohes Mass an Zentralität aufweisen. Die
tiefen Werte hier implizieren, dass zwischen sämtlichen Knoten durchaus geringe
durchschnittliche Distanzen bestehen. Gemäss Jansen (2006) dient das Mass auch
als Indikator für die Unabhängiigkeit bzw. Effizienz der Knoten, welche somit 
bei allen Teams hoch sein dürfte.
Einserseits sagen somit alle verschiedenen Zentralisierungs-Masse aus, dass im 
vorliegenden Netzwerk nicht allzu grosse Unterschiede bezüglich der zentralen
Positionierung der Knoten vorliegen und somit kaum "periphere" Akteure vorhanden
sind. Auf der anderen Seite legt v.a. die betweenness-basierte Zentralisierung
nahe, dass einige Knoten eine gewisse stärkere "Kontrolle" über das gesamte
Transfergeschehen haben als andere. 

```{r}
#Betweenness
bet <-betweenness(transfer_net, gmode = "graph")
bet
#Degree
deg <-degree(transfer_net, gmode = "graph")
deg
#Closeness
cls <-closeness(transfer_net, gmode = "graph")
cls
```

### 4. Visualisierung mit Knotenattributen
Der Vergleich der verschiedenen Zentralitätsmasse in der Visualisierung zeigt
wie bereits oben beschrieben, dass allgmein eher hohe Zentralität vorliegt. Vor
allem bei der Betweenness-Zentralität offenbaren sich aber dennoch einige 
Unterschiede. Es zeigt sich, dass Knoten, welche auch optisch am zentralsten im
Netzwerk liegen, ebenfalls grösser dargestellt werden (aufgrund der höheren 
Betweenness-Werte). Diese Knoten dürften somit eine wichtige Stellung für das 
Transfergeschehen in den beiden Ligen haben.
```{r}
par(mfrow=c(1,3))
linecol_pal <- c("#e0209d","#fa9fb5","#ffc74f")
league_pal = c("#aa36eec4", "#f353de")
type_cat <- as.factor(get.edge.attribute(transfer_net,"type"))
league_cat = as.factor(get.vertex.attribute(transfer_net,"league"))

#Using Betweenness as vertex size
set.seed(7)
gplot(transfer_net, vertex.col = league_pal[league_cat],
      displaylabels = FALSE,
      vertex.cex = sqrt(bet),
      vertex.border = "white",
      edge.col= linecol_pal[transfer_net %e% 'type'],
      edge.lwd = sqrt(transfer_net 
                      %e% "spendings")*0.35,mode = "fruchtermanreingold",
      boxed.labels = TRUE,label.border = "white", label.pos = 0,
      label.bg = "white", label.col = league_pal[league_cat],
      label.cex = 0.85, usearrows = TRUE, main = "Betweenness")

#Using Closness as vertex size
set.seed(7)
gplot(transfer_net, vertex.col = league_pal[league_cat],
      displaylabels = FALSE,
      vertex.cex = sqrt(cls),
      vertex.border = "white",
      edge.col= linecol_pal[transfer_net %e% 'type'],
      edge.lwd = sqrt(transfer_net 
                      %e% "spendings")*0.35,mode = "fruchtermanreingold",
      boxed.labels = TRUE,label.border = "white", label.pos = 0,
      label.bg = "white", label.col = league_pal[league_cat],
      label.cex = 0.85, usearrows = TRUE, main = "Closeness")

#Using Degree as vertex size
set.seed(7)
gplot(transfer_net, vertex.col = league_pal[league_cat],
      displaylabels = FALSE,
      vertex.cex = sqrt(deg),
      vertex.border = "white",
      edge.col= linecol_pal[transfer_net %e% 'type'],
      edge.lwd = sqrt(transfer_net 
                      %e% "spendings")*0.35,mode = "fruchtermanreingold",
      boxed.labels = TRUE,label.border = "white", label.pos = 0,
      label.bg = "white", label.col = league_pal[league_cat],
      label.cex = 0.85, usearrows = TRUE, main = "Degree")

```


### 5. Brücken und Cutpoints
Da es sich um ein sehr zentrales Netzwerk handelt, lassen sich darin weder 
Brücken noch Cutpoints finden. Schaut man sich aber bspw. nur die Super League
isoliert an, offenbaren sich der FC Basel und der FC Luzern als Cutpoints. 
Schliesst man die beiden Teams also aus dem Netzwerk aus, bilden sich mehrere 
Komponenten für die Liga. Dies kann bspw. so interpretiert werden, dass Teams
aus der Challenge League für das gesamte Transfergeschehen wichtige "Brücken" 
bilden, welche schliesslich den Spieleraustausch zwsischen Teams in der Super
League begünstigen und ansonsten vielleicht eher voneinander isoliert wären.
```{r}
#Cutpoints identifizieren (weak component rule for directed networks)
cps <- cutpoints(transfer_net, connected = "weak")
cps

#Brücken identifizieren
bridges <- function(dat,mode="graph",
                    connected=c("strong", "weak")){
e_cnt <- network.edgecount(dat)
if (mode == "graph") {
   cmp_cnt <- components(dat)
   b_vec <- rep(FALSE,e_cnt)
   for(i in 1:e_cnt){
      dat2 <- dat
      delete.edges(dat2,i)
      b_vec[i] <- (components(dat2) != cmp_cnt)
      }
}

else {
 cmp_cnt <- components(dat,connected=connected)
 b_vec <- rep(FALSE,e_cnt)
 for(i in 1:e_cnt){
 dat2 <- dat
 delete.edges(dat2,i)
b_vec[i] <- (components(dat2,connected=connected)
!= cmp_cnt)
}
}
return(b_vec)
}

bridges(transfer_net)

brnet <- bridges(transfer_net)
#Welche Edges sind Brücken?
which(brnet==TRUE)

par(mar=c(1,0,1,0), mfrow=c(1,1))
net2 <- transfer_net
components(net2)

delete.edges(net2,c(39,92))
components(net2)

gplot(net2,gmode="graph",vertex.col="red",
edge.col=brnet+2,
jitter=FALSE,displaylabels=TRUE)

#Cutpoints und Brücken für den Subgraphen der Super League
net_SL <- delete.vertices(transfer_net, 11:20)
gplot(net_SL, displaylabels=TRUE)
cutpoints(net_SL, connected = "weak")
brnet_SL <- bridges(net_SL)
which(brnet_SL==TRUE)

net_SL2 <- delete.vertices(net_SL, 2)
components(net_SL2)
gplot(net_SL2, displaylabels = TRUE)
```

### 6. Zentrale Aussage
Als zentrale Aussage lässt sich aus den empirischen Gegebenheiten ableiten, dass
das Transfernetzwerk der beiden höchsten Schweizer Fussballigen ein sehr dichtes
Gefüge ist, in welchem zwischen verschiedensten Teams Spieler ausgetauscht werden
und die meisten Teams ebenfalls eine durchaus hohe Zentralität im Netzwerk aufweisen,
was wiederum für einen starken "Wettbewerb" unter den Klubs spricht. Zudem 
wird dadurch auch aufgezeigt, wie wichtig die heimischen Ligen als Partner für
die Kaderplanung sein können.












