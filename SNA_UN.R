#Bevor wir die Analyse durchf�hren k�nnen, ben�tigen wir alle notwendige Packete.
require(sna)
require(igraph)

#Aus unseren Daten generieren wir ein Matrix.
data=as.matrix(data)
data

#Ich benutze "igraph-Packet", um das urspr�ngliche Netzwerk graphish darzustellen. 
#Dazu erstelle ich zun�chst aus der Matrix ein Graphenobjekt. Eine Edge-Tabelle enth�lt 
#Knoten in den ersten zwei Spalten. Wir f�gen dann die Kantengewichte indem wir 
#ein Kantenattribut namens "weight" zuweisen.
graph=graph.data.frame(data, FALSE)
E(graph)$weight=as.numeric(data[,3])
graph

#Da das Netzwerk gewichtet ist, will ich, dass die Gewichte in der Matrix angegeben werden. 
adj=get.adjacency(graph,attr='alliance',sparse=FALSE)
adj

#Ich visualisiere das Netzwerk als Graph.

plot(graph,layout=layout.fruchterman.reingold,edge.width=E(graph)$weight, 
     vertex.label.cex=0.7, asp = 0.5, vertex.size=5)

#Als n�chsten Scritt analysiere ich das Netzwerk. Ich fange an mit der Messung der Zentralit�t. 
#Bevor ich die Zentralit�t analysiere zerlege ich des gesamten Netzwerkes in seine Komponenten 
#um die Ausbreitung der Fehler einzud�mmen und die Funktionsf�higkeit des Gesamtnetzes zu erhalten.
decomp.graph <- decompose.graph(graph) 
graph_dg <- decomp.graph[[1]]

#Hochzentralisierte Netzwerke haben wenige Knoten mit vielen Verbindungen, 
#niedrigzentralere Netzwerke haben viele Knoten mit �hnlicher Anzahl von Kanten.
#F�r das gesamte Netzwerk berechne ich die Zentralit�t nach "degree", "closeness" 
#und "eigenvector centrality" von Knoten.
centr_degree(graph_dg, mode = "total")$centralization
centr_clo(graph_dg, mode = "total")$centralization
centr_eigen(graph_dg, directed = FALSE)$centralization

#Ich schaue jetzt die weitere Metriken von Knoten und deren "Node-degree" an.
#Ich mache die Standartisierung durch die Anzahl der Knoten:
graph_dg_degree <- degree(graph_dg, mode = "total")
graph_dg_degree_std <- graph_dg_degree / (vcount(graph_dg) - 1)
node_degree <- data.frame(degree = graph_dg_degree,
                          degree_std = graph_dg_degree_std) %>%
  tibble::rownames_to_column()
node_degree 

#Ich schaue jetzt die weitere Metrik "Node-closeness" an:
graph_dg_closeness <- closeness (graph_dg, mode = "total")
graph_dg_closeness_std <- graph_dg_closeness  / (vcount(graph_dg) - 1)
node_closeness <- data.frame(graph_dg_closeness = graph_dg_closeness,
                             graph_dg_closeness_std = graph_dg_closeness_std) %>%
  tibble::rownames_to_column()
node_closeness

#Ich schaue jetzt die weitere Metrik "Node-betweenness" an:
graph_dg_betweenness <- betweenness(graph_dg, directed = FALSE)
graph_dg_betweenness_std <- graph_dg_betweenness / ((vcount(graph_dg) - 1) * (vcount(graph_dg) - 2) / 2)
node_betweenness <- data.frame(graph_dg_betweenness = graph_dg_betweenness,
                               graph_dg_betweenness_std = graph_dg_betweenness_std) %>%
  tibble::rownames_to_column() 
node_betweenness

#Jetzt versuche ich die Communities zu identifizieren. Dies hilft mir die Z�geh�rigkeit
#von Staaten zu m�glichen Allianzen zu interpretieren. 
graph_simply <- simplify(graph)
cc <- cluster_fast_greedy(graph_simply)
dendPlot(cc)
fastgreedy.community(graph_simply)
membership(cc)
sizes(cc)
plot(cc, graph_simply, vertex.label.cex=0.7, asp = 0.5, vertex.size=5)
write.csv2(cc[[1]], file ="community1.csv")
write.csv2(cc[[2]], file ="community2.csv")

#Degree-Zentralit�t
degree.graph_dg <- data.frame(V1 = degree(graph_dg, mode="all"))

#N�he-Zentralit�t
closeness.graph_dg <- data.frame(V2 = closeness(graph_dg))

#Zwischenzentralit�t
betweenness.graph_dg <- data.frame(V3 = betweenness(graph_dg))

#Erstellen eines zusammenfassenden Dataframes
names.graph_dg <- rownames(degree.graph_dg)
net.measures <- data.frame(V1 = names.graph_dg, V2 = degree.graph_dg, V3 = closeness.graph_dg, V4 = betweenness.graph_dg, row.names = NULL)
colnames(net.measures) <- c("Country", "Degree", "Closeness", "Betweeness")

write.csv2(net.measures, file ="net.measures.csv")


