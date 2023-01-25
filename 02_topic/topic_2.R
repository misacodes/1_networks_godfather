#### MY461 FORMATIVE ASSIGNMENT 1 ####

## LOADING PACKAGES
library(igraph)
library(igraphdata)

## ON CAMERA DIALOGUES OF THE SICILIAN MAFIA NETWORK
mafia <- graph(edges=c(
  1,2,
  1,3,
  1,4,
  1,5,
  1,6,
  1,7,
  1,8,
  1,9,
  1,10,
  1,11,
  1,12,
  1,13,
  1,14,
  1,15,
  2,16,
  2,17,
  2,3,
  2,18,
  2,6,
  2,19,
  2,5,
  2,20,
  6,7,
  6,21, 
  6,3,
  6,22,
  6,23,
  6,24,
  6,18,
  6,10,
  6,25,
  6,26,
  6,27,
  6,28,
  6,29,
  6,20,
  6,30,
  6,9,
  6,31,
  6,32,
  6,33,
  3,21,
  3,34,
  3,10,
  3,24,
  3,4,
  3,35,
  8,11,
  8,10,
  10,11,
  5,20,
  5,9,
  5,17,
  9,34,
  18,19,
  19,32,
  7,21,
  7,30,
  20,11,
  20,12),
  directed=FALSE)

## VERTEX, CHARACTER ATTRIBUTES IN THE SICILIAN MAFIA NETWORK
# Character names
V(mafia)$name <-      c("Don Corleone","Sonny Corleone","Tom Hagen","Bonasera","Connie C. Rizzi",
                       "Michael Corleone","Fredo Corleone","Luca Brasi","Johnny Fontane","Virgil Sollozzo",  
                       "Don Tattaglia","Don Barzini","Don Cuneo","Don Stracci","grandson Anthony", 
                       "Lucy Mancini","Sonny's Wife","Peter Clemenza","Paulie Gatto","Carlo Rizzi",      
                       "Kay Adams","nurse","Enzo","Captain McCluskey","Don Tommasino",    
                       "Calo","Fabrizio","Don Vitelli","Apollonia","Moe Greene",       
                       "Willie Cicci","Rocco Lampone","Salvatore Tessio","Jack Woltz","Tessa")  

# Character status at the end of the film
V(mafia)$cue <-     c("deceased","deceased","alive","alive","alive",
                      "alive","alive","deceased","alive","deceased",
                      "deceased","deceased","deceased","deceased","alive",
                      "alive","alive","alive","deceased","deceased",
                      "alive","alive","alive","deceased","alive",
                      "alive","alive","alive","deceased","deceased",
                      "alive","alive","deceased","alive","alive")

# Vertex color associated with character status
V(mafia)$color <-   c("brown2","brown2","lightblue3","lightblue3","lightblue3",
                      "lightblue3","lightblue3","brown2","lightblue3","brown2",
                      "brown2","brown2","brown2","brown2","lightblue3",
                      "lightblue3","lightblue3","lightblue3","brown2","brown2",
                      "lightblue3","lightblue3","lightblue3","brown2","lightblue3",
                      "lightblue3","lightblue3","lightblue3","brown2","brown2",
                      "lightblue3","lightblue3","brown2","lightblue3","lightblue3")

# Character gender
V(mafia)$gender <-    c("M","M","M","M","F",
                        "M","M","M","M","M",
                        "M","M","M","M","M",
                        "F","F","M","M","M",
                        "F","F","M","M","M",
                        "M","M","M","F","M",
                        "M","M","M","M","M")

# Character estimated age in years
V(mafia)$age <-       c("60","35","35","60","25",
                        "25","30","40","40","60",
                        "60","60","60","60","2",
                        "25","30","40","50","40",
                        "30","40","30","45","60",
                        "30","30","55","20","50",
                        "35","40","45","45","45")


## DESCRIPTIVE STATISTICS OF THE SICILIAN MAFIA NETWORK 
summary(mafia)
vcount(mafia)
ecount(mafia)
degree(mafia)
mean(degree(mafia))
graph.density(mafia)
diameter(mafia)


## PLOTTING THE SICILIAN MAFIA NETWORK 
# Specifying that size of each node will depend on its interconnectedness
deg <- degree(mafia, mode="all")
plot(mafia, 
     vertex.size=deg*2.5,
     vertex.frame.color="grey",
     vertex.label.color="black",
     vertex.label.font=2,   
     vertex.label.cex=0.5,
     vertex.label.family="Times",
     edge.color="grey",                        
     edge.width=1.5)

## GETTING THE EDGELIST
get.edgelist(mafia)


## Getting the Adjacency Matrix
get.adjacency(mafia)
get.adjacency(mafia, sparse = FALSE)
