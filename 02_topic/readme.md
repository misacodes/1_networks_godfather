# Topic 02 
In this assignment, I am analyzing the network of **Corleone family**, as outlined in The Godfather (1972) film directed by Coppola and co-written by Mario Puzo. I depict the Corleone Sicilian mafia network based on characters' on-camera dialogues/appearances. For example, if Michael Corleone and Sonny Corleone have a dialogue in the movie, they are connected in this network. Since I am focusing on on-camera dialogues, the depicted network is undirected (i.e., there cannot be an arrow pointing from one character to the next as dialogues are bi-directional by their nature).

### Assigning edge attributes
I start by loading the necessary libraries (igraph and igraphdata) and by creating an edgelist dataframe (i.e., a dataframe of relationships in the network). For this, I use the graph() function.

```
graph(edges=c(...))
```

### Assigning vertex attributes
After assigning these edge attributes of the network, I move on to assign vertex/node attributes. In other words, I assign names to each member of the network, their gender, their age and whether they survived till the end of the first Godfather movie. Additionally, I assign corresponding dead or alive status colors to each character for subsequent graphing purposes. I assign these node characteristics manually, using the following functions:

```
V(mafia)$name <- c(...)
V(mafia)$age <- c(...)
V(mafia)$gender <- c(...)
V(mafia)$cue <-  c(...)

```

### Exploring network characteristics
I then proceed to explore the network. The following function ...

```
summary(mafia)
```
... assures me that all vertex attributes have been assigned correctly to the characters. The following functions vcount and ecount tell me the number of vertices and realized edges:

```
vcount(mafia)
ecount(mafia)
```


Nodes and edges
