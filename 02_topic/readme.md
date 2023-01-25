# The Corleone family network
In this assignment, I am analyzing the network of **Corleone family**, as outlined in **The Godfather (1972)** film directed by Coppola and co-written by Mario Puzo. I depict the Corleone Sicilian mafia network based on characters' **on-camera dialogues/appearances**. For example, if Michael Corleone and Sonny Corleone have a dialogue in the movie, they are connected in this network. Since I am focusing on on-camera dialogues, the depicted network is undirected (i.e., there cannot be an arrow pointing from one character to the next as dialogues are bi-directional by their nature).

### Assigning edge attributes
I start by loading the necessary libraries (igraph and igraphdata) and by creating an *edgelist dataframe* (i.e., a dataframe of relationships in the network). For this, I use the *graph()* function.

```
graph(edges=c(...))
```

### Assigning vertex attributes
After assigning these edge attributes of the network, I move on to assign *vertex/node attributes*. In other words, I assign names to each member of the network, their gender, their age and whether they survived till the end of the first Godfather movie. Additionally, I assign corresponding dead or alive status colors to each character for subsequent graphing purposes. I assign these node characteristics manually, using the following functions:

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
... assures me that all 5 vertex attributes have been assigned correctly to the characters, that the network has been correctly labelled as undirected. Then, the following functions tell me *the number of vertices (35 in total)* and *the number of realized edges (60 in total)*, respectively:

```
vcount(mafia)
ecount(mafia)
```
Finally, other descriptive functions are used also: 

```
degree(mafia)
mean(degree(mafia))
graph.density(mafia)
```
They uncover that **Michael Corleone (21)** and **Don Corleone (14)** have the greatest *degrees*, i.e., the greatest number of on-screen dialogues. It seems women have dialogues much less frequently then men in the film. That can be further demonstrated through the *adjacency matrix* described in the following code section. 

```
get.adjacency(mafia)
get.adjacency(mafia, sparse = FALSE)
```

Overall, a character in the film is involved in approx. *3,43 degrees (dialogues)*. The network has *density* of 0.1008403 which implies that approximately 10.08% of all potential network dialogues are realized. All potential dialogues would be realised if each individual character would talk to each other individual character in the movie. The *diameter*, i.e. longest geodesic path length, is 3. That means that at most 3 movie characters are connected through their shared dialogues (i.e., one talks to a second and the second talks to a third character).


### Plotting the network
To plot the network graph, I am using the basic *plot()* function in this exercise. While doing so, I specify the size and color of each node, together with multiple other characteristics. The resulting network graph plus all the R code can be found in this folder. 



