# Justify your decision about how many groups to identify. Discuss how you think the results of this analysis align, or not, with Roethlisberger & Dickson’s assessment (Fig. 10, above).

############################## LOADING & PREPARING DATA ##########################################
dev.off()
#install.packages("data.tree")
#install.packages("blockmodeling")
library(multigraph)
library(data.tree)
library(linkcomm)
library(gplots)
library(tibble)
library(concor)
detach("package:concor", unload=T)
library(ergm)
detach(package:ergm)
require(intergraph)
require(igraph)
require(blockmodeling)
library(data.tree)
knitr::opts_chunk$set(echo = TRUE)
options(digits = 2)

######################################## QUESTION 1 ############################################
# Generate the five networks. Use the five networks’ adjacency matrices combined to generate the structural equivalences of the nodes. 
# Make the negative relationships (NEG and CON) negative in this matrix. Remember that for directed networks, we need to consider both outgoing and incoming edges.

# (1) Whether people seem to like one another (undirected, “POS”)
liking <- read.csv("wiring_RDPOS.csv", header = T, row.names = 1)
liking_g <- graph_from_adjacency_matrix(as.matrix(liking),mode = "undirected")
plot(liking_g, names=TRUE)
liking_adj <- get.adjacency(liking_g)
liking_adj_matrix <- as.matrix(liking_adj)

# Whether someone helped another (directed, “HLP”)
helping <- read.csv("wiring_RDHLP.csv", header = T, row.names = 1)
helping_g <- graph_from_adjacency_matrix(as.matrix(helping),mode = "directed")
plot(helping_g, names=TRUE)
helping_adj <- get.adjacency(helping_g)
helping_adj_matrix <- as.matrix(helping_adj)
helping_both_matrix <- rbind(helping_adj_matrix, t(helping_adj_matrix)) 

# Whether people played games together and horsed around (undirected,“GAM”)
games <- read.csv("wiring_RDGAM.csv", header = T, row.names = 1)
games_g <- graph_from_adjacency_matrix(as.matrix(games),mode = "undirected")
plot(games_g, names=TRUE)
games_adj <- get.adjacency(games_g)
games_adj_matrix <- as.matrix(games_adj)

# Whether people had an antagonistic relationship (undirected,“NEG”)
negative <- read.csv("wiring_RDNEG.csv", header = T, row.names = 1)
negative_g <- graph_from_adjacency_matrix(as.matrix(negative),mode = "undirected")
plot(negative_g, names=TRUE)
negative_adj <- get.adjacency(negative_g)
negative_A_matrix <- as.matrix(negative_adj)
negative_adj_matrix <- (-1)*negative_A_matrix

# Whether people got in arguments/conflict (undirected, “CON”)
arguments <- read.csv("wiring_RDCON.csv", header = T, row.names = 1)
arguments_g <- graph_from_adjacency_matrix(as.matrix(arguments),mode = "undirected")
plot(arguments_g, names=TRUE)
arguments_adj <- get.adjacency(arguments_g)
arguments_A_matrix <- as.matrix(arguments_adj)
arguments_adj_matrix <- (-1)*arguments_A_matrix

all_relationship_matrix <- rbind(liking_adj_matrix, helping_both_matrix, games_adj_matrix, negative_adj_matrix, arguments_adj_matrix)
all_relationship_matrix
all_corrs <- cor(all_relationship_matrix)
all_dist <- as.dist(1 - all_corrs)
all_dist[is.na(all_dist)] <- 2
all_dist

######################################## QUESTION 2 ############################################
# Create a dendrogram from the resulting matrix of structural equivalences.
all_dendrogram <- hclust(all_dist)
plot(all_dendrogram)
rect.hclust(all_dendrogram, k = 2)
plot(all_dendrogram)
rect.hclust(all_dendrogram, k = 3)
plot(all_dendrogram)
rect.hclust(all_dendrogram, k = 4)
plot(all_dendrogram)
rect.hclust(all_dendrogram, k = 5)
all_matrix <- as.matrix(all_dist)
heatmap.2(all_matrix, 
          denscol="black",
          trace = "none",
          margins =c(6,4),
          revC = TRUE)

partit <- cutree(all_dendrogram, k = 4)
partit

######################################## QUESTION 3 ############################################
# Use the adjacency matrices to create two merged networks: one of positive relationships and one of negative relationships. 
# Plot both networks, colouring the nodes by block memberships, determined from the dendrogram.

positive_relationship_matrix <- liking_adj_matrix + helping_adj_matrix + t(helping_adj_matrix) + games_adj_matrix
liking_pos <- graph_from_adjacency_matrix(positive_relationship_matrix,mode = "undirected")
plot(liking_pos, 
  names=TRUE,
  vertex.color= cutree(all_dendrogram, k = 4),
  main = "Positive Relationships at Hawthorne")

negative_relationship_matrix <- negative_A_matrix + arguments_A_matrix
disliking_pos <- graph_from_adjacency_matrix(negative_relationship_matrix, mode = "undirected")
plot(disliking_pos, 
     names=TRUE,
     vertex.color= cutree(all_dendrogram, k = 4),
     main = "Negative Relationships at Hawthorne")

######################################## QUESTION 4 ############################################
# Justify your decision about how many groups to identify in the dendrograpms in Q2. 
# Discuss how you think the results of this analysis align, or not, with Roethlisberger & Dickson’s assessment (Fig. 10, above).

# After some deliberation, I decided to identify 4 blocks in the network. Why?
# Firstly, having 4 blocks allowed me to account for the 2 main cliques that were reported in Roethlisberger & Dickson (1939).
# And, secondly, it allowed me to account for the 4 workers who were members of neither group and who generally tended to 
# keep to themselves accoring to the records by Roethlisberger & Dickson (1939).

# You might be asking, why didnt I just pick 3 blocks then - 2x1 for each clique and 1 for the more isolated individuals?
# I originally tried to do that but the dendrogram identified S2 as a separate block before identifying the other clique-excluded individuals in the block (when k=3).
# To understand this assortment, I read some notes by Roethlisberger & Dickson (1939) and they reported that Solderer 2 had a speech handicap and, 
# in their view, S2 generally had even fewer relationship ties than the other excluded workers at the factory.

# Generally, my findings align with Figure 10 in Roethlisberger & Dickson (1939).
# The figure postulates 2 cliques - clique A consisting of workers W1, W3, W4, S1 and I1
# and clique B consisting of W6, W7, W8, W9 and S4.
# This is exactly consistent with my findings. 
# In the Positive Relationships graph, we see that the 5 workers W1, W3, W4, S1 and I1 are labelled as structurally equivalent in orange.
# And, likewise, W6, W7, W8, W9 and S4 are members of the same block in green. 
# In my analysis, W2, W5, an I3 are excluded from the 2 cliques (in blue) and are structurally equivalent. 
# And, finally, S2 is in a unique position in the network.


######################################## QUESTION 5 ############################################
# Create a new graph object representing who supervises whom (treat the solderers as “supervising” the wiremen they work with and the inspectors as supervising the solderers). 
# Calculate the regular equivalence of the nodes, and plot the network showing the resulting equivalence sets.

hawthorne <- Node$new("Hawthorne Director")
I1 <- hawthorne$AddChild("I1")
S1 <- I1$AddChild("S1")
W1 <- S1$AddChild("W1")
W2 <- S1$AddChild("W2")
W3 <- S1$AddChild("W3")
I3 <- hawthorne$AddChild("I3")
S2 <- I3$AddChild("S2")
W4 <- S2$AddChild("W4")
W5 <- S2$AddChild("W5")
W6 <- S2$AddChild("W6")
S4 <- I3$AddChild("S4")
W7 <- S4$AddChild("W7")
W8 <- S4$AddChild("W8")
W9 <- S4$AddChild("W9")
print(hawthorne)
hawthorne_ig <- as.igraph(hawthorne, directed = FALSE)
plot(hawthorne_ig)

hierarchy_adj <- get.adjacency(hawthorne_ig)
hierarchy_adj_matrix <- as.matrix(hierarchy_adj)
hierarchy_rege <- REGE.nm.for(hierarchy_adj_matrix)$E
hierarchy_rege
heatmap(hierarchy_rege)

plot.mat(hierarchy_adj_matrix, 
         print.val = TRUE)
hierarchy_dist <- as.dist(1 - hierarchy_rege)
hierarchy_hclust <- hclust(hierarchy_dist, method = "ward.D2")
hierarchy_clusters <- cutree(hierarchy_hclust, 
                       k = 4)
plot.mat(hierarchy_adj_matrix, 
         print.val = TRUE,
         clu = hierarchy_clusters)

plot(hawthorne_ig, 
     vertex.color = hierarchy_clusters)

