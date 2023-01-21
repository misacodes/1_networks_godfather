#####################################################################################
####################### LOADING PACKAGES, UPLOADING DATASETS ######################## 
# install.packages('plot.matrix')
data("karate")
summary(karate)
dev.off()

library(plot.matrix)
library(igraph)
library(igraphdata)
library(intergraph)
library(ape) 
library(Matrix) 
require(sna)
library(plyr)
library(dbplyr)

#####################################################################################
################################## QUESTION 1 ####################################### 
# How did our modeling of Zachary’s karate network with the stochastic block model go? 
# Did we capture the structure and dynamics of Zachary’s karate club? How could we possibly improve it? (2 points)

knet <- asNetwork(karate) 
blockmodel(knet,ec = knet %v% "Faction")
kbm <- blockmodel(knet,ec = knet %v% "Faction")$block.model 
g_kar <- sample_sbm(34, pref.matrix = kbm, block.sizes = c(16,18), directed = FALSE)
assortativity(g_kar, c(rep(1,16), rep(2,18))) 
par(mfrow=c(1,2))
plot(g_kar,
     vertex.color = c(rep(1,16), rep(2,18)), 
     vertex.label = NA,
     main = "Fabricated Network")
plot(karate, 
     vertex.label = NA,
     main = "Real Network")

# Degree centrality 
par(mfrow=c(1,2))
degree_cent_fabricated <- degree(g_kar, mode = "all")
hist(degree_cent_fabricated, 
     col = "#CD7F32",
     main = "Fabricated Network")
degree_cent_real <- degree(karate, mode = "all")
hist(degree_cent_real, 
     col = "#CD7F32",
     main = "Real Network")

# The plots and adjacency matrices compare the actual karate network to a fabricated network that has similar characteristics and equal block memberships.
# If we fully understood the data generating process (DGP) of the actual karate network, then we would be able to set up the fabricated network such that it would be indistinguishable from the real network.
# But, in our case, we observe that the fabricated network does not have exactly the same characteristics as the actual karate network. 
# Primarily, the real network has more vertices with very low degree centrality (i.e. 0-4 connections) and also some vertices with very high degree centrality (i.e. 15+ connections), compared to the fabricated network.
# Such network structure with a few very popular, 'celebrity' nodes has also been commonly reported in other social networks (e.g. school friendship groups) and it has sometimes been associated with the 'Matthew effect' (Merton, 1968).
# But in our specific example, the 2 'celebrity' nodes correspond to the fact that there are 2 instructors (1 instructor Mr. Hi and 1 administrator John A) in the Zachary karate club and vast majority of trainees are connected to at least 1 of them.
# Specifically, instructor Mr. Hi has 16 connections and club administrator John A has 17 connections. 
# In other words, there is a preferential attachment to coaches in the Zachary karate network.
# In contrast to this preferential attachment structure, our fabricated network is set such that every node within 1 block is stochastically equivalent (Yan et al.2014).
# That means that the probability of an edge between any two nodes in the fabricated network depends only on which blocks they belong to; connections between all vertices within a block are equally likely.
# And so, in order to make the fabricated network more similar to the actual karate network, we could use degree-corrected stochastic block modelling (Yan et al.2014). 
# This would relax the assumption of stochastic equivalence within blocks, and instead allow for preferential attachment in the fabricated network. 

#####################################################################################
################################## QUESTION 2 ####################################### 
# Recreate the three graphs plotted in lecture showing an assortative, ordered, and core-periphery structure, using the tie probabilities as shown in the lecture slide. 
# In those plots, block 1 has 20 nodes, block 2 has 20 nodes, and block 3 has 10 nodes. Plot each network (with nodes colored by block membership), as well as an image of the adjacency matrix. 
# For clarity, I replicate the block matrices below. (2 points)

my_block_sizes <- c(20,20,10)

# Assortative Graph
assortative_mat <- cbind(c(0.3,0.005,0.005),
                         c(0.005,0.3,0.005),
                         c(0.005,0.005,0.3))
assortative_graph <- sample_sbm(50, pref.matrix = assortative_mat, block.sizes=my_block_sizes, directed = FALSE, loops = FALSE)
ablock_layout <- layout_components(assortative_graph)
as_adj(assortative_graph)
par(mfrow=c(1,1))
plot(assortative_graph,
     main = "'Assortative'",
     vertex.color =c(rep(1,20), rep(2,20), rep(3,10)),
     vertex.label=NA,
     vertex.size=8,
     edge.width=0.5,
     layout=ablock_layout)
image(assortative_graph[])

# Ordered Graph
ordered_mat <- cbind(c(0.5,0.3,0.001),
                     c(0.3,0.5,0.3),
                     c(0.001,0.3,0.5))
ordered_graph <- sample_sbm(50, pref.matrix = ordered_mat, block.sizes = my_block_sizes, directed = FALSE, loops = FALSE)
oblock_layout <- layout_components(ordered_graph)
as_adj(ordered_graph)
par(mfrow=c(1,1))
plot(ordered_graph,
     main = "'Ordered'",
     vertex.color =c(rep(1,20), rep(2,20), rep(3,10)),
     vertex.label=NA,
     vertex.size=8,
     edge.width=0.5,
     layout=oblock_layout)
image(ordered_graph[])

# Core-periphery
coreper_mat <-  cbind(c(0.7,0.2,0.1),
                       c(0.2,0.4,0.1),
                       c(0.1,0.1,0.25))
coreper_graph <- sample_sbm(50, pref.matrix = coreper_mat, block.sizes = my_block_sizes, directed = FALSE, loops = FALSE)
cblock_layout <- layout_components(coreper_graph)
as_adj(coreper_graph)
par(mfrow=c(1,1))
plot(ordered_graph,
     main = "'Core-Periphery'",
     vertex.color =c(rep(1,20), rep(2,20), rep(3,10)),
     vertex.label=NA,
     vertex.size=8,
     edge.width=0.5,
     layout=cblock_layout)
image(coreper_graph[])

#####################################################################################
################################## QUESTION 3 ####################################### 
# Return to the Village 35 network that we studied in Week 5. 
# Use the code below to create the weighted network that combines all of the relationship types. 
# Calculate the maximum core membership of each node and plot the network with nodes colored by maximum coreness.

bor <- read.csv("adj_borrowmoney_HH_vilno_35.csv", header = FALSE, as.is = TRUE, sep=";")
lend <- read.csv("adj_lendmoney_HH_vilno_35.csv", header = FALSE, as.is = TRUE, sep=";")
kercome <- read.csv("adj_keroricecome_HH_vilno_35.csv", header = FALSE, as.is = TRUE, sep=";")
kergo <- read.csv("adj_keroricego_HH_vilno_35.csv", header = FALSE, as.is = TRUE, sep=";")
all <- bor + lend + kercome + kergo
meta <- read.csv("vil35_meta.csv", header = TRUE, as.is = TRUE)
n_all <- graph.adjacency(as.matrix(all), mode = "undirected", weighted = TRUE)
V(n_all)$caste <- meta$castesubcaste
V(n_all)$religion <- meta$hohreligion
V(n_all)$rooms <- meta$room_no
n_all <- delete.vertices(n_all, V(n_all)[degree(n_all)==0])
coreness(n_all)
range(coreness(n_all))
colors<-rainbow(max(6))
v35_layout <- layout_nicely(n_all)
plot(n_all, 
     vertex.color = colors[coreness(n_all)],
     vertex.size=8,
     edge.width=0.5,
     vertex.label=NA,
     layout=v35_layout)
legend("topright", legend=c(1:6),col = colors,pch = 16,title = "Coreness")

#####################################################################################
################################# QUESTIONS 4,5 ##################################### 
# Determine community membership in the Village 35 network using the cluster_fast_greedy community detection algorithm, making sure to use edge weights. 
# Plot and discuss the resulting communities, with particular mention of how they align (or not) with caste groups. 
# What other “metadata” that we don’t currently have do you think might help us explain the identified communities?  
# Using the intergraph and the sna packages, port that igraph network into a network object readible by the sna package. 
# Use the blockmodel function to assess the probability of ties within and between caste groups (you will have to find a way for R to treat caste as numerical). 
# Discuss the patterns, and how they align with the communities found with the cluster_fast_greedy community detection algorithm. 

fg <- cluster_fast_greedy(n_all, weights = E(n_all)$weight)
sizes(fg)
dendPlot(fg, mode="phylo")

V(n_all)$caste_n <- ifelse(V(n_all)$caste =="GENERAL", 1,
                           ifelse(V(n_all)$caste =="SCHEDULE CASTE",2, 
                                  ifelse(V(n_all)$caste =="OBC",3,
                                         ifelse(V(n_all)$caste =="SCHEDULE TRIBE", 4,
                                                ifelse(V(n_all)$caste =="MINORITY",5,6)))))


par(mfrow=c(1,2))
plot(fg, 
     n_all, 
     edge.width = E(n_all)$weight, 
     vertex.size=8,
     edge.width=0.5,
     vertex.label=NA,
     main="Detected Communities",
     layout = v35_layout)
plot(n_all, 
     edge.width = E(n_all)$weight, 
     vertex.size=8,
     edge.width=0.5,
     vertex.color=V(n_all)$caste_n,
     vertex.label=NA,
     main="Caste Membership",
     layout = v35_layout)
legend("bottomright", inset = c(0.10,-0.25),  
       legend = c("Forward Caste", "Scheduled Caste", "Other Backwards Caste", "Scheduled Tribe", "Minorities"), 
       pch = 19, 
       col = categorical_pal(5)[c(1,2,3,4,5)],
       cex = 0.6,
       title = "Castes",
       pt.cex = 1.1)

sum(V(n_all)$caste =="GENERAL")
sum(V(n_all)$caste =="SCHEDULE CASTE")
sum(V(n_all)$caste =="OBC")
sum(V(n_all)$caste =="SCHEDULE TRIBE")
sum(V(n_all)$caste =="MINORITY")

# The two plots indicate that there is some level of community sorting by caste groups.
# For example, the Fast and Greedy clustering algorithm identifies a considerable fraction of the Scheduled Caste (colored in light blue) as belonging to one community in the village.
# But, on the other hand, members of the Other Backwards Caste appear to be more widely distributed across many different communities within the village. 
# Since the Scheduled Caste is the smallest caste group in the village, whereas the Other Backwards Caste is the largest caste group, the observed community clustering could potentially be linked to social cohesion theory proposed by Georg Simmel (1902).
# Simmel (1902) argued that as group size expands, there is a general loss of internal cohesion in the group because norms that may drive cohesion tend to become more impersonal and unsuitable to the needs of all members (Arora and Sanditov, 2015).
# But it is also possible that the observed community formation patterns among individuals who belong to the same caste cannot be attributed to the caste membership itself, but rather that it masks community formation within families. 
# After all, families generally belong to the same castes. Therefore, future research could collect data on familiar ties within the village, in addition to caste membership, and estimate the relative importance of (1) lineage and (2) caste for community formation separately (e.g. via logit/probit modelling).
# Also, it is important to stress that this is a network of financial help, rather than a friendship network and so ties in this network might be formed less on the basis of homophily and more based on whether individuals believe each other and whether they can successfully share risk among each other.
# For example, as Rosenzweig (1988) points out, lending money to relatives rather than to strangers might be preferred by villagers, since it involves less uncertainty about repayment capacities (less asymmetric information) than lending to strangers, i.e. family members in small villages usually know each other well. 

# In addition, the village dataset comes from an area of rural Karnataka, where agriculture employs a considerable proportion of the local population. 
# And, furthermore, income from agriculture in developing countries tends to be highly volatile. In particular, crop yields from farming on non-irrigated land are highly susceptible to systemic crop failure (Banerjee et al, 2011). 
# This means that there is a high likelihood that all farmers who grow a particular crop, or generally all villagers who rely on crop farming within a village, might suffer from a crop failure at the same time (for example due to prolonged droughts) (Townsend, 1994; Udry, 1994).
# This might have considerable implications for money lending/borrowing network within the village. 
# When a farmer suffers from a crop failure that affects an entire village (and he/she is unable to access formal lending institutions), then he/she will probably not go to fellow crop farmers to ask for financial help,
# but rather, the farmer might ask a livestock farmer for help, or ask someone who has sources of income other than agriculture in the village.
# Therefore, it is worth considering to also incorporate main income sources of the villagers among vertex attributes of the lending/borrowing village network. 
# Unlike Crona and Bodin (2006) who found homophily based on occupation in friendship networks in Kenyan village, I would hypothesize that there might be heterophily based on occupation in this network.

# Taken together, it seems that caste membership isn’t fully dictating community formation in the village and the 'grand truth' behind the observed community clusters cannot be attributed purely to caste identity, but instead it appears to be more complex.
# Other factors to consider might include (1) familiar ties or (2) occupational choices. 
# But clearly, incorporating both of these factors must be evaluated against the potential risk of overfitting the community detection model.

image(n_all[][order(as.vector(order(V(n_all)$caste_n))),], 
      main="Adjacency matrix sorted by caste membership",
      axes=FALSE)
all_networks <- asNetwork(n_all) 
Blocks <- blockmodel(all_networks, ec = all_networks %v% "caste_n")
round(Blocks$block.model, 4)

# The 5x5 block model matrix suggests that there is generally a higher probability of a tie being formed within a caste than between them.
# It is evident from the matrix presented here that - of the 5 caste groups - the within-caste tie probability is the highest for members of the Scheduled Caste (Block 2).
# This is in line with findings from the previous section where we reported that members of the Scheduled Caste largely belong to one SC community.
# In addition, this matrix also indicates a relatively high within-caste tie probability for members of the Minority group in the village (Block 5). 
# The Minority group here refers to villagers who are Muslim. In other words, Muslim villagers tend to predominantly form ties among themselves.
# This finding also corresponds to a body of evidence indicating that Muslim minority in India tends to form seperate communities.
# For example, Mishra and Bhardwaj (2021) reports that Indian Muslims usually live in separate neighborhoods within cities, attend seperate schools and work in separate firms from the Hindu majority.

# On the other hand, the within-caste tie probability for the Other Backwards Caste (Block 3) is the lowest. In fact, members of the OBC are almost as likely to form a tie
# with members of the General Caste (Block 1), as they are likely to form a tie among themselves. 
# This corresponds to our previous community detection findings and it suggests that we could combine Block 1 and Block 3 without losing much explanatory value.
 
#####################################################################################
################################## REFERENCES ####################################### 

# Arora, S. and Sanditov, B., 2015. Cultures of caste and rural development in the social network of a South Indian Village. Sage Open, 5(3), p.2158244015598813.
# Banerjee, A.V., Banerjee, A. and Duflo, E., 2011. Poor economics: A radical rethinking of the way to fight global poverty. Public Affairs.
# Crona, B. and Bodin, Ö., 2006. What you know is who you know? Communication patterns among resource users as a prerequisite for co-management. Ecology and society, 11(2).
# Merton, R.K., 1968. The Matthew effect in science: The reward and communication systems of science are considered. Science, 159(3810), pp.56-63.
# Mishra, A.K. and Bhardwaj, V., 2021. Welfare implications of segregation of social and religious groups in India: analyzing from wealth perspectives. International Journal of Social Economics.
# Rosenzweig, M.R., 1988. Risk, implicit contracts and the family in rural areas of low-income countries. The Economic Journal, 98(393), pp.1148-1170.
# Simmel, G., 1902. The number of members as determining the sociological form of the group. I. American journal of Sociology, 8(1), pp.1-46.
# Townsend, R.M., 1994. Risk and insurance in village India. Econometrica: Journal of the Econometric Society, pp.539-591.
# Udry, C., 1994. Risk and insurance in a rural credit market: An empirical investigation in northern Nigeria. The Review of Economic Studies, 61(3), pp.495-526.
# Yan, X., Shalizi, C., Jensen, J.E., Krzakala, F., Moore, C., Zdeborová, L., Zhang, P. and Zhu, Y., 2014. Model selection for degree-corrected block models. Journal of Statistical Mechanics: Theory and Experiment, 2014(5), p.P05007.
