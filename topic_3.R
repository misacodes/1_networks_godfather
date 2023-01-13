#####################################################################################
###################### LOADING PACKAGES, ORGANIZATIONAL BASICS ###################### 
# install.packages("corrplot")
# install.packages("biganalytics")
# install.packages("tidyverse")

library(RColorBrewer)
library(corrplot)
library(biganalytics)
library(tidyverse)
library(ergm)
library(intergraph)
data(florentine)
detach(package:ergm)
detach(package:network)
require(intergraph)
require(igraph)
flo_m <- asIgraph(flomarriage)
flo_b <- asIgraph(flobusiness)


#####################################################################################
######################### DEFINING VARIABLES, DESCRIPTIVES ########################## 

# The Medici family was one of the most influential and controversial families in the history of the world. 
# They were able to rise above all the other upper-class families of Florence and dominate the political and cultural scene in the city state. 
# The Medici family were instrumental in the rise of the Italian Renaissance and were deeply involved in shaping European culture and politics for more than 300 years.
# Yet, the family rose to power from a relative obscurity. Medici’s only moved to Florence sometime in the 12th century and established a banking business in Florence in 1397 (Padgett and Ansell, 1993). 
# This raises the question: how were Giovanni and his son Cosimo de'Medici able to turn their banking business into the top financial institution and gain such overwhelming political influence over Florence? 

summary(flo_m)
summary(flo_b)
names <- V(flo_m)$vertex.names
priorates <- V(flo_m)$priorates
totalties_m <- V(flo_m)$totalties
wealth <- V(flo_m)$wealth
priorates_df <- data.frame(names, priorates)
priorates_df
totalties_df <- data.frame(names, totalties)
totalties_df
wealth_df <- data.frame(names, wealth)
wealth_df

# The marriage network dataset has a number of vertex/character attributes. These are: 
# 1) family names (we see that the dataset contains the names of 16 families that constituted the elite of Florentine state during the reneissance era).
# 2) priorates (number of seats of each family on the civic council between 1282 and 1344), we see that families Strozzi and Albizzi had the highest number of Priorates between 1282-1344. 
# 3) wealth (each family's net wealth in 1427 in thousands of lira). We also see that the Medici family was not the richest of the 16 families, at least in 1427. Another elite family involved in banking - i.e. Strozzi was richer at that time. 
# 4) totalties (the total number of family ties (both business and marriage) in an extended dataset of 116 families by Breiger and Pattison, 1986). And it seems that Medicis were the strongest in terms of their social network ties. 


#####################################################################################
################################## QUESTION 1 ####################################### 
# Calculate the degree, eigenvector, betweenness, and closeness centralities for both networks. 
# Organize these into a data frame, showing the centrality calculation for each Florentine family. 
# When you calculate closeness centrality, you should get warnings – why? Interpret the warning.

deg_m <- degree(flo_m)
eig_m <- evcent(flo_m)$vector
clo_m <- closeness(flo_m)
bet_m <- betweenness(flo_m)
deg_b <- degree(flo_b)
eig_b <- evcent(flo_b)$vector
clo_b <- closeness(flo_b)
bet_b <- betweenness(flo_b)

centralities_name_df <- data.frame(names, deg_m, eig_m, clo_m, bet_m, deg_b, eig_b, clo_b, bet_b)
centralities_name_df

# Closeness centrality expresses the mean distance from a vertex to other vertices.
# For each vertex, closeness centrality depicts the average shortest distance from the vertex to each other vertex.
# It is calculated as the inverse of the average geodesic distance from a given vertex to all other vertices in the network.
# By geodesic distance (also called geodesic path or shortest distance), I refer to the shortest network distance between any 2 vertices in question.
# Vertices that have short mean geodesic distance to other nodes tend to have better access to information and be able to influence others in the network.
# It is also possible that there is no geodesic path between 2 vertices, if the vertices are not connected together by any route through the network, 
# i.e. they fall into different components of the network. In such a case of disconnected vertices, it is a convention to define the geodesic path between the vertices as infinite (Newman, 2010). 

plot(flo_m, 
     vertex.frame.color="orange",
     vertex.label=names, 
     vertex.label.size=0.5,
     vertex.label.color="black",
     edge.arrow.size=0.25, 
     vertex.size=10, 
     edge.width=0.5)

plot(flo_b, 
     vertex.frame.color="orange",
     vertex.label=names, 
     vertex.label.size=0.5,
     vertex.label.color="black",
     edge.arrow.size=0.25, 
     vertex.size=10, 
     edge.width=0.5)

# Upon graphing the marriage network, we see that family Pucci is an isolate. There is no direct or indirect path between node Pucci and all other nodes in the marriage network. 
# Therefore, the geodesic distance between node Pucci and all other nodes is infinite and, in turn, the measure of closeness centrality loses its informative value for family Pucci.
# In a similar fashion, there are 5 isolates in the business network graph - families Ridolfi, Albizzi, Strozzi, Pucci and Acciaiuoli have no business ties with the other 15 elite families.
# And, as a result, the geodesic distance of these families to the remaining families in the network and the measure of closeness centrality for these families will not be informative.
# Moreover, the igraph documentation (Csardi et al, 2020) specifies that: "if there is no (directed) path between vertex 'v' and 'i', then the total number of vertices is used in the formula instead of the path length."
# This explains why family Pucci in the marriage network and families Ridolfi, Albizzi, Strozzi, Pucci and Acciaiuoli in the business network report positive values of closeness centrality.
# The warning message is appropriate here, because if the disconnected nature of the network went unnoticed, this might lead to mis-interpretation of the closeness centrality results. 


#####################################################################################
################################## QUESTION 2 ####################################### 
# Calculate the pairwise correlations (Pearson’s r) between the 8 centrality measures. 
# Which two measures are most closely correlated? Which are the most weakly correlated? 
# Why do you think these are the networks that are most/least correlated?

centralities_df <- data.frame(deg_m, eig_m, clo_m, bet_m, deg_b, eig_b, clo_b, bet_b)
plot(centralities_df)
cors <- cor(centralities_df, method = "pearson")
print(cors)
corrplot.mixed(cors, tl.col = "black")

# We find varied Pearson's correlations among the 8 centrality measures presented here. But, in vast majority of cases, the 8 centrality measures are positively correlated.
# The 2 most closely correlated metrics are (1) Degree centrality and (2) Eigenvector centrality in the marriage network.These 2 metrics have a strong positive correlation of approximately 0.94.
# This high correlation is congruent with the fact that Degree and Eigenvector centrality are theoretically interlinked concepts.
# Both Degree and Eigenvector centrality depict the prestige, influence and access to information among network nodes.
# They both consider the number of direct connections that a node has, although Eigenvector centrality extends on this by also measuring the relative importance of each immediate connection.

graph.density(flo_m)
graph.density(flo_b)

# The 2 least correlated metrics are (1) Betweenness centrality in the marriage network and (2) Eigenvector centrality in the business network.
# They have Peason correlation coefficient of approximately -0.02.
# I inscribe this low correlation to 2 factors: 
# (A) the disparate nature of the 2 centrality metrics and 
# (B) to the fact that the centrality measures are calculated for 2 different networks - i.e. business vs. marriage.
# For one, the marriage network is more dense, meaning there is a higher general level of cohesion in the network graph.
# The marriage network has density of 0.1666667, i.e. approximately 16.67% of all potential network connections are realized. 
# In contrast, the business network has density of 0.125, and so only 12.5% of all potential network connections are realized. 
# Furthermore, the marriage network also has only 1 isolate compared to the business network, where there are 5 isolates. 
# Given the dispare nature of the 2 networks, there is little reason to believe that their centrality measures should be perfectly correlated.
# In addition, betweenness centrality differs from the 3 other centrality measures (including Eigenvector centrality) in that it is not primarily concerned with
# how well-connected a vertex is. Instead, it measures how much a vertex falls between others. 
# Indeed a vertex can have a low degree and be connected to others that have low degree (definition of Eigenvector centrality) and still have high betweenness.
# One such scenario is when the vertex lies on a bridge joining 2 groups of other vertices.
# All paths between the 2 groups must pass through this vertex and so it has a high betweenness, even though its Eigenvector centrality is low. 
# Taken together, the concepts of Eigenvector and betweenness centrality are substantially different and so there is little reason to believe that the 2 metrics should correlate.

#####################################################################################
################################## QUESTION 3 ####################################### 
# Identify the family with the highest centrality of each type in each of the two networks 
# (so, 8 total measures: 2 networks, 4 centrality measures). 

# (1) Degree Centrality, Marriage Network
max(centralities_name_df$deg_m)
centralities_name_df %>% slice(which.max(centralities_name_df$deg_m))
# Family Medici has the highest degree centrality (6) in the marriage network. 
# This implies that Medicis were directly connected to 6 other elite families through direct marriage ties.

# (2) Eigenvector Centrality, Marriage Network
max(centralities_name_df$eig_m)
centralities_name_df %>% slice(which.max(centralities_name_df$eig_m))
# We see that family Medici also has the highest Eigenvector centrality.
# The Medici superiority in its Eigenvector centrality suggest that Medici were intermarried with other prestigious families in Florence.

# (3) Closeness Centrality, Marriage Network
max(centralities_name_df$clo_m)
centralities_name_df %>% slice(which.max(centralities_name_df$clo_m))
# Family Medici has the highest closeness centrality value of 0.0244, 
# but it is closely followed by family Ridolfi with 0.0223, 
# and families Tornabuoni and Albizzi both with 0.0222.

# (4) Betweenness Centrality, Marriage Network
max(centralities_name_df$bet_m)
centralities_name_df %>% slice(which.max(centralities_name_df$bet_m))
# When it comes to betweenness centrality, family Medici strongly dominates.
# It has betweenness centrality of 47.5.

# (5) Degree Centrality, Business Network
max(centralities_name_df$deg_b)
centralities_name_df %>% slice(which.max(centralities_name_df$deg_b))
# Family Medici has the highest degree centrality of 5 in the business network. 
# This implies that Medicis were directly connected to 5 other elite families through direct business ties.

# (6) Eigenvector Centrality, Business Network
max(centralities_name_df$eig_b)
centralities_name_df %>% slice(which.max(centralities_name_df$eig_b))
# We see that family Peruzzi  has the highest Eigenvector centrality in the business network.

# (7) Closeness Centrality, Business Network
max(centralities_name_df$clo_b)
centralities_name_df %>% slice(which.max(centralities_name_df$clo_b))
# Family Barbadori has the highest closeness centrality value of approximately 0.010 in the business network.

# (8) Betweenness Centrality, Business Network
max(centralities_name_df$bet_b)
centralities_name_df %>% slice(which.max(centralities_name_df$bet_b))
# Family Barbadori also has the highest betweenness centrality of 8 in the business network.

#####################################################################################
################################## QUESTION 4 ####################################### 
# Plot two of these 8 combinations, showing the network of choice (marriage or business) and the centrality measure of choice. 
# Make your choices based on which you think help explain the success of the Medici. 
# Make sure to scale the centrality scores, so that the plot is legible and informative. 

dlayout <- layout.fruchterman.reingold(flo_m)
plot(flo_m,
     vertex.size=bet_m*1.20,
     vertex.frame.color="orange",
     vertex.label=names,
     vertex.label.size=0.5,
     vertex.label.color="black",
     vertex.label.font=2,   
     vertex.label.cex=0.5,
     vertex.label.family="Times",
     edge.color="grey",                        
     edge.width=1.5,
     main="Betweenness Centrality in Marriage Network",
     layout=dlayout)


plot(flo_b, 
     vertex.size=deg_b*7,
     vertex.frame.color="orange",
     vertex.label=ifelse(deg_b > 0, names, NA),
     vertex.label.size=0.5,
     vertex.label.color="black",
     vertex.label.font=2,   
     vertex.label.cex=0.5,
     vertex.label.family="Times",
     edge.color="grey",                        
     edge.width=1.5,
     main="Degree Centrality in Business Network")


#####################################################################################
################################## QUESTION 5 ####################################### 
# Justify those selections. Why did you choose to present the business and/or marriage network, and why that particular centrality measure? 
# What do those particular plots tell you about the preeminence of the Medici? 
# Draw upon at least one of the concepts that we introduced in lecture (network social capital, strength of weak ties, structural holes, brokerage, closure, diversity-bandwidth trade-off).

# If we estimate the importance of elite Florentine families simply by the degree centrality in the marriage network,
# i.e. how many families a given family is linked to through marriages, then the Medici do come out as the most important ones.
# But they only surpass the next highest families, such as the Strozzis, by a ratio of 3 to 2. 
# And similarly tied findings are also reported for measures of closeness centrality and Eigenvector centrality.
# There must, therefore, be other factors at play behind the enormous success of the Medicis. And this appears to be, in part, betweenness centrality.
# Raw betweenness centrality asks: what is the total number of geodesics (shortest paths) going through a given vertex?
# In our marriage network, we see that Medicis have by far the highest raw betweenness. In fact, approximately 47 different geodesics go through family Medici.
# The second highest betweenness centrality is only less than half that of Medicis, i.e. family Guadagni with raw betweenness centrality of 23.16 paths.

# The concept of betweenness centrality was originally proposed by Freeman in 1977 (although Freeman himself pointed out that a similar concept was previously introduced by Anthonisse) (Newman, 2010).
# Freeman’s approach to betweenness was built on the concept of local dependency. A point is dependent on another if the paths that connect it to others pass through this point. 
# And this is also partially the idea behind Burt’s (1992) ‘Structural Holes Theory’.
# By structural hole, Burt refers to an 'empty space' between contacts in a person's network. It means that these contacts are disconnected, they do not interact closely (though they may be aware of one another).
# For example, there is a structural hole between family Pucci and all other elite Florentine families in the marriage network, since family Pucci does not hold any marriage ties with the other families.
# Furthermore, Burt (2004) links structural holes to the concept of network social capital, whereby he defines network social capital as "the contextual complement to human capital."
# And he further postulates that "social capital predicts that returns to intelligence, education and seniority depend in some part on a person's location in the social structure of a hierarchy."
# The Structural Holes Theory has an openness approach to social capital, it describes social capital as a function of the brokerage opportunities in a network.
# ANd it draws on Granovetter's (1973) strength of weak ties whereby weak ties bridge across homogeneous social network components and, hence, provide cohesion in the society.

# In our example, the family Medici had relationships that spanned the marriage network structural holes, i.e. they took the broker position between the large network component (with family Strozzi) and less inter-tied families Acciaiuoli, Salviati and Pazzi.
# According to the Structural Hole Theory, this brought them the opportunity to acquire non-redundant pieces of information from the different marriage ties and determine what gets relayed to the main component of the network.
# It is probable that each cluster of direct contacts constituted a single information source because families with direct marriage connections tend to have the same information at the same time.
# By being brokers, Medicis were the only family with direct access to unique information from family Acciaiuoli and they also had the valuable link to relatively isolated family Salviati. 
# These constituted non-redundant information sources also provided Medicis with broader information screen and hence ensured that the family was well-informed of impeding disasters. 
# Likewise, the Medicis' high betweenness centrality ensured that the family was the first to see new business opportunities in different parts of the network.
# A related concept to such 'information' subtype of social capital is 'coordination' social capital (Jackson, 2019).
# It can be defined as being situated as a ‘friend-in-common’ and so being in the position to coordinate others, combine the forces of familes which would not otherwise have come together.
# Since family Medici served as a bridge of other elite families, they were in the position to coordinate the Florentine oligarchy and their behavior. 
# This might also explain why the family Medici was comparatively more politically successful and better equipped to coordinate armed men, relative to their rival families Strozzi and Albizzi.

data.frame((deg_m+deg_b),names)

# Furthermore, the Medicis' power position in the marriage network might have been further strengthened by their ties in the business network. 
# Holler (2020) brings forward the observation that the Medici did not marry from those families with whom they engaged in business relations, nor did they do business with those whom they married. 
# According to Padgett and Ansell (1993), Medicis married into families from different neighbourhoods than the Medici, while they formed business ties with families living close to them.
# I have, therefore, complemented the graph of betweenness centrality in the marriage network with a graph of degree centrality in the business network.
# The Medici have 11 (either marriage or business) edges connected to theim in total – more than any other elite family.
# And they also have the most diversified links. Out of the 11 total, 5 links are either marriage links or business links, but not both.
# We also see that for families Pazzi, Salviati and Tornabuoni, the business link with family Medici constituted the only connection to the main business network component. 
# Therefore, the families Pazzi, Salviati and Tornabuoni were solely dependent on the Medici and likely created strong, close ties with the Medici family.
# This relates to the concepts of bonding and closure social capital (Burt, 2001).
# Such closed network structures are constituted of strong ties and they are associated with some positive consequences.
# For example, members of closed network structures feel obligated to help one another in times of a crisis (Prell, 2009). 
# This might help explain why Cosimo de'Medici received some much help from his friends after he was imprisoned in 1433 (Padgett and Ansell, 1993). 
# Taken together, the mixing of strong ties with geography and social class might have brought the Medici an extemely profitable mixed structure of both closure and openness social capital. 
# Through this position, the Medici arguably cemented together the overall elite family network and strengthened their power in Florence.  

#####################################################################################
################################## REFERENCES ####################################### 

# Breiger, R.L. and Pattison, P.E., 1986. Cumulated social roles: The duality of persons and their algebras. Social networks, 8(3), pp.215-256.
# Burt, R.S., 1998. The gender of social capital. Rationality and society, 10(1), pp.5-46.
# Burt, R.S., 2001. Closure as social capital. Social capital: Theory and research, pp.31-55.
# Burt, R.S., 2004. Structural holes and good ideas. American journal of sociology 110, no. 2 (2004): 349-399.
# Csardi, M.G., 2013. Package ‘igraph’. Project CRAN.
# Granovetter, M.S., 1973. The strength of weak ties. American journal of sociology, 78(6), pp.1360-1380.
# Holler, M., 2020. Power in Networks: The Medici. Available at SSRN 3673940.
# Jackson, M.O., 2019. A typology of social capital and associated network measures. Social Choice and Welfare, pp.1-26.
# Newman, M., 2010. Networks. Oxford university press.
# Padgett, J.F. and Ansell, C.K., 1993. Robust Action and the Rise of the Medici, 1400-1434. American journal of sociology, 98(6), pp.1259-1319.
# Prell, C., 2009. Linking social capital to small-worlds: a look at local and network-level processes and structure. Methodological innovations online, 4(1), pp.8-22.

