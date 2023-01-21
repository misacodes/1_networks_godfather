#####################################################################################
###################### LOADING PACKAGES, DEFINING VARIABLES ######################### 
# Download the network from https://snap.stanford.edu/data/email-Eu-core.html and create a directed multigraph in igraph from it. 
# Please note that you should not edit the original data file, as we will be testing your code on it. 
# Also, note that you may need to add 1 to all node ids because igraph does not like id=0, which is what we have in the data.

email.Eu.core <- read.table("~/Desktop/Network Analysis/Week 9/email-Eu-core.txt", quote="\"", comment.char="")
email_data <- email.Eu.core + 1
subset(email_data, select = c(V1,V2))
rownames(email_data) <- NULL

install.packages("igraph")
library(igraph)
install.packages("igraphdata")
library(igraphdata)
install.packages("scales")
library(scales)

email_graph <- graph_from_edgelist(as.matrix(email_data[,1:2, drop = FALSE],directed = TRUE))
email_graph_f <- simplify(email_graph, remove.multiple = FALSE, remove.loops = TRUE)

# MULTIGRAPHS, PSEUDOGRAPHS AND SELF-LOOPS
# In his book on Graph Theory, Harary (1994, p. 10) defines multigraph as a graph where "no loops are allowed but more than one line can join two points; 
# these are called multiple lines."
# In contrast, Harary (1994) also posits that "if both loops and multiple lines are permitted, we have a pseudograph."
# Therefore, in line with this definition of a multigraph, I excluded self-loops using the function simplify.
# Anyways, self-loops (sending emails to oneself) are not relevant here;
# they do not tell us anything about the workings of inter-personal email correspondence at the institute.

#####################################################################################
################################## QUESTION 1 ####################################### 
# Plot the indegree and outdegree distributions with probability on the y-axis and degree on the x-axis. 
# Plot the two distributions in two separate panels in the same figure. [1 point]

prob_in <- degree.distribution(email_graph_f, mode='in')
prob_in <- prob_in[-1] 
nonzero_pos <- which(prob_in!=0)
prob_in <- prob_in[nonzero_pos]

prob_out <- degree.distribution(email_graph_f, mode='out')
prob_out <- prob_out[-1] 
nonzero_posi <- which(prob_out!=0)
prob_out <- prob_out[nonzero_posi]

ind <- degree(email_graph_f, mode='in')
indegree <- 1:max(ind)
indegree <- indegree[nonzero_pos]

outd <- degree(email_graph_f, mode='out')
outdegree <- 1:max(outd)
outdegree <- outdegree[nonzero_posi]

par(mfrow=c(1,2))
plot(prob_in ~ indegree, 
     xlab='In-degree d', 
     ylab='Probability P(X=d)', 
     main = 'In-degree Distribution 
     of the Email Correspondence',
     col='blue', 
     pch=16, 
     cex.lab=0.75, 
     cex.main = 0.85,
     cex.axis=0.75,
     cex = 0.8,
     xlim = c(0,325))

plot(prob_out ~ outdegree, 
     xlab='Out-degree d', 
     ylab='Probability P(X=d)', 
     main = 'Out-Degree Distribution 
     of the Email Correspondence',
     col='darkslateblue', 
     pch=16, 
     cex.lab=0.75, 
     cex.main = 0.85,
     cex.axis=0.75,
     cex = 0.8,
     xlim = c(0,325))

#####################################################################################
################################## QUESTION 2 ####################################### 
# Plot the indegree and outdegree distributions as complementary cumulative distribution functions on a log-log scale. 
# Plot the two distribution functions in two separate panels in the same figure. [1 point]

ccdf_in <- NULL
for (i in 1:length(prob_in)) {
  ccdf_in[i] = sum( prob_in[ seq(i, length(prob_in)) ] )
}
ccdf_out <- NULL
for (i in 1:length(prob_out)) {
  ccdf_out[i] = sum( prob_out[ seq(i, length(prob_out)) ] )
}

par(mfrow=c(1,2))
plot(ccdf_in ~ indegree, 
     xlab='In-degree d', 
     ylab='Complementary CDF P(X>=d)', 
     log='xy', 
     col='blue',
     main = 'Cummulative Distribution Function 
     for the In-Degrees of Vertices',
     pch=16, 
     cex.lab=0.75, 
     cex.main = 0.85,
     cex.axis=0.75,
     cex = 0.8)

plot(ccdf_out ~ outdegree, 
     xlab='Out-degree d', 
     ylab='Complementary CDF P(X>=d)', 
     main = 'Cummulative Distribution Function 
     for the Out-Degrees of Vertices',
     log='xy', 
     col='darkslateblue',
     pch=16, 
     cex.lab=0.75, 
     cex.main = 0.85,
     cex.axis=0.75,
     cex = 0.8)

#####################################################################################
################################## QUESTION 3 ####################################### 
# Discuss what you observe in 1 and 2. Can you picture a straight line fitting on the log-log plot? 
# What does this tell us about this network’s degree distribution and work-related e-mail communication in general? [3 points]

summary(ind)
summary(outd)

# INTERPRETATION OF QUESTION 1 PLOTS
# In the left plot in Question 1, we observe that the in-degree in the email network (between members of the European research institution) is non-normally distributed.
# Althought the median in-degree in the email network is very low, there are also many email correspondents in the observed network who have very large number of in-links.
# For example, there seems to be some proprotion of email correspondents who receive 150 emails or more and the highest degree vertex in the network has a degree of 211. 
# Since there are 1005 vertices in the email correspondence network in total, this means that the vertex with the highest in-degree receives emails from more than 20% of all vertices in the network.
# Overall, the in-degree distribution has a tail of high-degree hubs, it is right skewed and it resembles either a power-law distribution or heavy-tailed distributions more broadly.
# The out-degree distribution in Question 1 is also not normal and it, again, has a right skew.
# Relative to in-degree, the out-degree distribution is even more right-skewed; i.e. it has even more prominent hubs.
# In fact, the highest degree vertex in the network has 333 connections, meaning that it sends emails to almost 1/3 of all vertices in the network.
# Again, the out-degree distribution resembles a power-law distribution or heavy-tailed distributions more broadly.

# INTERPRETATION OF QUESTION 2 PLOTS
# If there is a power law relationship in the email correspondence network, then when we plot a log-log plot of degree d 
# against the proportion of email correspondents who have degree d in Question 2, then we should see a straight line.
# But it is evident from the graphs that we cannot fit a straight line for the entirety of the log-log plots. 
# The straight line fits in the tail of the in-degree and out-degree distribution, for large values of d, but not in the small d regime.
# According to Newman, this is a common occurrence and these networks are often referred to as (weakly) power-law degree distributed networks; or weakly scale-free.

# FURTHER TOOLS TO DETECT POWER LAWS
# But as Clauset et al. (2009) points out, this line fitting is subjective and it does not give us a definitive evidence that the observed quantity is drawn from a power-law distribution.
# At most, it allows us to rule out some other competing hypotheses and to learn more about the underlying probability distribution.
# But to properly test whether the underlying probability distributions are power law, we would ideally utilize maximum-likelihood fitting with
# goodness-of-fit tests based on the Kolmogorov–Smirnov (KS) statistic and likelihood ratios as proposed by Clauset et al. (2009).

# INTERPRETING POWER-LAW VS. HEAVY-TAILED DEGREE DISTRIBUTIONS
# Also, we might not need to know whether the heavy-tailed degree distribution is exactly power-law degree distributed in order to infer insights about social processes in the network and about how the degree distribution emerged.
# In fact, many insights about the origins and robustness of networks that were originally formulated for power-law degree distributions can be extended to heavy-tailed degree distributions more broadly.
# For example, early research on in-degree in scientific paper citation network by Price (1965) found that although a large proportion of papers in the citation network had small degree, 
# there were also some notable outliers, aka 'academic celebrities' with extreme in-degree. He argued that the in-degree distribution was approximately power-law.
# A few years later, Price also published an explanatory piece regarding how such degree distribution emerged. 
# He argued that this network creation can be attributed to the Matthew effect/preferential attachment/cumulative advantage, abundantly discussed by Herbert Simon.
# This law states that once a social agent (node) gains a small advantage over other agents, that advantage will compound over time into an increasingly larger advantage.
# This is also sometimes referred to as the "rich get richer" law (Easley and Kleinberg, 2010). 
# In the Price's network of in-degree citations, researchers who originally had slightly more citations than the rest, received greater and greater share of citations over time.
# The same principle can also be applied to our heavy-tailed degree distributions. 
# In our in-degree distribution, for example, email correspondents who initially were more well-known and received greater number of emails than their peers might have become even more and more prominent over time, receiving greater and greater share of all emails over time.
# Also, with respect to spread of information/viruses in the email networks, just knowing that the degree distribution has fat tails might tell us a lot about robustness and resilience of the network.
# Heavy tailed distributions are resilient to random failure, whereby a node is removed uniformly at random, but much less resilient when nodes are removed in order of decreasing degree  (Newman et al. 2002).
# Therefore, a computer virus might spread slowly in our email network when the virus initially infects an email correspondent of a random degree, but it might cause much more disruption if it is sent specifically to a high out-degree hub correspondent. 
# Finally, it must be pointed out that the present network only represents communication between institution members (the core),
# and the dataset does not contain incoming messages from or outgoing messages to the rest of the world.
# So our data can tell us some information about the spread of viruses within a community, but not about how those viruses arrive in the first place. 

#####################################################################################
################################## QUESTION 4 ####################################### 
# Create 100 instances each of randomized versions of the e-mail network using two different network generation models: the configuration model and the Erdös-Rényi model. 
# Create and print a table that summarizes the average path length and the transitivity for the e-mail network and for its two randomized versions 
# (calculate the average over the 100 instances in each case). 

summary(email_graph_f)
n = 1005
p = graph.density(email_graph_f)

# Configuration model
configuration_model <- sample_degseq(out.deg = outd, in.deg = ind, method = 'simple')
g_ran_c <- lapply(rep(1, 100), function(x)
  sample_degseq(outd,ind))
g_ran_apl_c <- sapply(g_ran_c, average.path.length)
g_ran_acc_c <- sapply(g_ran_c, transitivity)

# Erdos model
Erdos <- sample_gnp(n, p, directed = TRUE) 
g_ran_e <- lapply(rep(1, 100), function(x)
  sample_gnp(n, p))
g_ran_apl_e <- sapply(g_ran_e, average.path.length)
g_ran_acc_e <- sapply(g_ran_e, transitivity)

res_table <- data.frame(c('Configuration Model', 'Erdos-Renyi Model', 'Real Email Network'), 
                          c(average.path.length(configuration_model), average.path.length(Erdos), average.path.length(email_graph_f)), 
                          c(transitivity(configuration_model), transitivity(Erdos), transitivity(email_graph_f)))
colnames(res_table) <- c('Name', 'Average path length', 'Transitivity')

# The average path length/transitivity of the Erdos-Renyi Model and Configuration Model slightly differ in decimal places across 
# the replication procedures and so I round the results.
res_table[,-1] <-round(res_table[,-1],4)
res_table

#####################################################################################
################################## QUESTION 5 ####################################### 
# Discuss the results from 4. How does the observed network compare to the randomized versions? 
# What does this tell us about the e-mail network and the nature of work-related e-mail communication in general? 
# How can you explain the differences between the two randomized versions of the e-mail network based on what you know about degree distributions, the configuration model, and the Erdös-Rényi model? [3 points]

# AVERAGE PATH LENGTH IN ERDOS-RENYI MODEL VS. IN OBSERVED NETWORK, IMPLICATIONS FOR REAL EMAIL NETWORK
# The average path length of the Erdos-Renyi model is lower but quite close to the observed email network.
# Generally, the average path length in the Erdos-Renyi model is a logarithmic function of network size, 
# meaning that the average path length grows at diminishing rate as the network size expands (Newman, 2003). 
# This logarithmic scaling is typical for small-world behaviour, whereby networks are connected by a short path through the network.
# The most famous empirical study that demonstrated this small-world phenomenon was the letter-passing experiment by Milgram in 1960s (Travers and Milgram, 1977). 
# In the study, Milgram found that a group of randomly selected individuals in Nebraska and Boston could reach an unknown stockbroker in Massachusetts through their social network in just a few steps (i.e. the so-called '6 degrees of separation').
# Although Milgram's research was not well-controlled (e.g. it did not properly account for high degree of attrition in the study), similar findings were also replicated in a more controlled research by Dodds et al. (2002) using email and using a broader geographical spread of participants.
# In fact, according to Newman (2010), most pairs of vertices in most social networks are connected by a short path through the network.
# And since the Erdos and Renyi model yields average path length relatively close to the average path length in the real network,
# this can be taken as evidence that the small-world behaviour is also present in the observed real email network.

# TRANSITIVITY IN ERDOS-RENYI MODEL VS. IN OBSERVED NETWORK, IMPLICATIONS FOR REAL EMAIL NETWORK
# But there is still a slightly lower average path length of Erdos-Renyi model relative to the real email network
# and, in addition, the Erdos-Renyi model also has much lower clustering coefficient relative to the real email network.
# The random model has a clustering coefficient of approx. 0.05, whereas the transitivity in the real network is 0.2674.
# In other words, in the real network there is a manyfold higher probability that 2 researchers who have some form of email correspondence with a 3rd researcher (either email received or sent) 
# are also having some email correspondence among one another, relative to what would be predicted by the Erdos-Renyi model.
# This could be explained by the fact that the Erdos Renyi model assumes that the probability that any 2 vertices have an edge between them is exactly the same.
# These probabilities of email correspondence are assumed to be p = c/(n-1), where c is the mean degree and n is the overall number of nodes in the network (Newman, 2010).
# But in the real email network, researchers within each department might communicate intensively and exchange email correspondence among each other.
# This might raise the clustering coefficient in the real network above what would be predicted by the Erdos-Renyi model.
# Taken together, the Erdos-Renyi Model does not reproduce the degree of clustering that is observed in the real email network, 
# because unlike the real network where emails might be extensively exchanged within certain groups of researchers (e.g. within departments), the Erdos-Renyi Model assumes that emails are exchanged randomly in the institute.

# AVERAGE PATH LENGTH IN CONFIGURATION MODEL VS. IN OBSERVED NETWORK, IMPLICATIONS FOR REAL EMAIL NETWORK
# In the configuration model, the average path length is also lower than in the real email network. 
# In general, the configuration model assumes that in-links and out-links between nodes are built at random, i.e. that there is not a strong correlation between in-links and out-links in the network.
# But, in reality, is quite reasonable to assume that some researchers at the institute (e.g. 2 researchers who collaborate on a paper) might exchange emails back and forth and so their in-links and out-links are very similar. 

# TRANSITIVITY IN OBSERVED NETWORK VS. IN CONFIGURATION MODEL, IMPLICATIONS FOR REAL EMAIL NETWORK
# In the same manner, the configuration model also has lower transitivity relative to the real email network.
# Again, this might be caused by the fact that the configuration model assumes that in-links and out-links between nodes are built at random, which is not representative of the real email network.
# To test whether mutual, back-and-forth email correspondence is more prevalent in the real network than in the network models, we cannot use the transitivity function.
# The transitivity function is specifically designed for an undirected graph, calculating the ratio of the triangles and the connected triples in the graph.
# And for directed graphs, the direction of the edges is ignored, meaning that all directed triads - both mutual and asymmetric - are considered equally (Hoffman, 2021).
# That is why I am supplementing my analysis also with the function triad.census.
# There are overall 16 different types of connections between any 3 nodes in a directed network and the function triad.closure determines how prevalent each type of the triad configuration is.
# The first number (equal to 153640073 in the real network) corresponds to a triadic relation whereby there are 0 mutual relations, 0 asymmetric relations and 3 null relations.
# The last number (equal to 34185 in the real network) corresponds to a sitation where there are 3 mutual relationships in the given triad.
# By triad with 3 mutual relations, I mean the following: a -> b, b -> c, c -> a, b -> a, a -> c, c -> b.
# In line with the hunch, we see that the mutual relationship type is considerably more common in the real network triads, relative to the configuration network and also relative to the Erdos-Renyi network.
# Taken together, the Erdos-Renyi Model and the Configuration model both fail to account for back and forth communication that is typical for email correspondence networks.

triad.census(email_graph_f)
triad.census(configuration_model)
triad.census(Erdos)

# DIFFERENCES BETWEEN ERDOS-RENYI MODEL VS. CONFIGURATION MODEL
# In general, the Erdos and Renyi random graph has each edge present or absent with equal probability, 
# and therefore the degree distribution is binomial or Poisson in the limit of large graph size (Newman, 2010). 
# This makes the Erdos and Renyi model well-suited for depicting road and train networks, where there is a physical limit on how many links you can have attached to any given node.
# But the email correspondence network is unlike road or train networks in that there is no physical limit on its degree distributions (i.e. no strict limit on how many emails one correspondent can send/receive). 
# As we saw in Questions 1 and 2 the in-degrees and out-degrees of the vertices in the email network are far from having a central tendency.
# Compared to the Erdos and Renyi Model, the real email network is much more skewed, it has a longer right tail of values that are far above the mean.
# This tells us a lot about the social dynamics in the email network (also discussed in Q3).
# The heavy tailed degree distribution observed in the email network is more robust to random failure and also more sensitive to targetted attacks, relative to the Erdos and Renyi random graph model.
# Attacking high-degree vertices in the email network with computer viruses might be a much more effective strategy for transmission than what would be predicted by the Erdos and Renyi Model.
# The same might also apply for information: sending some information email to the highest degree hub might be a more effective way to spread that information throughout the research institute, relative to what the Erdos and Renyi random graph model would predict.

# Unlike Erdos and Renyi random graph, the Configuration model does not have a Poisson degree distribution in the limit.
# Instead, the probability of connecting two vertices in the model depends only on their (out- and in-) degrees. 
# The configuration model assigns to each vertex as many out-stubs (or half-edges) as its out-degree, and as many in-stubs as its in-degree.
# It then randomly connects pairs of vertices joining out- and in-stubs. This is done by sampling uniformly at random one out- and one in-stub from the pool of all out- and in-stubs respectively and then connecting them, 
# until all stubs are connected (Newman, 2003). 
# In other words, the configuration model copies the in-degree and out-degree distributions of the real email network. 
# As a result, it should have the same fat tailed distribution and it should identify the same hubs as the real email network. 
# The configuration model should accurately depict how information and viruses propagate through the email network.
# And so, it is well-suited for simulations of diffusion, contaigion, robustness or resilience.

#####################################################################################
################################## REFERENCES ####################################### 

# Clauset, A., Shalizi, C. R., & Newman, M. E. (2009). Power-law distributions in empirical data. SIAM review, 51(4), 661-703.
# Dodds, P. S., Muhamad, R., & Watts, D. J. (2003). An experimental study of search in global social networks. science, 301(5634), 827-829.
# Easley, D., & Kleinberg, J. (2010). Networks, crowds, and markets (Vol. 8). Cambridge: Cambridge university press.
# Harary, F. (1994) Graph Theory. Reading, MA: Addison-Wesley, p. 10.
# Hoffman, M. (2021). 11 Transitivity, structural, balance, and hierarchy | Methods for Network Analysis. Retrieved 24 March 2021, from https://bookdown.org/markhoff/social_network_analysis/transitivity-structural-balance-and-hierarchy.html
# Newman, M. E. (2003). The structure and function of complex networks. SIAM review, 45(2), 167-256.
# Newman, M., (2010). Networks. Oxford university press.
# Newman, M. E., Forrest, S., & Balthrop, J. (2002). Email networks and the spread of computer viruses. Physical Review E, 66(3), 035101.
# Price, D. J. D. S. (1965). Networks of scientific papers. Science, 510-515.
# Travers, J., & Milgram, S. (1977). An experimental study of the small world problem. In Social networks (pp. 179-197). Academic Press.
