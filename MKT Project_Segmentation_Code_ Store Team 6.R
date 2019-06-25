---
title: "Marketing Analytics"
author: "Zerong (Josh) Chen"
date: "Feb 1, 2018"
---
# Data preperation
install.packages('data.table')
install.packages('mltools')
install.packages('factoextra')
library(psych)
library(data.table)
library(dplyr)
library(mltools)
library(cluster)
library(factoextra)

setwd("C:/Users/joshw/OneDrive - Emory University/Academic Work/MKT Analytics/Seg Project")
full <- read.csv("transaction_table.csv", stringsAsFactors=FALSE)
store <- read.csv("stores.csv", stringsAsFactors=FALSE)
customer <- read.csv("customer.csv", stringsAsFactors=FALSE)


store <- store[store$total_transact >= 50,]
store[store$total_transact <= 50,]

# Data cleaning and exploration
# Basic understanding of the dataset
summary(store)
describe(store)


# Check correlation = 0.87
cor(store$total_distinct_products, store$total_distinct_categories)
# 0.98
cor(store$total_distinct_products, store$total_distinct_brands)



# ------------------------------------------------------------------------------------
# Discriptive statistics
# Top stores based on different attribute
sum(head(store[order(store$total_revenue,decreasing=TRUE),'total_revenue'],84))/sum(store_nl[,'total_revenue'])
head(store[order(store$total_trans,decreasing=TRUE),],20)
head(store[order(store$total_distinct_customer,decreasing=TRUE),],20)
head(store[order(store$avg_discount_rate,decreasing=TRUE),],20)
head(store[order(store$total_distinct_categories,decreasing=TRUE),],20)
head(store[order(store$discount_score,decreasing=TRUE),],20)


# Share of revenue/transaction of the top 20 performing stores
sum(head(store[order(store$total_transact,decreasing=TRUE),'total_transact'],84))/sum(store_nl[,'total_transact'])
sum(head(store[order(store$total_distinct_customer,decreasing=TRUE),'total_distinct_customer'],84))/sum(store_nl[,'total_distinct_customer'])


# Calculate the cherry picker score
store_nl <- setDT(store)[,avg_discount_score:=normalize(avg_discount_rate)][,avg_dispurchase_score:=normalize(percentage_discount_purchase)]
store_nl$discount_score <- (store_nl$avg_discount_score+store_nl$avg_dispurchase_score)/2

# Calculate the avg dicount 
mean(unlist(head(store_nl[order(store_nl$discount_score,decreasing=TRUE),'avg_discount_rate'],20)))
mean(unlist(head(store_nl[order(store_nl$discount_score,decreasing=TRUE),'percentage_discount_purchase'],20)))

mean(store_nl$avg_discount_rate)
mean(store_nl$percentage_discount_purchase)



# Cluster by calculated percentage of loyal -----------------------------------------------------
# Number of visits by store and customer
cust_trans_store <- setDT(full)[,.(trans_ct=length(unique(tran_id))), by=.(cust_id, store_id)]

# Calculate avg trans by store
cust_trans_store <- cust_trans_store[,avg_trans:= mean(trans_ct), by="store_id"]

# Loyal is defined as visit 2+ times as the avg customer visit the store
#cust_trans_store$loyal <- ifelse(cust_trans_store$trans_ct > 2.5*cust_trans_store$avg_trans, 1, 0)
cust_trans_store <- setDT(cust_trans_store)[,min := min(trans_ct),by=.(store_id)][,max:=max(trans_ct),by=.(store_id)]
cust_trans_store <- setDT(cust_trans_store)[,percentile:=(trans_ct-min)/(max-min),by=.(store_id,cust_id)]
cust_trans_store$loyal <- cust_trans_store$percentile > 0.75


# Find the percentage of loyal customers each store
loyal_store <- cust_trans_store[, .(percent_loyal=(sum(loyal)/.N)), by="store_id"]

# Merge loyal rate with store
store <- merge(store, loyal_store, by='store_id')

# Label cluster based on loyal rate
store$group_loyal <- store$percent_loyal > 0.1
sum(store$group_loyal)/nrow(store)

# Calculate # of stores by loyal cluster
table(store$group_loyal)


# k-means Cluster -------------------------------------------------------------------------
# Standardize variables
store_sc <- store

# Drop col with high correlation
store_sc$total_distinct_brands <- NULL
store_sc$total_distinct_categories <- NULL

# Self-defined normalization function
normalize <- function(x)
{return((x- min(x)) /(max(x)-min(x)))}

store_sc <- as.data.frame(sapply(store_sc, normalize))
store_sc
# Identify best # of clusters
# Clear up stored image so code can run without error
dev.off()

# Within distance
fviz_nbclust(store_sc[, 2:7], kmeans, method = "wss")
# Measures the avg. distance between clusters
fviz_nbclust(store_sc[, 2:7], kmeans, method = "silhouette")
# Gap - whether the cluster is formed by random
fviz_nbclust(store_sc[, 2:7], kmeans, method = "gap_stat",nboot = 40)

# k-means clustering
clusters <- kmeans(store_sc[, 2:7], 3)


# Clusters by group
table(clusters$cluster)
store$kmeans_cl <- clusters$cluster
store <- merge(store, store_nl[,c('store_id',"discount_score")], by='store_id')


# Find traits of groups
setDT(store)[, lapply(.SD, mean), by='kmeans_cl']
clusplot(store_sc, clusters$cluster, color=TRUE, shade=TRUE, lines=0)


# Group by k-means cluster
store_kmean <- setDT(store)[,.(loyal_1=sum(group_loyal == 1), loyal_2=sum(group_loyal == 2),
                               loyal_3=sum(group_loyal == 3)), by="kmeans_cl"]


# Explore summary statistics of stores grouped by loyal percent exceed 10%
setDT(store)[, lapply(.SD, mean), by='group_loyal']


# ------------------------------------------------------------------------------------------------
# k-means Cluster include percentage of loyal customer as attribute
# The result has no significant difference from the clusters without
# the loyal attribute

clusters_loyal <- kmeans(store_sc[, 2:8], 3)

table(clusters_loyal$cluster)
store$loyal_cl <- clusters_loyal$cluster

setDT(store)[, lapply(.SD, mean), by='loyal_cl']

clusplot(store_sc, clusters$cluster, color=TRUE, shade=TRUE, lines=0)



# ------------------------------------------------------------------------
# Calcualte the revenue/transaction for the top 20 stores
store <- as.data.frame(store)
store <- store[order(store$total_revenue, decreasing = TRUE),]
sum(store[1:83,'total_revenue'])/sum(store$total_revenue)

length(unique((store$store_id)))*0.2
sum(store$total_revenue)*0.8


order_loyal <- store[order(store$total_revenue, decreasing = TRUE),]


# Hierarchical Model ---------------------------------------------------------------------------
# Applied hierarchy clustering but the reasult was hard to interpret
dist_mat <- dist(store_sc[,2:6], method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg)

# Select # of clusters
cut_avg <- cutree(hclust_avg, k = 3)
rect.hclust(hclust_avg , k = 3, border = 2:6)
table(cut_avg)

# Highlight tree
avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj, h = 2)
plot(avg_col_dend)

# Visualize cluster
fviz_cluster(list(data=store_sc, cluster=cut_avg))


