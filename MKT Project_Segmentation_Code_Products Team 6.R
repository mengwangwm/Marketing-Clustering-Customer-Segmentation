library(dplyr)
library(data.table)
library(stats)
library(purrr)
library(cluster)
library(factoextra)
library(gtools)
library(igraph)

product <- read.csv("product_table.csv")
transaction <- read.table("transaction_table.csv", header = TRUE, sep=",")

# Joining tables and create new transaction id ---------------------------------------
transaction_product <- inner_join(transaction,product, by="prod_id")
transaction_product$tran_id <- as.character(transaction_product$tran_id)
transaction_product$new_tran_id <- substring(transaction_product$tran_id,1,8)
transaction_product$new_tran_id <- paste(transaction_product$new_tran_id,as.character(transaction_product$cust_id),sep="")
transaction_product$new_tran_id <- paste(transaction_product$new_tran_id,as.character(transaction_product$store_id),sep="")


# Products descriptive analysis ------------------------------------------------------
## products with the best volumes (count and KG are separated)
prod_vol_count <- transaction %>% group_by(prod_id, prod_unit) %>% summarise(total_vol = sum(tran_prod_sale_qty)) %>% arrange(desc(total_vol)) %>% filter(prod_unit == "CT")
prod_vol_KG <- transaction %>% group_by(prod_id, prod_unit) %>% summarise(total_vol = sum(tran_prod_sale_qty)) %>% arrange(desc(total_vol)) %>% filter(prod_unit == "KG")
intersect(prod_vol_count$prod_id,prod_vol_KG$prod_id)

## products with the best revenues
prod_rev <- transaction %>% group_by(prod_id) %>% summarise(total_rev = sum(tran_prod_paid_amt)) %>% arrange(desc(total_rev))

## products with the most customers
prod_cust <- transaction %>% group_by(prod_id) %>% summarise(total_cust = n_distinct(cust_id)) %>% arrange(desc(total_cust))

## products with the most stores
prod_cust <- transaction %>% group_by(store_id) %>% summarise(total_store = n_distinct(store_id)) %>% arrange(desc(total_store))

## products with the most transactions
prod_rev <- transaction_product %>% group_by(new_tran_id) %>% summarise(total_trans = n()) %>% arrange(desc(total_trans))



## subcategories with the best volumes (count and KG are separated)
subcategory_vol_count <- transaction_product %>% group_by(subcategory_id, prod_unit) %>% summarise(total_vol = sum(tran_prod_sale_qty)) %>% arrange(desc(total_vol)) %>% filter(prod_unit == "CT")
subcategory_vol_KG <- transaction_product %>% group_by(subcategory_id, prod_unit) %>% summarise(total_vol = sum(tran_prod_sale_qty)) %>% arrange(desc(total_vol)) %>% filter(prod_unit == "KG")

## subcategories with the best revenues
subcategory_rev <- transaction_product %>% group_by(subcategory_id) %>% summarise(total_rev = sum(tran_prod_paid_amt)) %>% arrange(desc(total_rev))

## subcategories with the best customers
subcategory_cust <- transaction_product %>% group_by(subcategory_id) %>% summarise(total_cust = n_distinct(cust_id)) %>% arrange(desc(total_cust))



## categories with the best volumes (count and KG are separated)
category_vol_count <- transaction_product %>% group_by(category_id, prod_unit) %>% summarise(total_vol = sum(tran_prod_sale_qty)) %>% arrange(desc(total_vol)) %>% filter(prod_unit == "CT")
category_vol_KG <- transaction_product %>% group_by(category_id, prod_unit) %>% summarise(total_vol = sum(tran_prod_sale_qty)) %>% arrange(desc(total_vol)) %>% filter(prod_unit == "KG")

## categories with the best revenues
category_rev <- transaction_product %>% group_by(category_id) %>% summarise(total_rev = sum(tran_prod_paid_amt)) %>% arrange(desc(total_rev))

## categories with the best customers
category_cust <- transaction_product %>% group_by(category_id) %>% summarise(total_cust = n_distinct(cust_id)) %>% arrange(desc(total_cust))


# Product clustering ----------------------------------------------------------------
## create key attributes for product clustering
product_statistics <- transaction_product %>% group_by(prod_id) %>% summarise(total_revenue = sum(tran_prod_paid_amt), total_transact=n_distinct(new_tran_id), total_distinct_customer = n_distinct(cust_id), total_stores = n_distinct(store_id), avg_discount_rate = (abs(sum(tran_prod_discount_amt))/sum(tran_prod_sale_amt)))

discounted_product_cnt <- transaction_product %>% filter(tran_prod_discount_amt < 0) %>% group_by(prod_id) %>% summarise(discounted_tran_cnt=n_distinct(tran_id))

product_statistics <- inner_join(discounted_product_cnt,product_statistics, by="prod_id") %>% mutate(percentage_discount_product = discounted_tran_cnt/total_transact)

write.csv(product_statistics,"product_stat")

product_ori <- product_statistics

product_statistics <- product_statistics[,-1]
summary(product_statistics)


## k-means
### min-max data standardization
rescale.fun <- function(x){(x-min(x))/diff(range(x))}
product_statistics$total_revenue <- rescale.fun(product_statistics$total_revenue)
product_statistics$total_transact <- rescale.fun(product_statistics$total_transact)
product_statistics$total_distinct_customer <- rescale.fun(product_statistics$total_distinct_customer)
product_statistics$total_stores <- rescale.fun(product_statistics$total_stores)

### use three different methods to determine the optimal value of k
fviz_nbclust(product_statistics[, 2:7], kmeans, method = "wss", k.max = 15) # a measure of the variability of the observations within each cluster
fviz_nbclust(product_statistics[, 2:7], kmeans, method = "silhouette", k.max = 15) # a measure of how similar an object is to its own cluster
fviz_nbclust(product_statistics[, 2:7], kmeans, method = "gap_stat",nboot = 40, k.max = 15)

### set different random seed and fit model for several times to find the steady one, check the outcome of the model
set.seed(111) 
clusters <- kmeans(product_statistics[, 2:7], 5)
str(clusters)
clusters$centers
table(clusters$cluster)

### assign cluster number back to the original data set
product_statistics$cluster <- clusters$cluster
product_ori$cluster <- clusters$cluster



## Hierachical Clustering (we tried Hierachical Clustering but the number of products in different clusters was very uneven, we didn't use it for our final results)
suppressPackageStartupMessages(library(dendextend))
### Perform clustering
dist_mat <- dist(product_statistics[, 2:7], method = 'euclidean')
hclust_cent <- hclust(dist_mat, method = 'centroid')
plot(hclust_cent)
                 
### Dicide the # of cluster
cut_avg <- cutree(hclust_cent, k = 5)
rect.hclust(hclust_cent , k = 5, border = 2:6)
abline(h = 0.3, col = 'red')
                 
### Color cluster
cent_dend_obj <- as.dendrogram(hclust_cent)
cent_col_dend <- color_branches(cent_dend_obj, h = 0.3)
plot(cent_col_dend)


# examine the proportion of the products of a category in each cluster
prod_all <- product_ori %>% inner_join(product, by = "prod_id")
prod_all <- prod_all %>% group_by(category_desc_eng) %>% mutate(num_prod_category = n())
category_prop <- prod_all %>% group_by(category_desc_eng, cluster) %>% summarise(count = n())
category_num <- unique(prod_all %>% select(category_desc_eng, num_prod_category))
category_prop <- category_prop %>% left_join(category_num, by = "category_desc_eng")
category_prop <- category_prop %>% mutate(proportion = count / num_prod_category)
cluster_5_cat <- category_prop %>% filter(cluster==5) %>% select(category_desc_eng, proportion)
cluster_4_cat <- category_prop %>% filter(cluster==4) %>% select(category_desc_eng, proportion)
cluster_3_cat <- category_prop %>% filter(cluster==3) %>% select(category_desc_eng, proportion)
cluster_2_cat <- category_prop %>% filter(cluster==2) %>% select(category_desc_eng, proportion)
cluster_1_cat <- category_prop %>% filter(cluster==1) %>% select(category_desc_eng, proportion)


# Find basket drivers -------------------------------------------------------
# sample
randomRows = sample(1:length(transaction_product[,1]), 500000, replace=T)
set.seed(10)
transaction_product_sample <- transaction_product %>% slice(randomRows) %>% select(new_tran_id, prod_id)

temp_combination <- data.frame()
combinations <- data.frame()
for (i in unique(transaction_product_sample$new_tran_id)){
  tran_subset <- transaction_product_sample %>% filter(new_tran_id ==i) %>% select(prod_id) %>%distinct()
  tran_product_list <- tran_subset$prod_id
  if (length(tran_product_list) >= 2){
    pair <- combinations(n = length(tran_product_list), r=2,v=tran_product_list)
    temp_combination <- cbind.data.frame(i, pair)
    combinations <- rbind.data.frame(combinations, temp_combination)
  }
}

# build co-purchase network
co_purchase_count <- combinations %>% group_by(`1`,`2`) %>% summarise(n())
colnames(co_purchase_count) <- c("product1", "product2","frequency(weight)")
co_purchase_network <- graph.data.frame(co_purchase_count, directed=FALSE)
igraph.options(plot.layout=layout.graphopt, vertex.size=7)
plot(co_purchase_network, vertex.label = NA, vertex.size = 1, edge.width = E(co_purchase_network)$weight)

# calculate betweenness
betweenness <- as.data.frame(betweenness(co_purchase_network, v = V(co_purchase_network), directed = FALSE))
betweenness <- add_rownames(betweenness, "prod_id")
product$prod_id <- as.character(product$prod_id)
betweenness <- inner_join(betweenness, product,by="prod_id")
betweenness <- betweenness[,-c(3,4,5,6,7)]



# examine the correlation between clusters --------------------------------
prod_cluster <- unique(product_ori %>% select(prod_id, cluster))
store_cluster <- read.csv("store_cluster.csv") # read data created from store part
customer_cluster <- read.csv("customer_cluster.csv") # read data created from customer part

transaction_trim <- transaction %>% select(cust_id, store_id, prod_id)
transaction_trim <- transaction_trim %>% left_join(prod_cluster, by = "prod_id")
transaction_trim <- transaction_trim %>% left_join(customer_cluster, by = "cust_id")
transaction_trim <- transaction_trim %>% left_join(store_cluster, by = "store_id")
colnames(transaction_trim) <- c("cust_id","store_id","prod_id","prod_cluster","cust_cluster","store_cluster")

transaction_trim <- na.omit(transaction_trim)

# relationship between customer clusters and product clusters
prod_clus_num <- transaction_trim %>% group_by(prod_cluster) %>% summarise(count = n()) %>% select(count)
table(transaction_trim %>% filter(cust_cluster == 1) %>% select(prod_cluster)) / prod_clus_num
table(transaction_trim %>% filter(cust_cluster == 2) %>% select(prod_cluster)) / prod_clus_num
table(transaction_trim %>% filter(cust_cluster == 3) %>% select(prod_cluster)) / prod_clus_num
table(transaction_trim %>% filter(cust_cluster == 4) %>% select(prod_cluster)) / prod_clus_num
table(transaction_trim %>% filter(cust_cluster == 5) %>% select(prod_cluster)) / prod_clus_num
# relationship between customer clusters and store clusters
store_clus_num <- transaction_trim %>% group_by(store_cluster) %>% summarise(count = n()) %>% select(count)
table(transaction_trim %>% filter(cust_cluster == 1) %>% select(store_cluster)) / store_clus_num
table(transaction_trim %>% filter(cust_cluster == 2) %>% select(store_cluster)) / store_clus_num
table(transaction_trim %>% filter(cust_cluster == 3) %>% select(store_cluster)) / store_clus_num
table(transaction_trim %>% filter(cust_cluster == 4) %>% select(store_cluster)) / store_clus_num
table(transaction_trim %>% filter(cust_cluster == 5) %>% select(store_cluster)) / store_clus_num
cust_clus_num <- transaction_trim %>% group_by(cust_cluster) %>% summarise(count = n()) %>% select(count)
table(transaction_trim %>% filter(prod_cluster == 1) %>% select(store_cluster)) / store_clus_num
table(transaction_trim %>% filter(prod_cluster == 2) %>% select(store_cluster)) / store_clus_num
table(transaction_trim %>% filter(prod_cluster == 3) %>% select(store_cluster)) / store_clus_num
table(transaction_trim %>% filter(prod_cluster == 4) %>% select(store_cluster)) / store_clus_num
table(transaction_trim %>% filter(prod_cluster == 5) %>% select(store_cluster)) / store_clus_num
# relationship between product clusters and store clusters
table(transaction_trim %>% filter(prod_cluster == 1) %>% select(cust_cluster)) / cust_clus_num
table(transaction_trim %>% filter(prod_cluster == 2) %>% select(cust_cluster)) / cust_clus_num
table(transaction_trim %>% filter(prod_cluster == 3) %>% select(cust_cluster)) / cust_clus_num
table(transaction_trim %>% filter(prod_cluster == 4) %>% select(cust_cluster)) / cust_clus_num
table(transaction_trim %>% filter(prod_cluster == 5) %>% select(cust_cluster)) / cust_clus_num


# examine 20/80 rule for key attributes ---------------------------------
## for total_revenue
prod_disc <- prod_all %>% select(prod_id, total_revenue) %>% arrange(desc(total_revenue))
sum(head(prod_disc$total_revenue, n=nrow(prod_disc) * 0.2)) / sum(prod_disc$total_revenue)

