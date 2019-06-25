library(data.table)
library(ggplot2)
library(cluster)
library(factoextra)
library(dplyr)
setwd('C:/Users/Meng Wang/Desktop/MKT_Analysis/Project/Segmentation/files')
product <- fread('product_table.csv')
transaction <- fread('transaction_table.csv')

# check missing value
str(transaction)
sapply(transaction,function(x) sum(is.na(x)))
sapply(product,function(x) sum(is.na(x)))
hist(transaction[,sum(tran_prod_sale_amt),cust_id][,.(V1)])

# create new transaction id
transaction[,ti:=paste(tran_dt,store_id,cust_id)]

# question 1
unique(transaction[,.(cust_id)]) # 7920 distinct customers
transaction[,sum(tran_prod_paid_amt),cust_id][order(V1,decreasing = T)][c(1:792),sum(V1)] # revenues, top 10%
transaction[,length(unique(ti)),cust_id][order(V1,decreasing = T)][c(1:792),sum(V1)] # transactions, top 10%
transaction[prod_unit=='CT',sum(tran_prod_sale_qty),cust_id][order(V1,decreasing = T)][c(1:792),sum(V1)] # number of products in CT, top 10%
transaction[prod_unit=='KG',sum(tran_prod_sale_qty),cust_id][order(V1,decreasing = T)][c(1:792),sum(V1)] # number of products in KG, top 10%

# question 4

# RFM model
# calculate R,F and M
RFM <- transaction %>% 
  group_by(cust_id) %>% 
  summarise(recency=as.numeric(as.Date(max(transaction$tran_dt))-max(tran_dt)), # recency is how many days since last purchase to the last day in the dataset
            frequency=n_distinct(tran_id), 
            monetary= sum(tran_prod_paid_amt)/n_distinct(tran_id))

# champion customers
RFM %>% filter(recency <= 1) %>% filter(frequenci >= 388) %>% filter(monetary >= 30) #21
# loyal customers
RFM %>% filter(frequenci >= 388) %>% filter(monetary >= 30) #28
# potential loyalists 
RFM %>% filter(recency <= 1) %>% filter(monetary >= 30) #142
# at risk
RFM %>% filter(recency > 5) %>% filter(frequenci >= 388) %>% filter(monetary >= 30) #0. most recency is 4
# already lost
RFM %>% filter(recency > 5) %>% filter(frequenci <= 272) %>% filter(monetary <= 19) #3
summary(RFM)

# k-means
data <- merge(transaction,product,by = 'prod_id') # Merge transaction data and product data
                                                  #There are 500 transaction with products not inluded in product data
# create a binary variable to see which row is about discounted item
data[,onsale:=1]
data[tran_prod_sale_amt==tran_prod_paid_amt,onsale:=0]
# create a customer-level data
l <- data[,.(total_revenue=sum(tran_prod_paid_amt), # total revenue
         total_transaction=length(unique(ti)), # total number of transaction
         t_d_product=length(unique(prod_id)), # total number of distinct product purchased
         t_d_store=length(unique(store_id)), # total number of distinct stores visited 
         t_d_category=length(unique(category_id)), # total number of distinct categories purchased
         t_d_brand=length(unique(brand_desc)), # total number of distinct brands purchased
         p_o_discount_purchase=sum(onsale)/.N, # percentage of productes purchased on sale
         avg_discount_rate=-sum(tran_prod_discount_amt)/sum(tran_prod_sale_amt)),cust_id] # the sales amount on sale over total sales amount 

cor(l) # calculate correlation
l <- l[,-c('t_d_product','t_d_brand','p_o_discount_purchase')] # exclude attributes with high correlation
# create normalization function
standarlize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
# normalization
ll <- lapply(l,standarlize)
ll <- as.data.table(ll)
ll <- cbind(l[,.(cust_id)],ll[,-c('cust_id')])
# find the best k
fviz_nbclust(ll[, 2:6], kmeans, method = "wss", k.max = 10, print.summary = T)
fviz_nbclust(ll[, 2:6], kmeans, method = "silhouette", k.max = 10, print.summary = T)
fviz_nbclust(ll[, 2:6], kmeans, method = "gap_stat",nboot = 30, k.max = 10, print.summary = T)
# the best k we have
#set.seed(1234)
clusters <- kmeans(ll[, 2:6], 5)
clusters$size # 1942 1446 907 1713 1912
clusters$centers # centric after normalization
ll[,cluster:=clusters$cluster]
ll[,-c('cust_id','t_d_category')][,lapply(.SD,mean),cluster][order(cluster)] # centric before normalization


