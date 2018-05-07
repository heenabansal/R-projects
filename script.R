# prerequisite
#run sed '/^$/d' order_products__prior.csv | wc -l
#run split -l 6486898 order_products__prior.csv 
setwd('/home/fractaluser/Courses/R301/Instacart Market Basket Analysis')

library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(treemap)

orders <- fread('orders.csv')
products <- fread('products.csv')
order_products <- fread('order_products__train.csv')
order_products_prior1 <- fread('xaa',header = TRUE)
order_products_prior2 <- fread('xab')
order_products_prior3 <- fread('xac')
order_products_prior4 <- fread('xad')
order_products_prior5 <- fread('xae')
aisles <- fread('aisles.csv')
departments <- fread('departments.csv')

colnames(order_products_prior2) = colnames(order_products_prior1)
colnames(order_products_prior3) = colnames(order_products_prior1)
colnames(order_products_prior4) = colnames(order_products_prior1)
colnames(order_products_prior5) = colnames(order_products_prior1)
order_products_prior = rbind(order_products_prior1,order_products_prior2)
order_products_prior = rbind(order_products_prior,order_products_prior3)
order_products_prior = rbind(order_products_prior,order_products_prior4)
order_products_prior = rbind(order_products_prior,order_products_prior5)
rm(order_products_prior1)
rm(order_products_prior2)
rm(order_products_prior3)
rm(order_products_prior4)
rm(order_products_prior5)

head(orders,12)
glimpse(orders)
orders <- orders %>% mutate(order_hour_of_day = as.numeric(order_hour_of_day), eval_set = as.factor(eval_set))
glimpse(products)
glimpse(aisles)
glimpse(departments)

products <- products %>% mutate(product_name = as.factor(product_name))
aisles <- aisles %>% mutate(aisle = as.factor(aisle))
departments <- departments %>% mutate(department = as.factor(department))

g = orders %>% 
  ggplot(aes(x=order_hour_of_day)) + 
  geom_histogram(stat="count",fill="red")

grid.arrange(g)

g = orders %>% 
  ggplot(aes(x=order_dow)) + 
  geom_histogram(stat="count",fill="red")

grid.arrange(g)

g = orders %>% 
  ggplot(aes(x=days_since_prior_order)) + 
  geom_histogram(stat="count",fill="red")
grid.arrange(g)

# x = orders %>% filter(eval_set=="prior")

# x = x %>% 
#   group_by(user_id) %>%
#   summarise(total_prior_orders = max(order_number),
#             total = n())
# x = x %>% 
#   group_by(total_prior_orders) %>%
#   summarise(total = n())

#g = x %>% ggplot(aes(total_prior_orders,n)) + geom_line(color="red", size=1)+geom_point(size=2, color="red")
g = orders %>% filter(eval_set=="prior") %>% count(order_number) %>% ggplot(aes(order_number,n)) + geom_line(color="red", size=1)+geom_point(size=2, color="red")
grid.arrange(g)

g = orders %>% filter(eval_set=="train") %>% count(order_number) %>% ggplot(aes(order_number,n)) + geom_line(color="red", size=1)+geom_point(size=2, color="red")
grid.arrange(g)

g = order_products %>% 
  group_by(order_id) %>% 
  summarize(n_items = last(add_to_cart_order)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(stat="count",fill="red") + 
  geom_rug()+
  coord_cartesian(xlim=c(0,80))
grid.arrange(g)

g = order_products_prior %>% 
  group_by(order_id) %>% 
  summarize(n_items = last(add_to_cart_order)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(stat="count",fill="red") + 
  geom_rug()+
  coord_cartesian(xlim=c(0,80))
grid.arrange(g)


order_products %>% 
  group_by(product_id) %>% 
  summarize(total = n()) %>% arrange(desc(total)) %>% head(10) %>%
  left_join(select(products,product_id,product_name),by='product_id')

tmp = order_products %>% 
  group_by(product_id) %>% 
  summarize(count = n()) %>% 
  top_n(10, wt = count) %>%
  left_join(select(products,product_id,product_name),by="product_id") %>%
  arrange(desc(count)) 

order_products_prior %>% 
  group_by(product_id) %>% 
  summarize(total = n()) %>% arrange(desc(total)) %>% head(10) %>%
  left_join(select(products,product_id,product_name),by='product_id')


tmp %>% 
  ggplot(aes(x=reorder(product_name,-count), y=count))+
  geom_bar(stat="identity",fill="red")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())

order_products_prior %>% left_join(select(orders,order_id,user_id),by='order_id') %>%
  group_by(user_id,product_id) %>% 
  summarise(total = sum(reordered)) %>% arrange(desc(total)) %>% head(10) %>%
  left_join(select(products,product_id,product_name),by='product_id')

tmp = order_products %>% 
  group_by(reordered) %>% 
  summarize(count = n()) %>% 
  mutate(reordered = as.factor(reordered)) %>%
  mutate(proportion = count/sum(count))

tmp %>% 
  ggplot(aes(x=reordered,y=count,fill=reordered))+
  geom_bar(stat="identity")

tmp <-order_products %>% 
  group_by(product_id) %>% 
  summarize(proportion_reordered = mean(reordered), n=n()) %>%
  filter(n>40) %>% 
  top_n(10,wt=proportion_reordered) %>% 
  arrange(desc(proportion_reordered)) %>% 
  left_join(products,by="product_id")

tmp %>% 
  ggplot(aes(x=reorder(product_name,-proportion_reordered), y=proportion_reordered))+
  geom_bar(stat="identity",fill="red")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+coord_cartesian(ylim=c(0.85,0.95))

tmp <-order_products %>% filter(add_to_cart_order==1) %>%
  group_by(product_id) %>% 
  summarize(n=n()) %>%
  top_n(10,wt=n) %>% 
  arrange(desc(n)) %>% 
  left_join(products,by="product_id")

tmp <- order_products %>% 
  group_by(product_id, add_to_cart_order) %>% 
  summarize(count = n()) %>% mutate(pct=count/sum(count)) %>% 
  filter(add_to_cart_order == 1, count>10) %>% 
  arrange(desc(pct)) %>% 
  left_join(products,by="product_id") %>% 
  select(product_name, pct, count) %>% 
  ungroup() %>% 
  top_n(10, wt=pct)

tmp %>% 
  ggplot(aes(x=reorder(product_name,-pct), y=pct))+
  geom_bar(stat="identity",fill="red")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+coord_cartesian(ylim=c(0.4,0.7))


order_products %>% 
  left_join(orders,by="order_id") %>% 
  group_by(days_since_prior_order) %>%
  summarize(mean_reorder = mean(reordered)) %>%
  ggplot(aes(x=days_since_prior_order,y=mean_reorder))+
  geom_bar(stat="identity",fill="red")


order_products %>% 
  group_by(product_id) %>% 
  summarize(proportion_reordered = mean(reordered), n=n()) %>%
  ggplot(aes(x=n,y=proportion_reordered))+
  geom_point()+
  geom_smooth(color="red")+
  coord_cartesian(xlim=c(0,2000))


products <- products %>% 
  mutate(organic=ifelse(str_detect(str_to_lower(products$product_name),'organic'),"organic","not organic"), organic= as.factor(organic))

tmp <- order_products %>% 
  left_join(products, by="product_id") %>% 
  group_by(organic) %>% 
  summarize(count = n()) %>% 
  mutate(proportion = count/sum(count))

tmp %>% 
  ggplot(aes(x=organic,y=count, fill=organic))+
  geom_bar(stat="identity")

tmp <- order_products %>% 
  left_join(products,by="product_id") %>% 
  group_by(organic) %>%
  summarize(mean_reordered = mean(reordered))

tmp %>% 
  ggplot(aes(x=organic,fill=organic,y=mean_reordered))+
  geom_bar(stat="identity")

tmp <- products %>% group_by(department_id, aisle_id) %>% summarize(n=n())
tmp <- tmp %>% left_join(departments,by="department_id")
tmp <- tmp %>% left_join(aisles,by="aisle_id")

tmp2<-order_products %>% 
  group_by(product_id) %>% 
  summarize(count=n()) %>% 
  left_join(products,by="product_id") %>% 
  ungroup() %>% 
  group_by(department_id,aisle_id) %>% 
  summarize(sumcount = sum(count)) %>% 
  left_join(tmp, by = c("department_id", "aisle_id")) %>%
  mutate(onesize = 1)

treemap(tmp2,index=c("department","aisle"),
        vSize="onesize",vColor="department",palette="Set3",title="",
        sortID="-sumcount", border.col="#FFFFFF",
        type="categorical", fontsize.legend = 0,bg.labels = "#FFFFFF")


tmp <- order_products_prior %>% 
  group_by(order_id) %>% 
  summarize(m = mean(reordered),n=n()) %>% 
  right_join(filter(orders,order_number>2), by="order_id")

















