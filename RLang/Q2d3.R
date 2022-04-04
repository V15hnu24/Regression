
data <- read.csv("C:/Users/799vi/Desktop/vish_final1/vish_final1.csv")

install.packages("ggpubr")
library(ggpubr)

d <- data %>% filter(data$cropcategory=="Cash")

res <- cor.test(d$g_index,d$v40,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v42,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v43,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v44,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v45,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v46,method = "pearson",use = "complete.obs")
res


d <- data %>% filter(data$cropcategory=="Cereal")

res <- cor.test(d$g_index,d$v40,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v42,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v43,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v44,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v45,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v46,method = "pearson",use = "complete.obs")
res


d <- data %>% filter(data$cropcategory=="Horticulture")

res <- cor.test(d$g_index,d$v40,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v42,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v43,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v44,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v45,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v46,method = "pearson",use = "complete.obs")
res



d <- data %>% filter(data$cropcategory=="Pulse")

res <- cor.test(d$g_index,d$v40,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v42,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v43,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v44,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v45,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v46,method = "pearson",use = "complete.obs")
res


d <- data %>% filter(data$cropcategory=="Oilseed")

res <- cor.test(d$g_index,d$v40,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v42,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v43,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v44,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v45,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v46,method = "pearson",use = "complete.obs")
res


d <- data %>% filter(data$cropcategory=="Coarse Cereal")

res <- cor.test(d$g_index,d$v40,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v42,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v43,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v44,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v45,method = "pearson",use = "complete.obs")
res

res <- cor.test(d$g_index,d$v46,method = "pearson",use = "complete.obs")
res

