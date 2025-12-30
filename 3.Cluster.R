load("AEDPL.RData") 

summary(AEDPL)
sapply(AEDPL, function(x) sum(is.na(x)))
# remove NAs do dataset original
AEDPL <- na.omit(AEDPL)

# selecionar apenas variáveis quantitativas
aedl_quant <- AEDPL[, sapply(AEDPL, is.numeric)]
summary(aedl_quant)
str(aedl_quant)

# antes de fazer a scale vamos ver a media e o desvio padrão 
sapply(aedl_quant, mean)
sapply(aedl_quant, sd)

# fazer a standardização
aedl_s <- as.data.frame(scale(aedl_quant))
summary(aedl_s)
sapply(aedl_s, mean)
sapply(aedl_s, sd)

# --------------------------------------------------------------------------------------
#a aplicação de um método hierárquico (apresentando o respetivo dendrograma)

# Distância Euclidiana entre indivíduos
d_eucl <- dist(aedl_s, method = "euclidean")

# Método hierárquico com critério de Ward
hc_ward <- hclust(d_eucl, method = "ward.D2")

# plot de dendrogramas para interpretar com os rectangulos
plot(hc_ward, hang = -1,
     main = paste("Dendrograma (Distância Euclidiana / Ward.D2) -", 2, "clusters"),
     xlab = "", ylab = "Altura")
rect.hclust(hc_ward, 2, border="blue")

plot(hc_ward, hang = -1,
     main = paste("Dendrograma (Distância Euclidiana / Ward.D2) -", 3, "clusters"),
     xlab = "", ylab = "Altura")
rect.hclust(hc_ward, 3, border="blue")

plot(hc_ward, hang = -1,
     main = paste("Dendrograma (Distância Euclidiana / Ward.D2) -", 4, "clusters"),
     xlab = "", ylab = "Altura")
rect.hclust(hc_ward, 4, border="blue")


# a altura de corte do dendrograma é a distancia entre os clusters
# visualmente podemos ver que existem 3 cortes possiveis interessantes

# analise de clusters 
library(fpc)
library(factoextra)
library(cluster);

cluster2 <- cutree(hc_ward, 2)
calinhara(aedl_s, cluster2)
fviz_silhouette(silhouette(cluster2, dist(aedl_s)))

cluster3 <- cutree(hc_ward, 3)
calinhara(aedl_s, cluster3)
fviz_silhouette(silhouette(cluster3, dist(aedl_s)))

cluster4 <- cutree(hc_ward, 4)
calinhara(aedl_s, cluster4)
fviz_silhouette(silhouette(cluster4, dist(aedl_s)))

# tendo em conta os dados, embora o calinhara do cluster2 apresente um valor superior 
# vamos escolher o cluster3 porque tem um average silhouette melhor o que indica que os individuos estao melhor agrupados
# ja que o calinhara apenas indica a separacao entre os clusters e nao a coesao dentro dos clusters

k_escolhido <- 3

# ------------------------------------------------------

# a aplicação de um método não hierárquico (k-means);
set.seed(123)

library(factoextra); 

fviz_nbclust(aedl_s, kmeans, method = "wss")
fviz_nbclust(aedl_s, kmeans, method = "silhouette")
fviz_nbclust(aedl_s, kmeans, method = "gap_stat")

