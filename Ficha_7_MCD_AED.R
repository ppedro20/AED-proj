################################################
#############       Ficha 7       ##############
#######  Análise Exploratória de Dados  ########
################################################
############# Análise de clusters ##############
################################################


###############################################
############    Pergunta 1    #################
# Considere a base de dados iris (disponível no R).
###############################################
# a) Estandardize as variáveis.

data(iris) 
names(iris)

sapply(iris[,1:4], mean)
sapply(iris[,1:4], sd)
boxplot(iris[,1:4])

iris_s <- as.data.frame(scale(iris[,1:4]))
sapply(iris_s, mean)
sapply(iris_s, sd)
boxplot(iris_s)


###############################################
# b) Analise o número k de clusters adequado na aplicação 
#    do método não hierárquico k-means.

library(factoextra); 
fviz_nbclust(iris_s, kmeans, method = "wss")
# O método sugere que devem ser utilizados k=3 ou k=5 clusters
# Vamos só analisar k=3
fviz_nbclust(iris_s, kmeans, method = "silhouette")
# O valor máximo é atingido com k=2 clusters

###############################################
# c) Para o(s) valor(es) k escolhido(s), determine a dispersão 
#    intra-clusters, inter-clusters e total.
set.seed(1234)
km2 <- kmeans(iris_s, 2) 
print(km2)

# intra-clusters 
# 47.35062 no primeiro cluster
# 173.52867 no segundo cluster
# between_SS / total_SS =  62.9 %
km2$cluster
# Vetor com a classificação de cada indivíduo
km2$centers
# Centróides das variáveis estandardizadas
km2$totss
# Soma total dos quadrados é 596
km2$withinss
# dispersão intra clusters é 
# 47.35062 no primeiro cluster
# 173.52867 no segundo cluster
km2$tot.withinss
# totalizando a dispersão intra clusters é 220.8793
km2$betweenss
# dispersão inter clusters 375.1207
km2$size
# dimensão dos clusters: 50 no cluster 1 
# e 100 no cluster 2


# considerando 3 clusters
set.seed(1234)
km3 <- kmeans(iris_s, 3); 
km3$cluster
# Vetor com a classificação de cada indivíduo
km3$centers
# Centróides das variáveis estandardizadas
km3$totss
# Soma total dos quadrados é 596
km3$withinss
# dispersão intra clusters é 
# 47.35062  no primeiro cluster
# 44.08754  no segundo cluster
# 47.45019  no terceiro cluster
km3$tot.withinss
# totalizando a dispersão intra clusters é 138.8884
km3$betweenss
# dispersão inter clusters 457.1116
km3$size
# dimensão dos clusters: 50 no cluster 1, 
# 53 no cluster 2 e 47 no cluster 3



###############################################
# d) Indique os centróides de cada cluster.

# Centróides com 2 clusters:
sapply(iris[km2$cluster == 1, 1:4], mean)
sapply(iris[km2$cluster == 2, 1:4], mean)


# Centróides com 3 clusters:
sapply(iris[km3$cluster == 1, 1:4], mean)
sapply(iris[km3$cluster == 2, 1:4], mean)
sapply(iris[km3$cluster == 3, 1:4], mean)


###############################################
# e) Determine os índices de validação interna de 
#    Calinski-Harabasz e Silhouette.

library(fpc); 
calinhara(iris_s, km2$cluster)
calinhara(iris_s, km3$cluster)
# 2 é melhor do que 3
library(cluster); 
fviz_silhouette(silhouette(km2$cluster, dist(iris_s)))
fviz_silhouette(silhouette(km3$cluster, dist(iris_s)))
# 2 é melhor do que 3
# Ambos os critérios definem 2 cluster melhor que 3 clusters


###############################################
# f) Represente graficamente os clusters obtidos.
 
fviz_cluster(km2, iris_s, main=" ", ellipse.type="norm")
fviz_cluster(km2, iris_s, main=" ")

fviz_cluster(km3, iris_s, main=" ", ellipse.type="norm")
fviz_cluster(km3, iris_s, main=" ")

###############################################
# g) Compare os clusters obtidos com as espécies de cada flor 
#    utilizando medidas de validação externa.
library(clusterCrit)
concordance(as.integer(iris$Species), km2$cluster)
table(iris$Species, km2$cluster)
library(aricode); 
ARI(iris$Species, km2$cluster)

concordance(as.integer(iris$Species), km3$cluster)
table(iris$Species, km3$cluster)
ARI(iris$Species, km3$cluster)

###############################################
# h) Aplique um método hierárquico de análise de clusters e 
#    represente os dendrogramas obtidos através da utilização 
#    de diferentes medidas de dissemelhança entre indivíduos 
#    e entre clusters. 
#    Escolha um dos dendrogramas (justificando a sua escolha) 
#    e indique o número de clusters a utilizar utilizando 
#    medidas de validação interna.


# Distância entre indivíduos "euclidean" 
# Distância entre indivíduos "ward.D2"
d_e <- dist(iris_s, "euclidean")
as.matrix(d_e)[1:5,1:5]
hc_ew = hclust(d_e, method="ward.D2")
plot(hc_ew, hang=-1); 
rect.hclust(hc_ew, 2, border="blue")
rect.hclust(hc_ew, 3, border="red")


# Distância entre indivíduos "euclidean" 
# Distância entre indivíduos "single"
hc_es = hclust(d_e, method="single")
plot(hc_es, hang=-1); 
rect.hclust(hc_es, 2, border="blue")
rect.hclust(hc_es, 3, border="red")

# Distância entre indivíduos euclidean
# Distância entre indivíduos complete
hc_ec = hclust(d_e, method="complete")
plot(hc_ec, hang=-1); 
rect.hclust(hc_ec, 2, border="blue")
rect.hclust(hc_ec, 3, border="red")

# Distância entre indivíduos euclidean
# Distância entre indivíduos average
hc_ea = hclust(d_e, method="average")
plot(hc_ea, hang=-1); 
rect.hclust(hc_ea, 2, border="blue")
rect.hclust(hc_ea, 3, border="red")

# Distância entre indivíduos maximum
# Distância entre indivíduos ward.D2
d_m <- dist(iris_s, "maximum")
hc_mw = hclust(d_m, method="ward.D2")
plot(hc_mw, hang=-1); 
rect.hclust(hc_mw, 2, border="blue")
rect.hclust(hc_mw, 3, border="red")

# Distância entre indivíduos manhattan
# Distância entre indivíduos ward.D2
d_ma <- dist(iris_s, "manhattan")
hc_maw = hclust(d_ma, method="ward.D2")
plot(hc_maw, hang=-1); 
rect.hclust(hc_maw, 3, border="red")

# Distância entre indivíduos: euclidean, maximum, manhattan
# Distância entre clusters: ward.D2, single, average, complete, centroid
# ... Após análise de outras medidas (individuais e entre clusters)...
# optou-se por distância manhattan entre indivíduos e 
# ward.D2 entre clusters

###########################################
# Distância manhattan entre indivíduos
# Distância ward.D2 entre clusters
hc = hclust(d_ma, method="ward.D2")
plot(hc, hang=-1); 

cluster2 <- cutree(hc, 2)
cluster2

cluster3 <- cutree(hc, 3)
cluster3

library(cluster); 
fviz_silhouette(silhouette(cluster2, d_ma))
fviz_silhouette(silhouette(cluster3, d_ma))
# 2 clusters é melhor do que 3 clusters

library(fpc); 
calinhara(iris_s, cluster2)
calinhara(iris_s, cluster3)
# 2 clusters é melhor do que 3 clusters


###############################################
# i) Compare os clusters obtidos com as espécies de cada flor 
#    utilizando medidas de validação externa.
concordance(as.integer(iris$Species), cluster2)
table(iris$Species, cluster2)
library(aricode); 
ARI(iris$Species, cluster2)

concordance(as.integer(iris$Species), cluster3)
table(iris$Species, cluster3)
ARI(iris$Species, cluster3)
# Em termos de validação externa
# 3 clusters é melhor do que 2

###############################################
############    Pergunta 2    #################
###############################################
# Considere a base de dados crabs disponível no package MASS.
###############################################
# a) Aplique o método k-means justificando o 
#    número de clusters utilizado.
 
library(MASS)
data(crabs)
names(crabs)
table(crabs$sp)
table(crabs$sex)
crabs$index

STcrabs <- as.data.frame(scale(crabs[,4:8]))
head(STcrabs)

library(factoextra); 
fviz_nbclust(STcrabs, kmeans, method = "wss")
# Parece que 3 ou 4 serem a melhor escolha... vamos analisar k=3
fviz_nbclust(STcrabs, kmeans, method = "silhouette")
# 2 clusters é a melhor escolha

set.seed(1234)
km2 <- kmeans(STcrabs, 2)
set.seed(1234)
km3 <- kmeans(STcrabs, 3)

library(fpc); 
calinhara(STcrabs, km2$cluster)
calinhara(STcrabs, km3$cluster)
# 3 clusters é melhor do que 2...
library(cluster); 
fviz_silhouette(silhouette(km2$cluster, dist(STcrabs)))
fviz_silhouette(silhouette(km3$cluster, dist(STcrabs)))
# 2 clusters é melhor do que 3...

###############################################
# b) Compare os clusters obtidos com as espécies e 
#    o sexo dos caranguejos.


# Para 2 clusters
library(clusterCrit)
concordance(as.integer(crabs$sp), km2$cluster)
table(crabs$sp, km2$cluster) 
library(aricode); 
ARI(crabs$sp, km2$cluster)

concordance(as.integer(crabs$sex), km2$cluster)
table(crabs$sex, km2$cluster) 
ARI(crabs$sex, km2$cluster)

crabs$sp_sex <- factor(paste(crabs$sp, crabs$sex, sep=""))
concordance(as.integer(crabs$sp_sex), km2$cluster)
table(crabs$sp_sex)
table(crabs$sp_sex, km2$cluster) 
ARI(crabs$sp_sex, km2$cluster)

# Para 3 clusters

concordance(as.integer(crabs$sp), km3$cluster)
table(crabs$sp, km3$cluster) 
ARI(crabs$sp, km3$cluster)

concordance(as.integer(crabs$sex), km3$cluster)
table(crabs$sex, km3$cluster) 
ARI(crabs$sex, km3$cluster)

concordance(as.integer(crabs$sp_sex), km3$cluster)
table(crabs$sp_sex, km3$cluster) 
ARI(crabs$sp_sex, km3$cluster)


###############################################
# c) Aplique um método hierárquico de análise de 
#    clusters e represente os dendrogramas obtidos 
#    através da utilização de diferentes medidas 
#    de dissemelhança entre indivíduos e entre clusters. 
#    Escolha um dos dendrogramas (justificando a sua escolha) 
#    e indique o número de clusters a utilizar utilizando 
#    medidas de validação interna.


d_E <- dist(STcrabs, "euclidean")
hc_1 = hclust(d_E, method="single")
plot(hc_1, hang=-1); 

hc_2 = hclust(d_E, method="average")
plot(hc_2, hang=-1); 

hc_3 = hclust(d_E, method="ward.D2")
plot(hc_3, hang=-1); 

d_M <- dist(STcrabs, "manhattan")
hc_4 = hclust(d_M, method="ward.D2")
plot(hc_4, hang=-1); 

# Após análise de divesas medidas de distância individual e entre clusters
# foi escolhido euclidean/ward.D2 -> hc_3
plot(hc_3, hang=-1); 
rect.hclust(hc_3, 2, border="red")
rect.hclust(hc_3, 4, border="blue")

# O diagrama evidência que 2 e 4 clusters aprecem ser as únicas duas soluções adequadas ....
cluster2 <- cutree(hc_3, 2)
table(cluster2)
cluster4 <- cutree(hc_3, 4)
table(cluster4)


library(cluster); 
library(fpc); 
calinhara(STcrabs, cluster2)
calinhara(STcrabs, cluster4)
# 4 é melhor que 2...
fviz_silhouette(silhouette(cluster2, dist(STcrabs)))
fviz_silhouette(silhouette(cluster4, dist(STcrabs)))
# 2 é melhor que 4...


###############################################
# d) Compare os clusters obtidos com as espécies e 
#    o sexo dos caranguejos.
# 2 clusters

concordance(as.integer(crabs$sp), cluster2)
table(crabs$sp, cluster2) 
ARI(crabs$sp, cluster2)

concordance(as.integer(crabs$sex), cluster2)
table(crabs$sex, cluster2) 
ARI(crabs$sex, cluster2)

concordance(as.integer(crabs$sp_sex), cluster2)
table(crabs$sp_sex, cluster2) 
ARI(crabs$sp_sex, cluster2)

# 4 clusters

concordance(as.integer(crabs$sp), cluster4)
table(crabs$sp, cluster4) 
ARI(crabs$sp, cluster4)

concordance(as.integer(crabs$sex), cluster4)
table(crabs$sex, cluster4) 
ARI(crabs$sex, cluster4)

concordance(as.integer(crabs$sp_sex), cluster4)
table(crabs$sp_sex, cluster4) 
ARI(crabs$sp_sex, cluster4)

