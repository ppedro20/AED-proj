library(factoextra)
library(fpc)
library(cluster)
library(clusterCrit)
library(aricode)



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


fviz_nbclust(aedl_s, kmeans, method = "wss")
# a analise demonstra devem ser utilizados 2 a 3 clusters
# porque 
fviz_nbclust(aedl_s, kmeans, method = "silhouette")
# o valor maximo utilizado k_means com a silhouette é 3 clusters 

# vai-se analisar qual o melhor numero de clusters entre 2 e 3 
# para isso vai-se olhar para as dispersões: intra-clusters, inter-cluster e totais

analise_k_cluster <- function(k){
  set.seed(1234)
  km <- kmeans(aedl_s, centers = k)
  print(paste("Análise para k =", k))
  #print(paste("Centroides das variaveis estandardizadas:", toString(km$centers)))
  # soma total dos quadrados
  print(paste("Soma total dos quadrados:", km$totss))
  ratio <- km$betweenss / km$totss
  print(paste("Rácio de Separação ", round(ratio, 4)))
  # dispersao intra clusters
  print(paste("Dispersao intra clusters:", toString(km$withinss)))
  # total da dispersao intra clusters
  print(paste("Total da dispersao intra clusters:", km$tot.withinss))
  # dispersao inter clusters
  print(paste("Dispersao inter clusters:", km$betweenss))
  # dimensao dos clusters
  print(paste("Dimensao dos clusters:", toString(km$size)))
  
  return(km) 
  
  
}

km2_cluster <- analise_k_cluster(2)$cluster
km3_cluster <- analise_k_cluster(3)$cluster
# o k=3 apresenta um ratio melhor e uma dispersao intra clusters menor <- rever

calinhara(aedl_s, km2_cluster)
# 401.4296 é ao valor do calinhara para k=2, que corresponde à separação entre os clusters
calinhara(aedl_s, km3_cluster)
# 381.6143 é o valor do calinhara para k=3, que corresponde à separação entre os clusters
# o k=2 apresenta um valor melhor no calinhara


fviz_silhouette(silhouette(km2_cluster, dist(aedl_s)))
# cluster| size |ave.sil.width
# 1      |363   |0.56
# 2      |207   |0.17
# average silhouette width: 0.42

fviz_silhouette(silhouette(km3_cluster, dist(aedl_s)))
#cluster| size a| av.sil.width
#1      |346    |0.51
#2      |62     |0.28
#3      |162    |0.27
# average silhouette width: 0.42


# da analise feita pudemos verificar que o k=2 apresenta melhores resultados
# tanto no calinhara como na silhouette
# definimos "melhor calinhara" como o numero de clusters que apresenta maior separação entre os clusters
# definimos "melhor silhouette" como o numero de clusters que apresenta maior coesão dentro dos clusters ou seja, os individuos estão melhor agrupados))

fviz_cluster(kmeans(aedl_s, centers = 2), aedl_s, main=" ", ellipse.type="norm")
fviz_cluster(kmeans(aedl_s, centers = 3), aedl_s, main=" ", ellipse.type="norm")

k_escolhido <- 3

# ------------------------------------------------------
# a comparação dos grupos obtidos com os grupos definidos por cada uma das duas variáveis
# qualitativas utilizadas na primeira questão.
# Variaveis escolhidas:
# Nominal: Posicao
# Ordinal: FaixaEtaria




