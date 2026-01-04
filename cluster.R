library(factoextra)
library(fpc)
library(cluster)
library(clusterCrit)
library(aricode)
library(ggplot2)

load("AEDPL.RData") 

# preparar os dados - remover valores omissos
colSums(is.na(AEDPL))
AEDPL <- na.omit(AEDPL)

# dataset com variáveis quantitativas 
aedl_quant <- AEDPL[, sapply(AEDPL, is.numeric)]
summary(aedl_quant)

sapply(aedl_quant, mean)
#Idade Minutos_Jogados   Jogos_Jogados           Golos    Assistencias   ExpectedGoals ExpectedAssists         Remates 
#25.098246     1317.175439       20.270175        1.898246        1.410526        1.932281        1.427193       17.129825 
#RematesNoAlvo          Passes PassesCompletos        Desarmes  Interceptacoes    Recuperacoes 
#5.761404        9.580702      528.282456       23.452632       10.584211       55.415789 

sapply(aedl_quant, sd)
#Idade Minutos_Jogados   Jogos_Jogados           Golos    Assistencias   ExpectedGoals ExpectedAssists         Remates 
#4.450568     1051.895105       12.189078        3.546849        2.258332        3.183803        1.966087       21.803177 
#RematesNoAlvo          Passes PassesCompletos        Desarmes  Interceptacoes    Recuperacoes 
#8.505095        9.984064      504.617375       24.400410       12.425840       51.258065 

# aplicar standardização 
aedl_s <- as.data.frame(scale(aedl_quant))

# fazer a standardização
aedl_s <- as.data.frame(scale(aedl_quant))
sapply(aedl_s, mean)
sapply(aedl_s, sd)

# ------------------------------------------------------
# Tarefa: aplicação de um método hierárquico (apresentando o respetivo dendrograma) + justificação

# escolha do método de distância e do critério hierárquico
# o objetivo é conseguir ter cluster com uma boa separação entre clusters e uma boa coesão dentro dos clusters
# para isso vamos testar diferentes combinações de métodos de distância e critérios hierárquicos
# e avaliar os resultados com o índice de Calinski-Harabasz e o índice de Silhueta média

analise_de_medidas_distancia <- function(metodo_distancia, criterio_hierarquico){
  # Distância entre indivíduos
  d <- dist(aedl_s, method = metodo_distancia)
  # Método hierárquico 
  hc <- hclust(d, method = criterio_hierarquico)
  # plot do dendrograma
  par(mfrow=c(1,1))
  
  plot(hc, hang = -1,
       main = paste("Dendrograma:" , metodo_distancia, "/", criterio_hierarquico),
       xlab = "", ylab = "Altura")
  return(hc)
}

# ward.D2 vs complete vs average vs single
# ward.D2: minimiza a variância dentro dos clusters, criando grupos compactos
# complete: usa a distância máxima entre pontos de diferentes clusters, tendendo a criar clusters mais esféricos
# average: usa a distância média entre pontos de diferentes clusters, equilibrando entre
# single: usa a distância mínima entre pontos de diferentes clusters, podendo criar clusters alongados
hc_e_single <- analise_de_medidas_distancia("euclidean", "single") # discartado (divisao estranha dos clusters)
hc_m_single <- analise_de_medidas_distancia("manhattan", "single") # discartado (divisao estranha dos clusters)

hc_e_average <- analise_de_medidas_distancia("euclidean", "average") # discartado (divisao estranha dos clusters)
hc_m_average <- analise_de_medidas_distancia("manhattan", "average") # discartado (divisao estranha dos clusters)

hc_e_complete <- analise_de_medidas_distancia("euclidean", "complete") 
hc_m_complete <- analise_de_medidas_distancia("manhattan", "complete")
# é possivel ver-se uma divisao entre 2 grupos, mas a divisao em 3 grupos nao é muito clara,
# temos um cluster completamente dominante e dois clusters pequenos

hc_e_ward <- analise_de_medidas_distancia("euclidean", "ward.D2") 
hc_m_ward <- analise_de_medidas_distancia("manhattan", "ward.D2")
# ha uma melhor separação entre os clusters, tanto para 2 como para 3 clusters
# para 4 clusters ja começa haver uma sobreposição maior

# ward.D2 é o critério que melhor se adequa aos dados, pois cria clusters mais compactos e bem separados
# quando comparado com os outros critérios testados

# divisao por k clusters
metodo <- "euclidean"
criteiro <- "ward.D2"
d <- dist(aedl_s, metodo)
hc <- analise_de_medidas_distancia(metodo, criteiro)
rect.hclust(hc, 2, border="blue")
rect.hclust(hc, 3, border="green")
rect.hclust(hc, 4, border="red")

# a escolha vai recair entre 2 e 3 clusters
# 4 cluster ja apresenta uma altura de corte mais baixa, o que indica que os clusters estao mais proximos entre si

cluster2 <- cutree(hc, 2)
calinhara(aedl_s, cluster2) # 339.749
fviz_silhouette(silhouette(cluster2, dist(aedl_s))) # avg global: 0.39, avg cluster1: 0.52, avg cluster2: 0.15
# o cluster 2 apresenta uma silhouette muito baixa, o que indica que os pontos deste cluster estao mal atribuídos
# o cluster 1 apresenta uma silhouette boa, o que indica que os pontos deste cluster estao bem atribuídos
fviz_cluster(list(data=aedl_s, cluster=cluster2), geom="point", ellipse.type="norm") +
  ggtitle("Visualização dos Clusters (Hierárquico) - 2 Clusters")

cluster3 <- cutree(hc, 3)
calinhara(aedl_s, cluster3) # 339.5287
fviz_silhouette(silhouette(cluster3, d))
fviz_cluster(list(data=aedl_s, cluster=cluster3), geom="point", ellipse.type="norm") +
  ggtitle("Visualização dos Clusters (Hierárquico) - 3 Clusters")


# em termos de calinhara tanto k=2 e k=3 apresentam valores muito proximos 
# em termos de silhouette quando temos k=2 vemos que todo o cluster 2 esta abaixo da media 
# com k=2 tambem vemos que a media da silhouette do cluster 2 é de 0.15 (muito baixo)
# com k=3 vemos que o cluster 2 e 3 ja apresentam resultados acima da media global da silhouetta
# embora tambem nao tenham uma silhouetta boa (cluster2 = 0.29, cluster3=0.32)
# comparando k=2 e k=3, k=3 apresenta uma estrutura melhor

# ------------------------------------------------------
# Tarefa: aplicação de um método não hierárquico (kmeans) + justificação

set.seed(1234)

# encontrar o numero de clusters
fviz_nbclust(aedl_s, kmeans, method = "wss") # k = 2 a 3 (a partir do 3 o decréscimo é menos acentuado)
fviz_nbclust(aedl_s, kmeans, method = "silhouette") # k = 3

analise_kmeans <- function(k){
  km <- kmeans(aedl_s, centers = k)
  ratio <- km$betweenss / km$totss
  print(paste("Rácio de Separação (quanto maior melhor):", round(ratio, 4)))
  print(paste("Dispersão intra clusters:", toString(km$withinss)))
  print(paste("Total da dispersão intra clusters:", km$tot.withinss))
  print(paste("Dispersão inter clusters:", km$betweenss))
  return(km)
}

km2 <- analise_kmeans(2)
# "Rácio de Separação (quanto maior melhor): 0.4141"
# "Dispersão intra clusters: 3150.67157786437, 1516.69998225208"
# "Total da dispersão intra clusters: 4667.37156011645"
# "Dispersão inter clusters: 3298.62843988355"
# Dimensão dos clusters: 207, 363"
# K-means clustering with 2 clusters of sizes 207, 363

km3 <- analise_kmeans(3)
#"Rácio de Separação (quanto maior melhor): 0.5738"
#"Dispersão intra clusters: 1243.50862154221, 792.989209189692, 1358.95015625505"
#"Total da dispersão intra clusters: 3395.44798698696"
#"Dispersão inter clusters: 4570.55201301304"
#"Dimensão dos clusters: 162, 62, 346"
#K-means clustering with 3 clusters of sizes 162, 62, 346


calinhara(aedl_s, km2$cluster)
calinhara(aedl_s, km3$cluster)

fviz_silhouette(silhouette(km2$cluster, dist(aedl_s)))
fviz_silhouette(silhouette(km3$cluster, dist(aedl_s)))

fviz_cluster(km2, aedl_s, main=" ", ellipse.type="norm")
fviz_cluster(km3, aedl_s, main=" ", ellipse.type="norm")

#------------------------------------------------------
# Tarefa: comparação dos resultados obtidos com variáveis qualitativas (posição e faixa et

# ward.D2 k=3 cluster3
table(AEDPL$FaixaEtaria, cluster3)
ARI(AEDPL$FaixaEtaria, cluster3)

table(AEDPL$Posicao, cluster3)
ARI(AEDPL$Posicao, cluster3)

# K-means k=3
table(AEDPL$FaixaEtaria, km3$cluster)
ARI(AEDPL$FaixaEtaria, km3$cluster)

table(AEDPL$Posicao, km3$cluster)
ARI(AEDPL$Posicao, km3$cluster)
