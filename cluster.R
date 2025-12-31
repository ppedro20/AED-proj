#######################################################
##### Tarefa 3: Análise de Clusters
#######################################################

# --------------------------------------------------------------------------------------
# TEORIA GERAL: O que é a Análise de Clusters?
#
# A Análise de Clusters (ou Agrupamento) é uma técnica exploratória usada para identificar
# grupos de observações (neste caso, jogadores) que são semelhantes entre si dentro do mesmo grupo
# e distintos dos jogadores de outros grupos.
#
# Objetivo Principal: Encontrar padrões naturais nos dados sem usar as classificações pré-existentes
# (como a Posição ou Faixa Etária). No final, queremos ver se os grupos "matemáticos" que encontramos
# fazem sentido e se correspondem, por exemplo, à posição do jogador em campo ou à sua experiência.
# --------------------------------------------------------------------------------------

# Bibliotecas necessárias (conforme ex. Ficha 7)
library(factoextra)
library(cluster)
library(fpc)
library(clusterCrit)
library(aricode)
library(ggplot2) 

# Carregar dados (se necessário)
if(!exists("AEDPL")) {
  # Tenta carregar caso não esteja em memória
  if(file.exists("AEDPL.RData")) load("AEDPL.RData") 
}

# --------------------------------------------------------------------------------------
# PASSO 1: Preparação dos Dados
#
# TEORIA: O Clustering depende de distâncias matemáticas. Variáveis com escalas diferentes 
# (ex: Minutos Jogados vs Golos por jogo) podem distorcer os resultados, pois variáveis com 
# números maiores dominam o cálculo da distância.
#
# PERGUNTA A RESPONDER: As variáveis estão prontas para serem comparadas justa e "imparcialmente"?
# --------------------------------------------------------------------------------------

# Selecionar todas as variáveis numéricas
if(!exists("AEDPL")) stop("O objeto 'AEDPL' não foi encontrado. Por favor carregue os dados.")

vars_quant <- sapply(AEDPL, is.numeric)
dados_quant <- AEDPL[, vars_quant]

# IMPORTANTE: Remover variáveis de ID se existirem
# dados_quant <- dados_quant[, -1] 

# Tratar valores omissos
if(sum(is.na(dados_quant)) > 0){
  dados_quant <- na.omit(dados_quant)
  AEDPL_cluster <- AEDPL[rownames(dados_quant), ] 
} else {
  AEDPL_cluster <- AEDPL
}

# Estandardização (Scaling)
# Transformar os dados para que todos tenham média 0 e desvio padrão 1.
dados_pad <- as.data.frame(scale(dados_quant))
summary(dados_pad)


#######################################################
##### 3.b) Método não hierárquico (K-means)
#######################################################

# --------------------------------------------------------------------------------------
# TEORIA: K-means
# O algoritmo K-means tenta dividir os dados em 'k' grupos. Funciona definindo 'k' centros 
# (centróides) e atribuindo cada jogador ao centro mais próximo. Depois, ajusta os centros
# repetidamente até encontrar a melhor separação possível.
#
# Desafio: O algoritmo não sabe qual o melhor número de grupos ('k'). Temos de descobrir nós.
#
# MÉTODOS DE DECISÃO:
# 1. Elbow Method (Cotovelo): Procura o ponto onde adicionar mais um cluster já não reduz 
#    significativamente a variância dentro dos grupos.
# 2. Silhouette Method: Mede o quão "bem encaixado" cada jogador está no seu cluster
#    comparado com os clusters vizinhos.
#
# PERGUNTA A RESPONDER: Quantos grupos de jogadores (diferentes entre si) existem realmente nos dados?
# --------------------------------------------------------------------------------------

# Método WSS (Elbow)
fviz_nbclust(dados_pad, kmeans, method = "wss") +
  labs(title = "Número ótimo de clusters - WSS (Elbow)",
       subtitle = "Procure o 'cotovelo' onde a curva começa a aplanar")

# Método Silhouette
fviz_nbclust(dados_pad, kmeans, method = "silhouette") +
  labs(title = "Número ótimo de clusters - Silhouette",
       subtitle = "Procure o valor mais alto de silhueta média")

### [IMPORTANTE] Defina o k aqui baseado nos gráficos
k_escolhido <- 3

# Aplicação do K-means
set.seed(1234) # Para garantir que resultados são reproduzíveis
km_res <- kmeans(dados_pad, centers = k_escolhido, nstart = 25)

# --------------------------------------------------------------------------------------
# ANÁLISE DOS RESULTADOS DO K-MEANS
#
# Centróides: Representam o "jogador médio" de cada cluster. Olhando para os valores (positivos ou negativos),
# podemos perceber as características que definem o grupo.
#
# PERGUNTA A RESPONDER: O que define cada grupo? (Ex: O Grupo 1 são os 'Goleadores que jogam pouco'?)
# --------------------------------------------------------------------------------------

# Centróides
print(km_res$centers)

# Dispersão Intra-Cluster (Within-SS) vs Inter-Cluster (Between-SS)
# Queremos Intra baixo (grupos compactos) e Inter alto (grupos bem separados).
ratio <- km_res$betweenss / km_res$totss;print(paste("Rácio de Separação (quanto maior melhor):", round(ratio, 4)))

# Validação Interna
# Calinski-Harabasz: Índice de variância. Maior é melhor.
ch_val <- calinhara(dados_pad, km_res$cluster); print(paste("Calinski-Harabasz Index:", ch_val))

# Silhouette Plot
sil <- silhouette(km_res$cluster, dist(dados_pad))
fviz_silhouette(sil) + labs(title="Gráfico de Silhueta - Qualidade da atribuição")
# P: Existem barras negativas? Isso indica jogadores que provavelmente estão no grupo errado.

# Visualização Gráfica
fviz_cluster(km_res, data = dados_pad, geom = "point", ellipse.type = "norm") +
  ggtitle("Visualização dos Clusters (K-means)")


#######################################################
##### 3.a) Método Hierárquico
#######################################################

# --------------------------------------------------------------------------------------
# TEORIA: Clustering Hierárquico
# Ao contrário do K-means, este método cria uma "árvore" (Dendrograma) de ligações.
# Começa considerando cada jogador como um grupo individual e vai juntando (aglomerando) 
# os pares mais parecidos sucessivamente até todos estarem juntos.
#
# Método Ward.D2: É um critério popular para aglomeração porque tenta minimizar a variância 
# interna dos clusters em cada passo, criando grupos compactos (semelhante ao K-means).
#
# PERGUNTA A RESPONDER: A estrutura hierárquica dos dados confirma os grupos que vimos no K-means?
# --------------------------------------------------------------------------------------

# Matriz de distâncias
d_euc <- dist(dados_pad, method = "euclidean")

# Agrupamento Hierárquico (Ward)
hc_ward <- hclust(d_euc, method = "ward.D2")

# Dendrograma
plot(hc_ward, hang = -1, main = "Dendrograma (Ward.D2)")
rect.hclust(hc_ward, k = k_escolhido, border = "red") # Desenha caixas à volta dos grupos

# Comparar os resultados dos dois métodos (K-means vs Hierárquico)
groups_hc <- cutree(hc_ward, k = k_escolhido)
table(Kmeans = km_res$cluster, Hierarquico = groups_hc)
# P: A diagonal principal tem os números maiores? Se sim, os métodos concordam entre si.


#######################################################
##### Comparação com Variáveis Qualitativas
#######################################################

# --------------------------------------------------------------------------------------
# TEORIA: Validação Externa (Profiling)
# O computador encontrou grupos baseado apenas nos NÚMEROS (stats).
# Agora vamos ver se esses grupos correspondem aos RÓTULOS REAIS que nós conhecemos 
# (Posição no campo, Faixa Etária, etc).
#
# ARI (Adjusted Rand Index): Mede a concordância entre duas classificações.
# 0 = Concordância aleatória (não tem nada a ver um com o outro).
# 1 = Concordância perfeita.
#
# PERGUNTA A RESPONDER: Os meus clusters "descobriram" sozinhos as posições dos jogadores?
# Ou agruparam-nos por outra lógica (ex: performance)?
# --------------------------------------------------------------------------------------

AEDPL_cluster$Cluster_Kmeans <- factor(km_res$cluster)

# 1. Comparação com POSICAO (Nominal)
print("--- Posicao vs Clusters ---")
print(table(AEDPL_cluster$Posicao, AEDPL_cluster$Cluster_Kmeans))

ari_pos <- ARI(AEDPL_cluster$Posicao, AEDPL_cluster$Cluster_Kmeans)
print(paste("ARI (Posicao):", round(ari_pos, 4)))

# Gráfico de barras
ggplot(AEDPL_cluster, aes(x=Cluster_Kmeans, fill=Posicao)) +
  geom_bar(position="fill") +
  labs(y="Proporção", title="Composição dos Cluster por Posição",
       subtitle="Cada cluster é dominado por uma posição específica?")


# 2. Comparação com FAIXA ETARIA (Ordinal)
print("--- Faixa Etaria vs Clusters ---")
print(table(AEDPL_cluster$FaixaEtaria, AEDPL_cluster$Cluster_Kmeans))

# Remover NAs apenas para este cálculo se existirem
idx_valid <- !is.na(AEDPL_cluster$FaixaEtaria)
ari_fe <- ARI(AEDPL_cluster$FaixaEtaria[idx_valid], AEDPL_cluster$Cluster_Kmeans[idx_valid])
print(paste("ARI (Faixa Etaria):", round(ari_fe, 4)))

ggplot(AEDPL_cluster, aes(x=Cluster_Kmeans, fill=FaixaEtaria)) +
  geom_bar(position="fill") +
  labs(y="Proporção", title="Composição dos Cluster por Faixa Etária",
       subtitle="Os clusters distinguem jogadores jovens de veteranos?")

# Fim da Tarefa 3
