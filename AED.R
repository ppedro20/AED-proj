#######################################################
# Análise Exploratória de Dados
# Mestrado em Ciência de Dados - 1º Ano (2025/2026)

# José Almeida  - 2021122872
# Hugo Lopes    - 2025112531
# Pedro Pedro   - 2020120970

#######################################################
set.seed(123)

library(ggplot2)
library(car)
library(moments)
library(caret)
library(e1071)
library(DescTools)
library(corrplot)
library(psych)
library(factoextra)
library(fpc)
library(cluster)
library(clusterCrit)
library(aricode)

#AEDPL <- premier_league_data
#save(AEDPL, file="AEDPL.RData")
load("AEDPL.RData")

#######################################################
##### 1. Análise exploratória geral da base de dados

View(AEDPL)
str(AEDPL)
names(AEDPL)
dim(AEDPL)
head(AEDPL)

### Manipulação dos dados
AEDPL$Nacionalidade <- substr(AEDPL$Nacionalidade, nchar(as.character(AEDPL$Nacionalidade))-2, nchar(as.character(AEDPL$Nacionalidade)))

AEDPL$Posicao <- substr(as.character(AEDPL$Posicao), 1, 2)
AEDPL$Posicao <- factor(AEDPL$Posicao)

AEDPL$FaixaEtaria <- ordered(
  AEDPL$FaixaEtaria,
  levels = c("≤20","21-24","25-28","29-32","33+")
)

### Análise à existência de observações omissas
colSums(is.na(AEDPL))
AEDPL <- na.omit(AEDPL)

### Apresentação de tabelas, gráficos e medidas adequadas ao resumo da informação das 4 variáveis escolhidas para análise 
### Variaveis escolhidas:
# Nominal: Posicao
# Ordinal: FaixaEtaria
# Quantitativas: Minutos_Jogados, ExpectedGoals

## Variável Nominal: Posicao
# Tabela de Frequências
freq_abs_pos <- table(AEDPL$Posicao)
freq_rel_pos <- prop.table(freq_abs_pos) * 100
cbind(Absoluta = freq_abs_pos, Relativa = round(freq_rel_pos, 2))

# Moda
moda_pos <- names(freq_abs_pos)[which.max(freq_abs_pos)]
cat("Moda (Posicao):", moda_pos, "\n")

# Gráfico de Barras
ggplot(AEDPL, aes(x = Posicao)) +
  geom_bar(fill = "steelblue") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  labs(title = "Distribuição de Jogadores por Posição", x = "Posição", y = "Frequência") +
  theme_minimal()

## Variável Ordinal: FaixaEtaria
# Tabela de Frequências
freq_abs_etaria <- table(AEDPL$FaixaEtaria)
freq_rel_etaria <- prop.table(freq_abs_etaria) * 100
cbind(Absoluta = freq_abs_etaria, Relativa = round(freq_rel_etaria, 2))

# Moda e Mediana (para variável ordinal codificada como fator ordenado, a mediana pode ser obtida via índices ou as.numeric)
moda_etaria <- names(freq_abs_etaria)[which.max(freq_abs_etaria)]
cat("Moda (FaixaEtaria):", moda_etaria, "\n")

# Mediana
mediana_idx <- median(as.numeric(AEDPL$FaixaEtaria), na.rm = TRUE)
mediana_etaria <- levels(AEDPL$FaixaEtaria)[mediana_idx]
cat("Mediana (FaixaEtaria):", mediana_etaria, "\n")

# Gráfico de Barras
ggplot(AEDPL, aes(x = FaixaEtaria)) +
  geom_bar(fill = "darkorange") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  labs(title = "Distribuição de Jogadores por Faixa Etária", x = "Faixa Etária", y = "Frequência") +
  theme_minimal()

## Variáveis Quantitativas: Minutos_Jogados, ExpectedGoals
# Função auxiliar para medidas descritivas
calc_medidas <- function(x) {
  c(
    Media = mean(x, na.rm = TRUE),
    Mediana = median(x, na.rm = TRUE),
    DesvioPadrao = sd(x, na.rm = TRUE),
    Minimo = min(x, na.rm = TRUE),
    Maximo = max(x, na.rm = TRUE),
    Q1 = quantile(x, 0.25, na.rm = TRUE),
    Q3 = quantile(x, 0.75, na.rm = TRUE),
    Coef_Assimetria = skewness(x, na.rm = TRUE)
  )
}

## Minutos Jogados
print("Medidas Descritivas - Minutos Jogados:")
print(calc_medidas(AEDPL$Minutos_Jogados))

# Histograma
ggplot(AEDPL, aes(x = Minutos_Jogados)) +
  geom_histogram(bins = 30, fill = "forestgreen", color = "black") +
  labs(title = "Histograma - Minutos Jogados", x = "Minutos Jogados", y = "Frequência") +
  theme_minimal()

# Boxplot
ggplot(AEDPL, aes(y = Minutos_Jogados)) +
  geom_boxplot(fill = "forestgreen", color = "black") +
  labs(title = "Boxplot - Minutos Jogados", y = "Minutos Jogados") +
  theme_minimal()

## Expected Goals
print("Medidas Descritivas - Expected Goals:")
print(calc_medidas(AEDPL$ExpectedGoals))

# Histograma
ggplot(AEDPL, aes(x = ExpectedGoals)) +
  geom_histogram(bins = 30, fill = "purple", color = "black") +
  labs(title = "Histograma - Expected Goals", x = "Expected Goals", y = "Frequência") +
  theme_minimal()

# Boxplot
ggplot(AEDPL, aes(y = ExpectedGoals)) +
  geom_boxplot(fill = "purple", color = "black") +
  labs(title = "Boxplot - Expected Goals", y = "Expected Goals") +
  theme_minimal()



### Apresentação de intervalos com 95% de confiança para a percentagem de indivíduos em cada categoria da variável nominal
for (nivel in levels(AEDPL$Posicao)) {
  print(nivel)
  print(binom.test(sum(AEDPL$Posicao == nivel), nrow(AEDPL)))
}



### Análise se alguma das duas variáveis quantitativas escolhidas pode ser caracterizada por uma distribuição normal
## Variável: Minutos_Jogados
# Teste de Shapiro-Wilk
print("Teste de Normalidade (Shapiro-Wilk) - Minutos Jogados:")
print(shapiro.test(AEDPL$Minutos_Jogados))

# QQ-Plot
qqPlot(AEDPL$Minutos_Jogados, main="QQ-Plot - Minutos Jogados", xlab="Quantis Teóricos", ylab="Quantis Observados")

## Variável: ExpectedGoals
# Teste de Shapiro-Wilk
print("Teste de Normalidade (Shapiro-Wilk) - Expected Goals:")
print(shapiro.test(AEDPL$ExpectedGoals))

# QQ-Plot
qqPlot(AEDPL$ExpectedGoals, main="QQ-Plot - Expected Goals", xlab="Quantis Teóricos", ylab="Quantis Observados")



### Comparação dos valores observados nas duas variáveis quantitativas nas diferentes categorias
### da variável ordinal, incluindo uma comparação da sua variabilidade em cada categoria, bem
### como do seu valor médio
# Para Minutos_Jogados
print("Minutos_Jogados por FaixaEtaria (Média):")
print(tapply(AEDPL$Minutos_Jogados, AEDPL$FaixaEtaria, mean))
print("Minutos_Jogados por FaixaEtaria (Desvio Padrão):")
print(tapply(AEDPL$Minutos_Jogados, AEDPL$FaixaEtaria, sd))

boxplot(AEDPL$Minutos_Jogados ~ AEDPL$FaixaEtaria, 
        main = "Minutos Jogados por Faixa Etaria",
        xlab = "Faixa Etaria", 
        ylab = "Minutos Jogados",
        col = "forestgreen")

# Para ExpectedGoals
print("ExpectedGoals por FaixaEtaria (Média):")
print(tapply(AEDPL$ExpectedGoals, AEDPL$FaixaEtaria, mean))
print("ExpectedGoals por FaixaEtaria (Desvio Padrão):")
print(tapply(AEDPL$ExpectedGoals, AEDPL$FaixaEtaria, sd))

boxplot(AEDPL$ExpectedGoals ~ AEDPL$FaixaEtaria, 
        main = "Expected Goals por Faixa Etaria",
        xlab = "Faixa Etaria", 
        ylab = "Expected Goals",
        col = "purple")



### Análise da independência/associação entre as duas variáveis qualitativas 
# Tabela de frequências
tab_indep <- table(AEDPL$Posicao, AEDPL$FaixaEtaria)
print(tab_indep)

# Valores esperados
print(chisq.test(tab_indep)$expected)

# Testes de hipóteses
print(chisq.test(tab_indep, correct = FALSE))
print(fisher.test(tab_indep, simulate.p.value = TRUE))

# Medidas de associação
print(ContCoef(tab_indep))
print(CramerV(tab_indep))



### Análise da correlação entre as duas variáveis quantitativas escolhidas
# Diagrama de dispersão
plot(AEDPL$Minutos_Jogados, AEDPL$ExpectedGoals, 
     main = "Dispersão: Minutos Jogados vs Expected Goals", 
     xlab = "Minutos Jogados", ylab = "Expected Goals", 
     col = "blue")

# Coeficientes de correlação e Testes de Hipóteses
# Pearson
print("Correlação de Pearson:")
print(cor(AEDPL$Minutos_Jogados, AEDPL$ExpectedGoals, method = "pearson"))
print(cor.test(AEDPL$Minutos_Jogados, AEDPL$ExpectedGoals, method = "pearson"))

# Spearman
print("Correlação de Spearman:")
print(cor(AEDPL$Minutos_Jogados, AEDPL$ExpectedGoals, method = "spearman"))
print(cor.test(AEDPL$Minutos_Jogados, AEDPL$ExpectedGoals, method = "spearman"))

# Kendall
print("Correlação de Kendall:")
print(cor(AEDPL$Minutos_Jogados, AEDPL$ExpectedGoals, method = "kendall"))
print(cor.test(AEDPL$Minutos_Jogados, AEDPL$ExpectedGoals, method = "kendall"))

# Visualização da matriz de correlação
M <- cor(AEDPL[, c("Minutos_Jogados", "ExpectedGoals")])
corrplot(M, type = "lower")
corrplot(M, type = "lower", addCoef.col = "black")



#######################################################
##### 2. Análise fatorial em componentes principais a todas as variáveis quantitativas da base de dados

### Análise da correlação entre as variáveis e do índice KMO (Kaiser-Meyer-Olkin)
AED_quant <- AEDPL[, 5:18]

# Análise da correlação
M_corr <- cor(AED_quant)
corrplot(M_corr, type = "lower")
corrplot(M_corr, type = "lower", addCoef.col = "black")


# Índice KMO
KMO(AED_quant)


### Fundamentação do número de componentes utilizadas, com referência à variância explicada
pca <- princomp(AED_quant, cor=TRUE)
summary(pca)
fviz_eig(pca, addlabels=TRUE)


### Apresentação de gráficos que ilustrem as conclusões, avaliando se as componentes principais
### permitem distinguir os indivíduos das diferentes categorias de cada uma das duas variáveis
### qualitativas utilizadas na primeira questão

# Para a variável Posicao
fviz_pca_biplot(pca, fill.ind = AEDPL$Posicao, pointshape = 21, pointsize = 2,
                addEllipses = TRUE, legend.title = "Posicao")

fviz_pca_biplot(pca, fill.ind = AEDPL$Posicao, pointshape = 21, pointsize = 2,
                addEllipses = TRUE, legend.title = "Posicao", label = "var")

# Para a variável FaixaEtaria
fviz_pca_biplot(pca, fill.ind = AEDPL$FaixaEtaria, pointshape = 21, pointsize = 2,
                addEllipses = TRUE, legend.title = "FaixaEtaria")

fviz_pca_biplot(pca, fill.ind = AEDPL$FaixaEtaria, pointshape = 21, pointsize = 2,
                addEllipses = TRUE, legend.title = "FaixaEtaria", label = "var")
                


#######################################################
##### 3. Análise de clusters a todas as variáveis quantitativas da base de dados

# dataset com variáveis quantitativas 
aedl_quant <- AEDPL[, sapply(AEDPL, is.numeric)]
summary(aedl_quant)

# Estandardizar variáveis
aedl_s <- as.data.frame(scale(aedl_quant))

# ------------------------------------------------------
# Tarefa: aplicação de um método hierárquico (apresentando o respetivo dendrograma) + justificação

# Analise comparativa de varios metodos de calculo de distancia
# e de varios metodos de ligação com base em dendogramas
analise_dendograma <- function(df, metodo_distancia, criterio_ligacao){
  d <- dist(df, method = metodo_distancia)
  hc <- hclust(d, method = criterio_ligacao)
  # media global da silhueta
  plot(hc, hang = -1,
       main = paste("Dendrograma:" , metodo_distancia, "/", criterio_ligacao),
       xlab = "", ylab = "Altura")
}

# euclidean
analise_dendograma(aedl_s,"euclidean", "single") # descartado 
analise_dendograma(aedl_s,"euclidean", "average") # descartado 
analise_dendograma(aedl_s, "euclidean", "complete") # descartado 
analise_dendograma(aedl_s, "euclidean", "ward.D2") # ok 

# manhattan
analise_dendograma(aedl_s,"manhattan", "single") # descartado 
analise_dendograma(aedl_s,"manhattan", "average") # descartado 
analise_dendograma(aedl_s, "manhattan", "complete") # descartado 
analise_dendograma(aedl_s, "manhattan", "ward.D2") # ok

# ward.D2 é o melhor a definir a distancia entre clusters 
# como o que foi escolhido foi ward entao a distancia entre individuos é a euclidean 

metodo_distancia <- "euclidean"
criterio_ligacao <- "ward.D2"
d <- dist(aedl_s, method = metodo_distancia)
hc <- hclust(d, method = criterio_ligacao)

# Identificação do número de clusters (k) com base na Silhueta e de Calinski-Harabasz

analise_k_clusters <- function(hc, df,d, k){
  clusters <- cutree(hc, k = k)
  ch <- calinhara(df, clusters)
  sil <- silhouette(clusters, d)
  sil_med <- mean(sil[, 3])
  print(paste("k =", k, "| Índice de Calinski-Harabasz =", round(ch,4), "|  Silhueta média =", round(sil_med,4)))
  fviz_silhouette(sil) + ggtitle(paste("Silhueta para k =", k))
}
# testar k = 2, 3, 4
analise_k_clusters(hc, aedl_s, d, 2) # silhueta média: 0.3925
analise_k_clusters(hc, aedl_s, d, 3) # 0.405
analise_k_clusters(hc, aedl_s, d, 4) # 0.2484


k <- 3
cluster3 <- cutree(hc, k = k)
fviz_cluster(list(data = aedl_s, cluster = cluster3), 
             geom = "point", 
             ellipse.type = "norm",
             main = paste("Clusters Hierárquicos - k =", k))



# k = 2 apresenta um índice de Calinski-Harabasz ligeiramente superior, mas a silhueta média é inferior à de k = 3
# k = 3 tem uma silhueta média superior e a a silhueta de cada cluster é melhor
# k = 4 apresenta valores inferiores para ambos os índices
# Assim, a escolha recai sobre k = 3 clusters, que apresenta uma boa separação entre os clusters formados
# e uma silhueta média aceitável, indicando que os pontos estão razoavelmente bem atribuídos aos seus clusters



# ------------------------------------------------------
# Tarefa: aplicação de um método não hierárquico (kmeans) + justificação

set.seed(1234)

# encontrar o numero de clusters
fviz_nbclust(aedl_s, kmeans, method = "wss") # o cotovelo está no k= 3 
fviz_nbclust(aedl_s, kmeans, method = "silhouette") # k = 3
# ambos os métodos sugerem k = 3 como o número ideal de clusters

km3 <- kmeans(aedl_s, centers = 3)
fviz_cluster(km3, aedl_s, geom = "point", 
             ellipse.type = "norm",
             main = paste("Clusters K-means com k =", k))


# a escolha de k = 3 clusters é justificada pelos metodos de wss e silhouette

#------------------------------------------------------
# Tarefa: comparação dos resultados obtidos com variáveis qualitativas (posição e faixa etaria)

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

#######################################################
##### 4. Aplicação do algoritmo de classificação Naïve Bayes
# Dividir em treino (80%) e teste(20%)
set.seed(123)
s <- sample(1:nrow(AEDPL))
Treino <- s[1:floor(0.80*nrow(AEDPL))]
AEDTreino <- AEDPL[Treino,]
AEDTeste <- AEDPL[-Treino,]

# Classificar variável 'Posicao':
# Estimar o modelo com a amostra de treino
NBCPos <- naiveBayes(AEDTreino[,5:18], AEDTreino$Posicao)

# Classificação na amostra de teste
NBCPos.prob <- predict(NBCPos, AEDTeste[,5:18], type="raw")
NBCPos.class <- predict(NBCPos, AEDTeste[,5:18])

# Matriz de confusão - avaliação de fiabilidade da classificação da amostra de testes
confusionMatrix(NBCPos.class, AEDTeste$Posicao, mode="prec_recall")

# Classificar variável 'FaixaEtaria':
# Estimar o modelo com a amostra de treino
NBCFE <- naiveBayes(AEDTreino[,5:18], AEDTreino$FaixaEtaria)

# Classificação na amostra de teste
NBCFE.prob <- predict(NBCFE, AEDTeste[,5:18], type="raw")
NBCFE.class <- predict(NBCFE, AEDTeste[,5:18])

# Matriz de confusão - avaliação de fiabilidade da classificação da amostra de testes
confusionMatrix(NBCFE.class, AEDTeste$FaixaEtaria, mode="prec_recall")