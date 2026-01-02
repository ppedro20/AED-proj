#######################################################
# Análise Exploratória de Dados
# Mestrado em Ciência de Dados - 1º Ano (2025/2026)

# Identificação
# José Almeida  - 2021122872
# Hugo Lopes    - 
# Pedro Pedro   -

#######################################################
set.seed(123)

library(ggplot2)
library(car)
library(e1071)
library(caret)

load("AEDPL.RData")
#######################################################
##### Análise Estatistica 1

str(AEDPL)
names(AEDPL)
dim(AEDPL)
head(AEDPL)

# Preparação dos dados
AEDPL$Posicao <- factor(AEDPL$Posicao)

AEDPL$FaixaEtaria <- ordered(
  AEDPL$FaixaEtaria,
  levels = c("<=20","21-24","25-28","29-32",">33")
)

# Valores omissos
colSums(is.na(AEDPL))

### Variaveis escolhidas:
# Nominal: Posicao
# Ordinal: FaixaEtaria
# Quantitativas: Minutos_Jogados, ExpectedGoals

### Variavel qualitativa nominal: Posicao
table_pos <- table(AEDPL$Posicao)
table_pos
prop.table(table_pos)

# Intervalos de confiança (95%) para proporções
#prop.test(table_pos , conf.level = 0.95)

ggplot(AEDPL, aes(x = Posicao)) +
  geom_bar() +
  labs(
    title = "Distribuição dos Jogadores por Posição",
    x = "Posição",
    y = "Frequência"
  ) +
  theme_minimal()

### Variavel qualitativa ordinal: FaixaEtaria
table_fe <- table(AEDPL$FaixaEtaria)
table_fe
prop.table(table_fe)

ggplot(AEDPL, aes(x = FaixaEtaria)) +
  geom_bar() +
  labs(
    title = "Distribuição dos Jogadores por Faixa Etária",
    x = "Faixa Etária",
    y = "Frequência"
  ) +
  theme_minimal()

### Variavel Quantitativa: Minutos_Jogados
summary(AEDPL$Minutos_Jogados)
mean(AEDPL$Minutos_Jogados, na.rm = TRUE)
median(AEDPL$Minutos_Jogados, na.rm = TRUE)
sd(AEDPL$Minutos_Jogados, na.rm = TRUE)
IQR(AEDPL$Minutos_Jogados, na.rm = TRUE)

# Histograma
ggplot(AEDPL, aes(x = Minutos_Jogados)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Histograma dos Minutos Jogados",
    x = "Minutos Jogados",
    y = "Frequência"
  ) +
  theme_minimal()

# Boxplot
ggplot(AEDPL, aes(y = Minutos_Jogados)) +
  geom_boxplot() +
  labs(
    title = "Boxplot dos Minutos Jogados",
    y = "Minutos Jogados"
  ) +
  theme_minimal()

# Normalidade
shapiro.test(AEDPL$Minutos_Jogados)

# QQ-plot
qqPlot(AEDPL$Minutos_Jogados,
       main = "QQ-plot – Minutos Jogados")

### Variável quantitativa: ExpectedGoals
summary(AEDPL$ExpectedGoals)
mean(AEDPL$ExpectedGoals, na.rm = TRUE)
median(AEDPL$ExpectedGoals, na.rm = TRUE)
sd(AEDPL$ExpectedGoals, na.rm = TRUE)
IQR(AEDPL$ExpectedGoals, na.rm = TRUE)

ggplot(AEDPL, aes(x = ExpectedGoals)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Histograma do Expected Goals",
    x = "Expected Goals",
    y = "Frequência"
  ) +
  theme_minimal()

ggplot(AEDPL, aes(y = ExpectedGoals)) +
  geom_boxplot() +
  labs(
    title = "Boxplot do Expected Goals",
    y = "Expected Goals"
  ) +
  theme_minimal()

shapiro.test(AEDPL$ExpectedGoals)

qqPlot(AEDPL$ExpectedGoals,
       main = "QQ-plot – Expected Goals")

### Comparação das quantitativas por Faixa Etária
# Boxplots
ggplot(AEDPL, aes(x = FaixaEtaria, y = Minutos_Jogados)) +
  geom_boxplot() +
  labs(
    title = "Minutos Jogados por Faixa Etária",
    x = "Faixa Etária",
    y = "Minutos Jogados"
  ) +
  theme_minimal()

ggplot(AEDPL, aes(x = FaixaEtaria, y = ExpectedGoals)) +
  geom_boxplot() +
  labs(
    title = "Expected Goals por Faixa Etária",
    x = "Faixa Etária",
    y = "Expected Goals"
  ) +
  theme_minimal()

# Teste não paramétrico (k ≥ 2)
kruskal.test(Minutos_Jogados ~ FaixaEtaria, data = AEDPL)
kruskal.test(ExpectedGoals ~ FaixaEtaria, data = AEDPL)

### Associação entre variáveis qualitativas
tab_assoc <- table(AEDPL$Posicao, AEDPL$FaixaEtaria)
tab_assoc

chisq.test(tab_assoc)

### Correlação entre variáveis quantitativas
cor(AEDPL[, c("Minutos_Jogados", "ExpectedGoals")],
    method = "spearman",
    use = "complete.obs")

cor.test(
  AEDPL$Minutos_Jogados,
  AEDPL$ExpectedGoals,
  method = "spearman",
  conf.level = 0.95
)


### Classificação Naive Bayes
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