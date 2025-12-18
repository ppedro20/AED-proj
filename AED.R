#######################################################
# Análise Exploratória de Dados
# Mestrado em Ciência de Dados - 1º Ano (2025/2026)

# Identificação
# José Almeida  - 
# Hugo Lopes    - 
# Pedro Pedro   -

#######################################################
set.seed(123)

library(ggplot2)
library(car)

load("AEDPL.RData")
#######################################################
# Análise Estatistica 1

str(AEDPL)
names(AEDPL)
dim(AEDPL)
head(AEDPL)

