pacman::p_load(tidyverse,Matrix,readxl,forcats,psych,ggcorrplot,GGally)

dados_cirurgia <- read_excel("dados/Dados_exemplo9_2_comp.xlsx") %>% 
  rename(escore_coagulacao = X1,
         indice_prognostico = X2,
         escore_enzimatica = X3,
         escore_hepatica = X4,
         idade = X5,
         genero = X6,
         uso_alcool = X7,
         tempo_sobrevivencia = y) %>%
  mutate(genero = factor(as.factor(genero), levels = c(0,1),labels = c("masculino", "feminino")),
         uso_alcool = factor(as.factor(uso_alcool), levels = c(1,2,3),labels = c("nenhum", "moderado", "frequente")),
         uso_alcool_moderado = as.numeric(uso_alcool == "moderado"),
         uso_alcool_frequente = as.numeric(uso_alcool == "frequente")) %>%
  select(-ID)
  

#Analise descritiva

# Selecionar as colunas que contêm as variáveis a serem analisadas
variaveis <- dados_cirurgia$tempo_sobrevivencia

# Realizar a análise univariada para o conjunto de variáveis
resultados <- describe(variaveis)

# Visualizar os resultados
print(resultados)

hist(variaveis)
boxplot(variaveis)
qqnorm(variaveis)
qqline(variaveis)
shapiro.test(variaveis)


# Calcular a matriz de correlação
matriz_cor <- cor(dados_cirurgia %>% select(8,1,2,3,4,5))

# Converter a matriz de correlação em um data frame
df_cor <- as.data.frame(matriz_cor)

# Criar o gráfico de correlação (corr plot)
ggcorrplot(df_cor,lab = TRUE)

#matriz grafico de dispersao

ggpairs(dados_cirurgia %>% select(8,1,2,3,4,5),ggplot2::aes(colour=dados_cirurgia$genero))

#tabela de frequencia

tab_freq_genero = dados_cirurgia %>%
  group_by(genero) %>%
  summarise(frequencia = n()) %>%
  mutate(percentual = frequencia / sum(frequencia) * 100)

tabela_frequencias <- dados_cirurgia %>%
  group_by(uso_alcool) %>%
  summarise(frequencia = n()) %>%
  mutate(percentual = frequencia / sum(frequencia) * 100)

tabela_frequencias <- dados_cirurgia %>%
  count(genero) %>%
  mutate(percentual = n / sum(n) * 100)

tabela_frequencias <- dados_cirurgia %>%
  count(uso_alcool) %>%
  mutate(percentual = n / sum(n) * 100)

#ajustando o modelo completo para analisar os residuos

lm(data = dados_cirurgia, tempo_sobrevivencia ~ escore_coagulacao + indice_prognostico + escore_enzimatica + escore_hepatica + idade + genero + uso_alcool + uso_alcool_moderado + uso_alcool_frequente)


colnames(dados_cirurgia)





