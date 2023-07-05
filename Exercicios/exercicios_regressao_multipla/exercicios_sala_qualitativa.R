pacman::p_load(tidyverse,ggplot2,GGally)

#tarefa feito em sala regressao com variaveis qualitativas exercicios do slide (eu acho)
Y =  c(17,26,21,30,22,0,12,19,4,16,28,15,11,38,31,21,20,13,30,14)
X1 = c(151,92,175,31,104,277,210,120,290,238,164,272,295,68,85,224,166,305,124,246)
X2 = c("b","b","b","b","b","b","b","b","b","b","a","a","a","a","a","a","a","a","a","a")
X3 = c("a","a","a","a","a","b","b","b","b","b","c","c","c","c","c","d","d","d","d","d")

dados = data.frame(Y,X1,X2,X3)


#exploratoria
plot(X1,Y)

ggplot(data = dados, aes(x = X1, y = Y, color = X2)) +
  geom_point() +
  labs(x = "X1", y = "Y", color = "X2")


#boxplot

ggplot(data = dados, aes(x = X2, y = Y)) +
  geom_boxplot() +
  labs(x = "X2", y = "Y")

#corrplot e mais dados
ggpairs(dados)

#criando dummies
dados = dados %>%
  mutate(X2a = as.numeric(X2 == "a"))




#modelo

modelo = lm(Y ~ X1 + X2 ,dados)
summary(modelo)

anova(modelo)

#residuos

residuo = modelo$residuals

#valores ajustados

valores_ajustados = modelo$fitted.values


#banco para analise de residuos

dados = dados %>%
  cbind(residuo,valores_ajustados)




#grafico dos residuos


ggpairs(dados[, c(2,6)])


# Escrevendo o modelo em relacao a X3 que tem 4 categorias (vamos montar 3 dummies)

#exploratoria
plot(X1,Y)

ggplot(data = dados, aes(x = X1, y = Y, color = X3)) +
  geom_point() +
  labs(x = "X1", y = "Y", color = "X3")


#boxplot

ggplot(data = dados, aes(x = X3, y = Y)) +
  geom_boxplot() +
  labs(x = "X3", y = "Y")




#criando dummies
dados = dados %>%
  mutate(X3a = as.numeric(X3 == "a"),
         X3b = as.numeric(X3 == "b"),
         X3c = as.numeric(X3 == "c"))


#modelo


modelo = lm(Y ~ X1 + X3 ,dados) #por padrao usou A como referencia
summary(modelo)

anova(modelo)


modelo = lm(Y ~ X1 + X3a + X3b + X3c ,dados) #por padrao usou D como referencia
summary(modelo)

anova(modelo)

vcov(modelo) #matriz de covariancia dos parametros


