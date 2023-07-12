pacman::p_load(tidyverse,GGally,olsrr)


#montando data frame

Y = c(150, 86, 49, 288, 157, 131, 184, 109, 279, 235, 224)
X1 = c(0.6, 1.0, 1.4, 0.6, 1.0, 1.0, 1.0, 1.4, 0.6, 1.0, 1.4)
X2 = c(10, 10, 10, 20, 20, 20, 20, 20, 30, 30, 30)

dados = data.frame(Y,X1,X2)
summary(dados)

dados_formatado = dados %>%
  mutate(X1c = X1 - mean(X1), #centralizando X1 e X2
          X2c = X2 - mean(X2),
          X1_quad = X1c^2,
          X2_quad = X2c^2)


cor(dados)
ggpairs(dados)


round(cor(dados_formatado),5)
ggpairs(dados_formatado)


#modelo geral
modelo = lm(Y ~ X1c + X1_quad + X2c + X2_quad + X1c*X2c, data = dados_formatado)
summary(modelo)
anova(modelo)

residuo = modelo$residuals

fit_values = modelo$fitted.values



qqnorm(residuos)
qqline(residuos)


plot(fit_values,Y)
plot(residuo,fit_values)



#modelo reduzido

modelo_reduced = lm(Y ~ X1c + X2c, data = dados_formatado)
summary(modelo_reduced)
anova(modelo_reduced)

#teste entre os modelos (falta de ajustamento)

anova(modelo,modelo_reduced) 

#como nao obtemos um valor critico podemos assumir de que o modelo reduzido tem um ajustamento melhor








