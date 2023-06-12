pacman::p_load(tidyverse,Matrix,readxl)

library(kableExtra)
#modelo linear multiplo - dados gordura
dados <- read_excel("dados/gordura.xlsx")

#soma de quadrados extra

modelo_completo = lm(Y ~ X1 + X2 + X3,data = dados)
modelo_1 = lm(Y ~ X1,data = dados)
modelo_2 = lm(Y ~ X1 + X2,data = dados)

summary(modelo_completo)
anova(modelo_completo)

#find sse
sse <- sum((fitted(modelo_completo) - dados$Y)^2)
sse

#find ssr
ssr <- sum((fitted(modelo_completo) - mean(dados$Y))^2)
ssr

#find sst
sst <- ssr + sse
sst

#r squared

r2 = ssr / sst
r2

#SSR(X2|X1)
ssr1 <- sum((fitted(modelo_1) - mean(dados$Y))^2)
ssr2 <- sum((fitted(modelo_2) - mean(dados$Y))^2)

ssrx1x2 = ssr2 - ssr1

#SSR(X3|X1,X2)

ssrx1x2x3 = ssr - ssr2


#tabela anova na mao
anova_table <- data.frame(Fonte_de_variacao = c("Regressao", "X1", "X2|X1","X3|X1,X2","Residuos", "Total"),
                          GL = c(3,1,1,1,n-4, n-1),
                          SS = c(ssr,ssr1 ,ssrx1x2, ssrx1x2x3 ,sse, sst),
                          MQ = c(round(ssr/3,2),round(ssr1,2) ,round(ssrx1x2,2),round(ssrx1x2x3,2), round(sse/(n-4),2), ''),
                          stringsAsFactors = FALSE)
rownames(anova_table) <- NULL

anova_table

#alterando a ordem de entrada das variaveis

modelo_A = lm(Y ~ X1 + X2 + X3,data = dados)
modelo_B = lm(Y ~ X2 + X1 + X3,data = dados)
modelo_C = lm(Y ~ X3 + X2 + X1,data = dados)
modelo_D = lm(Y ~ X2 + X3 + X1,data = dados)
modelo_E = lm(Y ~ X1 + X3 + X2,data = dados)
anova(modelo_A)
anova(modelo_B)
anova(modelo_C)
anova(modelo_D)
anova(modelo_E)
