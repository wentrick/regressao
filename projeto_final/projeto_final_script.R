pacman::p_load(tidyverse,readxl,GGally,olsrr)
dados <- read_excel("projeto_final/DADOS_TRABALHO_2023_1.xlsx")

set.seed(361)
#formatando o banco

dados = dados %>%
  mutate(ID = as.numeric(ID),
         X5 = as.factor(X5),
         X7 = as.factor(X7),
         X9 = as.factor(X9),
         X11 = as.factor(X11)) %>%
  select(-ID)



amostra = sample_n(dados, 300)

x = round(cor(amostra[,c(2,3,4,6,8,10)]),3)

ggpairs(amostra[,c(1:4)])

ggcorrplot::ggcorrplot(x,lab = TRUE)

modelo = lm(X1~X2+X3+X4+X5+X6+X7+X8+X9+X10+X11,data = amostra)
summary(modelo)

modelo = lm(X1~X2+X3+X5+X6+X7+X8+X9+X10+X11,data = amostra)
summary(modelo)

modelo = lm(X1~X3+X5+X6+X7+X8+X9+X10+X11,data = amostra)
summary(modelo)



model <- lm(X1~X2+X3+X5+X6+X7+X8+X9+X10+X11, data = amostra)
k <- ols_step_all_possible(model)
plot(k)


modelo = lm(X1~X2+X8+X9+X10,data = amostra)
summary(modelo)

modelo = lm(X1~X2+X6+X8+X9+X10,data = amostra)
summary(modelo)
