pacman::p_load(plyr,tidyverse,readxl,olsrr,ggpubr,kableExtra,ggthemes,knitr,dplyr)
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

###########################################################

#histograma do valor de venda:

# Defina a notação numérica padrão para não usar notação científica
options(scipen = 999)

# Crie o histograma com cores personalizadas
hist(dados$X1, 
     main = "Histograma dos Valores de Venda", 
     xlab = "Valor de Venda",
     col = "steelblue",  # Cor das barras
     border = "white",   # Cor das bordas das barras
     breaks = 20         # Número de bins
)
summary(dados$X1)
#histograma do Tamanho da casa:
hist(dados$X2, 
     main = "Histograma do Tamanho da casa", 
     xlab = "Tamanho da casa",
     col = "steelblue",  # Cor das barras
     border = "white",   # Cor das bordas das barras
     breaks = 20         # Número de bins
)
summary(dados$X2)
# histograma do Número de quartos
hist(dados$X3, 
     main = "Histograma do Número de quartos", 
     xlab = "Número de quartos",
     col = "steelblue",  # Cor das barras
     border = "white",   # Cor das bordas das barras
     breaks = 20         # Número de bins
)

#histograma do Número de banheiros
hist(dados$X4, 
     main = "Histograma dos Número de banheiros", 
     xlab = "Número de banheiros",
     col = "steelblue",  # Cor das barras
     border = "white",   # Cor das bordas das barras
     breaks = 20         # Número de bins
)

#histograma do tamanho da garagem

hist(dados$X6, 
     main = "Histograma do Tamanho da garagem", 
     xlab = "Número de carros que podem ser guardados na garagem",
     col = "steelblue",  # Cor das barras
     border = "white",   # Cor das bordas das barras
     breaks =         # Número de bins
)

#histograma do tamanho do terreno
hist(dados$X10,
     main = "Histograma do Tamanho do terreno", 
     xlab = "Tamanho do terreno",
     col = "steelblue",  # Cor das barras
     border = "white",   # Cor das bordas das barras
     breaks =         # Número de bins
)


# Supondo que o ano atual é 2023 e a coluna com o ano de nascimento é chamada "x8"
ano_atual <- 2023

dados <- dados %>%
  mutate(idade = ano_atual - dados$X8)

hist(dados$idade,
     main = "Histograma da idade da casa", 
     xlab = "Idade da casa ",
     col = "steelblue",  # Cor das barras
     border = "white",   # Cor das bordas das barras
     breaks =         # Número de bins
)



summary(dados$X1)
summary(dados$X2)
summary(dados$X2)
summary(dados$X4)
summary(dados$X6)
summary(dados$X8)
summary(dados$X10)



#DISPERSÃO:

#grafico de dispersao
library(ggplot2)

ggplot(dados,aes(dados$X1,dados$X2, color = I("steelblue"))) +
  geom_point() + 
  theme_classic()+
  stat_cor(method = "pearson")+
  labs(title = "Gráfico de Dispersão - Tamanho da casa",y = "Valor da casa", x="Tamanho da casa")

ggplot(dados,aes(dados$X1,dados$X3, color = I("steelblue"))) + 
  geom_point() + 
  theme_classic()+
  stat_cor(method = "pearson")+ 
  labs(title = "Gráfico de Dispersão - Número de quartos na casa", y = "Valor da casa", x="Número de quartos")

ggplot(dados,aes(dados$X1,dados$X4, color = I("steelblue"))) + 
  geom_point() + 
  theme_classic()+
  stat_cor(method = "pearson")+ 
  labs(title = "Gráfico de Dispersão - Número de banheiros na casa", y = "Valor da casa", x="Número de banheiros")




ggplot(dados,aes(dados$X1,dados$X6, color = I("steelblue"))) + 
  geom_point() + 
  theme_classic()+
  stat_cor(method = "pearson")+ 
  labs(title = "Gráfico de Dispersão - Tamanho da garagem", y = "Valor da casa", x="Tamanho da garagem")


ggplot(dados,aes(dados$X1,dados$X10, color = I("steelblue"))) + 
  geom_point() + 
  theme_classic()+
  stat_cor(method = "pearson")+ 
  labs(title = "Gráfico de Dispersão - Tamanho do terreno", y = "Valor da casa", x="Tamanho do terreno")



ggplot(dados,aes(X1,idade, color = I("steelblue"))) + 
  geom_point() + 
  theme_classic()+
  stat_cor(method = "pearson")+ 
  labs(title = "Gráfico de Dispersão - Idade da casa", y = "Valor da casa", x="Idade da casa")

#dispersao para categoricos

library(ggplot2)

# Supondo que você tenha os dados nas colunas x1 (valor da casa) e x2 (presença de ar condicionado) no dataframe "dados"
# Crie o gráfico de dispersão com cores para representar a presença ou ausência de ar condicionado
x1<- dados$X1
x5<-dados$X5
ggplot(dados, aes(x = x1, y = factor(x5), color = factor(x5))) +
  geom_point() +
  theme_classic() +
  labs(title = "Gráfico de Dispersão - Valor da Casa vs. Ar Condicionado",
       x = "Valor da Casa",
       y = "Presença de Ar Condicionado",
       color = "Ar Condicionado")+
  scale_color_discrete(labels = c("Não", "Sim"))



# BOX-PLOT

ggplot(dados, aes(x = factor(X5), y = x1, fill = factor(X5))) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Box Plot - Valor da Casa por Ar Condicionado",
       x = "Presença de Ar Condicionado",
       y = "Valor da Casa") +
  scale_fill_manual(values = c("0" = "blue", "1" = "steelblue"),
                    labels = c("Não", "Sim"))


ggplot(dados, aes(x = factor(X7), y = x1, fill = factor(X7))) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Box Plot - Valor da Casa com proximidade da “highway",
       x = "Presença de piscina",
       y = "Valor da Casa") +
  scale_fill_manual(values = c("0" = "blue", "1" = "steelblue"),
                    labels = c("Não", "Sim"))


ggplot(dados, aes(x = factor(X11), y = x1, fill = factor(X11))) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Box Plot - Valor da Casa com proximidade da “highway",
       x = "Proximidade da “highway",
       y = "Valor da Casa") +
  scale_fill_manual(values = c("0" = "blue", "1" = "steelblue"),
                    labels = c("Não", "Sim"))


ggplot(dados, aes(x = factor(X9), y = x1, fill = factor(X9))) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Box Plot - Valor da Casa por qualidade da construção",
       x = "Qualidade da construção",
       y = "Valor da Casa") +
  scale_fill_manual(values = c("1" = "blue", "2" = "steelblue", "3" ="lightblue"),
                    labels = c("Alta qualidade", "Média qualidade", "Baixa qualidade"))



###########################################################
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


modelo = lm(log(X1)~X2+X8+X9+X10,data = amostra)
summary(modelo)

shapiro.test(modelo$residuals)

modelo = lm(X1~X2+X6+X8+X9+X10,data = amostra)
summary(modelo)


tabela_frequencias <- as.data.frame(dados) %>%
  group_by(X6) %>%
  summarise(frequencia = n()) %>%
  mutate(percentual = frequencia / sum(frequencia) * 100)

##############################

summa


