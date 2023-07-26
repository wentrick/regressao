pacman::p_load(plyr,tidyverse,readxl,olsrr,ggpubr,kableExtra,ggthemes,knitr,dplyr,MASS,leaps,car)
dados <- read_excel("projeto_final/DADOS_TRABALHO_2023_1.xlsx")

set.seed(361)
#formatando o banco

dados = dados %>%
  mutate(ID = as.numeric(ID),
         X5 = as.factor(X5),
         X7 = as.factor(X7),
         X9 = as.factor(X9),
         X11 = as.factor(X11),
         X8 = 2023 - dados$X8) %>%
  dplyr::select(-ID)



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

#ggpairs(amostra[,c(1:4)])

ggcorrplot::ggcorrplot(x,lab = TRUE)

modelo_completo = lm(X1~X2+X3+X4+X5+X6+X7+X8+X9+X10+X11,data = amostra)
summary(modelo_completo)

#Analise de residuo do modelo completo

resid = residuals(modelo_completo)
cook = cooks.distance(modelo_completo)
std_resid <- rstandard(modelo_completo)


layout(matrix(c(2,2,2,2), 2, 2, byrow = TRUE),
       widths=c(2,2), heights=c(2,2))

influencePlot(modelo_completo)
plot(cook, type="h",lwd=3,col="red", ylab="Cook's Distance")
abline(0,0,col="red")
qqPlot(resid, ylab="Residuals", main="")
hist(resid, xlab="Residuals", main="",nclass=30,col="orange")


par(mfrow=c(2,2))
plot(seq(1,length(modelo_completo$fitted.values)), std_resid, xlab="Predictor", ylab="Std. Residual", main="Predictor vs Residual (Linearity)")
abline(h=0, col="red", lty=2)
plot(modelo_completo$fitted.values, std_resid, xlab="Fitted", ylab="Std Resid", main="Fitted Vs Residual (Const Var & Indp)")
qqPlot(std_resid, main="QQ Plot Std. Residuals (Normality)")
hist(std_resid, main="Histogram of Std. Residuals (Normality")


######
model <- lm(X1~X2+X3+X5+X6+X7+X8+X9+X10+X11, data = amostra)
k <- ols_step_all_possible(model)
plot(k)


modelo_reduzido = lm(log(X1)~X2+X6+X8+X9+X10,data = amostra)
summary(modelo_reduzido)

shapiro.test(modelo$residuals)

model <- lm(log(X1)~X2+X6+X8+X9+X10+X2*X6*X8*X9*X10, data = amostra)
k <- ols_step_all_possible(model)
plot(k)

#multicolinearidade
model <- lm(log(X1)~X2+X3+X5+X6+X7+X8+X9+X10+X11, data = amostra)
ols_vif_tol(model)


################################
#Seleção de variaveis

step <- stepAIC(modelo_completo, direction = "both", trace = FALSE)
step

forward <- stepAIC(modelo_completo, direction = "forward", trace = FALSE)
forward 

backward <- stepAIC(modelo_completo, direction = "backward", trace = FALSE)
backward 

summary(step)
summary(forward)
summary(backward)

l<-list(data.frame(t(data.frame(coef(forward)))),
        data.frame(t(data.frame(coef(backward)))),
        data.frame(t(data.frame(coef(step)))))
stepwise_results <- do.call(rbind.fill, l)
row.names(stepwise_results) <- c('Forward','Backward','Both')
stepwise_results

#todas combinações

regfit_full = regsubsets(log(X1)~X2+X3+X4+X5+X6+X7+X8+X9+X10+X11, data = amostra)
summary(regfit_full)
reg_summary = summary(regfit_full)

reg_summary$outmat

# Set up a 2x2 grid so we can look at 4 plots at once
par(mfrow = c(2,2))
plot(reg_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

# We will now plot a red dot to indicate the model with the largest adjusted R^2 statistic.
# The which.max() function can be used to identify the location of the maximum point of a vector
adj_r2_max = which.max(reg_summary$adjr2) # 11

# The points() command works like the plot() command, except that it puts points 
# on a plot that has already been created instead of creating a new plot
points(adj_r2_max, reg_summary$adjr2[adj_r2_max], col ="red", cex = 2, pch = 20)

# We'll do the same for C_p and BIC, this time looking for the models with the SMALLEST statistic
plot(reg_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
cp_min = which.min(reg_summary$cp) # 10
points(cp_min, reg_summary$cp[cp_min], col = "red", cex = 2, pch = 20)

plot(reg_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
bic_min = which.min(reg_summary$bic) # 6
points(bic_min, reg_summary$bic[bic_min], col = "red", cex = 2, pch = 20)

plot(regfit_full, scale = "r2")

plot(regfit_full, scale = "adjr2")

plot(regfit_full, scale = "Cp")

plot(regfit_full, scale = "bic")
##############################
tabela_frequencias <- as.data.frame(dados) %>%
  group_by(X6) %>%
  summarise(frequencia = n()) %>%
  mutate(percentual = frequencia / sum(frequencia) * 100)


modelo_completo = lm(X1~X2+X3+X4+X5+X6+X7+X8+X9+X10+X11,data = amostra)
summary(modelo_completo)


resultado_boxcox <- boxcox(modelo_completo, plotit = TRUE)

lambda_otimo <- resultado_boxcox$x[which.max(resultado_boxcox$y)]

# Imprimir o valor de lambda ótimo


print(lambda_otimo)


dados_bc  = dados %>%
  mutate(X1 = (X1^lambda_otimo - 1) / lambda_otimo,
         X2 = (X2^lambda_otimo - 1) / lambda_otimo,
         X3 = (X3^lambda_otimo - 1) / lambda_otimo,
         X4 = (X4^lambda_otimo - 1) / lambda_otimo,
         X6 = (X6^lambda_otimo - 1) / lambda_otimo,
         X8 = (X8^lambda_otimo - 1) / lambda_otimo,
         X10 = (X10^lambda_otimo - 1) / lambda_otimo)

# Supondo que você tenha um dataframe chamado "meu_dataframe" e queira substituir valores -Inf na coluna "coluna_infinitos"
dados_bc$X6 <- replace(dados_bc$X6, is.infinite(dados_bc$X6), 0)
dados_bc$X3 <- replace(dados_bc$X3, is.infinite(dados_bc$X3), 0)
dados_bc$X4 <- replace(dados_bc$X4, is.infinite(dados_bc$X4), 0)

modelo_completo = lm(X1~X2+X3+X4+X5+X6+X7+X8+X9+X10+X11,data = dados_bc)
summary(modelo_completo)

hist(modelo_completo$residuals)
shapiro.test(modelo_completo$residuals)
plot(modelo_completo)


cook = cooks.distance(modelo_completo)
influential <- as.numeric(names(cook)[(cook > 4*mean(cook, na.rm=T))])  # influential row numbers
head(dados_bc[influential, ])
dados_bc <- dados_bc[-influential, ]


modelo_completo = lm(X1~X2+X3+X4+X5+X6+X7+X8+X9+X10+X11,data = dados_bc)
summary(modelo_completo)

hist(modelo_completo$residuals)
shapiro.test(modelo_completo$residuals)
plot(modelo_completo)


resid = residuals(modelo_completo)
cook = cooks.distance(modelo_completo)
std_resid <- rstandard(modelo_completo)


layout(matrix(c(2,2,2,2), 2, 2, byrow = TRUE),
       widths=c(2,2), heights=c(2,2))

influencePlot(modelo_completo)
plot(cook, type="h",lwd=3,col="red", ylab="Cook's Distance")
abline(0,0,col="red")
qqPlot(resid, ylab="Residuals", main="")
hist(resid, xlab="Residuals", main="",nclass=30,col="orange")


par(mfrow=c(2,2))
plot(seq(1,length(modelo_completo$fitted.values)), std_resid, xlab="Predictor", ylab="Std. Residual", main="Predictor vs Residual (Linearity)")
abline(h=0, col="red", lty=2)
plot(modelo_completo$fitted.values, std_resid, xlab="Fitted", ylab="Std Resid", main="Fitted Vs Residual (Const Var & Indp)")
qqPlot(std_resid, main="QQ Plot Std. Residuals (Normality)")
hist(std_resid, main="Histogram of Std. Residuals (Normality")


bptest(modelo_completo)
shapiro.test(modelo_completo$residuals)

