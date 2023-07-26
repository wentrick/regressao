# packages
pacman::p_load(MASS,knitr,kableExtra,xtable,plyr,tidyverse,readxl,olsrr,ggpubr,kableExtra,ggthemes,dplyr,car,caret,caTools,lmtest)

dados <- read_excel("DADOS_TRABALHO_2023_1.xlsx")
set.seed(361)
#formatando o banco

dados = dados %>%
  mutate(ID = as.numeric(ID),
         X5 = as.factor(X5),
         X7 = as.factor(X7),
         X9 = as.factor(X9),
         X11 = as.factor(X11)) %>%
  dplyr::select(-ID)



#train = sample_n(dados, 300)

sample <- sample.split(dados$X1, SplitRatio = 0.575)
train  <- subset(dados, sample == TRUE)
test   <- subset(dados, sample == FALSE)

# Supondo que você tenha os dados nas colunas x1 (valor da casa) e x2 (presença de ar condicionado) no dataframe "dados"
# Crie o gráfico de dispersão com cores para representar a presença ou ausência de ar condicionado
x1<- dados$X1
x5<-dados$X5


# Crie o histograma com cores personalizadas
options(scipen = 999)

hist(dados$X1, 
     main = "Histograma dos Valores de Venda", 
     xlab = "Valor de Venda",
     col = "steelblue",  # Cor das barras
     border = "white",   # Cor das bordas das barras
     breaks = 20         # Número de bins
)

qqnorm(dados$X1)
qqline(dados$X1)

summary(dados$X1)

shapiro.test(dados$X1)

#histograma do Tamanho da casa:
hist(dados$X2, 
     main = "Histograma do Tamanho da casa", 
     xlab = "Tamanho da casa",
     col = "steelblue",  # Cor das barras
     border = "white",   # Cor das bordas das barras
     breaks = 20         # Número de bins
)

ggplot(dados,aes(dados$X1,dados$X2, color = I("steelblue"))) +
  geom_point() + 
  theme_classic()+
  stat_cor(method = "pearson")+
  labs(title = "Gráfico de Dispersão - Tamanho da casa",y = "Valor da casa", x="Tamanho da casa")

summary(dados$X2)

shapiro.test(dados$X2)

cores_azul <- c("#1f77b4", "#aec7e8", "#9edae5", "#3182bd", "#6baed6", "#bdd7e7", "#6baed6")

#box plot 
ggplot(dados, aes(x = factor(X3), y = X1, fill = factor(X3))) +
  geom_boxplot() +
  scale_fill_manual(values = cores_azul, name = "Número de quartos") +  # Personalizar o nome da legenda
  theme_classic() +
  labs(title = "Box Plot - Número de quartos na casa",
       y = "Valor da casa",
       x = "Número de quartos") +
  scale_x_discrete(limits = 1:7)+  # Limitar o eixo x até o valor 7
  theme(legend.position = "none")

tabela_frequencias <- as.data.frame(dados) %>%
  group_by(X3) %>%
  dplyr::summarise(frequencia = n()) %>%
  mutate(percentual = frequencia / sum(frequencia) * 100)
colnames(tabela_frequencias) = c("Número de quartos","Frequencia","Porcentual")

shapiro.test(dados$X3)

cores_azul <- c("#1f77b4", "#aec7e8", "#9edae5", "#3182bd", "#6baed6", "#bdd7e7", "#6baed6")

#box plot 
ggplot(dados, aes(x = factor(X4), y = X1, fill = factor(X4))) +
  geom_boxplot() +
  scale_fill_manual(values = cores_azul, name = "Número de banheiros") +  # Personalizar o nome da legenda
  theme_classic() +
  labs(title = "Box Plot - Número de banheiros na casa",
       y = "Valor da casa",
       x = "Número de banheiros") +
  scale_x_discrete(limits = 1:7)+  # Limitar o eixo x até o valor 7
  theme(legend.position = "none")


tabela_frequencias <- as.data.frame(dados) %>%
  group_by(X4) %>%
  dplyr::summarise(frequencia = n()) %>%
  mutate(percentual = frequencia / sum(frequencia) * 100)
colnames(tabela_frequencias) = c("Número de banheiros","Frequencia","Porcentual")

shapiro.test(dados$X4)

ggplot(dados, aes(x = factor(X5), y = x1, fill = factor(X5))) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Box Plot - Valor da Casa por Ar Condicionado",
       x = "Presença de Ar Condicionado",
       y = "Valor da Casa") +
  scale_fill_manual(values = c("0" = "blue", "1" = "steelblue"),
                    labels = c("Não", "Sim"))

tabela_frequencias <- as.data.frame(dados) %>%
  group_by(X5) %>%
  dplyr::summarise(frequencia = n()) %>%
  mutate(percentual = frequencia / sum(frequencia) * 100)
colnames(tabela_frequencias) = c("Ar Condicionado","Frequencia","Porcentual")

cores_azul <- c("#1f77b4", "#aec7e8", "#9edae5", "#3182bd", "#6baed6", "#bdd7e7", "#6baed6")

# Criar o box plot com cores em tons de azul e remover a legenda lateral
ggplot(dados, aes(x = factor(X6), y = X1, fill = factor(X6))) +
  geom_boxplot() +
  scale_fill_manual(values = cores_azul, name = "Número de banheiros") +
  theme_classic() +
  labs(title = "Box Plot - Número de carros que podem ser guardados na garagem",
       y = "Valor da casa",
       x = "Número de carros") +
  scale_x_discrete(limits = 1:7) +
  theme(legend.position = "none")  # Remover a legenda lateral das cores

tabela_frequencias <- as.data.frame(dados) %>%
  group_by(X6) %>%
  dplyr::summarise(frequencia = n()) %>%
  mutate(percentual = frequencia / sum(frequencia) * 100)
colnames(tabela_frequencias) = c("Número de Garagens","Frequencia","Porcentual")


shapiro.test(dados$X6)

ggplot(dados, aes(x = factor(X7), y = x1, fill = factor(X7))) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Box Plot - Valor da Casa com Presença de piscina",
       x = "Presença de piscina",
       y = "Valor da Casa") +
  scale_fill_manual(values = c("0" = "blue", "1" = "steelblue"),
                    labels = c("Não", "Sim"))


tabela_frequencias <- as.data.frame(dados) %>%
  group_by(X7) %>%
  dplyr::summarise(frequencia = n()) %>%
  mutate(percentual = frequencia / sum(frequencia) * 100)
colnames(tabela_frequencias) = c("Presença de piscina","Frequencia","Porcentual")

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

ggplot(dados,aes(dados$X1,dados$idade, color = I("steelblue"))) + 
  geom_point() + 
  theme_classic()+
  stat_cor(method = "pearson")+ 
  labs(title = "Gráfico de Dispersão - Idade da casa", y = "Valor da casa", x="Idade da casa")


shapiro.test(dados$idade)

ggplot(dados, aes(x = factor(X9), y = x1, fill = factor(X9))) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Box Plot - Valor da Casa por qualidade da construção",
       x = "Qualidade da construção",
       y = "Valor da Casa") +
  scale_fill_manual(values = c("1" = "blue", "2" = "steelblue", "3" ="lightblue"),
                    labels = c("Alta qualidade", "Média qualidade", "Baixa qualidade"))

tabela_frequencias <- as.data.frame(dados) %>%
  group_by(X9) %>%
  dplyr::summarise(frequencia = n()) %>%
  mutate(percentual = frequencia / sum(frequencia) * 100)
colnames(tabela_frequencias) = c("qualidade da construção","Frequencia","Porcentual")

#histograma do tamanho do terreno
hist(dados$X10,
     main = "Histograma do Tamanho do terreno", 
     xlab = "Tamanho do terreno",
     col = "steelblue",  # Cor das barras
     border = "white",   # Cor das bordas das barras
     breaks =         # Número de bins
)

ggplot(dados,aes(dados$X1,dados$X10, color = I("steelblue"))) + 
  geom_point() + 
  theme_classic()+
  stat_cor(method = "pearson")+ 
  labs(title = "Gráfico de Dispersão - Tamanho do terreno", y = "Valor da casa", x="Tamanho do terreno")

shapiro.test(dados$X10)

ggplot(dados, aes(x = factor(X11), y = x1, fill = factor(X11))) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Box Plot - Valor da Casa com proximidade da “highway",
       x = "Presença de piscina",
       y = "Valor da Casa") +
  scale_fill_manual(values = c("0" = "blue", "1" = "steelblue"),
                    labels = c("Não", "Sim"))

tabela_frequencias <- as.data.frame(dados) %>%
  group_by(X11) %>%
  dplyr::summarise(frequencia = n()) %>%
  mutate(percentual = frequencia / sum(frequencia) * 100)
colnames(tabela_frequencias) = c("proximidade da highway","Frequencia","Porcentual")


x = round(cor(dados[,c(2,3,4,6,8,10)]),3)

ggcorrplot::ggcorrplot(x,lab = TRUE)


modelo_completo = lm(X1~X2+X3+X4+X5+X6+X7+X8+X9+X10+X11,data = train)
summary(modelo_completo)

#Analise de residuo do modelo completo

resid = residuals(modelo_completo)
cook = cooks.distance(modelo_completo)
std_resid <- rstandard(modelo_completo)

layout(matrix(c(1,2,1,2,3,4,3,4), 2, 2, byrow = TRUE),
       widths=c(2,2), heights=c(1,2))

influencePlot(modelo_completo)
plot(cook, type="h",lwd=3,col="red", ylab="Cook's Distance")
abline(0,0,col="red")
qqPlot(resid,id = FALSE, ylab="Residuals", main="")
hist(resid, xlab="Residuals", main="",nclass=30,col="orange")

layout(matrix(c(1,2,1,2,3,4,3,4), 2, 2, byrow = TRUE),
       widths=c(2,2), heights=c(1,2))

plot(seq(1,length(modelo_completo$fitted.values)), std_resid, xlab="Predictor", ylab="Std. Residual", main="Predictor vs Residual")
abline(h=0, col="red", lty=2)
plot(modelo_completo$fitted.values, std_resid, xlab="Fitted", ylab="Std Resid", main="Fitted Vs Residual")
qqPlot(std_resid, id = FALSE ,main="QQ Plot Std. Residuals")
hist(std_resid, main="Histogram of Std. Residuals")

ols_vif_tol(modelo_completo)

shapiro.test(modelo_completo$residuals)
bptest(modelo_completo)

resultado_boxcox <- boxcox(modelo_completo, plotit = TRUE)

lambda_otimo <- resultado_boxcox$x[which.max(resultado_boxcox$y)]

cat("O lambda otimo obtido pelo metodo de Box-Cox é:",lambda_otimo)

train_bc  = train %>%
  mutate(X1 = (X1^lambda_otimo - 1) / lambda_otimo,
         X2 = (X2^lambda_otimo - 1) / lambda_otimo,
         X3 = (X3^lambda_otimo - 1) / lambda_otimo,
         X4 = (X4^lambda_otimo - 1) / lambda_otimo,
         X6 = (X6^lambda_otimo - 1) / lambda_otimo,
         X8 = (X8^lambda_otimo - 1) / lambda_otimo,
         X10 = (X10^lambda_otimo - 1) / lambda_otimo)

# Supondo que você tenha um dataframe chamado "meu_dataframe" e queira substituir valores -Inf na coluna "coluna_infinitos"
train_bc$X6 <- replace(train_bc$X6, is.infinite(train_bc$X6), 0)
train_bc$X3 <- replace(train_bc$X3, is.infinite(train_bc$X3), 0)
train_bc$X4 <- replace(train_bc$X4, is.infinite(train_bc$X4), 0)

test_bc  = test %>%
  mutate(X1 = (X1^lambda_otimo - 1) / lambda_otimo,
         X2 = (X2^lambda_otimo - 1) / lambda_otimo,
         X3 = (X3^lambda_otimo - 1) / lambda_otimo,
         X4 = (X4^lambda_otimo - 1) / lambda_otimo,
         X6 = (X6^lambda_otimo - 1) / lambda_otimo,
         X8 = (X8^lambda_otimo - 1) / lambda_otimo,
         X10 = (X10^lambda_otimo - 1) / lambda_otimo)

# Supondo que você tenha um dataframe chamado "meu_dataframe" e queira substituir valores -Inf na coluna "coluna_infinitos"
test_bc$X6 <- replace(test_bc$X6, is.infinite(test_bc$X6), 0)
test_bc$X3 <- replace(test_bc$X3, is.infinite(test_bc$X3), 0)
test_bc$X4 <- replace(test_bc$X4, is.infinite(test_bc$X4), 0)

modelo_completo = lm(X1~X2+X3+X4+X5+X6+X7+X8+X9+X10+X11,data = train_bc)
summary(modelo_completo)

#Analise de residuo do modelo completo

resid = residuals(modelo_completo)
cook = cooks.distance(modelo_completo)
std_resid <- rstandard(modelo_completo)

layout(matrix(c(1,2,1,2,3,4,3,4), 2, 2, byrow = TRUE),
       widths=c(2,2), heights=c(1,2))

influencePlot(modelo_completo)
plot(cook, type="h",lwd=3,col="red", ylab="Cook's Distance")
abline(0,0,col="red")
qqPlot(resid,id = FALSE, ylab="Residuals", main="")
hist(resid, xlab="Residuals", main="",nclass=30,col="orange")

layout(matrix(c(1,2,1,2,3,4,3,4), 2, 2, byrow = TRUE),
       widths=c(2,2), heights=c(1,2))

plot(seq(1,length(modelo_completo$fitted.values)), std_resid, xlab="Predictor", ylab="Std. Residual", main="Predictor vs Residual")
abline(h=0, col="red", lty=2)
plot(modelo_completo$fitted.values, std_resid, xlab="Fitted", ylab="Std Resid", main="Fitted Vs Residual")
qqPlot(std_resid, id = FALSE ,main="QQ Plot Std. Residuals")
hist(std_resid, main="Histogram of Std. Residuals")

shapiro.test(modelo_completo$residuals)
bptest(modelo_completo)

cook = cooks.distance(modelo_completo)
influential <- as.numeric(names(cook)[(cook > 4*mean(cook, na.rm=T))])  # influential row numbers

cat("Observações influentes",influential)

train_bc <- train_bc[-influential, ]

modelo_completo = lm(X1~X2+X3+X4+X5+X6+X7+X8+X9+X10+X11,data = train_bc)
summary(modelo_completo)

#Analise de residuo do modelo completo

resid = residuals(modelo_completo)
cook = cooks.distance(modelo_completo)
std_resid <- rstandard(modelo_completo)

layout(matrix(c(1,2,1,2,3,4,3,4), 2, 2, byrow = TRUE),
       widths=c(2,2), heights=c(1,2))

influencePlot(modelo_completo)
plot(cook, type="h",lwd=3,col="red", ylab="Cook's Distance")
abline(0,0,col="red")
qqPlot(resid,id = FALSE, ylab="Residuals", main="")
hist(resid, xlab="Residuals", main="",nclass=30,col="orange")

layout(matrix(c(1,2,1,2,3,4,3,4), 2, 2, byrow = TRUE),
       widths=c(2,2), heights=c(1,2))

plot(seq(1,length(modelo_completo$fitted.values)), std_resid, xlab="Predictor", ylab="Std. Residual", main="Predictor vs Residual")
abline(h=0, col="red", lty=2)
plot(modelo_completo$fitted.values, std_resid, xlab="Fitted", ylab="Std Resid", main="Fitted Vs Residual")
qqPlot(std_resid, id = FALSE ,main="QQ Plot Std. Residuals")
hist(std_resid, main="Histogram of Std. Residuals")

shapiro.test(modelo_completo$residuals)
bptest(modelo_completo)

#Seleção de variaveis

step <- stepAIC(modelo_completo, direction = "both", trace = FALSE)

forward <- stepAIC(modelo_completo, direction = "forward", trace = FALSE)

backward <- stepAIC(modelo_completo, direction = "backward", trace = FALSE)

l<-list(data.frame(t(data.frame(coef(forward)))),
        data.frame(t(data.frame(coef(backward)))),
        data.frame(t(data.frame(coef(step)))))
stepwise_results <- do.call(rbind.fill, l)
row.names(stepwise_results) <- c('Forward','Backward','Both')

stepwise_results = stepwise_results %>% 
  dplyr::mutate(X.Intercept. = round(X.Intercept.,2)) %>%
  dplyr::mutate(X2 = round(X2,3)) %>%
  dplyr::mutate(X3 = round(X3,3)) %>%
  dplyr::mutate(X4 = round(X4,3)) %>%
  dplyr::mutate(X51 = round(X51,3)) %>%
  dplyr::mutate(X6 = round(X6,3)) %>%
  dplyr::mutate(X71 = round(X71,3)) %>%
  dplyr::mutate(X8 = round(X8,3)) %>%
  dplyr::mutate(X92 = round(X92,3)) %>%
  dplyr::mutate(X93 = round(X93,3)) %>%
  dplyr::mutate(X10 = round(X10,3)) %>%
  dplyr::mutate(X111 = round(X111,3))

#todas combinações

regfit_full = leaps::regsubsets(X1~X2+X3+X4+X5+X6+X7+X8+X9+X10+X11, data = train_bc, nbest = 1)
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


modelo_reduzido = lm(X1~X2+X8+X9+X10, data = train_bc)
summary(modelo_reduzido)

#Analise de residuo do modelo completo

resid = residuals(modelo_completo)
cook = cooks.distance(modelo_completo)
std_resid <- rstandard(modelo_completo)

layout(matrix(c(1,2,1,2,3,4,3,4), 2, 2, byrow = TRUE),
       widths=c(2,2), heights=c(1,2))

influencePlot(modelo_completo)
plot(cook, type="h",lwd=3,col="red", ylab="Cook's Distance")
abline(0,0,col="red")
qqPlot(resid,id = FALSE, ylab="Residuals", main="")
hist(resid, xlab="Residuals", main="",nclass=30,col="orange")

layout(matrix(c(1,2,1,2,3,4,3,4), 2, 2, byrow = TRUE),
       widths=c(2,2), heights=c(1,2))

plot(seq(1,length(modelo_completo$fitted.values)), std_resid, xlab="Predictor", ylab="Std. Residual", main="Predictor vs Residual")
abline(h=0, col="red", lty=2)
plot(modelo_completo$fitted.values, std_resid, xlab="Fitted", ylab="Std Resid", main="Fitted Vs Residual")
qqPlot(std_resid, id = FALSE ,main="QQ Plot Std. Residuals")
hist(std_resid, main="Histogram of Std. Residuals")

shapiro.test(modelo_completo$residuals)
bptest(modelo_completo)

# Make predictions and compute the R2, RMSE and MAE
predictions <- modelo_completo %>% predict(test_bc)
data.frame( R2 = R2(predictions, test_bc$X1),
            REQM = RMSE(predictions, test_bc$X1),
            EMA = MAE(predictions, test_bc$X1))

cat("Taxa de erro da predição do modelo completo",RMSE(predictions, test_bc$X1)/mean(test_bc$X1))


# Make predictions and compute the R2, RMSE and MAE
predictions <- modelo_reduzido %>% predict(test_bc)
data.frame( R2 = R2(predictions, test_bc$X1),
            REQM = RMSE(predictions, test_bc$X1),
            EMA = MAE(predictions, test_bc$X1))

cat("Taxa de erro da predição do modelo reduzido",RMSE(predictions, test_bc$X1)/mean(test_bc$X1))


modelo_completo = lm(X1~X2+X3+X4+X5+X6+X7+X8+X9+X10+X11,data = train)



# Make predictions and compute the R2, RMSE and MAE
predictions <- modelo_completo %>% predict(test)
data.frame( R2 = R2(predictions, test$X1),
            REQM = RMSE(predictions, test$X1),
            EMA = MAE(predictions, test$X1))

cat("Taxa de erro da predição do modelo completo",RMSE(predictions, test_bc$X1)/mean(test_bc$X1))

