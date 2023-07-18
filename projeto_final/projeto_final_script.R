pacman::p_load(tidyverse,readxl,GGally)
dados <- read_excel("projeto_final/DADOS_TRABALHO_2023_1.xlsx")

#formatando o banco

dados = dados %>%
  mutate(ID = as.numeric(ID),
         X5 = as.factor(X5),
         X7 = as.factor(X7),
         X9 = as.factor(X9),
         X11 = as.factor(X11)) %>%
  select(-ID)



amostra = sample_n(dados, 300)





if (!require("remotes")) install.packages("remotes")
remotes::install_github("uham-bio/UHHformats", build_vignettes = TRUE)



if (!require("rmarkdown")) install.packages("rmarkdown")
if (!require("knitr")) install.packages("knitr")
if (!require("bookdown")) install.packages("bookdown")
if (!require("quarto")) install.packages("quarto")
#teste