library(xgboost)
library(tidyverse)
library(janitor)
library(nlme)
library(scales)

ver <-"1.0.0"

#Sys.setlocale("LC_ALL", "Portuguese")
#options(encoding = "UTF-8")



# data(agaricus.train, package='xgboost')
# data(agaricus.test, package='xgboost')
# train <- agaricus.train
# test <- agaricus.test
# 
# bstSparse <- xgboost(x = train$data,
#                      y = train$label,
#                      max.depth = 2,
#                      learning_rate = 1,
#                      nthread = 2,
#                      nrounds = 2,
#                      objective = "reg:logistic")
# 
# bstDense <- xgboost(data = as.matrix(train$data), label = train$label, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")
# 
# dtrain <- xgb.DMatrix(data = train$data, label = train$label)
# bstDMatrix <- xgboost(data = dtrain, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")
# 
# X <- model.matrix( 
#   ~ marca + dep + unidade - 1,
#   data = ACUMULADO_MODELO


#_______

# Este arquivo abre a base que utilizamos mensalmente e arquivamos em excel (Sim eu sei...) trata os dados que estão em excel para facilitar manipular eles e salva os schemas para abrir mais rápido depois
# Normalemnte tem alguns tratamentos de excessões aqui, mas para esse case eu removi esses tratammentos para não revelar algum dado sensivel.


ACUMULADO <- readxl::read_excel("R:/REMUNERAÇAO E PLANEJAMENTO DE RH/DIVERSOS/Daniel/# - R/xgboost/previsão de rem. vendedor - amostra/Dados/Dados anonimos.xlsx",
                                sheet = "Base Calc.")
colnames(ACUMULADO) <- ACUMULADO[3,]
ACUMULADO <- ACUMULADO[-c(1:3),]

ACUMULADO <- clean_names(ACUMULADO)


ACUMULADO[, c(16:51)] <- lapply(
  ACUMULADO[, c(16:51)],
  as.numeric
)

ACUMULADO <- ACUMULADO %>%
  mutate(across(
    where(is.numeric),
    ~ replace_na(., 0)
  ))


ACUMULADO$mes_de_producao <- as.Date(as.numeric(ACUMULADO$mes_de_producao), origin = "1899-12-30")
ACUMULADO$mes_do_pagamento <- as.Date(as.numeric(ACUMULADO$mes_do_pagamento), origin = "1899-12-30")


ACUMULADO_EXIB <- ACUMULADO %>%
  mutate(
    across(
      where(is.numeric),
      ~ label_number(
        big.mark = ".",
        decimal.mark = ",",
        accuracy = 1
      )(.x)
    )
  )

ACUMULADO_MODELO <- ACUMULADO %>% select( -c(dep_unidade, dsr_2, c_de_custo,vendedor,semestre_referencia ,mes_de_producao, mes_do_pagamento, vol, margem, valor_fat, vol_2, margem_2, valor_fat_2, ret_financ, ret_financ_2, percent_meta, ajustes_de_negociacao, nota_de_qualidade_1, nota_de_qualidade_2, nota_de_qualidade_3, dsr, piso, cidade, funcao ) )

ACUMULADO_MODELO <- ACUMULADO_MODELO %>%
  mutate(across(
    where(is.numeric),
    ~ replace_na(., 0)
  ))

ACUMULADO_MODELO[, c(4:6)] <- lapply(
  ACUMULADO_MODELO[, c(4:6)],
  as.numeric
)



set.seed(123)
n <- nrow(ACUMULADO_MODELO)

train_size <- floor(0.70 * n)

train.indice <- sample(seq_len(n), size = train_size)

df.train <- ACUMULADO_MODELO[train.indice,]
df.test <- ACUMULADO_MODELO[-train.indice,]

library(dplyr)

dep_enc <- df.train %>%
  group_by(dep) %>%
  summarise(dep_enc = mean(remun_total), .groups = "drop")

df.train <- left_join(df.train, dep_enc, by = "dep")
df.test  <- left_join(df.test, dep_enc, by = "dep")

#

marca_freq <- table(df.train$marca)

df.train$marca_freq <- marca_freq[df.train$marca]
df.test$marca_freq  <- marca_freq[df.test$marca]

df.train$marca_freq[is.na(df.train$marca_freq)] <- 1
df.test$marca_freq[is.na(df.test$marca_freq)]   <- 1

X_unid_train <- model.matrix(~ unidade - 1, df.train)
X_unid_test  <- model.matrix(~ unidade - 1, df.test)

X_train <- cbind(
  volume        = df.train$vol_3,
  rentabilidade = df.train$rentab,
  faturamento   = df.train$fat_liq_total,
  dep_enc       = df.train$dep_enc,
  marca_freq    = df.train$marca_freq,
  X_unid_train
)

X_test <- cbind(
  volume        = df.test$vol_3,
  rentabilidade = df.test$rentab,
  faturamento   = df.test$fat_liq_total,
  dep_enc       = df.test$dep_enc,
  marca_freq    = df.test$marca_freq,
  X_unid_test
)

y_train <- df.train$remun_total
y_test  <- df.test$remun_total

library(xgboost)

model <- xgboost(
  x = as.matrix(X_train),
  y = y_train,
  objective = "reg:squarederror",
  nrounds = 400,
  max_depth = 4,
  learning_rate = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8,
  verbosity = 1
)

## Diagnóstico de ausência de colunas

ncol(X_train)
ncol(X_test)

setdiff(colnames(X_train), colnames(X_test))
setdiff(colnames(X_test), colnames(X_train))

missing_cols <- setdiff(colnames(X_train), colnames(X_test))
X_test <- as.data.frame(X_test)

for (col in missing_cols) {
  X_test[[col]] <- 0
}

# Reordenar exatamente como o treino
X_test <- X_test[, colnames(X_train)]

# Voltar para matrix (o xgboost exige)
X_test <- as.matrix(X_test)

##____ Predição

pred <- predict(model, as.matrix(X_test))


pred <- predict(model, X_test)

RMSE_XG <- sqrt(mean((y_test - pred)^2))
MAE_XG  <- mean(abs(y_test - pred))
R2_XG   <- 1 - sum((y_test - pred)^2) / sum((y_test - mean(y_test))^2)


RMSE_XG
MAE_XG
R2_XG

m5 <- lm(formula = remun_total ~ dep + marca + unidade + vol_3 + rentab + uf + gerente_de_marca , data = df.train)

pred_lm <- predict(m5, df.test)

RMSE_lm <- sqrt(mean((df.test$remun_total - pred_lm)^2))
MAE_lm  <- mean(abs(df.test$remun_total - pred_lm))
R2_lm   <- 1 - sum((df.test$remun_total - pred_lm)^2) / sum((df.test$remun_total - mean(df.test$remun_total))^2)

RMSE_lm
MAE_lm
R2_lm

# SCHEMA
fs::fs_path("R:/REMUNERAÇAO E PLANEJAMENTO DE RH/DIVERSOS/Daniel/# - R/xgboost/previsão de rem. vendedor - amostra")

DIRETORIO <- "R:/REMUNERAÇAO E PLANEJAMENTO DE RH/DIVERSOS/Daniel/# - R/xgboost/previsão de rem. vendedor - amostra"
short_path <- utils::shortPathName(
  DIRETORIO)

saveRDS(dep_enc, paste0(DIRETORIO, "/dep_enc.rds"))
saveRDS(marca_freq, paste0(DIRETORIO, "/marca_freq.rds"))
saveRDS(colnames(X_train), paste0(DIRETORIO,"/colnames_X.rds"))
saveRDS(levels(df.train$unidade), paste0(DIRETORIO,"/levels_unidade.rds"))

xgb.save(model, paste0(DIRETORIO, "/modelo_remuneracao.ubj"))

#schemas opcioanis
rentab_ref <- df.train %>%
  dplyr::group_by(dep, marca, unidade) %>%
  dplyr::summarise(
    rentab_media = mean(rentab, na.rm = TRUE),
    .groups = "drop"
  )

saveRDS(rentab_ref, paste0(DIRETORIO, "/rentab_ref.rds"))

ticket_ref <- df.train %>%
  dplyr::filter(vol_3 > 0) %>% 
  dplyr::group_by(dep, marca, unidade) %>%
  dplyr::summarise(
    ticket_medio = mean(fat_liq_total / vol_3, na.rm = T),
    .groups = "drop"
  )

rentab_media <- df.train %>%
  dplyr::filter(vol_3 > 0) %>% 
  dplyr::group_by(dep, marca, unidade) %>%
  dplyr::summarise(
    rentab_media = mean(rentab, na.rm = T),
    .groups = "drop"
  )

library(stringr)

dim_unidades <- ACUMULADO %>%
  distinct(unidade) %>%
  mutate(
    marca = case_when(
      TRUE ~ word(unidade, 1)
    )
  )


saveRDS(ticket_ref, paste0(DIRETORIO, "/ticket_ref.rds"))
saveRDS(ACUMULADO, paste0(DIRETORIO, "/data/data.rds"))
saveRDS(ACUMULADO_EXIB, paste0(DIRETORIO, "/data/data_dash.rds"))
saveRDS(rentab_media, paste0(DIRETORIO, "/rentab_media.rds"))
saveRDS(dim_unidades, paste0(DIRETORIO,"/data/dim_unidades.rds"))


