library(xgboost)
ver_utilizada <- "1.0.0"
DIRETORIO <- "B:/R/previsão de rem. vendedor - amostra/previsão de rem. vendedor - amostra"
setwd(DIRETORIO)

library(xgboost)



predict_remuneracao_opc <- function(
    departamento,
    unidade,
    volume,
    rentabilidade = NULL,
    faturamento   = NULL
) {
  
  # ---- carregar artefatos ----
  model           <- xgb.load("modelo_remuneracao.ubj")
  dep_enc_tbl     <- readRDS("dep_enc.rds")
  marca_freq_tbl  <- readRDS("marca_freq.rds")
  cols_model      <- readRDS("colnames_X.rds")
  levels_unidade  <- readRDS("levels_unidade.rds")
  dados           <- readRDS("data/data.rds")
  exib            <- readRDS("data/data_dash.rds")
  rentab_media    <- readRDS("rentab_media.rds")
  
  # ---- validação mínima ----
  volume <- suppressWarnings(as.numeric(volume))
  if (length(volume) != 1 || is.na(volume) || volume <= 0) {
    stop("Volume deve ser numérico e maior que zero.")
  }
  
  # ---- base inicial ----
  df <- data.frame(
    dep           = departamento,
    unidade       = unidade,
    vol_3         = volume,
    stringsAsFactors = FALSE
  )
  
  # ---- rentabilidade ----
  if (is.null(rentabilidade) || length(rentabilidade) == 0 || is.na(rentabilidade)) {
    
    rentabilidade <- rentab_media |>
      dplyr::filter(
        dep == df$dep,
        unidade == df$unidade
      ) |>
      dplyr::pull(rentab_media)
    
    
  }
  
  # ---- faturamento (ticket médio * volume) ----
  if (is.null(faturamento) || length(faturamento) == 0 || is.na(faturamento)) {
    
    ticket_ref <- readRDS("ticket_ref.rds")
    
    ticket <- ticket_ref |>
      dplyr::filter(
        dep == df$dep,
        unidade == df$unidade
      ) |>
      dplyr::pull(ticket_medio)
    
    if (length(ticket) == 0 || is.na(ticket)) {
      ticket <- mean(ticket_ref$ticket_medio, na.rm = TRUE)
    }
    
    faturamento <- ticket * volume
  }
  
  
  
  df$rentab        <- as.numeric(rentabilidade)
  df$fat_liq_total <- as.numeric(faturamento)
  
  # ---- dep_enc ----
  df <- dplyr::left_join(df, dep_enc_tbl, by = "dep")
  
  if (is.na(df$dep_enc)) {
    df$dep_enc <- mean(dep_enc_tbl$dep_enc, na.rm = TRUE)
  }
  

  # ---- one-hot unidade ----
  unidade_cols <- paste0("unidade", levels_unidade)
  
  X_unid <- as.data.frame(
    matrix(0, nrow = 1, ncol = length(unidade_cols))
  )
  colnames(X_unid) <- unidade_cols
  
  col_unidade <- paste0("unidade", unidade)
  if (col_unidade %in% unidade_cols) {
    X_unid[[col_unidade]] <- 1
  }
  
  # ---- matriz final ----
  X <- cbind(
    volume        = df$vol_3,
    rentabilidade = df$rentab,
    faturamento   = df$fat_liq_total,
    dep_enc       = df$dep_enc,
    X_unid
  )
  
  X <- as.data.frame(X)
  
  # ---- alinhar colunas ----
  missing_cols <- setdiff(cols_model, colnames(X))
  for (col in missing_cols) X[[col]] <- 0
  X <- X[, cols_model]
  
  # ---- predição ----
  pred <- predict(model, as.matrix(X))
  
  return(list(
    remuneracao_prevista = as.numeric(pred),
    inputs_utilizados = list(
      departamento  = departamento,
      unidade        = unidade,
      volume         = volume,
      rentabilidade  = df$rentab,
      faturamento    = df$fat_liq_total
    )
  ))
}
