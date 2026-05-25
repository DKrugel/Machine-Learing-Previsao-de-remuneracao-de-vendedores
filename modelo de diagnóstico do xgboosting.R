pred_train <- predict(model, as.matrix(X_train))
pred_test  <- predict(model, as.matrix(X_test))

library(ggplot2)

ggplot(data.frame(real = y_test, pred = pred_test),
       aes(x = real, y = pred)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Predito vs Real (Teste)",
       x = "Remuneração Real",
       y = "Remuneração Predita")

residuos <- y_test - pred_test

ggplot(data.frame(pred = pred_test, res = residuos),
       aes(x = pred, y = res)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Resíduo vs Predição")

ggplot(data.frame(res = residuos), aes(x = res)) +
  geom_histogram(bins = 40)

dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
dtest  <- xgb.DMatrix(data = as.matrix(X_test),  label = y_test)

watchlist <- list(train = dtrain, test = dtest)

model <- xgb.train(
  params = list(
    objective = "reg:squarederror",
    max_depth = 4,
    eta = 0.05,
    subsample = 0.8,
    colsample_bytree = 0.8
  ),
  data = dtrain,
  nrounds = 2000,
  watchlist = watchlist,
  early_stopping_rounds = 50,
  print_every_n = 50
)

importance <- xgb.importance(model = model)
xgb.plot.importance(importance)

install.packages("shapviz")
library(shapviz)

sv <- shapviz(model, X_pred = as.matrix(X_train))
plot(sv)

par(mfrow = c(2,2))
sv_dependence(sv, "volume")
sv_dependence(sv, "rentabilidade")
sv_dependence(sv, "faturamento")

df_analise <- data.frame(
  unidade = df.test$unidade,
  erro = abs(y_test - pred_test)
)

df_analise %>%
  group_by(unidade) %>%
  summarise(erro_medio = mean(erro)) %>%
  arrange(desc(erro_medio)) %>% 
  barplot()

df_analise <- data.frame(
  real = y_test,
  erro = abs(y_test - pred_test)
)

df_analise$faixa <- cut(df_analise$real, 5)

df_analise %>%
  group_by(faixa) %>%
  summarise(erro_medio = mean(erro))

library(dplyr)
library(ggplot2)

df_analise <- data.frame(
  unidade = df.test$unidade,
  real    = as.numeric(y_test),
  pred    = as.numeric(pred_test)
) %>%
  mutate(
    erro_abs = abs(real - pred),
    erro_pct = ifelse(real == 0, NA, abs(real - pred) / real)
  )

erro_unidade <- df_analise %>%
  group_by(unidade) %>%
  summarise(
    erro_medio = mean(erro_abs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(erro_medio))

ggplot(erro_unidade,
       aes(x = reorder(unidade, erro_medio),
           y = erro_medio)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Erro Médio Absoluto por Unidade",
    x = "Unidade",
    y = "Erro Médio Absoluto"
  ) +
  theme_minimal()

top10_unidades <- erro_unidade %>%
  arrange(desc(erro_medio)) %>%
  slice_head(n = 10)


ggplot(top10_unidades,
       aes(x = reorder(unidade, erro_medio),
           y = erro_medio)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Unidades com Maior Erro Médio",
    x = "Unidade",
    y = "Erro Médio Absoluto"
  ) +
  theme_minimal()

df_analise$faixa <- cut(
  df_analise$real,
  breaks = quantile(df_analise$real,
                    probs = seq(0, 1, 0.2),
                    na.rm = TRUE),
  include.lowest = TRUE
)

erro_faixa <- df_analise %>%
  group_by(faixa) %>%
  summarise(
    erro_medio = mean(erro_abs, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(erro_faixa,
       aes(x = faixa, y = erro_medio)) +
  geom_col() +
  labs(
    title = "Erro Médio Absoluto por Faixa de Remuneração",
    x = "Faixa de Remuneração",
    y = "Erro Médio Absoluto"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

breaks <- quantile(
  df_analise$real,
  probs = seq(0, 1, 0.2),
  na.rm = TRUE
)

library(scales)

labels <- paste0(
  "R$ ",
  comma(round(head(breaks, -1), 0), big.mark = ".", decimal.mark = ","),
  " - ",
  "R$ ",
  comma(round(tail(breaks, -1), 0), big.mark = ".", decimal.mark = ",")
)

df_analise$faixa <- cut(
  df_analise$real,
  breaks = breaks,
  include.lowest = TRUE,
  labels = labels
)

erro_faixa <- df_analise %>%
  group_by(faixa) %>%
  summarise(
    erro_medio = mean(erro_abs, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(erro_faixa,
       aes(x = faixa, y = erro_medio)) +
  geom_col() +
  labs(
    title = "Erro Médio Absoluto por Faixa de Remuneração",
    x = "Faixa de Remuneração",
    y = "Erro Médio Absoluto"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
