# Modelo Preditivo de Remuneração Variável com XGBoost

Projeto desenvolvido para previsão de remuneração variável utilizando técnicas de Machine Learning com XGBoost em R.

O pipeline contempla tratamento de dados, engenharia de atributos, encoding categórico, treinamento supervisionado, avaliação de performance, persistência de artefatos e inferência desacoplada para predição futura.

---

## Objetivo

O objetivo do projeto é estimar a remuneração total de vendedores com base em variáveis operacionais, comerciais e estruturais da organização.

A solução foi construída visando:

- Padronização de previsões
- Redução de distorções analíticas
- Escalabilidade de inferência
- Reprodutibilidade do modelo
- Diagnóstico de performance

---

## Stack Utilizada

- R
- XGBoost
- Tidyverse
- ggplot2
- shapviz
- janitor
- readxl

---

## Estrutura do Projeto

```bash
.
├── data/
│   ├── data.rds
│   ├── data_dash.rds
│   └── dim_unidades.rds
│
├── modelo_remuneracao.ubj
├── dep_enc.rds
├── marca_freq.rds
├── ticket_ref.rds
├── rentab_media.rds
│
├── Fitting do modelo.R
├── modelo de diagnóstico do xgboosting.R
├── predict function - A.R
└── Modelo anonimizado_html.Rmd