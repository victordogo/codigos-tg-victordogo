# Lendo banco de dados

df <- readr::read_rds('data/pokebattle.rds') |>
   dplyr::select(-c(2:7))

# Definindo funcao para extracao de metricas

metricas_extract <- function(conf, roc){
  acc <- conf$overall[1] # Acuracia

  risk <- 1-acc # Risco

  other <- conf$byClass[1:4] # Sensibilidade, Especificidade, VPP e VPN

  auc <- roc$auc[1] # Area sob Curva ROC

  result <- c(acc, risk, other, auc)

  names(result) <- c('Acurácia', 'Risco', 'Sensibilidade', 'Especificidade',
                     'VPP', 'VPN', 'AUC')
  return(result)
}

### Dividindo dados em treino, teste e validação

## Treino e teste

set.seed(1985)

split_tre <- rsample::initial_split(df, prop=0.7)

tre <- rsample::training(split_tre)
tes <- rsample::testing(split_tre)

x_tre <- tre[,-1] |>
  as.matrix.data.frame() |>
  as.double() |>
  matrix(ncol=12)

y_tre <- tre[,1] |>
  as.matrix() |>
  as.double() |>
  matrix(ncol=1)

x_tes <- tes[,-1] |>
  as.matrix.data.frame() |>
  as.double() |>
  matrix(ncol=12)

y_tes <- tre[,1] |>
  as.matrix.data.frame() |>
  as.double() |>
  matrix(ncol=1)

# Verificando balanceamento no treino e no teste

tibble::tibble(
  Conjunto=c('Treino', 'Teste'),
  `win=1`= c(mean(tre$win==1), mean(tes$win==1)),
  `win=0`= c(mean(tre$win==0), mean(tes$win==0))
) |> knitr::kable('latex', digits=4)

## Validação

split_val <- rsample::initial_split(tre, prop=0.6)

tre_val <- rsample::training(split_val)
val <- rsample::testing(split_val)

x_tre_val <- tre_val[,-1] |>
  as.matrix.data.frame() |>
  as.double() |>
  matrix(ncol=12)

y_tre_val <- tre_val[,1] |>
  as.matrix() |>
  as.double() |>
  matrix(ncol=1)

x_val <- val[,-1] |>
  as.matrix.data.frame() |>
  as.double() |>
  matrix(ncol=12)

y_val <- val[,1] |>
  as.matrix.data.frame() |>
  as.double() |>
  matrix(ncol=1)

# Verificando balanceamento no treino e validacao

tibble::tibble(
  Conjunto=c('Treino', 'Validação'),
  `win=1`= c(mean(tre_val$win==1), mean(val$win==1)),
  `win=0`= c(mean(tre_val$win==0), mean(val$win==0))
) |> knitr::kable('latex', digits=4)

###### COMPARACOES ENTRE MODELOS COMPLETOS ######

### AJUSTE BI GAUSSIANO

ajs_gaus <- naivebayes::naive_bayes(win~., data=tre, usekernel=FALSE)

### AJUSTE REG. LOG.

ajs_log <- glm(win~., data=tre, family = binomial)

### AJUSTE BI FLEXIVEL

## Ajustando para cada kernel

ajs_flex_gaus <- naivebayes::naive_bayes(win~., data=tre_val,
                                         usekernel=TRUE,
                                         kernel='gaussian')

ajs_flex_uni <- naivebayes::naive_bayes(win~., data=tre_val,
                                        usekernel=TRUE,
                                        kernel='rectangular')

ajs_flex_epa <- naivebayes::naive_bayes(win~., data=tre_val,
                                        usekernel=TRUE,
                                        kernel='epanechnikov')

## Obtendo matriz de confusao para cada kernel

conf_flex_gaus <- caret::confusionMatrix(
  data=predict(ajs_flex_gaus, newdata = val[,-1]),
  reference=val$win
)

conf_flex_uni <- caret::confusionMatrix(
  data=predict(ajs_flex_uni, newdata = val[,-1]),
  reference=val$win
)

conf_flex_epa <- caret::confusionMatrix(
  data=predict(ajs_flex_epa, newdata = val[,-1]),
  reference=val$win
)

## Plotando curva ROC dos tres ajustes flexiveis

roc_flex_gaus <- pROC::roc(response = val$win,
                           predictor = predict(ajs_flex_gaus, newdata = val[,-1],
                                               type='prob')[,1])

roc_flex_uni <- pROC::roc(response = val$win,
                           predictor = predict(ajs_flex_uni, newdata = val[,-1],
                                               type='prob')[,1])

roc_flex_epa <- pROC::roc(response = val$win,
                          predictor = predict(ajs_flex_epa, newdata = val[,-1],
                                              type='prob')[,1])

pROC::ggroc(
  list(Parabolico=roc_flex_epa, Uniforme=roc_flex_uni,
       Gaussiano=roc_flex_gaus),
  legacy.axes=TRUE, size=1
)+
  ggplot2::labs(color='Kernel', x='1-Especificidade', y='Sensibilidade',
                title='Curvas ROC para ajustes via Bayes Ingênuo Flexível',
                subtitle = 'considerando Kernels Gaussiano, Uniforme e Parabólico (com diff_spd)')+
  ggplot2::theme_minimal()+
  ggplot2::theme(legend.position = 'top')

ggplot2::ggsave('img/6-roc-completo-flex.png')

## Apresentando metricas

tibble::tibble(
  Metrica=names(metricas_extract(conf_flex_epa, roc_flex_epa)),
  Gaussiano=metricas_extract(conf_flex_gaus, roc_flex_gaus),
  Uniforme=metricas_extract(conf_flex_uni, roc_flex_uni),
  Parabolico=metricas_extract(conf_flex_epa, roc_flex_epa)
) |>
  tidyr::pivot_longer(!Metrica) |>
  tidyr::pivot_wider(names_from=Metrica) |>
  dplyr::rename(Kernel=name) |>
  knitr::kable('latex', digits=4)

## Escolhido BI Flexível com kernel gaussiano

ajs_flex <- naivebayes::naive_bayes(win~., data=tre,
                                    usekernel=TRUE,
                                    kernel='gaussian')

### COMPARANDO MODELOS COMPLETOS

## Matrizes de Confusao

conf_gaus <- caret::confusionMatrix(
  data = predict(ajs_gaus, newdata = tes[,-1]),
  reference = tes$win
)

conf_flex <- caret::confusionMatrix(
  data = predict(ajs_flex, newdata = tes[,-1]),
  reference = tes$win
)

pred_log <- predict(ajs_log, newdata=tes[,-1], type='response')
pred_log <- as.factor(ifelse(pred_log>=0.5, 1, 0))
conf_log <- caret::confusionMatrix(
  data=pred_log,
  reference=tes$win
)

## Curvas ROC

roc_gaus <- pROC::roc(response = tes$win,
                      predictor = predict(ajs_gaus, newdata = tes[,-1],
                                          type='prob')[,1])

roc_flex <- pROC::roc(response = tes$win,
                      predictor = predict(ajs_flex, newdata = tes[,-1],
                                          type='prob')[,1])

roc_log <- pROC::roc(response = tes$win,
                     predictor = predict(ajs_log, newdata = tes[,-1],
                                         type='response'))

pROC::ggroc(
  list(`BI Gaussiano`=roc_gaus, `BI Flexível`=roc_flex,
       `Reg. Logistica`=roc_log),
  legacy.axes=TRUE, size=1
)+
  ggplot2::labs(color='Ajuste', x='1-Especificidade', y='Sensibilidade',
                title='Curvas ROC para Modelos Completos',
                subtitle = 'considerando BI Gaussiano, BI Flexível e Reg. Logística (com diff_spd)')+
  ggplot2::theme_minimal()+
  ggplot2::theme(legend.position = 'top')

ggplot2::ggsave('img/7-roc-completo-all.png')

## Apresentando metricas

tibble::tibble(
  Metrica=names(metricas_extract(conf_flex, roc_flex)),
  `BI Gaussiano` = metricas_extract(conf_gaus, roc_gaus),
  `BI Flexível`=metricas_extract(conf_flex, roc_flex),
  `Regressão Logística`=metricas_extract(conf_log, roc_log)
) |>
  tidyr::pivot_longer(!Metrica) |>
  tidyr::pivot_wider(names_from=Metrica) |>
  dplyr::rename(Ajuste=name) |>
  knitr::kable('latex', digits=4)

###### AJUSTES COM A VARIAVEL DIFF_SPD ######

#### ESCOLHENDO VARIAVEIS PARA NB

var_lasso <- glmnet::cv.glmnet(x=x_tre,y=y_tre, alpha=1)
var_lasso <- glmnet::glmnet(x=x_tre,y=y_tre, alpha=1,
                            lambda = var_lasso$lambda.1se) |>
  coef()
rownames(var_lasso) <- names(tre)

var_lasso |> round(5) ## diff_hp, diff_atk, diff_sdef, diff_spd, res_tipo1, res_tipo2

var_arv <- rpart::rpart(win~., data=tre) # diff_spd
rpart.plot::rpart.plot(var_arv)

#### VALIDACAO NB GAUSSIANO

## Ajustes

ajs_gaus_full <- naivebayes::naive_bayes(win~., data=tre_val,
                                         usekernel=FALSE)

ajs_gaus_arv <- naivebayes::naive_bayes(win~., data=tre_val[,c(1,7)],
                                        usekernel=FALSE)

ajs_gaus_lasso <- naivebayes::naive_bayes(win~., data=tre_val[,c(1,2,3,6,7,8,9)],
                                          usekernel=FALSE)

## Matrizes de Confusão

conf_full_gaus <- caret::confusionMatrix(
  data=predict(ajs_gaus_full, val),
  reference=val$win
)

conf_arv_gauss <- caret::confusionMatrix(
  data=predict(ajs_gaus_arv, val),
  reference=val$win
)

conf_lasso_gauss <- caret::confusionMatrix(
  data=predict(ajs_gaus_lasso, val),
  reference=val$win
)

## Curvas ROC

roc_full_gaus <- pROC::roc(response = val$win,
                           predictor = predict(ajs_gaus_full, newdata = val[,-1],
                                               type='prob')[,1])

roc_arv_gaus <- pROC::roc(response = val$win,
                          predictor = predict(ajs_gaus_arv, newdata = val[,-1],
                                              type='prob')[,1])

roc_lasso_gaus <- pROC::roc(response = val$win,
                            predictor = predict(ajs_gaus_lasso, newdata = val[,-1],
                                                type='prob')[,1])

pROC::ggroc(
  list(`Completo`=roc_full_gaus, `Arvore`=roc_arv_gaus,
       `Lasso`=roc_lasso_gaus),
  legacy.axes=TRUE, size=1
)+
  ggplot2::labs(color='Seleção', x='1-Especificidade', y='Sensibilidade',
                title='Curvas ROC para Bayes Ingênuo Gaussiano',
                subtitle = 'com modelo completo e seleção de variáveis via Árvore e Lasso (com diff_spd)')+
  ggplot2::theme_minimal()+
  ggplot2::theme(legend.position = 'top')

ggplot2::ggsave('img/9-roc-gaus-diff-spd.png')

## Apresentando Métricas

tibble::tibble(
  Metrica=names(metricas_extract(conf_arv_gauss, roc_arv_gaus)),
  `Completo` = metricas_extract(conf_full_gaus, roc_full_gaus),
  `Arvore`=metricas_extract(conf_arv_gauss, roc_arv_gaus),
  `Lasso`=metricas_extract(conf_lasso_gauss, roc_lasso_gaus)
) |>
  tidyr::pivot_longer(!Metrica) |>
  tidyr::pivot_wider(names_from=Metrica) |>
  dplyr::rename(Ajuste=name) |>
  knitr::kable('latex', digits=4)

#### VALIDACAO NB FLEXIVEL

## Ajustes

# Arvore
ajs_flex_epa_arv <- naivebayes::naive_bayes(win~., data=tre_val[,c(1,7)],
                                             usekernel=TRUE,
                                             kernel='epanechnikov')

ajs_flex_uni_arv <- naivebayes::naive_bayes(win~., data=tre_val[,c(1,7)],
                                             usekernel=TRUE,
                                             kernel='rectangular')

ajs_flex_gaus_arv <- naivebayes::naive_bayes(win~., data=tre_val[,c(1,7)],
                                              usekernel=TRUE,
                                              kernel='gaussian')

# Lasso
ajs_flex_epa_las <- naivebayes::naive_bayes(win~., data=tre_val[,c(1,2,3,6,7,8,9)],
                                            usekernel=TRUE,
                                            kernel='epanechnikov')

ajs_flex_uni_las <- naivebayes::naive_bayes(win~., data=tre_val[,c(1,2,3,6,7,8,9)],
                                            usekernel=TRUE,
                                            kernel='rectangular')

ajs_flex_gaus_las <- naivebayes::naive_bayes(win~., data=tre_val[,c(1,2,3,6,7,8,9)],
                                             usekernel=TRUE,
                                             kernel='gaussian')

## Matrizes de Confusão

# Arvore
conf_arv_flex_epa <- caret::confusionMatrix(
  data=predict(ajs_flex_epa_arv, val),
  reference=val$win
)

conf_arv_flex_uni <- caret::confusionMatrix(
  data=predict(ajs_flex_uni_arv, val),
  reference=val$win
)

conf_arv_flex_gaus <- caret::confusionMatrix(
  data=predict(ajs_flex_gaus_arv, val),
  reference=val$win
)

# Lasso
conf_las_flex_epa <- caret::confusionMatrix(
  data=predict(ajs_flex_epa_las, val),
  reference=val$win
)

conf_las_flex_uni <- caret::confusionMatrix(
  data=predict(ajs_flex_uni_las, val),
  reference=val$win
)

conf_las_flex_gaus <- caret::confusionMatrix(
  data=predict(ajs_flex_gaus_las, val),
  reference=val$win
)

## Curvas ROC

#Arvore
roc_arv_flex_epa <- pROC::roc(response = val$win,
                              predictor = predict(ajs_flex_epa_arv, newdata = val[,-1],
                                                  type='prob')[,1])

roc_arv_flex_uni <- pROC::roc(response = val$win,
                              predictor = predict(ajs_flex_uni_arv, newdata = val[,-1],
                                                  type='prob')[,1])

roc_arv_flex_gaus <- pROC::roc(response = val$win,
                              predictor = predict(ajs_flex_gaus_arv, newdata = val[,-1],
                                                  type='prob')[,1])

pROC::ggroc(
  list(`Parabólico`=roc_arv_flex_epa, `Uniforme`=roc_arv_flex_uni,
       `Gaussiano`=roc_arv_flex_gaus),
  legacy.axes=TRUE, size=1
)+
  ggplot2::labs(color='Kernel', x='1-Especificidade', y='Sensibilidade',
                title='Curvas ROC para Bayes Ingênuo Flexível',
                subtitle = 'considerando seleção de variáveis via Árvore (com diff_spd)')+
  ggplot2::theme_minimal()+
  ggplot2::theme(legend.position = 'top')

ggplot2::ggsave('img/10-roc-flex-arv-diff-spd.png')

# Lasso
roc_las_flex_epa <- pROC::roc(response = val$win,
                              predictor = predict(ajs_flex_epa_las, newdata = val[,-1],
                                                  type='prob')[,1])

roc_las_flex_uni <- pROC::roc(response = val$win,
                              predictor = predict(ajs_flex_uni_las, newdata = val[,-1],
                                                  type='prob')[,1])

roc_las_flex_gaus <- pROC::roc(response = val$win,
                               predictor = predict(ajs_flex_gaus_las, newdata = val[,-1],
                                                   type='prob')[,1])

pROC::ggroc(
  list(`Parabólico`=roc_las_flex_epa, `Uniforme`=roc_las_flex_uni,
       `Gaussiano`=roc_las_flex_gaus),
  legacy.axes=TRUE, size=1
)+
  ggplot2::labs(color='Kernel', x='1-Especificidade', y='Sensibilidade',
                title='Curvas ROC para Bayes Ingênuo Flexível',
                subtitle = 'considerando seleção de variáveis via Lasso (com diff_spd)')+
  ggplot2::theme_minimal()+
  ggplot2::theme(legend.position = 'top')

ggplot2::ggsave('img/11-roc-flex-lasso-diff-spd.png')

## Apresentando Metricas

# Arvore
tibble::tibble(
  Metrica=names(metricas_extract(conf_arv_flex_epa, roc_arv_flex_epa)),
  `Parabolico` = metricas_extract(conf_arv_flex_epa, roc_arv_flex_epa),
  `Uniforme`=metricas_extract(conf_arv_flex_uni, roc_arv_flex_uni),
  `Gaussiano`=metricas_extract(conf_arv_flex_gaus, roc_arv_flex_gaus)
) |>
  tidyr::pivot_longer(!Metrica) |>
  tidyr::pivot_wider(names_from=Metrica) |>
  dplyr::rename(Ajuste=name) |>
  knitr::kable('latex', digits=4)

# Lasso
tibble::tibble(
  Metrica=names(metricas_extract(conf_las_flex_epa, roc_las_flex_epa)),
  `Parabolico` = metricas_extract(conf_las_flex_epa, roc_las_flex_epa),
  `Uniforme`=metricas_extract(conf_las_flex_uni, roc_las_flex_uni),
  `Gaussiano`=metricas_extract(conf_las_flex_gaus, roc_las_flex_gaus)
) |>
  tidyr::pivot_longer(!Metrica) |>
  tidyr::pivot_wider(names_from=Metrica) |>
  dplyr::rename(Ajuste=name) |>
  knitr::kable('latex', digits=4)

## Curva ROC melhores árvore e lasso

pROC::ggroc(
  list(`Arvore`=roc_arv_flex_gaus, `Lasso`=roc_las_flex_gaus,
       `Completo`=roc_flex_gaus),
  legacy.axes=TRUE, size=1
)+
  ggplot2::labs(color='Seleção', x='1-Especificidade', y='Sensibilidade',
                title='Curvas ROC para Bayes Ingênuo Flexível',
                subtitle = 'considerando modelo completo e seleção de variáveis via Árvore e Lasso (com diff_spd)')+
  ggplot2::theme_minimal()+
  ggplot2::theme(legend.position = 'top')

ggplot2::ggsave('img/12-roc-flex-all-diff-spd.png')

## Metricas melhores arvore e lasso

tibble::tibble(
  Metrica=names(metricas_extract(conf_las_flex_gaus, roc_las_flex_gaus)),
  `Completo` = metricas_extract(conf_flex_gaus, roc_flex_gaus),
  `Arvore`=metricas_extract(conf_arv_flex_gaus, roc_arv_flex_gaus),
  `Lasso`=metricas_extract(conf_las_flex_gaus, roc_las_flex_gaus)
) |>
  tidyr::pivot_longer(!Metrica) |>
  tidyr::pivot_wider(names_from=Metrica) |>
  dplyr::rename(Ajuste=name) |>
  knitr::kable('latex', digits=4)

#### VALIDACAO REG. LOGISTICA

## Ajustes

# Completo
ajs_log_full <- glm(win~., data=tre_val, family = binomial)

# Lasso

ajs_log_lasso <- glmnet::cv.glmnet(x=x_tre_val,y=y_tre_val,
                                   alpha=1, family='binomial')
ajs_log_lasso <- glmnet::glmnet(x=x_tre_val,y=y_tre_val, alpha=1,
                                lambda = ajs_log_lasso$lambda.1se,
                                family='binomial')

## Matrizes de Confusao

# Completo
pred_log_full <- predict(ajs_log_full, newdata=val[,-1], type='response')
pred_log_full <- as.factor(ifelse(pred_log_full>=0.5, 1, 0))

conf_log_full <- caret::confusionMatrix(
  data=pred_log_full,
  reference=val$win
)

# Lasso

pred_log_lasso <- predict(ajs_log_lasso, x_val,type='response')
pred_log_lasso <- as.factor(ifelse(pred_log_lasso>=0.5,1,0))

conf_log_lasso <- caret::confusionMatrix(
  data=pred_log_lasso,
  reference=val$win
)

## Curvas ROC

roc_log_full <- pROC::roc(response = val$win,
                          predictor = predict(ajs_log_full, newdata = val[,-1],
                                              type='response'))

roc_log_lasso <- pROC::roc(response = val$win,
                           predictor = as.vector(predict(ajs_log_lasso, newx = x_val,
                                               type='response')))

pROC::ggroc(
  list(`Completo`=roc_log_full, `Lasso`=roc_log_lasso),
  legacy.axes=TRUE, size=1
)+
  ggplot2::labs(color='Seleção', x='1-Especificidade', y='Sensibilidade',
                title='Curvas ROC para Regressão Logística',
                subtitle = 'considerando modelo completo e seleção de variáveis via Lasso (com diff_spd)')+
  ggplot2::theme_minimal()+
  ggplot2::theme(legend.position = 'top')

ggplot2::ggsave('img/13-roc-log-diff-spd.png')

## Apresentando Metricas

tibble::tibble(
  Metrica=names(metricas_extract(conf_log_full, roc_log_full)),
  `Completo` = metricas_extract(conf_log_full, roc_log_full),
  `Lasso`=metricas_extract(conf_log_lasso, roc_log_lasso)
) |>
  tidyr::pivot_longer(!Metrica) |>
  tidyr::pivot_wider(names_from=Metrica) |>
  dplyr::rename(Ajuste=name) |>
  knitr::kable('latex', digits=4)

#### COMPARANDO MELHORES MODELOS

## Ajustes

# Lasso
ajs_log <- glmnet::cv.glmnet(x=x_tre,y=y_tre,
                                   alpha=1, family='binomial')
ajs_log <- glmnet::glmnet(x=x_tre,y=y_tre, alpha=1,
                                lambda = ajs_log$lambda.1se,
                                family='binomial')

# Arvore
ajs_arv <- rpart::rpart(win~., data=tre)

# BI Gaussiano
ajs_gaus <- naivebayes::naive_bayes(win~., data=tre[,c(1,7)],
                                    usekernel=FALSE)

# BI Flexivel
ajs_flex <- naivebayes::naive_bayes(win~., data=tre[,c(1,7)],
                                    usekernel=TRUE,
                                    kernel='gaussian')

## Matrizes de Confusao

# Lasso
pred_log <- predict(ajs_log, x_tes,type='response')
pred_log <- as.factor(ifelse(pred_log>=0.5,1,0))
conf_log <- caret::confusionMatrix(
  data=pred_log,
  reference=tes$win
)

# Arvore
conf_arv <- caret::confusionMatrix(
  data=predict(ajs_arv, tes[,-1], type='class'),
  reference=tes$win
)

# BI Gaussiano
conf_gaus <- caret::confusionMatrix(
  data=predict(ajs_gaus, tes[,-1]),
  reference=tes$win
)

# BI Flexível
conf_flex <- caret::confusionMatrix(
  data=predict(ajs_flex, tes[,-1]),
  reference=tes$win
)

## Curvas ROC

# Lasso
roc_log <- pROC::roc(response = tes$win,
                     predictor = as.vector(predict(ajs_log, newx = x_tes,
                                                   type='response')))

# Arvore
roc_arv <- pROC::roc(response = tes$win,
                     predictor = predict(ajs_arv, tes[,-1])[,2])

# BI Gaussiano
roc_gaus <- pROC::roc(response = tes$win,
                      predictor = predict(ajs_gaus, tes[,-1], type='prob')[,1])

# BI Flexível
roc_flex <- pROC::roc(response = tes$win,
                      predictor = predict(ajs_flex, tes[,-1], type='prob')[,1])

pROC::ggroc(
  list(`Logistica`=roc_log, `Arvore`=roc_arv, `BI Gaussiano`=roc_gaus,
       `BI Flexível`=roc_flex),
  legacy.axes=TRUE, size=1
)+
  ggplot2::labs(color='Ajuste', x='1-Especificidade', y='Sensibilidade',
                title='Curvas ROC para Melhores Ajustes',
                subtitle = 'considerando a variável diff_spd')+
  ggplot2::theme_minimal()+
  ggplot2::theme(legend.position = 'top')

ggplot2::ggsave('img/14-roc-all-diff-spd.png')

## Apresentando Metricas

tibble::tibble(
  Metrica=names(metricas_extract(conf_log, roc_log)),
  `Logística` = metricas_extract(conf_log, roc_log),
  `Árvore`=metricas_extract(conf_arv, roc_arv),
  `BI Gaussiano`=metricas_extract(conf_gaus, roc_gaus),
  `BI Flexível`=metricas_extract(conf_flex, roc_flex)
) |>
  tidyr::pivot_longer(!Metrica) |>
  tidyr::pivot_wider(names_from=Metrica) |>
  dplyr::rename(Ajuste=name) |>
  knitr::kable('latex', digits=4)
