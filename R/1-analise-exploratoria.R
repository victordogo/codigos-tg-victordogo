### Lendo dados

df <- readRDS('data/pokebattle.rds')

### Carregando patchwork para junção de graficos

library(patchwork)

#### Verificando balanceamento

n <- nrow(df)

df$win |>
  table()/n

#### Grafico de Correlacao

cor_win_0 <- df[df$win==0,] |>
  corrr::correlate(method='pearson') |>
  ggplot2::autoplot(high='red')+
  ggplot2::geom_text(ggplot2::aes(label=round(r,2)))+
  ggplot2::theme_minimal(base_size = 16)+
  ggplot2::theme(legend.position = 'none')+
  ggplot2::labs(title='Correlação de Pearson',
                subtitle = 'de covariáveis numéricas (win=0)')

cor_win_1 <- df[df$win==1,] |>
  corrr::correlate(method='pearson') |>
  ggplot2::autoplot(high='red')+
  ggplot2::geom_text(ggplot2::aes(label=round(r,2)))+
  ggplot2::theme_minimal(base_size = 16)+
  ggplot2::theme(legend.position = 'none')+
  ggplot2::labs(title='Correlação de Pearson',
                subtitle = 'de covariáveis numéricas (win=1)')

cor_win_0 + cor_win_1

ggplot2::ggsave('img/1-corr-plot.png')

#### Densidade de variaveis numericas

kernel_hp <- df |>
  ggplot2::ggplot()+
  ggplot2::aes(x=diff_hp)+
  # Estimativa do Kernel Gaussiano
  ggplot2::stat_density(kernel='gaussian', size=1,
                        ggplot2::aes(color='kernel gaussiano'), alpha=0)+
  # Estimativa Normal
  ggplot2::stat_function(fun=dnorm,n=101,args=list(mean=mean(df$diff_hp),
                                                   sd=sd(df$diff_hp)),
                         ggplot2::aes(color='normal'), size=1)+
  # Kernel parabolico ou epanechnikov
  ggplot2::stat_density(kernel='epanechnikov', alpha=0, size=1,
                        ggplot2::aes(color='kernel parabolico'))+
  # Kernel uniforme
  ggplot2::stat_density(kernel='rectangular', alpha=0, size=1,
                        ggplot2::aes(color='kernel uniforme'))+
  ggplot2::theme_minimal()+
  ggplot2::theme(legend.position = 'none')+
  ggplot2::labs(y='densidade',
                title='diff_hp',
                colour='')+
  ggplot2::coord_cartesian(ylim=c(0.005, 0.015),
                           xlim=c(-50,50))

kernel_atk <- df |>
  ggplot2::ggplot()+
  ggplot2::aes(x=diff_atk)+
  # Estimativa do Kernel Gaussiano
  ggplot2::stat_density(kernel='gaussian', size=1,
                        ggplot2::aes(color='kernel gaussiano'), alpha=0)+
  # Estimativa Normal
  ggplot2::stat_function(fun=dnorm,n=101,args=list(mean=mean(df$diff_atk),
                                                   sd=sd(df$diff_atk)),
                         ggplot2::aes(color='normal'), size=1)+
  # Kernel parabolico ou epanechnikov
  ggplot2::stat_density(kernel='epanechnikov', alpha=0, size=1,
                        ggplot2::aes(color='kernel parabolico'))+
  # Kernel uniforme
  ggplot2::stat_density(kernel='rectangular', alpha=0, size=1,
                        ggplot2::aes(color='kernel uniforme'))+
  ggplot2::theme_minimal()+
  ggplot2::theme(legend.position = 'none')+
  ggplot2::labs(y='densidade',
                title='diff_atk',
                colour='')+
  ggplot2::coord_cartesian(ylim=c(0.005, 0.01),
                           xlim=c(-50,50))

kernel_def <- df |>
  ggplot2::ggplot()+
  ggplot2::aes(x=diff_def)+
  # Estimativa do Kernel Gaussiano
  ggplot2::stat_density(kernel='gaussian', size=1,
                        ggplot2::aes(color='kernel gaussiano'), alpha=0)+
  # Estimativa Normal
  ggplot2::stat_function(fun=dnorm,n=101,args=list(mean=mean(df$diff_def),
                                                   sd=sd(df$diff_def)),
                         ggplot2::aes(color='normal'), size=1)+
  # Kernel parabolico ou epanechnikov
  ggplot2::stat_density(kernel='epanechnikov', alpha=0, size=1,
                        ggplot2::aes(color='kernel parabolico'))+
  # Kernel uniforme
  ggplot2::stat_density(kernel='rectangular', alpha=0, size=1,
                        ggplot2::aes(color='kernel uniforme'))+
  ggplot2::theme_minimal()+
  ggplot2::theme(legend.position = 'none')+
  ggplot2::labs(y='densidade',
                title='diff_def',
                colour='')+
  ggplot2::coord_cartesian(ylim=c(0.005, 0.011),
                           xlim=c(-50,50))

kernel_satk <- df |>
  ggplot2::ggplot()+
  ggplot2::aes(x=diff_satk)+
  # Estimativa do Kernel Gaussiano
  ggplot2::stat_density(kernel='gaussian', size=1,
                        ggplot2::aes(color='kernel gaussiano'), alpha=0)+
  # Estimativa Normal
  ggplot2::stat_function(fun=dnorm,n=101,args=list(mean=mean(df$diff_satk),
                                                   sd=sd(df$diff_satk)),
                         ggplot2::aes(color='normal'), size=1)+
  # Kernel parabolico ou epanechnikov
  ggplot2::stat_density(kernel='epanechnikov', alpha=0, size=1,
                        ggplot2::aes(color='kernel parabolico'))+
  # Kernel uniforme
  ggplot2::stat_density(kernel='rectangular', alpha=0, size=1,
                        ggplot2::aes(color='kernel uniforme'))+
  ggplot2::theme_minimal()+
  ggplot2::theme(legend.position = 'right')+
  ggplot2::labs(y='densidade',
                title='diff_satk',
                colour='')+
  ggplot2::coord_cartesian(ylim=c(0.005, 0.0102),
                           xlim=c(-50,50))

kernel_sdef <- df |>
  ggplot2::ggplot()+
  ggplot2::aes(x=diff_sdef)+
  # Estimativa do Kernel Gaussiano
  ggplot2::stat_density(kernel='gaussian', size=1,
                        ggplot2::aes(color='kernel gaussiano'), alpha=0)+
  # Estimativa Normal
  ggplot2::stat_function(fun=dnorm,n=101,args=list(mean=mean(df$diff_sdef),
                                                   sd=sd(df$diff_sdef)),
                         ggplot2::aes(color='normal'), size=1)+
  # Kernel parabolico ou epanechnikov
  ggplot2::stat_density(kernel='epanechnikov', alpha=0, size=1,
                        ggplot2::aes(color='kernel parabolico'))+
  # Kernel uniforme
  ggplot2::stat_density(kernel='rectangular', alpha=0, size=1,
                        ggplot2::aes(color='kernel uniforme'))+
  ggplot2::theme_minimal()+
  ggplot2::theme(legend.position = 'none')+
  ggplot2::labs(y='densidade',
                title='diff_sdef',
                colour='')+
  ggplot2::coord_cartesian(ylim=c(0.005, 0.012),
                           xlim=c(-50,50))

kernel_spd <- df |>
  ggplot2::ggplot()+
  ggplot2::aes(x=diff_spd)+
  # Estimativa do Kernel Gaussiano
  ggplot2::stat_density(kernel='gaussian', size=1,
                        ggplot2::aes(color='kernel gaussiano'), alpha=0)+
  # Estimativa Normal
  ggplot2::stat_function(fun=dnorm,n=101,args=list(mean=mean(df$diff_spd),
                                                   sd=sd(df$diff_spd)),
                         ggplot2::aes(color='normal'), size=1)+
  # Kernel parabolico ou epanechnikov
  ggplot2::stat_density(kernel='epanechnikov', alpha=0, size=1,
                        ggplot2::aes(color='kernel parabolico'))+
  # Kernel uniforme
  ggplot2::stat_density(kernel='rectangular', alpha=0, size=1,
                        ggplot2::aes(color='kernel uniforme'))+
  ggplot2::theme_minimal()+
  ggplot2::theme(legend.position = 'none')+
  ggplot2::labs(y='densidade',
                title='diff_spd',
                colour='')+
  ggplot2::coord_cartesian(ylim=c(0.005, 0.01),
                           xlim=c(-50,50))

(kernel_hp+kernel_atk)/(kernel_def+kernel_satk)/(kernel_sdef+kernel_spd)

ggplot2::ggsave('img/2-kernel-compar.png')

#### Densidade de variaveis numericas para win=0 e win=1

dif_hp <- df |>
  ggplot2::ggplot()+
  ggplot2::aes(x=diff_hp, fill=win)+
  ggplot2::geom_density(alpha=0.6)+
  ggplot2::theme_minimal()+
  ggplot2::theme(legend.position = 'none')+
  ggplot2::labs(y='densidade', title='diff_hp')

dif_atk <- df |>
  ggplot2::ggplot()+
  ggplot2::aes(x=diff_atk, fill=win)+
  ggplot2::geom_density(alpha=0.6)+
  ggplot2::theme_minimal()+
  ggplot2::theme(legend.position = 'none')+
  ggplot2::labs(y='densidade', title='diff_atk')

dif_def <- df |>
  ggplot2::ggplot()+
  ggplot2::aes(x=diff_def, fill=win)+
  ggplot2::geom_density(alpha=0.6)+
  ggplot2::theme_minimal()+
  ggplot2::theme(legend.position = 'none')+
  ggplot2::labs(y='densidade', title='diff_def')

dif_satk <- df |>
  ggplot2::ggplot()+
  ggplot2::aes(x=diff_satk, fill=win)+
  ggplot2::geom_density(alpha=0.6)+
  ggplot2::theme_minimal()+
  ggplot2::labs(y='densidade', title='diff_satk')

dif_sdef <- df |>
  ggplot2::ggplot()+
  ggplot2::aes(x=diff_sdef, fill=win)+
  ggplot2::geom_density(alpha=0.6)+
  ggplot2::theme_minimal()+
  ggplot2::theme(legend.position = 'none')+
  ggplot2::labs(y='densidade', title='diff_sdef')

dif_spd <- df |>
  ggplot2::ggplot()+
  ggplot2::aes(x=diff_spd, fill=win)+
  ggplot2::geom_density(alpha=0.6)+
  ggplot2::theme_minimal()+
  ggplot2::theme(legend.position = 'none')+
  ggplot2::labs(y='densidade', title='diff_spd')

(dif_hp+dif_atk)/(dif_def+dif_satk)/(dif_sdef+dif_spd)

ggplot2::ggsave('img/3-dens-win-compar.png')

#### Comparação de pokemon lendario e nao lendario para win=0 e win=1

leg_atk <- df |>
  dplyr::group_by(leg_atk,win) |>
  dplyr::summarise(n=dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    dplyr::across(.cols=-3,
                  .fns=as.character)
  ) |>
  ggplot2::ggplot()+
  ggplot2::aes(y=n,x=leg_atk,fill=win)+
  ggplot2::geom_bar(stat='identity', position='fill', color='black')+
  ggplot2::theme_minimal()+
  ggplot2::theme(legend.position = 'left')+
  ggplot2::labs(y='proporção observada',
                title='leg_atk')

leg_def <- df |>
  dplyr::group_by(leg_def,win) |>
  dplyr::summarise(n=dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    dplyr::across(.cols=-3,
                  .fns=as.character)
  ) |>
  ggplot2::ggplot()+
  ggplot2::aes(y=n,x=leg_def,fill=win)+
  ggplot2::geom_bar(stat='identity', position='fill', color='black')+
  ggplot2::theme_minimal()+
  ggplot2::theme(legend.position = 'none')+
  ggplot2::labs(y='proporção observada',
                title='leg_def')

leg_atk+leg_def

ggplot2::ggsave('img/4-leg-win-compar.png')

#### Comparação de gerações para win=0 e win=1

gen_atk <- df |>
  dplyr::group_by(gen_atk,win) |>
  dplyr::summarise(n=dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    dplyr::across(.cols=-3,
                  .fns=as.character)
  ) |>
  ggplot2::ggplot()+
  ggplot2::aes(y=n,x=gen_atk,fill=win)+
  ggplot2::geom_bar(stat='identity', position='fill', color='black')+
  ggplot2::theme_minimal()+
  ggplot2::theme(legend.position = 'none')+
  ggplot2::labs(y='proporção observada',
                title='gen_atk')

gen_def <- df |>
  dplyr::group_by(gen_def,win) |>
  dplyr::summarise(n=dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    dplyr::across(.cols=-3,
                  .fns=as.character)
  ) |>
  ggplot2::ggplot()+
  ggplot2::aes(y=n,x=gen_def,fill=win)+
  ggplot2::geom_bar(stat='identity', position='fill', color='black')+
  ggplot2::theme_minimal()+
  ggplot2::theme(legend.position = 'top')+
  ggplot2::labs(y='proporção observada',
                title='gen_def')

gen_atk/gen_def

ggplot2::ggsave('img/5-gen-win-compar.png')

### Estimação da rede bayesiana com base nos dados

df[,c(1,8:13)] |>
  bnlearn::tabu() |>
  plot()
