### Lendo dados

df <- readRDS('data/pokebattle.rds')

#### Grafico de Correlacao

df |>
  corrr::correlate(method='pearson') |>
  ggplot2::autoplot(high='red')+
  ggplot2::geom_text(ggplot2::aes(label=round(r,2)))+
  ggplot2::theme(legend.position = 'none')+
  ggplot2::labs(title='Gráfico de Correlação de Pearson',
                subtitle = 'de covariáveis numéricas')

#### Densidade de variaveis numericas

vida <- df |>
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

atk <- df |>
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
  ggplot2::theme(legend.position = 'top')+
  ggplot2::labs(y='densidade',
                title='diff_atk',
                colour='')+
  ggplot2::coord_cartesian(ylim=c(0.005, 0.01),
                           xlim=c(-50,50))

gridExtra::grid.arrange(vida, atk, ncol=1)

def <- df |>
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
  ggplot2::coord_cartesian(ylim=c(0.005, 0.01),
                           xlim=c(-50,50))

satk <- df |>
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
  ggplot2::theme(legend.position = 'top')+
  ggplot2::labs(y='densidade',
                title='diff_satk',
                colour='')+
  ggplot2::coord_cartesian(ylim=c(0.005, 0.01),
                           xlim=c(-50,50))

gridExtra::grid.arrange(def, satk, ncol=1)

sdef <- df |>
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

spd <- df |>
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
  ggplot2::theme(legend.position = 'top')+
  ggplot2::labs(y='densidade',
                title='diff_spd',
                colour='')+
  ggplot2::coord_cartesian(ylim=c(0.005, 0.01),
                           xlim=c(-50,50))

gridExtra::grid.arrange(sdef, spd, ncol=1)
