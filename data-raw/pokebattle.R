##### CODIGO PARA PREPARACAO DO DATASET 'pokebattle' #####

combats <- readr::read_csv('data-raw/combats.csv') |>
  dplyr::rename(
    atacante=First_pokemon,
    defesa=Second_pokemon
  ) |>
  dplyr::mutate(
    win=as.factor(ifelse(
      atacante==Winner,1,0
    ))
  ) |>
  dplyr::select(win,atacante,defesa)

pokemon <- readr::read_csv('data-raw/pokemon.csv') |>
  dplyr::rename(
    id=`#`, nome=Name, tipo_1=`Type 1`,
    tipo_2=`Type 2`, hp=HP, atk=Attack,
    def=Defense, satk=`Sp. Atk`, sdef=`Sp. Def`,
    spd=Speed, geracao=Generation, lendario=Legendary
  ) |>
  dplyr::mutate(
    lendario=as.factor(ifelse(lendario==TRUE, 1, 0)),
    geracao=as.factor(geracao)
  )

names <- pokemon |>
  dplyr::select(id,nome)

tipos <- readRDS("data-raw/tipos.rds")

### Preparando covariaveis

# Atacante e Defensor

combats <- combats |>
  dplyr::mutate(
    nome_atk=sapply(atacante,
                    \(x) names$nome[match(x, names$id)]),
    nome_def=sapply(defesa,
                    \(x) names$nome[match(x, names$id)])
)

# Diferenca de atk

combats <- combats |>
  dplyr::mutate(
    First_pokemon=sapply(nome_atk,
                         \(x) pokemon$atk[match(x, pokemon$nome)]),
    Second_pokemon=sapply(nome_def,
                          \(x) pokemon$atk[match(x, pokemon$nome)]),
    diff_atk=First_pokemon-Second_pokemon
  )

# Diferenca de defesa

combats <- combats |>
  dplyr::mutate(
    First_pokemon=sapply(nome_atk,
                         \(x) pokemon$def[match(x, pokemon$nome)]),
    Second_pokemon=sapply(nome_def,
                          \(x) pokemon$def[match(x, pokemon$nome)]),
    diff_def=First_pokemon-Second_pokemon
  )

# Diferenca de ataque especial

combats <- combats |>
  dplyr::mutate(
    First_pokemon=sapply(nome_atk,
                         \(x) pokemon$satk[match(x, pokemon$nome)]),
    Second_pokemon=sapply(nome_def,
                          \(x) pokemon$satk[match(x, pokemon$nome)]),
    diff_satk=First_pokemon-Second_pokemon
  )

# Diferenca de defesa especial

combats <- combats |>
  dplyr::mutate(
    First_pokemon=sapply(nome_atk,
                         \(x) pokemon$sdef[match(x, pokemon$nome)]),
    Second_pokemon=sapply(nome_def,
                          \(x) pokemon$sdef[match(x, pokemon$nome)]),
    diff_sdef=First_pokemon-Second_pokemon
  )

# Diferenca de velocidade

combats <- combats |>
  dplyr::mutate(
    First_pokemon=sapply(nome_atk,
                         \(x) pokemon$spd[match(x, pokemon$nome)]),
    Second_pokemon=sapply(nome_def,
                          \(x) pokemon$spd[match(x, pokemon$nome)]),
    diff_spd=First_pokemon-Second_pokemon
  )

# Diferenca de hp

combats <- combats |>
  dplyr::mutate(
    First_pokemon=sapply(nome_atk,
                         \(x) pokemon$hp[match(x, pokemon$nome)]),
    Second_pokemon=sapply(nome_def,
                          \(x) pokemon$hp[match(x, pokemon$nome)]),
    diff_hp=First_pokemon-Second_pokemon
  )

# Variavel de geracao

combats <- combats |>
  dplyr::mutate(
    gen_atk=sapply(nome_atk,
                   \(x) pokemon$geracao[match(x, pokemon$nome)]),
    gen_def=sapply(nome_def,
                   \(x) pokemon$geracao[match(x, pokemon$nome)])
  )

# Variavel de pokemons lendarios

combats <- combats |>
  dplyr::mutate(
    leg_atk=sapply(nome_atk,
                   \(x) pokemon$lendario[match(x, pokemon$nome)]),
    leg_def=sapply(nome_def,
                   \(x) pokemon$lendario[match(x, pokemon$nome)])
  )

## Preparando variavel de vantagem/desvantagem de tipos

combats <- combats |>
  dplyr::mutate(
    tipo1_atk=sapply(nome_atk,
                     \(x) pokemon$tipo_1[match(x, pokemon$nome)]),
    tipo2_atk=sapply(nome_atk,
                     \(x) pokemon$tipo_2[match(x, pokemon$nome)]),
    tipo1_def=sapply(nome_def,
                     \(x) pokemon$tipo_1[match(x, pokemon$nome)]),
    tipo2_def=sapply(nome_def,
                     \(x) pokemon$tipo_2[match(x, pokemon$nome)])
  )

combats[is.na(combats)] <- 'None'

# Funcao para calculo do coeficiente de vantagem/desvantagem

resist_tipo <- function(atk1,atk2,def){

  res <- tipos[tipos$Attacking==def,]

  vtg1 <- res[names(res)==atk1]

  vtg2 <- res[names(res)==atk2]

  vtg1*vtg2
}

resist_tipo <- Vectorize(resist_tipo)

combats <- combats |>
  dplyr::mutate(
    res_tipo1 = unlist(
      resist_tipo(atk1=as.vector(tipo1_atk),
                  atk2=as.vector(tipo2_atk),
                  def=as.vector(tipo1_def))
    ),
    res_tipo2 = unlist(
      resist_tipo(atk1=as.vector(tipo1_atk),
                  atk2=as.vector(tipo2_atk),
                  def=as.vector(tipo2_def))
    )
  )

## Detecção de outliers via mahalanobis

# Selecionando dataset final

df <- combats |>
  dplyr::mutate(
    across(
      .cols=c(tipo1_atk:tipo2_def,res_tipo1,res_tipo2),
      as.factor
    )
  ) |>
  dplyr::select(
    win,nome_atk,nome_def,tipo1_atk,tipo2_atk,
    tipo1_def,tipo2_def,diff_hp,diff_atk,
    diff_def,diff_satk,diff_sdef,diff_spd,
    res_tipo1,res_tipo2, gen_atk, gen_def,
    leg_atk, leg_def
  )

# Preparando dataset mahalanobis

df_ma <- df[-c(2:7)] |>
  dplyr::mutate(
    dplyr::across(.fns=as.numeric)
  )

# Realizando calculo das distancias

md <- mahalanobis(df_ma, center = colMeans(df_ma), cov = cov(df_ma))

# Estabelecendo limite

alpha <- .001

lim <- (qchisq(p = 1 - alpha, df = ncol(df_ma)))

# Encontrando outliers

out <- which(md > lim)

df <- df[-out,]

## Exportando dataset final

df |>
  readr::write_rds('data/pokebattle.rds')
