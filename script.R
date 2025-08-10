library(readr)
library(dplyr)
library(lubridate)
library(arrow)
library(tidyr) 
library(ggplot2)
library(forcats)
library(dplyr)
library(stringr)
library(caret)        
library(nnet)         
library(randomForest) 
library(pROC)  
library(gmodels)
library(ggcorrplot)
library(vcd)
library(rpart) # Pacote para a Árvore de Decisão
library(tibble)


##Importaçao e Limpeza dos dados:

# Diretório atual
#diretorio <- getwd()

# Lista os arquivos CSV
#arquivos <- list.files(path = diretorio, pattern = "\\.csv$", full.names = TRUE)

# Loop para ler e processar cada arquivo
#dados <- read_delim(arquivo, delim = ";", col_types = cols(.default = "c"))
  
dados_interesse <- read_parquet("dados_interesse.parquet")
dados_focados <- read_parquet("dados_focados.parquet")


# Carregar o modelo GLM
modelo_glm <- readRDS("modelo_glm.rds")

# Carregar o modelo de Árvore de Decisão
modelo_arvore <- readRDS("modelo_arvore.rds")


# Agora você pode usar os novos objetos 'modelo_carregado' para fazer previsões

#colnames(dados)
#dados_interesse <- dados[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,15,16,17,22,23,25,29,
                           # 30,32,33,34,36,38,46,48,50,51,55,60)]

#dados_interesse <- dados_interesse %>%
  #mutate(across(everything(), ~na_if(., "NULL")))

#colSums(is.na(dados_interesse))
#colSums(is.na(dados_focados))


#dados_interesse <- dados_interesse %>%
 # drop_na(all_of(colunas_para_remover_na)) 

#colunas_para_remover_na <- c("Raça_Cor_da_vítima", "Gênero_da_vítima", 
                          #  "Faixa_etária_da_vítima",
                           # "UF","Gênero_do_suspeito")

#unique(dados_focados$Relação_vítima_suspeito)
#write_parquet(dados_interesse, "dados_interesse.parquet")
#write_parquet(dados_focados, "dados_focados.parquet")

# 1. Filtra as linhas onde a coluna 'violacao' NÃO é "MEIO AMBIENTE"
#dados_focados <- dados_focados %>%
 # filter(Faixa_etária_da_vítima != "NÃO INFORMADO")

#2. Atualiza os níveis do fator para remover o nível não usado
#dados_focados$Faixa_etária_da_vítima <- droplevels(dados_focados$Faixa_etária_da_vítima)

# Agora, se você verificar os níveis, "MEIO AMBIENTE" não estará mais lá
#levels(dados_focados$Faixa_etária_da_vítima)


####Transformando as variáveis de interesse em factor:

#dados_interesse <- dados_interesse %>%
 # mutate(
  #  Raça_Cor_da_vítima = as.factor(Raça_Cor_da_vítima),
   # Gênero_da_vítima = as.factor(Gênero_da_vítima),
  #  Faixa_etária_da_vítima = as.factor(Faixa_etária_da_vítima),
   # UF = as.factor(UF),
  #  Relação_vítima_suspeito  = as.factor(Relação_vítima_suspeito),
   # Gênero_do_suspeito = as.factor(Gênero_do_suspeito),
    #violacao = as.factor(violacao),
    #Grupo_vulnerável = as.factor(Grupo_vulnerável)
  #)


#dados_focados <- dados_focados %>%
 # mutate(
  #  Faixa_etária_da_vítima = as.factor(Faixa_etária_da_vítima))
    
#dados_interesse <- dados_interesse %>%
 # mutate(
  #  Raça_Cor_da_vítima = fct_collapse(Raça_Cor_da_vítima,
                                     # "PRETA/PARDA" = c("PRETA", "PARDA"))
  #)


#dados_interesse <- dados_interesse %>%
 # mutate(Gênero_do_suspeito= fct_collapse(Gênero_do_suspeito, "INTERSEXO" =
                                           # c("INTERSEXO", "INTERSEXO ")))

# Dicionário de conversão para valores unitários → intervalos
#converter_idade <- function(x) {
  #x <- str_trim(x)
  
  # Se for um valor unitário do tipo "01 ANO", "02 ANOS", etc
  #if (str_detect(x, "^\\d{2} ANOS?$")) {
    #idade <- as.numeric(str_extract(x, "\\d{2}"))
    #prox <- str_pad(idade + 1, 2, pad = "0")
    #atual <- str_pad(idade, 2, pad = "0")
   # return(paste0(atual, " A ", prox, " ANOS"))
  #}
  
  # Se for um valor unitário do tipo "1 ANO", sem zero à esquerda (caso raro)
  #if (str_detect(x, "^\\d{1} ANOS?$")) {
    #idade <- as.numeric(str_extract(x, "\\d{1}"))
    #prox <- str_pad(idade + 1, 2, pad = "0")
   # atual <- str_pad(idade, 2, pad = "0")
  #  return(paste0(atual, " A ", prox, " ANOS"))
 # }
  
  # Se já for um intervalo ou rótulo descritivo, mantemos como está
#  return(x)
#}

# =Aplicar a conversão à coluna


#dados_interesse <- dados_interesse %>%
 # mutate(
    # Extrai o número da idade se a string corresponder ao padrão "XX ANO" ou "XX ANOS"
  #  idade_singular_extraida = as.numeric(str_extract(Faixa_etária_da_vítima, "^\\d{1,2}(?=\\sANOS?$)")),
    
    # Padroniza apenas os valores que eram idades singulares
   # Faixa_etária_da_vítima = case_when(
    #  !is.na(idade_singular_extraida) ~ paste0(idade_singular_extraida, " A ", idade_singular_extraida + 1, " ANOS"),
      
      # Mantém os demais valores como estão
     # TRUE ~ Faixa_etária_da_vítima
    #)
  #)


###############################################################
##### ANALISE EXPLORATÓRIA ###################################
############################################################

################# RAÇA COR DA VÍTIMA ####################

# Frequência absoluta
#table(dados_interesse$Raça_Cor_da_vítima)

# Frequência relativa (%)
#prop.table(table(dados_interesse$Raça_Cor_da_vítima)) * 100

# Valores únicos
#levels(dados_interesse$Raça_Cor_da_vítima)

# Visualização
ggplot(dados_interesse, aes(x = Raça_Cor_da_vítima)) +
  geom_bar(fill = "#66c2a5") +
  labs(title = "Distribuição por Raça/Cor da Vítima", x = "Raça/Cor", y = "Frequência") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6), 
        plot.title = element_text(hjust = 0.5))


############ GENERO DA VITIMA ##########################

#table(dados_interesse$Gênero_da_vítima)
#prop.table(table(dados_interesse$Gênero_da_vítima)) * 100
#levels(dados_interesse$Gênero_da_vítima)

ggplot(dados_interesse, aes(x = Gênero_da_vítima)) +
  geom_bar(fill = "#fc8d62") +
  labs(title = "Distribuição por Gênero da Vítima", x = "Gênero", y = "Frequência") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6), 
        plot.title = element_text(hjust = 0.5))


#####################################################
# FAIXA ETÁRIA DA VÍTIMA ####################
#############################################


#table(dados_interesse$Faixa_etária_da_vítima)
#prop.table(table(dados_interesse$Faixa_etária_da_vítima)) * 100
#levels(dados_interesse$Faixa_etária_da_vítima)

#library(dplyr)

#dados_interesse <- dados_interesse %>%
 # filter(!Faixa_etária_da_vítima %in% c(
  #  "2 A 4 ANOS",
   # "7 A 9 ANOS",
    #"10 A 11 ANOS",
    #"12 A 14 ANOS"
  #))


ggplot(dados_interesse, aes(x = Faixa_etária_da_vítima)) +
  geom_bar(fill = "#8da0cb") +
  labs(title = "Distribuição por Faixa Etária da Vítima", x = "Faixa Etária", y = "Frequência") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6), 
        plot.title = element_text(hjust = 0.5))


# RELAÇÃO VITIMA SUSPEITO ####################


#table(dados_interesse$Relação_vítima_suspeito)
#prop.table(table(dados_interesse$Relação_vítima_suspeito)) * 100
#levels(dados_interesse$Relação_vítima_suspeito)

ggplot(dados_interesse, aes(x = Relação_vítima_suspeito)) +
  geom_bar(fill = "#a6d854") +
  labs(title = "Relação entre Vítima e Suspeito", x = "Relação", y = "Frequência") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6), 
        plot.title = element_text(hjust = 0.5))

ggplot(dados_focados, aes(x = Relação_vítima_suspeito)) +
  geom_bar(fill = "#a6d854") +
  labs(title = "Relação entre Vítima e Suspeito", x = "Relação", y = "Frequência") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6), 
        plot.title = element_text(hjust = 0.5))

# GENERO SUSPEITO ####################


#table(dados_interesse$Gênero_do_suspeito)
#prop.table(table(dados_interesse$Gênero_do_suspeito)) * 100
#levels(dados_interesse$Gênero_do_suspeito)

ggplot(dados_interesse, aes(x = Gênero_do_suspeito)) +
  geom_bar(fill = "#ffd92f") +
  labs(title = "Distribuição por Gênero do Suspeito", x = "Gênero", y = "Frequência") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6), 
        plot.title = element_text(hjust = 0.5))


# VIOLACAO ####################

#table(dados_interesse$violacao)
#prop.table(table(dados_interesse$violacao)) * 100
#levels(dados_interesse$violacao)

ggplot(dados_interesse, aes(x = violacao)) +
  geom_bar(fill = "#e5c494") +
  labs(title = "Tipo de Violação", x = "Violação", y = "Frequência") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4), 
        plot.title = element_text(hjust = 0.5))

ggplot(dados_focados, aes(x = violacao)) +
  geom_bar(fill = "#e5c494") +
  labs(title = "Tipo de Violação", x = "Violação", y = "Frequência") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4), 
        plot.title = element_text(hjust = 0.5))


#######################3
#### Grupo_vulnerável #############

table(dados_interesse$Grupo_vulnerável)
prop.table(table(dados_interesse$Grupo_vulnerável)) * 100
levels(dados_interesse$Grupo_vulnerável)

ggplot(dados_interesse, aes(x = Grupo_vulnerável)) +
  geom_bar(fill = "lightpink") +
  labs(title = "Grupo_vulnerável", x = "Grupo_vulnerável", y = "Frequência") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7), 
        plot.title = element_text(hjust = 0.5))


# ==============================================================================
# Gráficos de Relação entre Variáveis Categóricas
# ==============================================================================

# Gráfico 1: Gênero da Vítima vs. Raça/Cor da Vítima (Barras Agrupadas)
grafico_genero_raca <- ggplot(data = dados_focados, 
                              aes(x = Gênero_da_vítima,
                                                         
                                   fill = Raça_Cor_da_vítima)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribuição de Raça/Cor da Vítima por Gênero",
       x = "Gênero da Vítima",
       y = "Contagem",
       fill = "Raça/Cor") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        plot.title = element_text(hjust = 0.5))
print(grafico_genero_raca)

# Gráfico 2: Relação Vítima-Suspeito vs. Gênero do Suspeito (Barras Empilhadas)
grafico_relacao_genero_vitima <- ggplot(data = dados_focados, 
                                          aes(x = Relação_vítima_suspeito, 
                             fill = Gênero_da_vítima)) +
  geom_bar(position = "stack") + # Use "fill" se quiser proporção (percentual)
  labs(title = "Relação Vítima-Suspeito por Gênero do Suspeito",
       x = "Relação Vítima-Suspeito",
       y = "Contagem",
       fill = "Gênero da vítima") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        plot.title = element_text(hjust = 0.5))
print(grafico_relacao_genero_vitima)



#### grafico faixa etaria e genero 

g_Faixa_etária_da_vítima <- ggplot(data = dados_interesse, 
                                          aes(x = Faixa_etária_da_vítima, 
                                              fill = Gênero_da_vítima)) +
  geom_bar(position = "stack") + # Use "fill" se quiser proporção (percentual)
  labs(title = "Relação Faixa_etária_da_vítima por Gênero_da_vítima",
       x = "Relação Faixa_etária_da_vítima",
       y = "Contagem",
       fill = "Gênero_da_vítima") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        plot.title = element_text(hjust = 0.5))
print(g_Faixa_etária_da_vítima)


# Gráfico 4: Gênero da Vítima por UF (Barras Empilhadas para Proporção)
grafico_genero_uf <- ggplot(data = dados_interesse, 
                            aes(x = UF, fill = Gênero_da_vítima)) +
  geom_bar(position = "fill") + # Normaliza as barras para 100%
  labs(title = "Proporção de Gênero da Vítima por Unidade Federativa",
       x = "Unidade Federativa (UF)",
       y = "Proporção",
       fill = "Gênero da Vítima") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        plot.title = element_text(hjust = 0.5))
print(grafico_genero_uf)


library(ggplot2)
library(dplyr)
library(sf)
library(ggspatial)
library(geobr)

# 1. Calcular proporção de cada gênero por UF
dados_mapa <- dados_interesse %>%
  group_by(UF, `Gênero_da_vítima`) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(UF) %>%
  mutate(proporcao = n / sum(n))

# 2. Baixar shapefile dos estados
shp_estados <- read_state(year = 2020, showProgress = FALSE) %>%
  st_transform(crs = 4674)

# 3. Juntar dados com o shapefile
mapa_dados <- shp_estados %>%
  left_join(dados_mapa, by = c("abbrev_state" = "UF"))

# 4. Criar mapa temático igual à lógica do gráfico original
mapa_genero_uf <- ggplot(data = mapa_dados) +
  geom_sf(aes(fill = proporcao), color = "white") +
  scale_fill_gradient(low = "#fee5d9", high = "#a50f15",
                      name = "Proporção", labels = scales::percent) +
  facet_wrap(~`Gênero_da_vítima`) +
  annotation_north_arrow(
    location = "tr", which_north = "true",
    style = north_arrow_fancy_orienteering
  ) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  labs(
    title = "Proporção de Gênero da Vítima por Unidade Federativa",
    subtitle = "Brasil",
    caption = "Fonte: Ministério dos Direitos Humanos e Cidadania | Elaboração própria\nCRS: SIRGAS 2000 (EPSG:4674)"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

print(mapa_genero_uf)



### GRAFICO GRUPO VULNERAVEL E FAIXA ETARIA DA VITIMA

grafico_Grupo_vulnerável<- ggplot(data = dados_interesse, 
                                  aes(x = Faixa_etária_da_vítima, 
                                      fill = Grupo_vulnerável)) +
  geom_bar(position = "stack") + # Use "fill" se quiser proporção (percentual)
  labs(title = "Relação Faixa_etária_da_vítima por Grupo_vulnerável",
       x = "Relação Faixa_etária_da_vítima",
       y = "Contagem",
       fill = "Grupo_vulnerável") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        plot.title = element_text(hjust = 0.5))
print(grafico_Grupo_vulnerável)

##### grupo vulneravel e genero da vitima 

grafico_Grupo_vulnerável_genero<- ggplot(data = dados_interesse, 
                                  aes(x = Gênero_da_vítima, 
                                      fill = Grupo_vulnerável)) +
  geom_bar(position = "fill") + # Use "fill" se quiser proporção (percentual)
  labs(title = "Relação Faixa_etária_da_vítima por Gênero_da_vítima",
       x = "Gênero_da_vítima",
       y = "Contagem",
       fill = "Grupo_vulnerável") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        plot.title = element_text(hjust = 0.5))
print(grafico_Grupo_vulnerável_genero)


ggplot(dados_interesse, aes(x = Grupo_vulnerável, fill = Gênero_da_vítima)) +
  geom_bar(position = "fill") +
  labs(
    title = "Distribuição de Grupo Vulnerável por Gênero da Vítima",
    x = "Grupo Vulnerável",
    fill = "Gênero da Vítima"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
    plot.title = element_text(hjust = 0.5)
  )



#############################################
############ ANALISE MULTIVARIADA ################


################## TESTE DE QUI QUADRADO #####################

### HIPOTESES :

#Hipótese Nula (H_0): Não há uma associação estatisticamente significativa entre as duas variáveis categóricas comparadas. Elas são independentes.

#Hipótese Alternativa (H_1): Existe uma associação estatisticamente significativa entre as duas variáveis categóricas comparadas. Elas não são independentes.

# Certifique-se de que 'dados_interesse' esteja carregado e 'vars' definido
vars <- c("Raça_Cor_da_vítima", "Gênero_da_vítima",
          "Relação_vítima_suspeito",
          "violacao")

# Inicializa A MATRIZ PRINCIPAL para armazenar os p-valores como CARACTERES (strings)
# As dimensões da matriz serão (número de variáveis) x (número de variáveis)
num_vars <- length(vars)
p_value_matrix_formatted <- matrix(NA_character_, nrow = num_vars, ncol = num_vars) # <--- CORREÇÃO AQUI: NA_character_ e inicialização FORA do loop

# Define os nomes das linhas e colunas da matriz
colnames(p_value_matrix_formatted) <- vars
rownames(p_value_matrix_formatted) <- vars

# Loop para preencher a matriz de p-valores
for (i in 1:num_vars) {
  for (j in 1:num_vars) {
    # Para a diagonal principal (variável vs ela mesma), o p-valor não faz sentido
    if (i == j) {
      p_value_matrix_formatted[i, j] <- "-" # Definindo como um traço (string) para a diagonal
    } else if (i < j) { # Calcula apenas para a metade superior da matriz (para evitar repetição e duplicidade)
      tab <- table(dados_focados[[vars[i]]], dados_focados[[vars[j]]])
      
      # Verifica se a tabela é adequada para o teste Qui-quadrado
      # (min. 2 linhas/colunas e nenhuma linha/coluna com soma zero)
      if (min(dim(tab)) > 1 && all(rowSums(tab) > 0) && all(colSums(tab) > 0)) {
        test_result <- chisq.test(tab)
        
        # Formata o p-valor para notação científica
        current_p_value_formatted <- formatC(test_result$p.value, format = "e", digits = 4) # <--- AQUI: nova variável para o p-valor formatado
      } else {
        current_p_value_formatted <- "NA" # Se a tabela não for adequada, o p-valor é "NA" como string
        warning(paste("Teste Qui-quadrado não realizado para", vars[i], "vs", vars[j],
                      "devido a zeros em linhas/colunas da tabela de contingência ou dimensão insuficiente."))
      }
      
      # Preenche A MATRIZ DE CARACTERES simetricamente
      p_value_matrix_formatted[i, j] <- current_p_value_formatted
      p_value_matrix_formatted[j, i] <- current_p_value_formatted # Preenche o valor correspondente na parte inferior
    }
  }
}

# Exibe a matriz de p-valores formatada
print(p_value_matrix_formatted)

library(writexl)

# Converter a matriz para data frame
p_value_df <- as.data.frame(p_value_matrix_formatted)

# Adicionar coluna com nomes das linhas para manter os nomes originais
p_value_df <- tibble::rownames_to_column(p_value_df, var = "Variável")

# Exportar para Excel
write_xlsx(p_value_df, "p_values_tabela.xlsx")

cat("Arquivo 'p_values_tabela.xlsx' criado com sucesso.\n")



# Definir as categorias que você deseja remover
#categorias_a_remover <- c("NÃO INFORMADO", "NÃO SE APLICA - VÍTIMA COMUNIDADE/FAMÍLIA",
                       #   "INTERSEXO")

# Filtrar o dataframe 'dados_focados' para remover as linhas com essas categorias
# na coluna 'Gênero_da_vítima'
#dados_focados <- dados_focados %>%
  #filter(!Gênero_da_vítima %in% categorias_a_remover)

# Importante: Remover os níveis (fatores) que não são mais usados.
# Isso garante que o modelo não "veja" essas categorias como opções, mesmo que vazias.
#dados_focados$Gênero_da_vítima <- droplevels(dados_focados$Gênero_da_vítima)

# Verificar as novas categorias e suas contagens para confirmar a remoção
#cat("Novos valores únicos e suas contagens para 'Gênero_da_vítima' em dados_focados:\n")
#print(table(dados_focados$Gênero_da_vítima))
#cat(paste("\nTotal de linhas restantes no dataframe dados_focados:", nrow(dados_focados), "\n"))

########################### #################################
##################### LIMPEZA DOS DADOS #####################


# Definir as variáveis de interesse para este modelo
#variaveis_para_o_modelo <- c("Raça_Cor_da_vítima", "Gênero_da_vítima", "violacao", 
                    #         "Relação_vítima_suspeito", "Faixa_etária_da_vítima","violacao" )

# Selecionar apenas essas variáveis do seu dataframe original (dados_interesse)
# <- dados_interesse %>%
 # select(all_of(variaveis_para_o_modelo))


#unique(dados_interesse$Relação_vítima_suspeito)
#unique(dados_interesse$violacao)



# Atualizar a coluna 'violacao' no dataframe 'dados_interesse'
#dados_interesse <- dados_interesse %>%
 # mutate(
  #  violacao = str_split_i(violacao, ">", 1) # Divide a string por ">" e pega o 1º elemento
  #) %>%
  # É importante garantir que ela continue sendo um fator, caso já fosse, ou converter se era caracter
 # mutate(violacao = as.factor(violacao))

# Vamos verificar os novos valores únicos da coluna 'violacao' atualizada
#cat("Valores únicos ATUALIZADOS da coluna 'violacao':\n")
#print(unique(dados_interesse$violacao))

# E a contagem de cada um para ter uma ideia das novas frequências
#cat("\nContagem dos novos valores únicos de 'violacao':\n")
#print(table(dados_interesse$Relação_vítima_suspeito))

# 1. Calcular a frequência de cada categoria de Relação_vítima_suspeito
#frequencias <- dados_interesse %>%
 # count(Relação_vítima_suspeito, sort = TRUE, name = "contagem") %>%
  #mutate(
  #  percentual = (contagem / sum(contagem)) * 100,
   # percentual_acumulado = cumsum(percentual)
  #)

#cat("Análise de Frequência para Relação_vítima_suspeito:\n")
#print(frequencias)

# 2. Identificar as categorias que contemplam 80% dos dados
# Filtra as categorias cuja percentagem acumulada é até 80%
# ou, se a última categoria para atingir os 80% fizer a soma ultrapassar, ela será incluída.
#categorias_80_porcento <- frequencias %>%
 # filter(percentual_acumulado <= 80 | lag(percentual_acumulado, default = 0) < 80) %>%
  #pull(Relação_vítima_suspeito)

# Se a última categoria filtrada sozinha ultrapassar 80%, precisamos garantir que ela seja incluída
# Isso acontece se 80% cai "dentro" de uma categoria, mas ela ainda é importante.
# Para ter certeza, vamos pegar as categorias que juntas somam pelo menos 80%
# Uma maneira robusta é ordenar por frequência e ir somando até passar de 80%
#frequencias_ordenadas <- dados_interesse %>%
 # count(Relação_vítima_suspeito) %>%
  #arrange(desc(n)) %>%
 # mutate(
   # prop = n / sum(n),
   # cum_prop = cumsum(prop)
 # )

# Selecionar as categorias cuja proporção acumulada não excede 80%
# ou seja, pegamos as categorias mais frequentes até atingir ou ultrapassar 80%
#categorias_manter <- frequencias_ordenadas %>%
 # filter(cum_prop <= 0.8) %>%
  #pull(Relação_vítima_suspeito)

# Para garantir que a categoria que "completa" os 80% seja incluída,
# se ela não foi pega pelo filtro acima (o que pode acontecer se o 0.8 cair bem no meio dela)
#if (max(frequencias_ordenadas$cum_prop) > 0.8 && length(categorias_manter) < nrow(frequencias_ordenadas)) {
  # Adiciona a próxima categoria na lista ordenada se o 80% não foi atingido
  # ou se a próxima categoria faz a soma ultrapassar mas é essencial
  #proxima_categoria_index <- length(categorias_manter) + 1
 # if (proxima_categoria_index <= nrow(frequencias_ordenadas)) {
   # categorias_manter <- c(categorias_manter, frequencias_ordenadas$Relação_vítima_suspeito[proxima_categoria_index])
    # Remove duplicatas se a categoria já foi incluída por acidente
   # categorias_manter <- unique(categorias_manter)
  #}
#}

#cat("\nCategorias que contemplam aproximadamente 80% dos dados de Relação_vítima_suspeito:\n")
#print(categorias_manter)

# 3. Filtrar o dataframe 'dados_interesse' para manter apenas essas categorias
#dados_interesse <- dados_interesse %>%
 # filter(Relação_vítima_suspeito %in% categorias_manter)

# Opcional: Remover níveis não utilizados do fator (importante para modelos)
#dados_interesse$Relação_vítima_suspeito <- droplevels(dados_interesse$Relação_vítima_suspeito)

#cat("\nNovas contagens para Relação_vítima_suspeito no dataframe filtrado:\n")
#print(table(dados_interesse$Relação_vítima_suspeito))
#cat(paste("Total de linhas no dataframe filtrado:", nrow(dados_interesse), "\n"))


#######################################################
##### MODELO RANDOM FOREST ##################################
########################################################

# --- 1. Preparação dos Dados (Usando o seu script como base) ---

# Certificar-se de que a variável resposta 'Gênero_da_vítima' é um fator
# com os níveis corretos.
# A função glm() não precisa de um nível "positivo" explícito, mas é bom para consistência.
#dados_focados$Gênero_da_vítima <- factor(dados_focados$Gênero_da_vítima,
                                     #    levels = c("MASCULINO", "FEMININO"))

# Dividir a base de dados em treino e teste (70/30)
set.seed(42) # Para reprodutibilidade
treino_index <- createDataPartition(dados_focados$Gênero_da_vítima, p = 0.7, list = FALSE)
treino_set <- dados_focados[treino_index, ]
teste_set  <- dados_focados[-treino_index, ]
#table(dados_focados)

# Pacote rápido
library(ranger)
library(caret)
# Converter para fator apenas se ainda não for
dados_focados[] <- lapply(dados_focados, as.factor)


# Modelo otimizado com ranger
modelo_rf <- ranger(
  formula = `Gênero_da_vítima` ~ .,
  data = treino_set,
  num.trees = 200,            # menos árvores para rodar rápido
  mtry = floor(sqrt(ncol(treino_set) - 1)), # nº de variáveis por split
  importance = "impurity",    # calcula importância das variáveis
  probability = TRUE,         # retorna probabilidades
  sample.fraction = 0.7,      # usa 70% dos dados por árvore
  num.threads = 2,            # limita uso de CPU
  verbose = TRUE
)

# Previsão no conjunto de teste
pred_prob <- predict(modelo_rf, data = teste_set)$predictions
prob_fem <- pred_prob[, "FEMININO"]

# Converter para classes
pred_class <- ifelse(prob_fem > 0.5, "FEMININO", "MASCULINO")
pred_class <- factor(pred_class, levels = levels(teste_set$`Gênero_da_vítima`))

# Avaliação
library(pROC)
library(caret)
conf_mat <- confusionMatrix(pred_class, teste_set$`Gênero_da_vítima`, positive = "FEMININO")
print(conf_mat)

roc_obj <- roc(response = teste_set$`Gênero_da_vítima`,
               predictor = prob_fem,
               levels = c("MASCULINO", "FEMININO"))
plot(roc_obj, print.auc = TRUE, col = "darkgreen")

# Importância das variáveis
vip <- sort(modelo_rf$variable.importance, decreasing = TRUE)
print(vip)




#########################################################
############### MODELO 2 ##################
#######################################

##Como o modelo ficou muito pesado, decidi reduzir 
##as categorias de faixa_etária. 

# 1. TRATAR E REMOVER a categoria "NÃO INFORMADO"
# O primeiro passo é remover as linhas onde a faixa etária é "NÃO INFORMADO".
# A função `filter` já faz isso e atualiza o `dados_focados`.
#dados_focados <- dados_focados %>%
 # filter(Faixa_etária_da_vítima != "MAIS DE 60 ANOS")

#dados_focados <- dados_focados %>%
 # mutate(Faixa_etária_da_vítima = NULL)


# 2. DEFINIR as CATEGORIAS A SEREM AGRUPADAS
# Vamos criar vetores com os níveis que pertencem a cada nova categoria.
#niveis_crianca_adolescente <- c(
 # "RECÉM-NASCIDO (ATÉ 28 DIAS)",
  #"MENOS DE 01 ANO",
  #"1 A 2 ANOS", "2 A 3 ANOS", "3 A 4 ANOS", "4 A 5 ANOS",
  #"5 A 6 ANOS", "6 A 7 ANOS", "7 A 8 ANOS", "8 A 9 ANOS",
  #"9 A 10 ANOS", "10 A 11 ANOS", "11 A 12 ANOS", "12 A 13 ANOS",
  #"13 A 14 ANOS", "14 A 15 ANOS", "15 A 16 ANOS", "16 A 17 ANOS",
  #"17 A 18 ANOS", # A pedido, 17 anos entra aqui.
  #"CRIANÇA/ADOLESCENTE IDADE NÃO INFORMADA"
#)

#niveis_adulto <- c(
 # "18 A 19 ANOS", "20 A 24 ANOS", "25 A 29 ANOS",
  #"30 A 34 ANOS", "35 A 39 ANOS", "40 A 44 ANOS",
  #"45 A 49 ANOS", "50 A 54 ANOS", "55 A 59 ANOS"
#)

#niveis_mais_60 <- c(
  #"60 A 64 ANOS", "65 A 69 ANOS", "70 A 74 ANOS", "75 A 79 ANOS",
  #"80 A 84 ANOS", "85 A 89 ANOS", "90+", "IDOSO IDADE NÃO INFORMADA"
#)


# 3. APLICAR A RECATEGORIZAÇÃO usando fct_collapse() e ATUALIZAR 'dados_focados'
#dados_focados <- dados_focados %>%
 # mutate(
  #  Faixa_etária_da_vítima = fct_collapse(Faixa_etária_da_vítima,
   #                                       "CRIANÇA OU ADOLESCENTE" = niveis_crianca_adolescente,
    #                                      "ADULTO" = niveis_adulto,
     #                                     "MAIS DE 60 ANOS" = niveis_mais_60
    #)
#  )

# 4. VERIFICAR o resultado final
# Imprima os novos níveis e a contagem para conferir
#levels(dados_focados$Faixa_etária_da_vítima)
#table(dados_focados$Faixa_etária_da_vítima)
#unique(dados_focados$Faixa_etária_da_vítima)

# Certifique-se de que os pacotes necessários estão carregados

# --- TREINAMENTO DO MODELO DE ÁRVORE DE DECISÃO ---
# Usamos o método "rpart" para treinar a Árvore de Decisão.
modelo_arvore <- train(
  Gênero_da_vítima ~ .,
  data = treino_set,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)

# --- PREVISÃO E ANÁLISE DE DESEMPENHO ---
# O predict do caret funciona perfeitamente com a Árvore de Decisão.

# Previsão da CLASSE
pred_class_arvore <- predict(modelo_arvore, newdata = teste_set)

# Previsão das PROBABILIDADES
pred_prob_arvore <- predict(modelo_arvore, newdata = teste_set, type = "prob")[, "FEMININO"]

# Matriz de Confusão
matriz_confusao_arvore <- confusionMatrix(pred_class_arvore, teste_set$Gênero_da_vítima)
print(matriz_confusao_arvore)

# Curva ROC e AUC
curva_roc_arvore <- roc(teste_set$Gênero_da_vítima, pred_prob_arvore)
print(paste("AUC:", curva_roc_arvore$auc))

# Este comando plota a curva ROC a partir do objeto 'curva_roc_arvore'
plot(curva_roc_arvore, main = "Curva ROC - Árvore de Decisão")



# Salvar o modelo de Árvore de Decisão
saveRDS(modelo_arvore, file = "modelo_arvore.rds")


####################################################
######### COMPARAÇÃO #######################


library(tibble)

# --- Calcular métricas para Regressão Logística ---
cm_rf <- confusionMatrix(pred_class, teste_set$`Gênero_da_vítima`, positive = "FEMININO")

# AUC
auc_rf <- roc(teste_set$`Gênero_da_vítima`,
              prob_fem,
              levels = c("MASCULINO", "FEMININO"))$auc


# --- Calcular métricas para Árvore de Decisão ---
cm_arvore <- confusionMatrix(pred_class_arvore, teste_set$Gênero_da_vítima)
auc_arvore <- roc(teste_set$Gênero_da_vítima, pred_prob_arvore)$auc

# --- Extrair valores da matriz de confusão ---
# cm$table é uma matriz com: 
# [1,1] = TN, [1,2] = FP, [2,1] = FN, [2,2] = TP
get_conf_vals <- function(cm) {
  mat <- as.matrix(cm$table)
  TN <- mat[1,1]
  FP <- mat[1,2]
  FN <- mat[2,1]
  TP <- mat[2,2]
  return(c(TN = TN, FP = FP, FN = FN, TP = TP))
}

vals_rf <- get_conf_vals(cm_rf)
vals_arvore <- get_conf_vals(cm_arvore)

# --- Criar tibble de comparação ---
tabela_comparacao <- tibble(
  Modelo = c("Random Forest", "Árvore de Decisão"),
  Acuracia = c(cm_rf$overall["Accuracy"], cm_arvore$overall["Accuracy"]),
  Kappa = c(cm_rf$overall["Kappa"], cm_arvore$overall["Kappa"]),
  Sensibilidade = c(cm_rf$byClass["Sensitivity"], cm_arvore$byClass["Sensitivity"]),
  Especificidade = c(cm_rf$byClass["Specificity"], cm_arvore$byClass["Specificity"]),
  AUC = c(auc_rf, auc_arvore),
  TN = c(vals_rf["TN"], vals_arvore["TN"]),
  FP = c(vals_rf["FP"], vals_arvore["FP"]),
  FN = c(vals_rf["FN"], vals_arvore["FN"]),
  TP = c(vals_rf["TP"], vals_arvore["TP"])
)

print(tabela_comparacao)

library(writexl)

# Exportar para um arquivo Excel
write_xlsx(tabela_comparacao, "tabela_comparacao_modelos.xlsx")

library(tibble)
library(dplyr)

# Função para transformar cm$table em tibble formatado
cm_to_tibble <- function(cm, modelo_nome) {
  mat <- as.data.frame.matrix(cm$table) # transforma em data frame
  mat <- tibble::rownames_to_column(mat, var = "Prediction") # adiciona coluna Prediction
  mat <- mat %>%
    mutate(Modelo = modelo_nome) %>%
    select(Modelo, everything()) # Modelo na frente
  return(mat)
}

# Criar as tabelas para cada modelo
tabela_glm_ref <- cm_to_tibble(cm_rf, "Random Forest")
tabela_arvore_ref <- cm_to_tibble(cm_arvore, "Árvore de Decisão")

# Unir tudo
tabela_referencia <- bind_rows(tabela_glm_ref, tabela_arvore_ref)

print(tabela_referencia)

write_xlsx(tabela_referencia, "tabela_referencia.xlsx")


