## Base dados aberta site: https://dados.cvm.gov.br/dataset/cia_aberta-doc-dfp

library(tidyverse)
library(readxl)

dre_2018 <- read.csv("dre_2018.csv",
                     sep = ";",
                     encoding = "latin1")

dre_2019 <- read.csv("dre_2019.csv",
                     sep = ";",
                     encoding = "latin1")

dre_2020 <- read.csv("dre_2020.csv",
                     sep = ";",
                     encoding = "latin1")

dados_cadastrais <- read.csv("dados_cvm.csv",
                             sep = ";",
                             encoding = "latin1")

glimpse(dre_2018)
glimpse(dre_2019)
glimpse(dre_2020)

dre_todos <- rbind(dre_2018,dre_2019,dre_2020)
options(max.print = 3000)
unique(dre_todos$DS_CONTA)

# Filtrando dados de receita e lucro unico para ver qual analisar.
ds_conta_receita  <- dre_todos %>% 
  filter(str_detect(DS_CONTA,"^Receita")) %>%
  distinct(CD_CONTA,DS_CONTA)  
ds_conta_lucro <- dre_todos %>% 
  filter(str_detect(DS_CONTA,"^Lucro")) %>%
  distinct(CD_CONTA,DS_CONTA)  

rm(ds_conta_receita,ds_conta_lucro)

# Analisar esses dois 3.01 - Receita  / 3.11 Lucro Prejuizo 


dados_cadastrais <- dados_cadastrais %>% 
  select (CD_CVM, SETOR_ATIV) %>% 
  filter(SETOR_ATIV != "") %>% 
  distinct()

dre_lucro <- dre_todos %>% 
  filter(CD_CONTA == "3.11")  %>% 
  left_join(dados_cadastrais, by = "CD_CVM") %>% 
  relocate(SETOR_ATIV, .after = DENOM_CIA )

dre_receita <- dre_todos %>%
  filter(CD_CONTA == "3.01") %>% 
  left_join(dados_cadastrais, by = "CD_CVM") %>% 
  relocate(SETOR_ATIV, .after = DENOM_CIA )

dre_lucro <- dre_lucro %>%
  group_by(CD_CVM) %>%
  select(-(GRUPO_DFP:DT_FIM_EXERC)) %>%
  filter(VERSAO == max(VERSAO)) %>%
  arrange(CD_CVM,DT_REFER) %>%
  ungroup()

dre_receita <- dre_receita %>% 
  group_by(CD_CVM) %>%
  select(-(GRUPO_DFP:DT_FIM_EXERC)) %>%
  filter(VERSAO == max(VERSAO)) %>%
  arrange(CD_CVM,DT_REFER) %>%
  ungroup()

  dre_lucro   <- distinct(dre_lucro)
  dre_receita <- distinct(dre_receita)

options(scipen = 999,digits = 2)

estatisticas_receita_empresa <-  dre_receita %>%
  group_by(CD_CVM,DENOM_CIA) %>%
  summarize(media = mean(VL_CONTA, na.rm = T),
            minimo = min(VL_CONTA),
            maximo = max(VL_CONTA),
            .groups = "drop") %>%
  arrange(desc(media))


estatisticas_lucro_empresa <-  dre_lucro %>%
  group_by(CD_CVM,DENOM_CIA) %>%
  summarize(media = mean(VL_CONTA,na.rm = T),
    minimo = min(VL_CONTA),
    maximo = max(VL_CONTA),
    .groups = "drop")  %>%
  arrange(desc(media))

estatisticas_lucro_setor <-  dre_lucro %>%
  group_by(SETOR_ATIV) %>%
  summarize(media = mean(VL_CONTA,na.rm = T),
            minimo = min(VL_CONTA),
            maximo = max(VL_CONTA),
            .groups = "drop")  %>%
  arrange(desc(media))


estatisticas_receita_setor <-  dre_receita %>%
  group_by(SETOR_ATIV) %>%
  summarize(media = mean(VL_CONTA, na.rm = T),
            minimo = min(VL_CONTA),
            maximo = max(VL_CONTA),
            .groups = "drop") %>%
  arrange(desc(media))


