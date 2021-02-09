library(readxl); library(janitor); library(dplyr); library(lubridate); library(writexl); library(stringr)

dt_demandas_historico_raw <- read_excel("data-raw/respostas_faleconosco-2016-ago-2020.xlsx", 
                                        col_types = c("text", "text", "date", "date", "text", "text", "text"),
                                        .name_repair = make_clean_names)
dt_demandas_atual_raw <- read_excel("data-raw/faleconosco-relatorio-set-20-jan-21.xls", .name_repair = make_clean_names)

dt_demandas_historico <- dt_demandas_historico_raw %>% 
  mutate(protocolo = 1:n()) %>% 
  mutate(protocolo = formatC(protocolo, width = 10, flag = 0)) %>% 
  select(protocolo, 
         data_pergunta = recebimento_transparencia_ativa, 
         data_resposta = data_de_resposta, 
         pergunta = solicitacao, 
         resposta) %>% 
  mutate(data_pergunta = date(data_pergunta), 
         data_resposta = date(data_resposta)) %>% 
  mutate(assunto = "portal da transparencia") %>% 
  mutate(canal = "fale conosco - email")

dt_demandas_atual <- dt_demandas_atual_raw %>% 
  select(protocolo,
         data_pergunta = data_de_criacao,
         data_resposta = data_resposta_conclusao,
         pergunta,
         resposta,
         assunto = pertinencia_portal) %>% 
  mutate(data_pergunta = date(dmy_hms(data_pergunta)),
         data_resposta = date(mdy_hms(data_resposta))) %>% 
  mutate(assunto = ifelse(assunto == "sim", "portal da transparencia", "outros")) %>% 
  mutate(canal = "fale conosco - bpms")


dt <- rbind(dt_demandas_historico, dt_demandas_atual)

write_xlsx(dt, "data/demandas-fale-conosco-portal.xlsx")
