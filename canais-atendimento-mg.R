library("tidytext"); library("dplyr"); library("stringi"); library("writexl"); library("readxl")

df_raw <- read_excel("data/relatorio.xls")

names(df_raw) <- janitor::make_clean_names(names(df_raw))

df <- df_raw %>% 
      select(protocolo, pergunta)

df_tidy <- df %>% 
          unnest_tokens(keyword, pergunta) %>% # extrair todas as palavras da pergunta
          mutate(keyword = stri_trans_general(keyword, "latin-ascii")) # remover acentos

# remover palavras que nao auxiliam o processo de classificacao
stopwords <- read.csv2("data/stopwords.txt", stringsAsFactors = FALSE)

df_tidy <- anti_join(df_tidy, stopwords, by = c("keyword"))

df_tidy %>%
  count(keyword, sort = TRUE) %>% 
  mutate(part = n / sum(n))

# excel para gerar tabela dinamica que permite filtro por palavra chave
left_join(df_tidy, df, by = "protocolo") %>% write_xlsx("data/keywords.xlsx")
  