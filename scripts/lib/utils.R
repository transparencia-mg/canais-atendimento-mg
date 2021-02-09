mask_cpf <- function(x) {
  regex <- "((\\d{3}.?\\d{3}.?\\d{3}-?\\d{2}))"
  #x <- paste("CPF 067.558.786-77 ou ainda 067558786-77 ou mesmo 06755878677 ou talvez 067558786-77")
  stringr::str_replace_all(x, regex, "REDACTED")
}

mask_rg <- function(x) {
  # https://guilhermesteves.dev/tutoriais/regex-uteis-para-o-seu-dia-a-dia/
  regex <- " (\\d{1,2}).?\\d{3}.?(\\d{3})-?(\\d{1}|X|x)?"
  #x <- paste("RG MG 10.002.124 ou ainda 10002124")
  stringr::str_replace_all(x, regex, "REDACTED")
}

mask_tel <- function(x) {
  # https://gist.github.com/boliveirasilva/c927811ff4a7d43a0e0c
  regex <- " (?:(?:\\+|00)?(55)\\s?)?(?:\\(?([1-9][0-9])\\)?\\s?)?(?:((?:9\\d|[2-9])\\d{3})\\-?(\\d{4}))"
  stringr::str_replace_all(x, regex, "REDACTED")
}

mask_email <- function(x) {
  # https://emailregex.com/
  regex <- "(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|'(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*')@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])"
  stringr::str_replace_all(x, regex, "REDACTED")
}

# protocolos_dados_pessoais <- dt %>% 
#   tidytext::unnest_tokens(keyword, pergunta, drop = FALSE) %>% 
#   filter(keyword %in% c("cpf", "rg", "cep", "endereco", "tel", "telefone", "celular", "email", "e-mail")) %>% 
#   pull(protocolo) %>% 
#   unique()
# 
# dt_masked <- dt %>% 
#   mutate(pergunta = mask_email(pergunta),
#          pergunta = mask_cpf(pergunta),
#          pergunta = mask_rg(pergunta),
#          pergunta = mask_tel(pergunta)) %>% 
#   mutate(resposta = mask_email(resposta),
#          resposta = mask_cpf(resposta),
#          resposta = mask_rg(resposta),
#          resposta = mask_tel(resposta))

# x <- c("R$ 340.169.185.197,47 email fjunior.alves.oliveira@gmail.com 067.558.786-77.")
# 
# x %>% mask_cpf() %>% mask_rg() %>% mask_email() %>% mask_tel()
# 
# 
# x <- dt %>% filter(protocolo == "0000000010") %>% pull(pergunta)
# x <- dt %>% filter(protocolo == "0000000102") %>% pull(pergunta)
# 
# x <- dt %>% filter(protocolo == "0000000118") %>% pull(pergunta)
# 
# x <- dt %>% filter(protocolo == "0000000161") %>% pull(pergunta)
# 
# x <- dt %>% filter(protocolo == "0000000225") %>% pull(pergunta)
# 
# x <- dt %>% filter(protocolo == "0000000008") %>% pull(pergunta)
# 
# x <- dt %>% filter(protocolo == "0000000010") %>% pull(pergunta)