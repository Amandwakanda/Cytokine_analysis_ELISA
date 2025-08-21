# ===================================================================
#   SCRIPT MESTRE PARA ANÁLISE DE DADOS DE ELISA
#   Autor: Amanda Lima
#   Projeto: Adjuvants
#   Data: 13 de agosto de 2025
#   Vs: 4.0
#   Descrição: Versão refatorada com função mestre para automação.
# --------------------------------------------------------------------
# Carregamento de Pacotes
# --------------------------------------------------------------------
library(tidyverse)
library(openxlsx)
library(readxl)
library(ggpubr)

#--------------------------------------------------
# FUNÇÃO - TEMA PERSONALIZADO PARA OS GRÁFICOS
# --------------------------------------------------------------------
theme_CTV <- function(base_size = 14) {
  theme_classic(base_size = base_size) +
    theme(
      # Títulos e Legendas
      plot.title = element_text(hjust = 0.5, size = rel(2), face = "bold", margin = margin(b = 10)),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = rel(1.0)),
      
      # Eixos
      axis.title.y = element_text(size = rel(1.1), margin = margin(r = 10)),
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = rel(1.3)),
      axis.text.y = element_text(size = rel(1.5)),
      
      # Títulos das Facetas
      strip.text = element_text(size = rel(1.1), face = "bold", margin = margin(t = 2)),
      strip.background = element_rect(linetype = "blank")
    )
}

# ===================================================================
# FUNÇÃO MESTRE PARA ANÁLISE COMPLETA DE UMA CITOCINA
# ===================================================================
analise_elisa <- function(arquivo_excel,
                          aba,
                          ordem_tratamento,
                          ordem_fenotipo,
                          paleta,
                          excluir_tratamento,
                          renomear_tratamento = NULL,
                          titulo,
                          caminho_salvar) {
  
  # --------------------------------------------------------------------
  # Mensagem de Inicio
  # --------------------------------------------------------------------
  cat(paste("\n\n--- Iniciando análise para:", aba, "---\n"))
  
  # --------------------------------------------------------------------
  # 1. LEITURA E PREPARAÇÃO DOS DADOS
  # --------------------------------------------------------------------
  wb <- openxlsx::loadWorkbook(arquivo_excel)
  tabs <- getTables(wb, sheet = aba)
  intervalo_tabs <- attr(tabs, "refs")
  names(intervalo_tabs) <- tabs
  
  lista_tabs <- map(intervalo_tabs, ~ read_excel(
    path = arquivo_excel, 
    sheet = aba, 
    range = .x, 
    col_names = TRUE,
    col_types = "text"
  ) %>%
    mutate(across(starts_with("R"), as.numeric)))
  
  #>> CORREÇÃO: A função do dplyr é 'list_rbind()', não 'list_rbind(names = ...)'
  dados <- list_rbind(lista_tabs, names_to = "Experimento") %>%
    filter(!if_all(starts_with("R"), is.na)) %>%
    filter(!Treatment %in% excluir_tratamento)
  
  # Dados Longo com Outlier = dados_l_outlier
  #>> CORREÇÃO: Toda a manipulação de colunas precisa estar dentro de um único 'mutate()', com as operações separadas por vírgulas.
  dados_l_outlier <- dados %>% 
    mutate(
      Experimento = stringr::str_replace_all(Experimento, "_", " "),
      Treatment = as.character(Treatment),
      Treatment = if (!is.null(renomear_tratamento)) {
        recode(Treatment, !!!renomear_tratamento)
      } else {
        Treatment
      },
      #>> CORREÇÃO: Usando os nomes dos argumentos passados para a função (ex: 'ordem_fenotipo')
      Fenotype = factor(Fenotype, levels = ordem_fenotipo),
      Treatment = factor(Treatment, levels = ordem_tratamento)
    ) %>% 
    pivot_longer(
      cols = c(R1, R2, R3),
      names_to = "Replica",
      #>> CORREÇÃO: Corrigido typo "COncentration" para "Concentration"
      values_to = "Concentration"
    ) %>% 
    mutate(Concentration = Concentration * 2)
  
  # --------------------------------------------------------------------
  # Dados Long (dados_l): Dados Longos final - Sem outliers
  # --------------------------------------------------------------------
  dados_l <- dados_l_outlier %>%
    group_by(Experimento, Day, Fenotype, Treatment) %>%
    mutate(
      Q1 = quantile(Concentration, 0.25, na.rm = TRUE),
      Q3 = quantile(Concentration, 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      limite_inferior = Q1 - 1 * IQR,
      limite_superior = Q3 + 1 * IQR
    ) %>%
    filter(Concentration >= limite_inferior & Concentration <= limite_superior) %>%
    ungroup() %>%
    select(-c(Q1, Q3, IQR, limite_inferior, limite_superior))
  
  # --------------------------------------------------------------------
  # Dados Wide (dados_w): Média e Desvio Padrão - Para gráfico pool
  # --------------------------------------------------------------------
  dados_w <- dados_l %>%
    group_by(Experimento, Day, Fenotype, Treatment) %>%
    summarise(
      media = mean(Concentration, na.rm = TRUE),
      sd = sd(Concentration, na.rm = TRUE),
      n_replicas = n(),
      sem = sd / sqrt(n_replicas),
      .groups = "drop"
    )
  
  # --------------------------------------------------------------------
  # Dados Agrupados (dados_pool): Cálculo de Estatísticas
  # --------------------------------------------------------------------
  #>> CORREÇÃO: A função do dplyr é 'summarise' com 's' minúsculo.
  dados_pool <- dados_w %>% 
    group_by(Day, Fenotype, Treatment) %>% 
    summarise(
      media_pool = mean(media, na.rm = TRUE),
      sd_pool = sd(media, na.rm = TRUE),
      n_experimentos = n(),
      sem_pool = sd_pool / sqrt(n_experimentos),
      .groups = "drop"
    )
# --------------------------------------------------------------------
# Gráfico Pool (gg_pool)
# --------------------------------------------------------------------
  gg_pool <- ggplot(data = dados_pool,
                    aes(x = Fenotype, 
                        #color = Treatment, 
                        group = Treatment)) +
    
    geom_col(aes(y= media_pool, 
                 fill = Treatment),
             color = "black",
             position = position_dodge(width = 0.9),
             width = 0.9,
             alpha = 0.3) +
    
    geom_jitter(data = dados_w,
                aes(y = media, 
                    color = Treatment),
                position = position_dodge(0.9),
                size = 3, 
                alpha = 0.8,
                show.legend = FALSE,
                na.rm = TRUE) +
    
    # --------------------------------------------------------------------
  # Barra de Erros
  # --------------------------------------------------------------------
  geom_errorbar(aes(x = Fenotype, ymin = media_pool, 
                    ymax = media_pool + sem_pool, 
                    group = Treatment),
                width = 0.3, 
                position = position_dodge(0.9),
                color = "black",
                show.legend = FALSE) +
    
    # --------------------------------------------------------------------
  # Cores, escalas e facetas
  # -------------------------------------------------------------------- 
  scale_color_manual(values = paleta) +
    scale_fill_manual(values = paleta)+
    facet_wrap(~ Day, ncol = 2,scales = "free", axes = "all_y") +
    coord_cartesian(ylim = c(0, NA), clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
    guides(color = guide_legend(override.aes = list(size = 7))) +
    
    # --------------------------------------------------------------------
  # Estatística:
  # --------------------------------------------------------------------
  ggpubr::geom_pwc(data = dados_w,
                   aes(x = Fenotype, y = media),
                   method = "dunn_test", 
                   p.adjust.method = "bonferroni",
                   hide.ns = "p.adj",
                   label = "p.adj.signif", 
                   size = 0.8, 
                   label.size = 5) +
    # --------------------------------------------------------------------
  # Titulos e Etiquetas
  # --------------------------------------------------------------------
  
  labs(
    title = titulo,
    x = NULL,
    y = "Concentration (pg/mL)",
    color = NULL) +
    
# --------------------------------------------------------------------
# Aplicando edição gráfica
# --------------------------------------------------------------------
  theme_CTV()
  ggsave(
    filename = file.path(caminho_salvar, paste0(aba, "_pool.png")),
    plot = gg_pool,
    width = 16, height = 8, dpi = 300)
# --------------------------------------------------------------------
# Gráficos de Cada experimento (gg_individuais)
# --------------------------------------------------------------------

  gg_individuais <- ggplot(data = dados_w, 
                           aes(x = Fenotype, color = Treatment, 
                               group = Treatment)) + 
    
    geom_col( aes(y = media),
              fill = "white", # Define o preenchimento como branco, fora do aes()
              color = "black", # Define a cor do contorno da barra
              position = position_dodge(width = 0.9), 
              width = 0.9) +
    
    geom_jitter(data = dados_l,
                aes(y = Concentration),
                position = position_dodge(0.9),
                size = 2,
                alpha = 0.8,
                na.rm = TRUE) +
    # --------------------------------------------------------------------
  # Barra de Erros
  # --------------------------------------------------------------------
  geom_errorbar(aes(x = Fenotype, ymin = media, 
                    ymax = media + sem, 
                    group = Treatment),
                width = 0.8, position = position_dodge(0.9),
                show.legend = FALSE) +
    
    # --------------------------------------------------------------------
  # Cores, escalas e facetas
  # --------------------------------------------------------------------
  scale_color_manual(values = paleta) +
    facet_wrap(~ Experimento + Day, ncol = 2, scales = "free") +
    coord_cartesian(ylim = c(0, NA), clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    guides(color = guide_legend(override.aes = list(size = 6))) +
    
    # --------------------------------------------------------------------
  # CONTROLE Dos Titulos
  # --------------------------------------------------------------------
  labs(
    title = titulo,
    x = NULL,
    y = "Concentration (pg/mL)",
    color = "Treatment") +
    
# --------------------------------------------------------------------
# Aparencia
# --------------------------------------------------------------------
theme_CTV()
  
# --------------------------------------------------------------------
# Salvando:
# --------------------------------------------------------------------
  ggsave(
    filename = file.path(caminho_salvar, paste0(aba, "_individuais.png")),
    plot = gg_individuais,
    width = 10, height = 15, dpi = 300)
# --------------------------------------------------------------------
# A função retorna os objetos criados
  return(list(
    dados_l = dados_l,
    dados_w = dados_w,
    dados_pool = dados_pool,
    grafico_pool = gg_pool
  ))}
# ===================================================================
# PAINEL DE CONTROLE - EXECUÇÃO DAS ANÁLISES
# ===================================================================

# --- Parâmetros Gerais ---
caminho_arquivo_excel <- "C:/Users/mairo/OneDrive/Desktop/OneDrive/Área de Trabalho/Adjuvants/Data_ELISA.xlsx"
caminho_para_salvar_graficos <- "../Graficos_ELISA"
# --------------------------------------------------------------------
# --- Parâmetros Específicos para a Análise de TNF com mRNA ---
# --------------------------------------------------------------------
aba_wb <- "SPN_IL12"
titulo_principal <- "IL12 - BMDM"
# --------------------------------------------------------------------
#>> CORREÇÃO: Renomeando variáveis para evitar conflitos de nomes (ex: ordem_de_tratamento -> ordem_tratamento_mrna)
ordem_de_tratamento <- c("Media", "LNP-CT")
ordem_fenotipo_geral <- c("WT", "Myd88 ko", "TLR4 ko", "CD14 ko")
paleta_CT <- c(
  "Media" = "#A6A5A4",
  "LPS" = "#DD743B",
  "LNP-CT" = "#2EB260"
)
excluir_tratamento_mrna <- c("LPS", "SM102 Ø", "SM102", "SM102 mRNA-Luc", "SM102 2%SFB", "SM102 10%SFB", "SM102 new", "SM102 old" )
# --------------------------------------------------------------------
tratamentos_renomeados_mrna <- c(
  "LNP-CT mRNA-Luc" = "LNP-CT",
  "LNP-CT Ø" = "LNP-CT",
  "LNP CT 10%SFB" = "LNP-CT",
  "LNP CT 2%SFB" = "LNP-CT",
  "LNP-CT old" = "LNP-CT",
  "LNP-CT new" = "LNP-CT")

# --- Executar a Análise ---
#>> CORREÇÃO: Passando os nomes corretos dos argumentos para a função
resultados <- analise_elisa(
  arquivo_excel = caminho_arquivo_excel,
  aba = 
    aba_wb,
  ordem_tratamento = ordem_de_tratamento,
  ordem_fenotipo = ordem_fenotipo_geral,
  paleta = paleta_CT,
  excluir_tratamento = excluir_tratamento_mrna,
  renomear_tratamento = tratamentos_renomeados_mrna,
  titulo = titulo_principal,
  caminho_salvar = caminho_para_salvar_graficos
)

cat("--- Salvando objetos da análise para uso posterior ---\n")

dados_l    <- resultados$dados_l
dados_w    <- resultados$dados_w
dados_pool <- resultados$dados_pool
save(
  dados_l,
  dados_w,
  dados_pool,
  paleta_CT,
  theme_CTV,
  aba_wb,
  titulo_principal,
  file = "Resultados_Analise_Principal.RData" # Salva tudo em um único arquivo
)

cat("--- Análise principal concluída e resultados salvos! ---\n")

# Certifique-se de que a biblioteca está carregada
library(openxlsx)

# --- Painel de Controle para Exportação ---

# 1. Defina qual dataframe você quer salvar (acessando a partir da lista 'resultados')
dataframe_para_salvar <- resultados$dados_l

# 2. Defina o nome do arquivo Excel de saída
nome_arquivo_saida <- "dados_l_processados_SPN_IL12.xlsx"

# 3. Defina a pasta onde o arquivo será salvo
caminho_para_salvar <- "../Graficos_ELISA/" # Ou qualquer outra pasta


# --- Execução da Exportação ---
# Cria o caminho completo do arquivo
caminho_completo <- file.path(caminho_para_salvar, nome_arquivo_saida)

# Escreve o dataframe para o arquivo .xlsx
write.xlsx(dataframe_para_salvar, file = caminho_completo, asTable = TRUE)

cat("Arquivo salvo com sucesso em:", caminho_completo, "\n")
