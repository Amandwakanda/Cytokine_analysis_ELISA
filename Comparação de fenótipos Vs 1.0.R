# ===================================================================
#   SCRIPT PARA GRÁFICOS COMPARATIVOS (EX: WT vs KO)
#   Descrição: Carrega os dados da análise principal e gera
#              gráficos específicos comparando fenótipos com WT.
# ===================================================================

# --------------------------------------------------------------------
# 1. CONFIGURAÇÃO INICIAL
# --------------------------------------------------------------------
# Carregamento de Pacotes
library(tidyverse)
library(ggpubr)
library(rstatix) 

# Carrega TODOS os objetos salvos pelo script principal
load("Resultados_Analise_Principal.RData")

## ===================================================================
# FUNÇÃO AUXILIAR PARA CRIAR GRÁFICOS COMPARATIVOS (VERSÃO APENAS VISUAL)
# ===================================================================
criar_grafico_comparativo <- function(dados_w_orig, 
                                      dados_pool_orig, 
                                      fenotipo_alvo, 
                                      titulo_do_grafico,
                                      paleta_de_cores) {
  
  # --- 1. Bases Filtradas ---
  # A lógica de filtragem para selecionar os dados corretos permanece a mesma.
  fenotipos_a_manter <- c("WT", fenotipo_alvo)
  
  dados_pool_filtrado <- dados_pool_orig %>%
    filter(Fenotype %in% fenotipos_a_manter)
  
  dados_w_filtrado <- dados_w_orig %>%
    filter(Fenotype %in% fenotipos_a_manter) %>%
    group_by(Experimento, Day, Treatment) %>%
    filter(all(fenotipos_a_manter %in% Fenotype)) %>%
    ungroup()
  
  # --- 2. Criação do Gráfico ---
  # O gráfico é criado sem as camadas de estatística.
  grafico <- ggplot(data = dados_pool_filtrado, aes(x = Fenotype, y = media_pool, fill = Treatment)) +
    geom_col(
      color = "black",
      position = position_dodge(width = 0.9),
      width = 0.9,
      alpha = 0.3) +
    
    geom_errorbar(
      aes(ymin = media_pool, ymax = media_pool + sem_pool), # Corrigido para erro simétrico
      width = 0.28, 
      position = position_dodge(0.9),
      color = "black",
      show.legend = FALSE) +
    
    geom_jitter(
      data = dados_w_filtrado,
      aes(x = Fenotype, y = media, color = Treatment),
      inherit.aes = FALSE, 
      size = 2.5,
      position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.9),
      show.legend = FALSE
    ) +
    
    # A camada 'stat_pvalue_manual' foi REMOVIDA.
    
    # --- 3. Camadas de Escala e Controle ---
    facet_wrap(~ Day, ncol = 2, scales = "free_y") +
    scale_color_manual(values = paleta_de_cores, drop = FALSE) +
    scale_fill_manual(values = paleta_de_cores, drop = FALSE) +
    # Reduzimos a expansão, já que não precisamos de espaço para as anotações
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
    coord_cartesian(ylim = c(0, NA), clip = "off") +
    labs(
      title = titulo_do_grafico,
      x = NULL,
      y = "Concentration (pg/mL)",
      fill = "Treatment"
    ) +
    theme_CTV()
  
  # A função retorna o gráfico e os dataframes, como antes.
  return(list(
    grafico_final = grafico,
    dados_w_filtrados = dados_w_filtrado,
    dados_pool_filtrados = dados_pool_filtrado
  ))
}

# --------------------------------------------------------------------
# 3. PAINEL DE CONTROLE
# --------------------------------------------------------------------
fenotipos_comparativos <- c("Myd88 ko", "TLR4 ko", "CD14 ko")
titulo_base <-  titulo_principal
caminho_para_salvar <- paste0("../Graficos_ELISA/", aba_wb)

#>> APRIMORAMENTO: Definir o prefixo no painel de controle é mais organizado
prefixo_arquivo <- aba_wb

# --------------------------------------------------------------------
# 4. EXECUÇÃO AUTOMÁTICA
# --------------------------------------------------------------------
resultados_comparativos <- purrr::map(fenotipos_comparativos, function(ko) {
  
  titulo_grafico_atual <- paste(titulo_base, "- WT vs", ko)
  nome_arquivo_atual <- file.path(caminho_para_salvar,
                                  paste0(prefixo_arquivo, "_WT_vs_", gsub(" ", "_", ko), ".png"))
  
  cat(paste("--- Gerando resultados para:", titulo_grafico_atual, "---\n"))
  
  resultados_da_funcao <- criar_grafico_comparativo(
    dados_w_orig = dados_w,
    dados_pool_orig = dados_pool,
    fenotipo_alvo = ko,
    titulo_do_grafico = titulo_grafico_atual,
    paleta_de_cores = paleta_CT
  )
  
  #>> CORREÇÃO: Você precisa IMPRIMIR o objeto de gráfico que está DENTRO da lista
  print(resultados_da_funcao$grafico_final)
  
  ggsave(
    filename = nome_arquivo_atual,
    plot = resultados_da_funcao$grafico_final,
    width = 10,
    height = 7,
    dpi = 300
  )
  
  return(resultados_da_funcao)
})

names(resultados_comparativos) <- fenotipos_comparativos

cat("\n--- Processo de geração de gráficos comparativos concluído! ---\n")



