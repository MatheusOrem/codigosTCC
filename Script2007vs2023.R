# =========================
# EXPLORATÓRIA — DF 2007 vs 2023
# =========================

# ---- Arquivos ----
ARQ_2007 <- "C:/Users/mathe/Desktop/UnB/TCC/Dados TCC/Dados TCC/Vigitel-2007-peso-rake.xls"
ARQ_2023 <- "C:/Users/mathe/Desktop/UnB/TCC/Dados TCC/Dados TCC/Vigitel-2023-peso-rake.xlsx"

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(forcats)
library(rlang)
library(broom)
library(performance)
library(pROC)
library(tibble)

# ---- Tema, tamanhos e paleta
BASE_SZ   <- 18
TITLE_SZ  <- 20
AXIS_SZ   <- 16
STRIP_SZ  <- 16
LEGEND_SZ <- 15
LABEL_SZ  <- 6.8  

theme_set(
  theme_minimal(base_size = BASE_SZ) +
    theme(
      plot.title    = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption  = element_blank()
    )
)
pal <- c("#4E79A7","#E15759","#76B7B2","#F28E2B","#59A14F",
         "#EDC949","#AF7AA1","#FF9DA7","#9C755F","#BAB0AC")

dir_out <- file.path(getwd(), "exploratoria_2007_2023")
dir.create(dir_out, showWarnings = FALSE)

# =========================
# PREPARO DOS DADOS (criar dados_all e prev_tab)
# =========================

# --- colunas e variáveis categóricas usadas no modelo ---
cols_alvo <- c("cidade","q6","q7","q16","q27","q29","r143","q35","q45","q75","q76","r205","imc","obesid")
vars_cat  <- c("q7","q16","q27","q29","r143","q35","q45","q75","q76","r205")
codigos_na <- c(77,88,99,777,888,999)

# --- rótulos para categorias (iguais aos do seu modelo) ---
labels_list <- list(
  q7  = data.frame(code=c(1,2), label=c("masculino","feminino"), ordered=FALSE),
  q16 = data.frame(code=1:6, label=c("1 a 2 dias/sem","3 a 4 dias/sem","5 a 6 dias/sem",
                                     "todos os dias","quase nunca","nunca"), ordered=TRUE),
  q27 = data.frame(code=1:6, label=c("1 a 2 dias/sem","3 a 4 dias/sem","5 a 6 dias/sem",
                                     "todos os dias","quase nunca","nunca"), ordered=TRUE),
  q29 = data.frame(code=1:6, label=c("1 a 2 dias/sem","3 a 4 dias/sem","5 a 6 dias/sem",
                                     "todos os dias","quase nunca","nunca"), ordered=TRUE),
  r143= data.frame(code=1:6, label=c("1 a 2 dias/sem","3 a 4 dias/sem","5 a 6 dias/sem",
                                     "todos os dias","quase nunca","nunca"), ordered=TRUE),
  q35 = data.frame(code=c(1,2,888), label=c("sim","não","não quis informar"), ordered=FALSE),
  q45 = data.frame(code=1:4, label=c("1 a 2 dias/sem","3 a 4 dias/sem","5 a 6 dias/sem","todos os dias"),
                   ordered=TRUE),
  q75 = data.frame(code=c(1,2,777), label=c("sim","não","não lembra"), ordered=FALSE),
  q76 = data.frame(code=c(1,2,777), label=c("sim","não","não lembra"), ordered=FALSE),
  r205= data.frame(code=c(1,2,3), label=c("sim","não","não lembra"), ordered=FALSE)
)

aplica_rotulos <- function(df, lab){
  for(v in intersect(names(lab), names(df))){
    lv  <- suppressWarnings(as.numeric(lab[[v]]$code))
    lb  <- lab[[v]]$label
    ord <- isTRUE(lab[[v]]$ordered[1])
    df[[v]] <- factor(df[[v]], levels = lv, labels = lb, ordered = ord)
  }
  df
}

# --- leitura e higienização por ano ---
ler_ano <- function(arq, ano){
  s <- readxl::excel_sheets(arq)[1]
  df <- readxl::read_excel(arq, sheet = s, progress = TRUE) |>
    dplyr::select(any_of(cols_alvo)) |>
    dplyr::mutate(
      cidade = suppressWarnings(as.numeric(gsub("\\D","", as.character(cidade)))),
      dplyr::across(any_of(vars_cat), ~ ifelse(. %in% codigos_na, NA, .))
    ) |>
    dplyr::filter(cidade == 27) |>
    aplica_rotulos(labels_list) |>
    dplyr::mutate(
      q6     = suppressWarnings(as.numeric(q6)),
      imc    = suppressWarnings(as.numeric(imc)),
      obesid = suppressWarnings(as.numeric(obesid)),
      ano    = factor(ano)
    ) |>
    tidyr::drop_na()
  df
}

# --- ler as duas bases, juntar e criar fatores auxiliares ---
dados_2007 <- ler_ano(ARQ_2007, 2007)
dados_2023 <- ler_ano(ARQ_2023, 2023)

dados_all <- dplyr::bind_rows(dados_2007, dados_2023) |>
  dplyr::mutate(obesid_f = factor(obesid, levels = c(0,1), labels = c("Não obeso","Obeso")))

# --- tabela de prevalência por ano (objeto que faltava) ---
prev_tab <- dados_all |>
  dplyr::group_by(ano) |>
  dplyr::summarise(
    n = dplyr::n(),
    prev = mean(obesid == 1),
    prev_pct = round(100 * prev, 2),
    .groups = "drop"
  )
print(prev_tab)


# =========================
# 1) Prevalência por ano (gráfico + CSV)
# =========================
readr::write_csv(prev_tab, file.path(dir_out, "prevalencia_por_ano.csv"))

g_prev <- ggplot(prev_tab, aes(x = ano, y = prev, fill = ano)) +
  geom_col(width = 0.52) +
  geom_text(aes(label = scales::percent(prev, accuracy = 0.1)),
            vjust = -0.35, size = LABEL_SZ + 0.8, fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, max(prev_tab$prev) * 1.25)) +
  scale_fill_manual(values = pal, guide = "none") +
  labs(x = NULL, y = "Prevalência de obesidade") +   
  theme(
    plot.title   = element_blank(),                 
    axis.text.x  = element_text(size = AXIS_SZ + 2, face = "bold"),
    axis.text.y  = element_text(size = AXIS_SZ),
    axis.title.y = element_text(size = AXIS_SZ + 1, face = "bold")
  )

print(g_prev)
ggsave(file.path(dir_out, "prev_obesidade_por_ano.png"), g_prev, width = 8, height = 5, dpi = 300)

# =========================
# 2) Boxplots (idade e IMC) facetados por ano
# =========================
if ("q6" %in% names(dados_all)) {
  g_age <- ggplot(dados_all, aes(x = obesid_f, y = q6, fill = obesid_f)) +
    geom_boxplot(alpha = .9, width = .55, outlier.alpha = .35) +
    facet_wrap(~ ano) +
    scale_fill_manual(values = pal[1:2], guide = "none") +
    labs(x = "Obesidade", y = "Idade (anos)") +        
    theme(
      plot.title  = element_blank(),                   
      axis.text.x = element_text(size = AXIS_SZ, face = "bold"),
      axis.text.y = element_text(size = AXIS_SZ),
      axis.title  = element_text(size = AXIS_SZ + 1, face = "bold"),
      strip.text  = element_text(size = STRIP_SZ, face = "bold")
    )
  
  print(g_age)
  ggsave(file.path(dir_out, "boxplot_idade_por_obesidade.png"), g_age, width = 9, height = 5.5, dpi = 300)
}

if ("imc" %in% names(dados_all)) {
  g_imc <- ggplot(dados_all, aes(x = obesid_f, y = imc, fill = obesid_f)) +
    geom_boxplot(alpha = .9, width = .55, outlier.alpha = .35) +
    facet_wrap(~ ano) +
    scale_fill_manual(values = pal[1:2], guide = "none") +
    labs(x = "Obesidade", y = "IMC (kg/m²)") +         
    theme(
      plot.title  = element_blank(),                   
      axis.text.x = element_text(size = AXIS_SZ, face = "bold"),
      axis.text.y = element_text(size = AXIS_SZ),
      axis.title  = element_text(size = AXIS_SZ + 1, face = "bold"),
      strip.text  = element_text(size = STRIP_SZ, face = "bold")
    )
  
  print(g_imc)
  ggsave(file.path(dir_out, "boxplot_imc_por_obesidade.png"), g_imc, width = 9, height = 5.5, dpi = 300)
}

# =========================
# 3) Categóricas — barras 100% por ano e por obesidade dentro do ano
# =========================
vars_cat_presentes <- intersect(c("q7","q16","q27","q29","r143","q35","q45","q75","q76","r205"),
                                names(dados_all))

var_lab <- c(q7="Sexo",
             q16="Frequência de hortaliças",
             q27="Frequência de frutas",
             q29="Frequência de refrigerantes",
             r143="Consumo de doces",
             q35="Bebida alcoólica",
             q45="Frequência de exercício",
             q75="Pressão alta",
             q76="Diabetes",
             r205="Já teve depressão",
             obesid_f = "Obesidade")

# ========= PATCH: funções de barras 100% com rótulos corretos =========

# helper para imprimir porcentagens só se a fatia for >= 3%
.lbl_pct <- function(p) ifelse(p < 0.03, "", scales::percent(p, accuracy = 1))

# ----- distribuição por ano (100%) -----
plot_cat_ano <- function(var){
  rot <- if (!is.na(var_lab[var])) var_lab[var] else var
  agg <- dados_all |>
    dplyr::count(ano, nivel = .data[[var]], name = "n") |>
    dplyr::group_by(ano) |>
    dplyr::mutate(prop = n / sum(n)) |>
    dplyr::ungroup()
  
  levs <- levels(dados_all[[var]])
  agg$nivel <- factor(as.character(agg$nivel), levels = levs)
  
  ggplot(agg, aes(x = ano, y = prop, fill = nivel)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = .lbl_pct(prop), group = nivel),
              position = position_stack(vjust = 0.5),
              size = LABEL_SZ, fontface = "bold") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1), expand = expansion(mult = c(0, .02))) +
    scale_fill_manual(values = pal, drop = FALSE, breaks = levs) +
    labs(x = "Ano", y = "Proporção", fill = rot) +   
    theme(
      plot.title   = element_blank(),                
      axis.text.x  = element_text(size = AXIS_SZ, face = "bold"),
      axis.text.y  = element_text(size = AXIS_SZ),
      axis.title.x = element_text(size = AXIS_SZ, face = "bold"),
      axis.title.y = element_text(size = AXIS_SZ, face = "bold"),
      legend.title = element_text(size = AXIS_SZ, face = "bold"),
      legend.text  = element_text(size = AXIS_SZ - 1)
    )
}


# ----- distribuição por obesidade dentro do ano (100%) -----
plot_cat_por_obes_ano <- function(var){
  rot <- if (!is.na(var_lab[var])) var_lab[var] else var
  agg <- dados_all |>
    dplyr::count(ano, obesid_f, nivel = .data[[var]], name = "n") |>
    dplyr::group_by(ano, obesid_f) |>
    dplyr::mutate(prop = n / sum(n)) |>
    dplyr::ungroup()
  
  levs <- levels(dados_all[[var]])
  agg$nivel <- factor(as.character(agg$nivel), levels = levs)
  
  ggplot(agg, aes(x = obesid_f, y = prop, fill = nivel)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = .lbl_pct(prop), group = nivel),
              position = position_stack(vjust = 0.5),
              size = LABEL_SZ, fontface = "bold") +
    facet_wrap(~ ano, nrow = 1) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1), expand = expansion(mult = c(0, .02))) +
    scale_fill_manual(values = pal, drop = FALSE, breaks = levs) +
    labs(x = "Obesidade", y = "Proporção dentro do grupo", fill = rot) +  
    theme(
      plot.title   = element_blank(),                                      
      axis.text.x  = element_text(size = AXIS_SZ, face = "bold"),
      axis.text.y  = element_text(size = AXIS_SZ),
      axis.title.x = element_text(size = AXIS_SZ, face = "bold"),
      axis.title.y = element_text(size = AXIS_SZ, face = "bold"),
      legend.title = element_text(size = AXIS_SZ, face = "bold"),
      legend.text  = element_text(size = AXIS_SZ - 1),
      strip.text   = element_text(size = STRIP_SZ, face = "bold")
    )
}


# =========================
# 4) GERAR TODOS OS GRÁFICOS CATEGÓRICOS
# =========================

# utilitário para nomes de arquivo
.sanitize <- function(x){
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("_+$", "", x)
  x
}


# garante que a pasta exista
dir.create(dir_out, showWarnings = FALSE)

# --- 4.1 Distribuição por ANO (100%) ---
plots_ano <- lapply(vars_cat_presentes, function(v){
  p <- plot_cat_ano(v)
  fn <- file.path(dir_out, paste0("cat_", .sanitize(v), "_por_ano_100.png"))
  ggsave(fn, p, width = 10, height = 6, dpi = 300)
  p
})

# imprimir todos no device
invisible(lapply(plots_ano, print))

# --- 4.2 Distribuição por OBESIDADE dentro do ano (100%) ---
plots_por_obes <- lapply(vars_cat_presentes, function(v){
  p <- plot_cat_por_obes_ano(v)
  fn <- file.path(dir_out, paste0("cat_", .sanitize(v), "_por_obesidade_ano_100.png"))
  ggsave(fn, p, width = 12, height = 6, dpi = 300)
  p
})

invisible(lapply(plots_por_obes, print))

#depressão de 2023 apenas
library(scales)

dep23 <- dados_all %>%
  filter(ano == 2023) %>%
  count(nivel = r205, name = "n") %>%
  mutate(
    prop  = n / sum(n),
    nivel = factor(as.character(nivel),
                   levels = c("sim", "não", "não lembra"))
  )

cores_dep <- c("sim" = "#4E79A7", "não" = "#E15759", "não lembra" = "#BAB0AC")

g_dep23 <- ggplot(dep23, aes(x = factor("2023"), y = prop, fill = nivel)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = percent(prop, accuracy = 1), group = nivel),
            position = position_stack(vjust = 0.5),
            size = LABEL_SZ, fontface = "bold") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, .02))) +
  scale_fill_manual(values = cores_dep, name = "Já teve depressão") +
  labs(x = NULL, y = "Proporção") +                 
  theme(
    plot.title   = element_blank(),               
    axis.text.x  = element_text(size = AXIS_SZ + 2, face = "bold"),
    axis.text.y  = element_text(size = AXIS_SZ),
    axis.title.y = element_text(size = AXIS_SZ + 1, face = "bold")
  )

print(g_dep23)

# --- Depressão (r205) vs Obesidade 
dep23_obes <- dados_all %>%
  dplyr::filter(ano == 2023) %>%
  dplyr::mutate(
    r205 = forcats::fct_relevel(r205, "não", "sim", "não lembra")  # ordem da legenda
  ) %>%
  dplyr::count(obesid_f, r205, name = "n") %>%
  dplyr::group_by(obesid_f) %>%
  dplyr::mutate(prop = n / sum(n)) %>%
  dplyr::ungroup()

g_dep23_obes <- ggplot(dep23_obes, aes(x = obesid_f, y = prop, fill = r205)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = scales::percent(prop, accuracy = 1), group = r205),
            position = position_stack(vjust = 0.5),
            size = LABEL_SZ, fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, .02))) +
  scale_fill_manual(values = c("não" = "#E15759", "sim" = "#4E79A7", "não lembra" = "#BAB0AC"),
                    name = "Já teve depressão") +
  labs(x = "Obesidade", y = "Proporção dentro do grupo") +
  theme(
    plot.title   = element_blank(),                 
    axis.text.x  = element_text(size = AXIS_SZ + 2, face = "bold"),
    axis.text.y  = element_text(size = AXIS_SZ),
    axis.title.y = element_text(size = AXIS_SZ + 1, face = "bold")
  )

print(g_dep23_obes)
ggsave(file.path(dir_out, "depressao_vs_obesidade_2023.png"),
       g_dep23_obes, width = 8, height = 5, dpi = 300)


