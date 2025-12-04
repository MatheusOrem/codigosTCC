# =========================
# VIGITEL 2023 — DF (cidade==27)
# =========================

# ==== Parâmetros ====
ANO     <- 2023
ARQUIVO <- "C:/Users/mathe/Desktop/UnB/TCC/Dados TCC/Dados TCC/Vigitel-2023-peso-rake.xlsx"
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
# ==== Colunas de interesse ====
cols <- c("cidade","q6","q7","q16","q27","q29","r143","q35","q45","q75","q76","r205","imc","obesid","pesorake")

# ==== 1) Importa base inteira e seleciona colunas ====
dados <- read_xlsx(path = ARQUIVO, sheet = 1, progress = TRUE) %>%
  dplyr::select(any_of(cols))

# se 'cidade' vier como texto do tipo "27 - DF", converte:
if ("cidade" %in% names(dados) && !is.numeric(dados$cidade)) {
  dados <- dados %>% mutate(cidade = as.numeric(gsub("\\D", "", cidade)))
}

# ==== 2) Filtra DF e trata NAs (após tratar códigos especiais) ====
codigos_na <- c(77,88,99,777,888,999)
vars_cat   <- intersect(c("q7","q16","q27","q29","r143","q35","q45","q75","q76","r205"),
                        names(dados))

dados <- dados %>%
  mutate(across(any_of(vars_cat), ~ ifelse(. %in% codigos_na, NA, .))) %>%
  filter(cidade == 27)

# ==== 3) Dicionários e rótulos ====
labels_list <- list(
  q7  = data.frame(code=c(1,2), label=c("masculino","feminino"), ordered=FALSE),
  q16 = data.frame(code=1:6, label=c("1 a 2 dias por semana","3 a 4 dias por semana",
                                     "5 a 6 dias por semana",
                                     "todos os dias (inclusive sábado e domingo)",
                                     "quase nunca","nunca"), ordered=TRUE),
  q27 = data.frame(code=1:6, label=c("1 a 2 dias por semana","3 a 4 dias por semana",
                                     "5 a 6 dias por semana",
                                     "todos os dias (inclusive sábado e domingo)",
                                     "quase nunca","nunca"), ordered=TRUE),
  q29 = data.frame(code=1:6, label=c("1 a 2 dias por semana","3 a 4 dias por semana",
                                     "5 a 6 dias por semana",
                                     "todos os dias (inclusive sábado e domingo)",
                                     "quase nunca","nunca"), ordered=TRUE),
  r143 = data.frame(code=1:6, label=c("1 a 2 dias por semana","3 a 4 dias por semana",
                                      "5 a 6 dias por semana",
                                      "todos os dias (inclusive sábado e domingo)",
                                      "quase nunca","nunca"), ordered=TRUE),
  q35 = data.frame(code=c(1,2,888), label=c("sim","não","não quis informar"), ordered=FALSE),
  q45 = data.frame(code=c(1,2,3,4), label=c("1 a 2 dias por semana","3 a 4 dias por semana",
                                            "5 a 6 dias por semana",
                                            "todos os dias (inclusive sábado e domingo)"),
                   ordered=TRUE),
  q75 = data.frame(code=c(1,2,777), label=c("sim","não","não lembra"), ordered=FALSE),
  q76 = data.frame(code=c(1,2,777), label=c("sim","não","não lembra"), ordered=FALSE),
  r205= data.frame(code=c(1,2,3),  label=c("sim","não","não lembra"), ordered=FALSE)
)

aplica_rotulos <- function(df, lab_list) {
  for (v in intersect(names(lab_list), names(df))) {
    dsub <- lab_list[[v]]
    dsub$code <- suppressWarnings(as.numeric(dsub$code))
    ord <- isTRUE(dsub$ordered[1])
    df[[v]] <- factor(df[[v]], levels = dsub$code, labels = dsub$label, ordered = ord)
  }
  df
}
dados <- aplica_rotulos(dados, labels_list)

# garante tipos numéricos essenciais
dados <- dados %>%
  mutate(q6 = suppressWarnings(as.numeric(q6)),
         imc = suppressWarnings(as.numeric(imc)),
         obesid = suppressWarnings(as.numeric(obesid)))

# remove NAs finais
dados_df <- tidyr::drop_na(dados)
# =========================
# ANÁLISE DESCRITIVA (2023)
# =========================

# Versão amigável p/ plotar
dados_plot <- dados_df %>%
  mutate(
    obesid_f = factor(obesid, levels = c(0, 1),
                      labels = c("Não obeso", "Obeso"))
  ) %>%
  mutate(
    across(
      any_of(c("q35","q75","q76","r205")),
      ~ if (is.factor(.x) && all(c("não","sim") %in% levels(.x))) {
        forcats::fct_relevel(.x, "não", "sim")
      } else .x
    )
  )

# Rótulos p/ títulos
var_lab <- c(
  q6 = "Idade (anos)", imc = "IMC (kg/m²)", obesid_f = "Obesidade",
  q7 = "Sexo", q16 = "Frequência de hortaliças", q27 = "Frequência de frutas",
  q29 = "Frequência de refrigerantes", r143 = "Consumo de doces",
  q35 = "Bebida alcoólica", q45 = "Frequência de exercício",
  q75 = "Pressão alta", q76 = "Diabetes", r205 = "Já teve depressão"
)

# Tamanho da amostra e prevalência
cat("\n>>> [", ANO, "] Tamanho da amostra (DF, sem NA): ", nrow(dados_df), "\n", sep = "")
prev <- round(100 * prop.table(table(dados_df$obesid))["1"], 2)
cat(">>> [", ANO, "] Prevalência de obesidade (%): ", ifelse(is.na(prev), 0, prev), "\n", sep = "")

# Tema
theme_set(theme_minimal(base_size = 12))

# Paleta (TCC)
pal <- c("#4E79A7","#E15759","#76B7B2","#F28E2B","#59A14F",
         "#EDC949","#AF7AA1","#FF9DA7","#9C755F","#BAB0AC")

# Escala de fill consistente
escala_fill_padrao <- function(df, var) {
  levs <- levels(df[[var]])
  if (!is.null(levs) && all(c("não","sim") %in% levs)) {
    scale_fill_manual(values = c("não" = pal[1], "sim" = pal[2]))
  } else {
    vals <- pal[seq_len(min(length(pal), length(levs)))]
    names(vals) <- levs
    scale_fill_manual(values = vals, drop = FALSE)
  }
}

# 1) Prevalência
p_prev <- dados_plot |>
  count(obesid_f) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = obesid_f, y = prop, fill = obesid_f)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
            vjust = -0.2, size = 4) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, NA)) +
  guides(fill = "none") +
  labs(x = NULL, y = "Proporção",
       title = paste0("Prevalência de obesidade (DF) em ", ANO))
print(p_prev)

# 2) Numéricas: histogramas + densidade
plot_hist_dens <- function(df, var, titulo, xlab){
  ggplot(df, aes(x = .data[[var]])) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.7) +
    geom_density(linewidth = 1) +
    labs(title = titulo, x = xlab, y = "Densidade")
}
print(plot_hist_dens(dados_plot, "q6",
                     paste0("Distribuição de idade no DF (", ANO, ")"),
                     var_lab["q6"]))
print(plot_hist_dens(dados_plot, "imc",
                     paste0("Distribuição de IMC no DF (", ANO, ")"),
                     var_lab["imc"]))

# 3) Categóricas: proporções 100% com rótulos corretos (pré-agregação)

# Proporção geral (100%), rótulo central
plot_cat_geral <- function(df, var) {
  rotulo <- ifelse(!is.na(var_lab[var]), var_lab[var], var)
  
  tab <- df |>
    dplyr::count(nivel = .data[[var]], name = "n") |>
    dplyr::mutate(prop = n / sum(n))
  
  # escala de cores consistente
  levs <- levels(df[[var]])
  if (!is.null(levs) && all(c("não","sim") %in% levs)) {
    scale_fill_var <- scale_fill_manual(values = c("não" = pal[1], "sim" = pal[2]))
  } else {
    vals <- pal[seq_len(min(length(pal), length(levs)))]
    names(vals) <- levs
    scale_fill_var <- scale_fill_manual(values = vals, drop = FALSE)
  }
  
  ggplot(tab, aes(x = "", y = prop, fill = nivel)) +
    geom_col(position = position_stack(reverse = TRUE), width = 0.6) +
    geom_text(
      aes(label = scales::percent(prop, accuracy = 1)),
      stat = "identity",
      position = position_stack(vjust = 0.5, reverse = TRUE),
      size = 3
    ) +
    coord_flip() +
    scale_fill_var +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = NULL, y = "Proporção", fill = rotulo,
         title = paste0(rotulo, " — proporção geral (", ANO, ", 100%)"))
}

plot_cat_por_obes <- function(df, var) {
  rotulo <- ifelse(!is.na(var_lab[var]), var_lab[var], var)
  
  tab <- df |>
    dplyr::count(obesid_f, nivel = .data[[var]], name = "n") |>
    dplyr::group_by(obesid_f) |>
    dplyr::mutate(prop = n / sum(n)) |>
    dplyr::ungroup()
  
  # escala de cores consistente
  levs <- levels(df[[var]])
  if (!is.null(levs) && all(c("não","sim") %in% levs)) {
    scale_fill_var <- scale_fill_manual(values = c("não" = pal[1], "sim" = pal[2]))
  } else {
    vals <- pal[seq_len(min(length(pal), length(levs)))]
    names(vals) <- levs
    scale_fill_var <- scale_fill_manual(values = vals, drop = FALSE)
  }
  
  ggplot(tab, aes(x = obesid_f, y = prop, fill = nivel)) +
    geom_col(position = position_stack(reverse = TRUE)) +
    geom_text(
      aes(label = scales::percent(prop, accuracy = 1)),
      stat = "identity",
      position = position_stack(vjust = 0.5, reverse = TRUE),
      size = 3
    ) +
    scale_fill_var +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = var_lab["obesid_f"], y = "Proporção dentro do grupo",
         fill = rotulo,
         title = paste0(rotulo, " por ", var_lab["obesid_f"], " (", ANO, ", 100%)"))
}

# Exemplo: depressão por obesidade
print(plot_cat_por_obes(dados_plot, "r205"))
#Hist IMC
cutoff <- 30
p_imc <- ggplot(dados_plot, aes(x = imc)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30, fill = "#BAB0AC", color = "white", alpha = 0.9) +
  geom_density(linewidth = 0.8, color = "black") +
  geom_vline(xintercept = cutoff, linetype = 2) +
  annotate("text", x = cutoff, y = 0, label = "30 kg/m²", vjust = -0.8, size = 3) +
  labs(title = paste0("Distribuição de IMC no DF (", ANO, ")"),
       x = "IMC (kg/m²)", y = "Densidade")
p_imc

# =========================
# REGRESSÃO LOGÍSTICA (2023)
# =========================
dados_modelo <- dados_df
dados_modelo$obes_bin <- as.numeric(dados_modelo$obesid)

vars_alvo <- c("q6","q7","q16","q27","q29","q35","q45","q75","q76","r205")
presentes <- intersect(vars_alvo, names(dados_modelo))

# Define referências
set_ref <- function(x, ref_options = c("nunca","quase nunca","não")) {
  if (!is.factor(x)) return(x)
  lv <- levels(x); ref <- ref_options[ref_options %in% lv][1]
  if (!is.na(ref)) x <- forcats::fct_relevel(x, ref, after = 0)
  x
}
if ("q7" %in% names(dados_modelo) && is.factor(dados_modelo$q7) &&
    "feminino" %in% levels(dados_modelo$q7)) {
  dados_modelo$q7 <- forcats::fct_relevel(dados_modelo$q7, "feminino", after = 0)
}
for (v in intersect(c("q16","q27","q29","q45"), names(dados_modelo))) {
  dados_modelo[[v]] <- set_ref(dados_modelo[[v]])
}
for (v in intersect(c("q35","q75","q76","r205"), names(dados_modelo))) {
  if (is.factor(dados_modelo[[v]]) && "não" %in% levels(dados_modelo[[v]])) {
    dados_modelo[[v]] <- forcats::fct_relevel(dados_modelo[[v]], "não", after = 0)
  }
}

# Modelo dinâmico
form_dyn <- reformulate(termlabels = presentes, response = "obes_bin")
cat("\nFórmula utilizada [", ANO, "]:\n", sep=""); print(form_dyn)
mod_full <- glm(form_dyn, data = dados_modelo, family = binomial())

# ORs
tab_or <- broom::tidy(mod_full, exponentiate = TRUE, conf.int = TRUE) |>
  mutate(term = sub("^\\(Intercept\\)$","Intercepto", term),
         OR = estimate, IC_low = conf.low, IC_high = conf.high) |>
  select(term, OR, IC_low, IC_high, p.value) |>
  arrange(p.value)
print(tab_or, n = Inf)

# ---------- Métricas (robusto a diferentes formatos do r2_nagelkerke) ----------
r2_val <- NA_real_

r2_try <- try(performance::r2_nagelkerke(mod_full), silent = TRUE)
if (!inherits(r2_try, "try-error")) {
  if (is.numeric(r2_try) && length(r2_try) == 1) {
    r2_val <- as.numeric(r2_try)
  } else if (is.list(r2_try) && !is.null(r2_try$R2)) {
    r2_val <- as.numeric(r2_try$R2)
  } else if (is.data.frame(r2_try) && "R2" %in% names(r2_try)) {
    r2_val <- as.numeric(r2_try$R2[1])
  }
}

if (is.na(r2_val)) {
  if (!requireNamespace("pscl", quietly = TRUE)) install.packages("pscl")
  r2_pscl <- try(pscl::pR2(mod_full), silent = TRUE)  # named numeric
  if (!inherits(r2_pscl, "try-error")) {
    if ("Nagelkerke" %in% names(r2_pscl)) {
      r2_val <- unname(r2_pscl["Nagelkerke"])
      cat("\nPseudo-R² (Nagelkerke via pscl): ", round(r2_val, 3), "\n")
    } else if ("McFadden" %in% names(r2_pscl)) {
      r2_val <- unname(r2_pscl["McFadden"])
      cat("\nMcFadden R² (fallback): ", round(r2_val, 3), "\n")
    }
  }
} else {
  cat("\nPseudo-R² (Nagelkerke): ", round(r2_val, 3), "\n")
}

# AUC ROC e acurácia
roc_obj <- pROC::roc(response = dados_modelo$obes_bin,
                     predictor = fitted(mod_full), quiet = TRUE)
cat("AUC: ", round(pROC::auc(roc_obj), 3), "\n")

pred_prob  <- fitted(mod_full)
pred_class <- ifelse(pred_prob >= 0.5, 1, 0)
cat("Acurácia (threshold 0.5): ", round(mean(pred_class == dados_modelo$obes_bin), 3), "\n")
cat("\nMatriz de confusão (0/1):\n")
print(table(Real = dados_modelo$obes_bin, Previsto = pred_class))

# Tendência (ordinais -> escore)
ordinal_as_score <- function(x) if (is.ordered(x) || is.factor(x)) as.numeric(x) else x
dados_trend <- dados_modelo
if ("q16" %in% names(dados_trend)) dados_trend$q16_s <- ordinal_as_score(dados_trend$q16)
if ("q27" %in% names(dados_trend)) dados_trend$q27_s <- ordinal_as_score(dados_trend$q27)
if ("q29" %in% names(dados_trend)) dados_trend$q29_s <- ordinal_as_score(dados_trend$q29)
if ("q45" %in% names(dados_trend)) dados_trend$q45_s <- ordinal_as_score(dados_trend$q45)
vars_trend <- intersect(c("q6","q7","q16_s","q27_s","q29_s","q35","q45_s","q75","q76","r205"),
                        names(dados_trend))
mod_trend <- glm(reformulate(vars_trend, "obes_bin"), data = dados_trend, family = binomial())
cat("\nModelo de tendência (ordinais como escore) — ORs:\n")
print(tidy(mod_trend, exponentiate=TRUE, conf.int=TRUE) |>
        select(term, estimate, conf.low, conf.high, p.value) |>
        arrange(p.value), n=Inf)

# --- DUMMIES por nível ---
make_dummies <- function(x, ref=NULL){
  x <- as.factor(x)
  if (!is.null(ref) && ref %in% levels(x)) x <- forcats::fct_relevel(x, ref, after=0)
  x
}
dm <- dados_modelo
if ("q16" %in% names(dm)) dm$q16 <- make_dummies(dm$q16, "nunca")
if ("q27" %in% names(dm)) dm$q27 <- make_dummies(dm$q27, "nunca")
if ("q29" %in% names(dm)) dm$q29 <- make_dummies(dm$q29, "nunca")
if ("q45" %in% names(dm)) dm$q45 <- make_dummies(dm$q45, "nunca")
if ("q35" %in% names(dm) && is.factor(dm$q35)) dm$q35 <- fct_relevel(dm$q35, "não", after=0)
if ("q75" %in% names(dm) && is.factor(dm$q75)) dm$q75 <- fct_relevel(dm$q75, "não", after=0)
if ("q76" %in% names(dm) && is.factor(dm$q76)) dm$q76 <- fct_relevel(dm$q76, "não", after=0)
if ("r205" %in% names(dm) && is.factor(dm$r205)) dm$r205 <- fct_relevel(dm$r205, "não", after=0)

vars_dum <- intersect(c("q6","q7","q16","q27","q29","q35","q45","q75","q76","r205"), names(dm))
form_dum <- reformulate(vars_dum, "obes_bin")
cat("\nFórmula (dummies) utilizada [", ANO, "]:\n", sep=""); print(form_dum)

mod_dum <- glm(form_dum, data = dm, family = binomial())
tab_or_dum <- broom::tidy(mod_dum, exponentiate=TRUE, conf.int=TRUE) |>
  mutate(
    Variável = case_when(term=="(Intercept)" ~ "Intercepto",
                         TRUE ~ sub("^([A-Za-z0-9_]+).*","\\1", term)),
    Nível = case_when(
      grepl(":", term) ~ sub("^[^:]+:(.*)$","\\1", term),
      grepl("^[A-Za-z0-9_]+", term) ~ sub("^[A-Za-z0-9_]+","", term),
      TRUE ~ ""
    ),
    Nível = trimws(gsub("^\\.|^\\(|\\)$","", Nível)),
    OR = estimate, `IC95%_inf`=conf.low, `IC95%_sup`=conf.high
  ) |>
  select(Variável, Nível, OR, `IC95%_inf`, `IC95%_sup`, p.value) |>
  arrange(p.value)
print(tab_or_dum, n=Inf)

# Teste conjunto por variável
joint_dum <- drop1(mod_dum, test="Chisq")
print(joint_dum[order(joint_dum$`Pr(>Chi)`), , drop = FALSE])

# (Opcional) aviso de variáveis ausentes neste ano
faltando <- setdiff(c("q6","q7","q16","q27","q29","r143","q35","q45","q75","q76","r205","imc","obesid"),
                    names(dados))
if (length(faltando)) message("Ausentes em ", ANO, ": ", paste(faltando, collapse=", "))

# =========================
# SENSIBILIDADE PONDERADA — 2023 (survey)
# =========================
# cols <- c("cidade","q6","q7","q16","q27","q29","r143","q35","q45","q75","q76","r205","imc","obesid","pesorake")

library(survey)

# 0) Garante que o peso está disponível para o data.frame usado no modelo em dummies (dm)
if (!"pesorake" %in% names(dados_modelo) && "pesorake" %in% names(dados_df)) {
  dados_modelo$pesorake <- dados_df$pesorake
}
if (!"pesorake" %in% names(dm) && "pesorake" %in% names(dados_modelo)) {
  dm$pesorake <- dados_modelo$pesorake
}
if (!"pesorake" %in% names(dm)) {
  warning("Coluna 'pesorake' não encontrada. Usando pesos = 1 (apenas para ilustração).")
  dm$pesorake <- 1
}
dm$pesorake <- suppressWarnings(as.numeric(dm$pesorake))

# 1) Cria o design (sem estrato/cluster — arquivo atual não traz esses campos)
des2023 <- svydesign(ids = ~1, weights = ~pesorake, data = dm)

# 2) Mesmo modelo com pesos
mod_dum_svy_2023 <- svyglm(form_dum, design = des2023, family = quasibinomial())

# 3) ORs ponderadas (IC95% por erro-padrão robusto)
tab_or_dum_svy_2023 <- broom::tidy(mod_dum_svy_2023) |>
  dplyr::mutate(
    OR      = exp(estimate),
    IC_low  = exp(estimate - 1.96*std.error),
    IC_high = exp(estimate + 1.96*std.error)
  ) |>
  dplyr::select(term, OR, IC_low, IC_high, p.value) |>
  dplyr::arrange(p.value)

cat("\n--- ORs ponderadas (svyglm) — 2023 ---\n")
print(tab_or_dum_svy_2023, n = Inf)

# 4) Testes conjuntos ponderados (Wald) por variável
vars_joint <- intersect(c("q6","q7","q16","q27","q29","r143","q35","q45","q75","q76","r205"),
                        names(dm))

tem_variacao <- function(v){
  x <- dm[[v]]
  if (is.factor(x)) nlevels(droplevels(x)) > 1 else stats::var(x, na.rm = TRUE) > 0
}
vars_joint <- vars_joint[vapply(vars_joint, tem_variacao, logical(1))]

termos_no_modelo <- attr(terms(mod_dum_svy_2023), "term.labels")
vars_joint <- intersect(vars_joint, termos_no_modelo)

get_joint_safe <- function(v){
  fml <- stats::as.formula(paste("~", v))
  out <- tryCatch(survey::regTermTest(mod_dum_svy_2023, fml), error = function(e) NULL)
  if (is.null(out)) {
    return(data.frame(var=v, stat=NA_real_, df=NA_real_, p.value=NA_real_, stat_name=NA_character_))
  }
  stat      <- if (!is.null(out$Chisq)) out$Chisq else
    if (!is.null(out$Ftest)) out$Ftest else
      if (!is.null(out$statistic)) as.numeric(out$statistic)[1] else NA_real_
  stat_name <- if (!is.null(out$Chisq)) "Chisq" else
    if (!is.null(out$Ftest)) "F" else
      if (!is.null(out$statistic)) names(out$statistic)[1] else NA_character_
  df        <- if (!is.null(out$df)) out$df[1] else
    if (!is.null(out$parameter)) as.numeric(out$parameter)[1] else NA_real_
  pval      <- if (!is.null(out$p)) out$p else
    if (!is.null(out$p.value)) out$p.value else NA_real_
  data.frame(var=v, stat=unname(stat), df=df, p.value=pval, stat_name=stat_name)
}

joint_svy_all_2023 <- do.call(rbind, lapply(vars_joint, get_joint_safe))

falhos <- joint_svy_all_2023$var[!is.finite(joint_svy_all_2023$p.value)]
if (length(falhos)) {
  message("Aviso (2023): não foi possível calcular o teste conjunto para: ",
          paste(falhos, collapse = ", "),
          ". Motivos comuns: termo não presente no modelo após ajuste/colinearidade ",
          "ou variável sem variação após filtros.")
}

joint_svy_2023 <- subset(joint_svy_all_2023, is.finite(p.value))
joint_svy_2023 <- joint_svy_2023[order(joint_svy_2023$p.value), , drop = FALSE]

cat("\n--- Testes conjuntos ponderados (Wald, survey) — 2023 ---\n")
print(joint_svy_2023, row.names = FALSE)

# 5) Métricas ponderadas (AUC e limiar de Youden ponderados)
truth_w_23 <- dm$obes_bin
prob_w_23  <- as.numeric(predict(mod_dum_svy_2023, type = "response"))

roc_w_23 <- pROC::roc(truth_w_23, prob_w_23, weights = dm$pesorake, quiet = TRUE)
auc_w_23 <- as.numeric(pROC::auc(roc_w_23))
best_w_23 <- pROC::coords(roc_w_23, x = "best", best.method = "youden",
                          ret = c("threshold","sensitivity","specificity"),
                          transpose = FALSE)
thr_w_23 <- as.numeric(best_w_23["threshold"])

cat(sprintf("\nAUC ponderada (2023): %.3f | Limiar de Youden ponderado (2023): %.4f\n",
            auc_w_23, thr_w_23))

wtab <- function(obs, pred, w) xtabs(w ~ obs + pred)
tab_w_23 <- wtab(truth_w_23, as.integer(prob_w_23 >= thr_w_23), dm$pesorake)

sens_w_23 <- tab_w_23["1","1"] / sum(tab_w_23["1",])
spec_w_23 <- tab_w_23["0","0"] / sum(tab_w_23["0",])
ppv_w_23  <- tab_w_23["1","1"] / sum(tab_w_23[,"1"])
npv_w_23  <- tab_w_23["0","0"] / sum(tab_w_23[,"0"])
acc_w_23  <- sum(diag(tab_w_23)) / sum(tab_w_23)
bal_w_23  <- (sens_w_23 + spec_w_23) / 2

cat("\n=== Métricas ponderadas (corte Youden) — 2023 ===\n")
print(tab_w_23)
cat(sprintf("Sens=%.3f  Espec=%.3f  PPV=%.3f  NPV=%.3f  Acc=%.3f  BalAcc=%.3f\n",
            sens_w_23, spec_w_23, ppv_w_23, npv_w_23, acc_w_23, bal_w_23))


# ==== EXPORTS PADRÃO 2023 ====
dir_out <- file.path(getwd(), paste0("resultado_", ANO))
dir.create(dir_out, showWarnings = FALSE)

# 1) amostra e prevalência
prev_2023 <- data.frame(
  ano = ANO,
  n = nrow(dados_df),
  prev_pct = round(100 * mean(dados_df$obesid == 1), 2)
)
readr::write_csv(prev_2023, file.path(dir_out, paste0("amostra_prev_", ANO, ".csv")))

# 2) ORs (modelo dummies)
readr::write_csv(tab_or_dum, file.path(dir_out, paste0("ORs_dummies_", ANO, ".csv")))

# 3) Teste conjunto por variável (drop1/LRT)
joint_df <- tibble::as_tibble(joint_dum, rownames = "termo")
readr::write_csv(joint_df, file.path(dir_out, paste0("drop1_", ANO, ".csv")))

# 4) Métricas no corte 0,5 e no limiar de Youden
roc_obj <- pROC::roc(dados_modelo$obes_bin, fitted(mod_dum), quiet = TRUE)
auc_val <- as.numeric(pROC::auc(roc_obj))

best <- pROC::coords(roc_obj, "best", best.method = "youden",
                     ret = c("threshold","sensitivity","specificity"), transpose = FALSE)
thr_y <- as.numeric(best["threshold"])

calc_metrics <- function(thr){
  pred <- ifelse(fitted(mod_dum) >= thr, 1, 0)
  tab  <- table(Observado = dados_modelo$obes_bin, Previsto = pred)
  sens <- tab["1","1"]/sum(tab["1",])
  spec <- tab["0","0"]/sum(tab["0",])
  ppv  <- tab["1","1"]/sum(tab[,"1"])
  npv  <- tab["0","0"]/sum(tab[,"0"])
  acc  <- sum(diag(tab))/sum(tab)
  bal  <- (sens + spec)/2
  c(sens = sens, espec = spec, ppv = ppv, npv = npv, acc = acc, bal_acc = bal, limiar = thr)
}

m05 <- calc_metrics(0.5)
mY  <- calc_metrics(thr_y)

# Pseudo-R² de Nagelkerke
r2_val <- NA_real_
r2_try <- try(performance::r2_nagelkerke(mod_dum), silent = TRUE)
if (!inherits(r2_try,"try-error")) {
  r2_val <- if (is.list(r2_try) && !is.null(r2_try$R2)) r2_try$R2 else as.numeric(r2_try)
} else {
  if (!requireNamespace("pscl", quietly = TRUE)) install.packages("pscl")
  r2_pscl <- try(pscl::pR2(mod_dum), silent = TRUE)
  if (!inherits(r2_pscl,"try-error") && "Nagelkerke" %in% names(r2_pscl)) r2_val <- unname(r2_pscl["Nagelkerke"])
}

metricas <- rbind(
  data.frame(corte = "0.5",  t(m05)),
  data.frame(corte = "youden", t(mY))
)
metricas$AUC <- auc_val
metricas$pseudoR2_Nagelkerke <- r2_val

readr::write_csv(metricas, file.path(dir_out, paste0("metricas_", ANO, ".csv")))

# ==== VIF (multicolinearidade)
vif_2023_raw <- performance::check_collinearity(mod_dum)
print(vif_2023_raw) 
