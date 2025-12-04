# =========================
# VIGITEL 2007 — DF (cidade==27)
# =========================

# ---- Parâmetros ----
ANO <- 2007
ARQ <- "C:/Users/mathe/Desktop/UnB/TCC/Dados TCC/Dados TCC/Vigitel-2007-peso-rake.xls"
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


# Tema + paleta padrão
theme_set(ggplot2::theme_minimal(base_size = 12))
pal <- c("#4E79A7", "#E15759", "#76B7B2", "#F28E2B", "#59A14F",
         "#EDC949", "#AF7AA1", "#FF9DA7", "#9C755F", "#BAB0AC")

# ---- Colunas de interesse  ----
cols     <- c("cidade","q6","q7","q16","q27","q29","r143","q35","q45","q75","q76","r205",
              "imc","obesid","pesorake")  

vars_cat <- c("q7","q16","q27","q29","r143","q35","q45","q75","q76","r205")
codigos_na <- c(77,88,99,777,888,999)

# ---- Importa, seleciona, filtra DF, trata códigos especiais ----
dados <- readxl::read_xls(ARQ, sheet = 1, progress = TRUE) |>
  dplyr::select(any_of(cols)) |>
  dplyr::mutate(
    cidade   = suppressWarnings(as.numeric(gsub("\\D","", as.character(cidade)))),
    dplyr::across(any_of(vars_cat), ~ ifelse(. %in% codigos_na, NA, .)),
    pesorake = suppressWarnings(as.numeric(pesorake)) 
  ) |>
  dplyr::filter(cidade == 27)


# ---- Dicionário/labels (apenas o que usamos) ----
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
dados <- dados |>
  aplica_rotulos(labels_list) |>
  dplyr::mutate(q6 = as.numeric(q6), imc = as.numeric(imc), obesid = as.numeric(obesid))

# Remove NAs (após rotular/tratar)
dados_df <- tidyr::drop_na(dados)

# =========================
# GRÁFICOS EXPLORATÓRIOS — TODAS AS VARIÁVEIS PRESENTES
# =========================
dados_plot <- dados_df |>
  dplyr::mutate(obesid_f = factor(obesid, levels = c(0,1), labels = c("Não obeso","Obeso")))

if ("q6" %in% names(dados_plot)) {
  p_age_box <- ggplot(dados_plot, aes(x = obesid_f, y = q6, fill = obesid_f)) +
    geom_boxplot(alpha = .9, width = .6, outlier.alpha = .4) +
    scale_fill_manual(values = pal[1:2], guide = "none") +
    labs(x = "Obesidade", y = "Idade (anos)",
         title = paste0("Idade por obesidade — DF, ", ANO))
  print(p_age_box)
}
if ("imc" %in% names(dados_plot)) {
  p_imc_box <- ggplot(dados_plot, aes(x = obesid_f, y = imc, fill = obesid_f)) +
    geom_boxplot(alpha = .9, width = .6, outlier.alpha = .4) +
    scale_fill_manual(values = pal[1:2], guide = "none") +
    labs(x = "Obesidade", y = "IMC (kg/m²)",
         title = paste0("IMC por obesidade — DF, ", ANO))
  print(p_imc_box)
}
if (all(c("imc","q7") %in% names(dados_plot))) {
  p_imc_sexo <- ggplot(dados_plot, aes(x = q7, y = imc, fill = q7)) +
    geom_boxplot(alpha = .9, width = .6, outlier.alpha = .4) +
    scale_fill_manual(values = pal[1:2], guide = "none") +
    labs(x = "Sexo", y = "IMC (kg/m²)",
         title = paste0("IMC por sexo — DF, ", ANO))
  print(p_imc_sexo)
}

vars_cat_todas <- c("q7","q16","q27","q29","r143","q35","q45","q75","q76","r205")
vars_cat_presentes <- intersect(vars_cat_todas, names(dados_plot))
var_lab <- c(q7="Sexo", q16="Frequência de hortaliças", q27="Frequência de frutas",
             q29="Frequência de refrigerantes", r143="Consumo de doces",
             q35="Bebida alcoólica", q45="Frequência de exercício",
             q75="Pressão alta", q76="Diabetes", r205="Já teve depressão")

plot_cat_geral <- function(df, var) {
  rot <- ifelse(!is.na(var_lab[var]), var_lab[var], var)
  df |>
    dplyr::count(!!rlang::sym(var)) |>
    dplyr::mutate(prop = n/sum(n)) |>
    ggplot(aes(x = "", y = prop, fill = .data[[var]])) +
    geom_col(width = .7) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = pal) +
    labs(x = NULL, y = "Proporção", fill = rot,
         title = paste0(rot, " — proporção geral (", ANO, ")"))
}
plot_cat_por_obes <- function(df, var) {
  rot <- ifelse(!is.na(var_lab[var]), var_lab[var], var)
  ggplot(df, aes(x = obesid_f, fill = .data[[var]])) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = pal) +
    labs(x = "Obesidade", y = "Proporção dentro do grupo", fill = rot,
         title = paste0(rot, " por obesidade (", ANO, ", 100%)"))
}
invisible(lapply(vars_cat_presentes, \(v) print(plot_cat_geral(dados_plot, v))))
invisible(lapply(vars_cat_presentes, \(v) print(plot_cat_por_obes(dados_plot, v))))
#hist 2007
# Função genérica: histograma de IMC com corte OMS (30 kg/m²)
plot_imc_hist <- function(df, ano, cutoff = 30) {
  stopifnot("imc" %in% names(df))
  prop_obeso <- mean(df$imc >= cutoff, na.rm = TRUE)
  
  ggplot(df, aes(x = imc)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 30, fill = "#BAB0AC", color = "white", alpha = 0.9) +
    geom_density(linewidth = 0.8, color = "black") +
    geom_vline(xintercept = cutoff, linetype = 2) +
    annotate("text", x = cutoff, y = 0, label = "30 kg/m²",
             vjust = -0.8, size = 3) +
    annotate("text", x = cutoff + 6, y = 0.008,
             label = paste0("Obs: ", scales::percent(prop_obeso, accuracy = 0.1)),
             hjust = 0, size = 3) +
    labs(title = paste0("Distribuição de IMC no DF (", ano, ")"),
         x = "IMC (kg/m²)", y = "Densidade")
}
print(plot_imc_hist(dados_df, 2007))
# =========================
# LOGÍSTICA — MODELO PRINCIPAL EM DUMMIES
# =========================
dados_modelo <- dados_df
dados_modelo$obes_bin <- as.integer(dados_modelo$obesid)

make_dummies <- function(x, ref=NULL){
  x <- as.factor(x)
  if(!is.null(ref) && ref %in% levels(x)) x <- forcats::fct_relevel(x, ref, after=0)
  x
}
for(v in intersect(c("q16","q27","q29","q45"), names(dados_modelo)))
  dados_modelo[[v]] <- make_dummies(dados_modelo[[v]], "nunca")
for(v in intersect(c("q35","q75","q76","r205"), names(dados_modelo)))
  if(is.factor(dados_modelo[[v]])) dados_modelo[[v]] <- forcats::fct_relevel(dados_modelo[[v]], "não", after=0)
if("q7" %in% names(dados_modelo) && is.factor(dados_modelo$q7) && "feminino" %in% levels(dados_modelo$q7))
  dados_modelo$q7 <- forcats::fct_relevel(dados_modelo$q7, "feminino", after=0)

vars_dum <- intersect(c("q6","q7","q16","q27","q29","r143","q35","q45","q75","q76","r205"), names(dados_modelo))
form_dum <- reformulate(vars_dum, response = "obes_bin")
cat("\nFórmula (dummies):\n"); print(form_dum)

mod_dum <- glm(form_dum, data = dados_modelo, family = binomial())

# OR por nível + IC95% (ordenado por p)
tab_or_dum <- broom::tidy(mod_dum, exponentiate=TRUE, conf.int=TRUE) |>
  dplyr::mutate(OR = estimate, IC_low = conf.low, IC_high = conf.high) |>
  dplyr::select(term, OR, IC_low, IC_high, p.value) |>
  dplyr::arrange(p.value)
print(tab_or_dum, n=Inf)

# Teste conjunto por variável (quem pesa no global)
cat("\nTeste conjunto por variável (drop1, LRT):\n")
joint_dum <- drop1(mod_dum, test="Chisq")
print(joint_dum[order(joint_dum$`Pr(>Chi)`), , drop=FALSE])

# =========================
# SENSIBILIDADE PONDERADA (survey)
# =========================
library(survey)

# design só com pesos (sem estrato/cluster no seu arquivo atual)
des2007 <- svydesign(ids = ~1, weights = ~pesorake, data = dados_modelo)

# mesmo modelo, mas ponderado
mod_dum_svy <- svyglm(form_dum, design = des2007, family = quasibinomial())

# ORs ponderadas (com IC aproximado via erro-padrão robusto)
tab_or_dum_svy <- broom::tidy(mod_dum_svy) |>
  dplyr::mutate(
    OR      = exp(estimate),
    IC_low  = exp(estimate - 1.96*std.error),
    IC_high = exp(estimate + 1.96*std.error)
  ) |>
  dplyr::select(term, OR, IC_low, IC_high, p.value) |>
  dplyr::arrange(p.value)

cat("\n--- ORs ponderadas (svyglm) ---\n")
print(tab_or_dum_svy, n = Inf)

# --------------------------------------------
# Testes conjuntos ponderados (Wald) por variável — ROBUSTO
# --------------------------------------------

# 1) candidatos (os mesmos termos que você criou para o modelo)
vars_joint <- intersect(c("q6","q7","q16","q27","q29","r143","q35","q45","q75","q76","r205"),
                        names(dados_modelo))

# 2) variação real
tem_variacao <- function(v){
  x <- dados_modelo[[v]]
  if (is.factor(x)) nlevels(droplevels(x)) > 1 else stats::var(x, na.rm = TRUE) > 0
}
vars_joint <- vars_joint[vapply(vars_joint, tem_variacao, logical(1))]

# 3) apenas termos que de fato entraram no modelo ponderado
termos_no_modelo <- attr(terms(mod_dum_svy), "term.labels")
vars_joint <- intersect(vars_joint, termos_no_modelo)

# 4) teste seguro
get_joint_safe <- function(v){
  fml <- stats::as.formula(paste("~", v))
  out <- tryCatch(survey::regTermTest(mod_dum_svy, fml), error = function(e) NULL)
  if (is.null(out)) {
    return(data.frame(var = v, stat = NA_real_, df = NA_real_, p.value = NA_real_, stat_name = NA_character_))
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
  
  data.frame(var = v, stat = unname(stat), df = df, p.value = pval, stat_name = stat_name)
}

# 5) aplica
joint_svy_all <- do.call(rbind, lapply(vars_joint, get_joint_safe))

falhos <- joint_svy_all$var[!is.finite(joint_svy_all$p.value)]
if (length(falhos)) {
  message("Aviso: não foi possível calcular o teste conjunto para: ",
          paste(falhos, collapse = ", "),
          ". Motivos comuns: termo não presente no modelo após ajuste/colinearidade ",
          "ou variável sem variação após filtros.")
}

joint_svy <- subset(joint_svy_all, is.finite(p.value))
joint_svy <- joint_svy[order(joint_svy$p.value), , drop = FALSE]

cat("\n--- Testes conjuntos ponderados (Wald, survey) ---\n")
print(joint_svy, row.names = FALSE)

# -------------------------
# Métricas ponderadas
# -------------------------
truth_w <- dados_modelo$obes_bin
prob_w  <- as.numeric(predict(mod_dum_svy, type = "response"))

# AUC ponderada e limiar de Youden ponderado
roc_w   <- pROC::roc(truth_w, prob_w, weights = dados_modelo$pesorake, quiet = TRUE)
auc_w   <- as.numeric(pROC::auc(roc_w))
best_w  <- pROC::coords(roc_w, x = "best", best.method = "youden",
                        ret = c("threshold","sensitivity","specificity"), transpose = FALSE)
thr_w   <- as.numeric(best_w["threshold"])

cat(sprintf("\nAUC ponderada: %.3f | Limiar de Youden ponderado: %.4f\n", auc_w, thr_w))

# Tabela de confusão ponderada e métricas no corte de Youden
wtab <- function(obs, pred, w) xtabs(w ~ obs + pred)
tab_w <- wtab(truth_w, as.integer(prob_w >= thr_w), dados_modelo$pesorake)
sens_w <- tab_w["1","1"]/sum(tab_w["1",])
spec_w <- tab_w["0","0"]/sum(tab_w["0",])
ppv_w  <- tab_w["1","1"]/sum(tab_w[,"1"])
npv_w  <- tab_w["0","0"]/sum(tab_w[,"0"])
acc_w  <- sum(diag(tab_w))/sum(tab_w)
bal_w  <- (sens_w + spec_w)/2

cat("\n=== Métricas ponderadas (corte Youden) ===\n")
print(tab_w)
cat(sprintf("Sens=%.3f  Espec=%.3f  PPV=%.3f  NPV=%.3f  Acc=%.3f  BalAcc=%.3f\n",
            sens_w, spec_w, ppv_w, npv_w, acc_w, bal_w))


# =========================
# MÉTRICAS — 0,5 vs limiar de YOUDEN
# =========================
truth <- dados_modelo$obes_bin
prob  <- fitted(mod_dum)

# AUC e pseudo-R2
r2_try <- try(performance::r2_nagelkerke(mod_dum), silent=TRUE)
if(!inherits(r2_try,"try-error")){
  r2_val <- if(is.list(r2_try) && !is.null(r2_try$R2)) r2_try$R2 else as.numeric(r2_try)
  cat("\nPseudo-R² (Nagelkerke): ", round(r2_val,3), "\n")
}
roc_obj <- pROC::roc(truth, prob, quiet=TRUE)
cat("AUC: ", round(pROC::auc(roc_obj),3), "\n")

# --- métricas no corte 0.5
pred_05 <- ifelse(prob >= 0.5, 1, 0)
tab_05  <- table(Observado=truth, Previsto=pred_05)
sens_05 <- tab_05["1","1"]/sum(tab_05["1",])
spec_05 <- tab_05["0","0"]/sum(tab_05["0",])
ppv_05  <- tab_05["1","1"]/sum(tab_05[,"1"])
npv_05  <- tab_05["0","0"]/sum(tab_05[,"0"])
bal_05  <- (sens_05 + spec_05)/2
acc_05  <- sum(diag(tab_05))/sum(tab_05)

# --- limiar ótimo de Youden
best <- pROC::coords(roc_obj, x="best", best.method="youden",
                     ret=c("threshold","sensitivity","specificity"), transpose = FALSE)
thr_y <- as.numeric(best["threshold"])
cat("Limiar de Youden: ", signif(thr_y,5), "\n")

pred_y <- ifelse(prob >= thr_y, 1, 0)
tab_y  <- table(Observado=truth, Previsto=pred_y)
sens_y <- tab_y["1","1"]/sum(tab_y["1",])
spec_y <- tab_y["0","0"]/sum(tab_y["0",])
ppv_y  <- tab_y["1","1"]/sum(tab_y[,"1"])
npv_y  <- tab_y["0","0"]/sum(tab_y[,"0"])
bal_y  <- (sens_y + spec_y)/2
acc_y  <- sum(diag(tab_y))/sum(tab_y)

cat("\n=== Métricas no corte 0.5 ===\n")
print(tab_05)
cat(sprintf("Sens=%.3f  Espec=%.3f  PPV=%.3f  NPV=%.3f  Acc=%.3f  BalAcc=%.3f\n",
            sens_05,spec_05,ppv_05,npv_05,acc_05,bal_05))
cat("\n=== Métricas no corte Youden ===\n")
print(tab_y)
cat(sprintf("Sens=%.3f  Espec=%.3f  PPV=%.3f  NPV=%.3f  Acc=%.3f  BalAcc=%.3f\n",
            sens_y,spec_y,ppv_y,npv_y,acc_y,bal_y))

# =========================
# RESUMOS (para comparação entre anos)
# =========================
dir_out <- file.path(getwd(), paste0("resultado_", ANO))
dir.create(dir_out, showWarnings = FALSE)

# 1) amostra e prevalência
write.csv(data.frame(ano=ANO, n=nrow(dados_df),
                     prev_pct=round(100*mean(dados_df$obesid==1),2)),
          file.path(dir_out, paste0("amostra_prev_", ANO, ".csv")), row.names=FALSE)

# 2) ORs (modelo dummies)
write.csv(tab_or_dum, file.path(dir_out, paste0("ORs_dummies_", ANO, ".csv")), row.names=FALSE)

# 3) Teste conjunto por variável
joint_df <- tibble::as_tibble(joint_dum, rownames = "termo")
write.csv(joint_df, file.path(dir_out, paste0("drop1_", ANO, ".csv")), row.names=FALSE)

# 4) Exposição aos fatores
exp_vars <- intersect(c("q75","q76","q35","q45","q27","q29"), names(dados_df))
exp_list <- lapply(exp_vars, function(v){
  tb <- prop.table(table(dados_df[[v]]))
  data.frame(var=v, nivel=names(tb), prop=as.numeric(tb))
})
expo <- dplyr::bind_rows(exp_list)
write.csv(expo, file.path(dir_out, paste0("exposicao_", ANO, ".csv")), row.names=FALSE)

# 5) Exporta métricas 0.5 vs Youden
met_05 <- data.frame(corte="0.5", sens=sens_05, espec=spec_05, ppv=ppv_05, npv=npv_05,
                     acc=acc_05, bal_acc=bal_05, limiar=0.5)
met_y  <- data.frame(corte="youden", sens=sens_y, espec=spec_y, ppv=ppv_y, npv=npv_y,
                     acc=acc_y, bal_acc=bal_y, limiar=thr_y)
write.csv(rbind(met_05, met_y), file.path(dir_out, paste0("metricas_", ANO, ".csv")),
          row.names=FALSE)

# Aviso de variáveis ausentes (informativo)
faltando <- setdiff(c("q6","q7","q16","q27","q29","r143","q35","q45","q75","q76","r205","imc","obesid"),
                    names(dados))
if (length(faltando)) message("Ausentes em ", ANO, ": ", paste(faltando, collapse=", "))

# ==== VIF (multicolinearidade)
vif_raw <- performance::check_collinearity(mod_dum)
print(vif_raw)  

