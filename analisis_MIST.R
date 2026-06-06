# =============================================================
# ANÁLISIS MIST — STAI-6 y VAS
# =============================================================
# Objetivo: evaluar si el MIST indujo estrés agudo (PRE vs POST)
# y si la respuesta difirió entre grupo control y experimental.
#
# Muestra: 27 controles y 26 experimentales con datos completos
#
# Variables:
#   - STAI-6: ansiedad estado (6 ítems, escala 1-4, rango 6-24)
#   - VAS: estrés subjetivo percibido (escala 0-10)
#
# Análisis:
#   1. Estadísticos descriptivos (mediana y RIC)
#   2. Normalidad: Shapiro-Wilk
#   3. Eficacia del MIST: Wilcoxon pareado PRE vs POST por grupo
#   4. Tamaño del efecto: r de Wilcoxon
#   5. Diferencias entre grupos: Mann-Whitney en cada momento
#   6. Gráficos
# =============================================================


# ---- 1. PAQUETES --------------------------------------------
# Descomentar la primera vez:
# install.packages("tidyverse")
# install.packages("rstatix")
# install.packages("ggpubr")
# install.packages("coin")

library(tidyverse)
library(coin)
library(rstatix)
library(ggpubr)


# ---- 2. CARGAR Y PREPARAR DATOS ----------------------------
datos <- read.csv("C:/Users/HP/Documents/TESIS ANT/Cuestionarios/MIST/MIST_limpio_final.csv")

datos$num     <- as.numeric(datos$num)
datos$ID      <- factor(datos$num)
datos$momento <- factor(datos$pre_post,
                        levels = c("Pre", "Post"),
                        labels = c("PRE-MIST", "POST-MIST"))
datos$grupo   <- factor(datos$grupo,
                        levels = c("control", "experimental"))

# Solo participantes con PRE y POST completos
datos_pareados <- datos %>%
  group_by(ID) %>%
  filter(n() == 2) %>%
  ungroup()

cat("=== MUESTRA ===\n")
print(table(datos_pareados$grupo, datos_pareados$momento))


# ---- 3. ESTADÍSTICOS DESCRIPTIVOS --------------------------
# Se reporta Mediana [P25 - P75]

cat("\n=== DESCRIPTIVOS — STAI-6 ===\n")
datos_pareados %>%
  group_by(grupo, momento) %>%
  summarise(
    n   = n(),
    Mdn = median(stai_total),
    P25 = quantile(stai_total, 0.25),
    P75 = quantile(stai_total, 0.75),
    .groups = "drop"
  ) %>% print()

cat("\n=== DESCRIPTIVOS — VAS ===\n")
datos_pareados %>%
  group_by(grupo, momento) %>%
  summarise(
    n   = n(),
    Mdn = median(vas_estres),
    P25 = quantile(vas_estres, 0.25),
    P75 = quantile(vas_estres, 0.75),
    .groups = "drop"
  ) %>% print()


# ---- 4. NORMALIDAD (Shapiro-Wilk) --------------------------
cat("\n=== NORMALIDAD (Shapiro-Wilk) ===\n")
cat("p > .05 = distribución normal\n\n")

cat("STAI-6:\n")
datos_pareados %>%
  group_by(grupo, momento) %>%
  shapiro_test(stai_total) %>%
  select(grupo, momento, statistic, p) %>%
  print()

cat("\nVAS:\n")
datos_pareados %>%
  group_by(grupo, momento) %>%
  shapiro_test(vas_estres) %>%
  select(grupo, momento, statistic, p) %>%
  print()


# ---- 5. EFICACIA DEL MIST: WILCOXON PAREADO ----------------
# Pregunta: ¿indujo el MIST estrés agudo en cada grupo?
# Se reporta: W, p, significancia y r (tamaño del efecto)

cat("\n=== WILCOXON PAREADO: PRE-MIST vs POST-MIST ===\n\n")

for (g in c("control", "experimental")) {
  cat(paste0("--- ", toupper(g), " ---\n"))
  sub <- datos_pareados %>%
    filter(grupo == g) %>%
    mutate(ID = droplevels(ID))

  w_stai <- sub %>%
    rstatix::wilcox_test(stai_total ~ momento, paired = TRUE) %>%
    add_significance()
  e_stai <- sub %>%
    wilcox_effsize(stai_total ~ momento, paired = TRUE)
  cat(sprintf("STAI-6: W = %.1f, p = %.4f %s, r = %.3f (%s)\n",
              w_stai$statistic, w_stai$p, w_stai$p.signif,
              e_stai$effsize, e_stai$magnitude))

  w_vas <- sub %>%
    rstatix::wilcox_test(vas_estres ~ momento, paired = TRUE) %>%
    add_significance()
  e_vas <- sub %>%
    wilcox_effsize(vas_estres ~ momento, paired = TRUE)
  cat(sprintf("VAS:    W = %.1f, p = %.4f %s, r = %.3f (%s)\n\n",
              w_vas$statistic, w_vas$p, w_vas$p.signif,
              e_vas$effsize, e_vas$magnitude))
}


# ---- 6. DIFERENCIAS ENTRE GRUPOS: MANN-WHITNEY -------------
# Pregunta: ¿difieren control y experimental en cada momento?
# PRE-MIST: verifica equivalencia basal
# POST-MIST: verifica si la recuperación fue distinta

cat("=== MANN-WHITNEY: CONTROL vs EXPERIMENTAL ===\n\n")

cat("STAI-6:\n")
datos_pareados %>%
  group_by(momento) %>%
  rstatix::wilcox_test(stai_total ~ grupo) %>%
  add_significance() %>%
  select(momento, statistic, p, p.signif) %>%
  print()

cat("\nVAS:\n")
datos_pareados %>%
  group_by(momento) %>%
  rstatix::wilcox_test(vas_estres ~ grupo) %>%
  add_significance() %>%
  select(momento, statistic, p, p.signif) %>%
  print()


# ---- 7. GRÁFICOS --------------------------------------------

# Figura 1: STAI-6
fig1 <- ggplot(datos_pareados,
               aes(x = momento, y = stai_total, fill = grupo)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 2,
               width = 0.5, position = position_dodge(0.6)) +
  scale_fill_manual(
    values = c("control" = "#E07B54", "experimental" = "#4A90D9"),
    labels = c("Control", "Experimental")
  ) +
  scale_x_discrete(labels = c("PRE-MIST"  = "Pre-MIST",
                               "POST-MIST" = "Post-MIST")) +
  labs(
    title = "Figura 1. Ansiedad estado (STAI-6) antes y después del MIST",
    x     = NULL,
    y     = "Puntaje STAI-6",
    fill  = "Grupo"
  ) +
  theme_pubr() +
  theme(legend.position  = "top",
        plot.title       = element_text(size = 11))

# Figura 2: VAS
fig2 <- ggplot(datos_pareados,
               aes(x = momento, y = vas_estres, fill = grupo)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 2,
               width = 0.5, position = position_dodge(0.6)) +
  scale_fill_manual(
    values = c("control" = "#E07B54", "experimental" = "#4A90D9"),
    labels = c("Control", "Experimental")
  ) +
  scale_x_discrete(labels = c("PRE-MIST"  = "Pre-MIST",
                               "POST-MIST" = "Post-MIST")) +
  labs(
    title = "Figura 2. Estrés subjetivo (VAS) antes y después del MIST",
    x     = NULL,
    y     = "Estrés percibido VAS (0–10)",
    fill  = "Grupo"
  ) +
  theme_pubr() +
  theme(legend.position  = "top",
        plot.title       = element_text(size = 11))

print(fig1)
print(fig2)

# Para guardar en alta resolución:
# ggsave("Fig1_MIST_STAI6.png", fig1, width = 7, height = 5, dpi = 300)
# ggsave("Fig2_MIST_VAS.png",   fig2, width = 7, height = 5, dpi = 300)
