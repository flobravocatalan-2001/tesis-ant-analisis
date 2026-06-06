# =============================================================
# ANÁLISIS STAI-E (Ansiedad Estado) — VERSIÓN FINAL
# =============================================================
# Diseño: medidas repetidas (PRE / POST-inmediato / POST-2 semanas)
#         comparando grupo control vs experimental
#
# Se utilizaron pruebas no paramétricas porque los datos no
# cumplen el supuesto de normalidad (Shapiro-Wilk p < 0.05
# en 4 de 6 grupos/momentos).
#
# Pruebas utilizadas:
#   - Shapiro-Wilk: verificación de normalidad
#   - Friedman: comparación de los 3 momentos dentro de cada grupo
#   - Wilcoxon pareado + Bonferroni: post-hoc entre pares de momentos
#   - Mann-Whitney: comparación entre grupos en cada momento
# =============================================================


# ---- 1. PAQUETES --------------------------------------------
# Descomentar la primera vez que se ejecuta el script:
# install.packages("tidyverse")
# install.packages("rstatix")
# install.packages("ggpubr")

library(tidyverse)
library(rstatix)
library(ggpubr)


# ---- 2. CARGAR Y PREPARAR DATOS ----------------------------
datos <- read.csv("C:/Users/HP/Documents/TESIS ANT/Cuestionarios/STAI/STAI_estado_scored.csv")

datos <- datos %>%
  select(ID, momento, grupo, stai_e_total) %>%
  filter(!is.na(stai_e_total),
         !is.na(grupo),
         grupo %in% c("control", "experimental"))

# Convertir a factores con etiquetas legibles
datos$ID      <- factor(datos$ID)
datos$momento <- factor(datos$momento,
                        levels = c("pre", "post_inmediato", "post_2semanas"),
                        labels = c("PRE", "POST-inmediato", "POST-2 semanas"))
datos$grupo   <- factor(datos$grupo,
                        levels = c("control", "experimental"))

cat("\n=== PARTICIPANTES POR GRUPO Y MOMENTO ===\n")
print(table(datos$grupo, datos$momento))


# ---- 3. ESTADÍSTICAS DESCRIPTIVAS --------------------------
# Se reporta mediana y rango intercuartílico (RIC) porque
# los datos no son normales.
# RIC_25 = percentil 25 | RIC_75 = percentil 75

cat("\n=== MEDIANA Y RANGO INTERCUARTÍLICO (RIC) ===\n")
descriptivos <- datos %>%
  group_by(grupo, momento) %>%
  summarise(
    n       = n(),
    mediana = median(stai_e_total, na.rm = TRUE),
    RIC_25  = quantile(stai_e_total, 0.25, na.rm = TRUE),
    RIC_75  = quantile(stai_e_total, 0.75, na.rm = TRUE),
    .groups = "drop"
  )
print(descriptivos)


# ---- 4. VERIFICACIÓN DE NORMALIDAD (Shapiro-Wilk) ----------
# p > 0.05 → distribución normal
# p < 0.05 → distribución NO normal → usar pruebas no paramétricas

cat("\n=== PRUEBA DE NORMALIDAD (Shapiro-Wilk) ===\n")
normalidad <- datos %>%
  group_by(grupo, momento) %>%
  shapiro_test(stai_e_total)
print(normalidad)


# ---- 5. PRUEBA DE FRIEDMAN ----------------------------------
# Evalúa si la ansiedad cambia significativamente entre los
# 3 momentos dentro de cada grupo.
# Solo incluye participantes con datos en los 3 momentos.
# Se usa droplevels() para eliminar niveles vacíos del factor ID.

# Filtrar participantes con los 3 momentos completos
datos_completos <- datos %>%
  group_by(ID) %>%
  filter(n() == 3) %>%
  ungroup()

cat("\n=== PRUEBA DE FRIEDMAN (por grupo) ===\n")
cat("Participantes con los 3 momentos completos:\n")
print(table(datos_completos$grupo, datos_completos$momento))

cat("\n--- Grupo CONTROL ---\n")
friedman_control <- datos_completos %>%
  filter(grupo == "control") %>%
  mutate(ID = droplevels(ID)) %>%
  friedman_test(stai_e_total ~ momento | ID)
print(friedman_control)

cat("\n--- Grupo EXPERIMENTAL ---\n")
friedman_experimental <- datos_completos %>%
  filter(grupo == "experimental") %>%
  mutate(ID = droplevels(ID)) %>%
  friedman_test(stai_e_total ~ momento | ID)
print(friedman_experimental)


# ---- 6. POST-HOC: WILCOXON PAREADO -------------------------
# Indica entre qué pares de momentos hay diferencias significativas.
# Corrección de Bonferroni para controlar el error por comparaciones múltiples.
# p.adj < 0.05 → diferencia significativa entre ese par de momentos.

cat("\n=== POST-HOC: WILCOXON PAREADO + BONFERRONI ===\n")

cat("\n--- Grupo CONTROL ---\n")
wilcox_control <- datos_completos %>%
  filter(grupo == "control") %>%
  mutate(ID = droplevels(ID)) %>%
  pairwise_wilcox_test(
    stai_e_total ~ momento,
    paired          = TRUE,
    p.adjust.method = "bonferroni"
  )
print(wilcox_control)

cat("\n--- Grupo EXPERIMENTAL ---\n")
wilcox_experimental <- datos_completos %>%
  filter(grupo == "experimental") %>%
  mutate(ID = droplevels(ID)) %>%
  pairwise_wilcox_test(
    stai_e_total ~ momento,
    paired          = TRUE,
    p.adjust.method = "bonferroni"
  )
print(wilcox_experimental)


# ---- 7. MANN-WHITNEY ----------------------------------------
# Compara control vs experimental en cada momento por separado.
# Evalúa si los grupos parten o llegan a niveles distintos de ansiedad.

cat("\n=== MANN-WHITNEY: CONTROL vs EXPERIMENTAL POR MOMENTO ===\n")
mann_whitney <- datos %>%
  group_by(momento) %>%
  wilcox_test(stai_e_total ~ grupo) %>%
  add_significance()
print(mann_whitney)


# ---- 8. GRÁFICO ---------------------------------------------
# Boxplot: muestra mediana, RIC y valores atípicos.
# Apropiado para datos no normales.

grafico <- ggplot(datos, aes(x = momento, y = stai_e_total, fill = grupo)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 2) +
  scale_fill_manual(
    values = c("control" = "#E07B54", "experimental" = "#4A90D9"),
    labels = c("Control", "Experimental")
  ) +
  labs(
    title    = "Ansiedad Estado (STAI-E) según grupo y momento",
    subtitle = "Mediana y rango intercuartílico — pruebas no paramétricas",
    x        = "Momento de medición",
    y        = "Puntaje total STAI-E (0–60)",
    fill     = "Grupo"
  ) +
  theme_pubr() +
  theme(legend.position = "top")

print(grafico)

# Para guardar el gráfico como imagen, descomentar:
# ggsave("grafico_STAI.png", grafico, width = 8, height = 5, dpi = 300)

