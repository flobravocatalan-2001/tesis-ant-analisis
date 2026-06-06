# =============================================================
# ANÁLISIS PRS-11 (Restauración Percibida) — VERSIÓN FINAL
# =============================================================
# Diseño: medidas repetidas (PRE / POST-inmediato)
# ⚠️ ATENCIÓN: este cuestionario solo tiene 2 momentos,
# no hay datos de POST-2 semanas.
#
# Pruebas utilizadas:
#   - Shapiro-Wilk: verificación de normalidad
#   - Wilcoxon pareado: comparación PRE vs POST dentro de cada grupo
#   - Mann-Whitney: comparación entre grupos en cada momento
# =============================================================


# ---- 1. PAQUETES --------------------------------------------
# Descomentar la primera vez:
# install.packages("tidyverse")
# install.packages("rstatix")
# install.packages("ggpubr")
install.packages("coin")
library(coin)
library(rstatix)  # cargado DESPUÉS de coin, así sus funciones tienen prioridad
library(tidyverse)
library(rstatix)
library(ggpubr)


# ---- 2. CARGAR Y PREPARAR DATOS ----------------------------
datos_prs <- read.csv("C:/Users/HP/Documents/TESIS ANT/Cuestionarios/PRS-11/PRS11_restauracion_scored.csv")

datos_prs <- datos_prs %>%
  select(ID, momento, grupo, prs_total) %>%
  filter(!is.na(prs_total),
         !is.na(grupo),
         grupo %in% c("control", "experimental"))

datos_prs$ID      <- factor(datos_prs$ID)
datos_prs$momento <- factor(datos_prs$momento,
                            levels = c("pre", "post_inmediato"),
                            labels = c("PRE", "POST-inmediato"))
datos_prs$grupo   <- factor(datos_prs$grupo,
                            levels = c("control", "experimental"))

cat("\n=== PARTICIPANTES POR GRUPO Y MOMENTO ===\n")
print(table(datos_prs$grupo, datos_prs$momento))


# ---- 3. ESTADÍSTICAS DESCRIPTIVAS --------------------------
cat("\n=== MEDIANA Y RANGO INTERCUARTÍLICO (RIC) ===\n")
descriptivos <- datos_prs %>%
  group_by(grupo, momento) %>%
  summarise(
    n       = n(),
    mediana = median(prs_total, na.rm = TRUE),
    RIC_25  = quantile(prs_total, 0.25, na.rm = TRUE),
    RIC_75  = quantile(prs_total, 0.75, na.rm = TRUE),
    .groups = "drop"
  )
print(descriptivos)


# ---- 4. VERIFICACIÓN DE NORMALIDAD (Shapiro-Wilk) ----------
cat("\n=== PRUEBA DE NORMALIDAD (Shapiro-Wilk) ===\n")
cat("p > 0.05 → normal | p < 0.05 → NO normal\n\n")
normalidad <- datos_prs %>%
  group_by(grupo, momento) %>%
  shapiro_test(prs_total)
print(normalidad)


# ---- 5. WILCOXON PAREADO ------------------------------------
# Como solo hay 2 momentos (PRE y POST-inmediato), usamos
# directamente Wilcoxon pareado en vez de Friedman.
# Compara si la restauración percibida cambia dentro de cada grupo.

datos_completos <- datos_prs %>%
  group_by(ID) %>%
  filter(n() == 2) %>%
  ungroup()

cat("\n=== WILCOXON PAREADO (PRE vs POST-inmediato) ===\n")
cat("Participantes con ambos momentos completos:\n")
print(table(datos_completos$grupo, datos_completos$momento))

cat("\n--- Grupo CONTROL ---\n")
wilcox_control <- datos_completos %>%
  filter(grupo == "control") %>%
  mutate(ID = droplevels(ID)) %>%
  wilcox_test(prs_total ~ momento, paired = TRUE) %>%
  add_significance()
print(wilcox_control)

cat("\n--- Grupo EXPERIMENTAL ---\n")
wilcox_experimental <- datos_completos %>%
  filter(grupo == "experimental") %>%
  mutate(ID = droplevels(ID)) %>%
  wilcox_test(prs_total ~ momento, paired = TRUE) %>%
  add_significance()
print(wilcox_experimental)


# ---- 6. TAMAÑO DEL EFECTO ----------------------------------
# r de Wilcoxon: 0.1 = pequeño, 0.3 = mediano, 0.5 = grande

cat("\n=== TAMAÑO DEL EFECTO (r de Wilcoxon) ===\n")

cat("--- Grupo CONTROL ---\n")
ef_control <- datos_completos %>%
  filter(grupo == "control") %>%
  mutate(ID = droplevels(ID)) %>%
  wilcox_effsize(prs_total ~ momento, paired = TRUE)
print(ef_control)

cat("\n--- Grupo EXPERIMENTAL ---\n")
ef_experimental <- datos_completos %>%
  filter(grupo == "experimental") %>%
  mutate(ID = droplevels(ID)) %>%
  wilcox_effsize(prs_total ~ momento, paired = TRUE)
print(ef_experimental)


# ---- 7. MANN-WHITNEY ----------------------------------------
# Compara control vs experimental en cada momento por separado.

# Mann-Whitney especificando el paquete rstatix
mann_whitney <- datos_prs %>%
  group_by(momento) %>%
  rstatix::wilcox_test(prs_total ~ grupo) %>%
  add_significance()
print(mann_whitney)

cat("\n=== MANN-WHITNEY: CONTROL vs EXPERIMENTAL POR MOMENTO ===\n")
mann_whitney <- datos_prs %>%
  group_by(momento) %>%
  wilcox_test(prs_total ~ grupo) %>%
  add_significance()
print(mann_whitney)


# ---- 8. GRÁFICO ---------------------------------------------


# Para guardar el gráfico, descomentar:
# ggsave("grafico_PRS.png", grafico, width = 7, height = 5, dpi = 300)

grafico <- ggplot(datos_prs, aes(x = momento, y = prs_total, fill = grupo)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 2) +
  scale_fill_manual(
    values = c("control" = "#E07B54", "experimental" = "#4A90D9"),
    labels = c("Control", "Experimental")
  ) +
  scale_y_continuous(limits = c(20, 65)) +   # ajustado al rango real
  labs(
    title    = "Restauración Percibida (PRS-11) según grupo y momento",
    subtitle = "Mediana y rango intercuartílico — pruebas no paramétricas",
    x        = "Momento de medición",
    y        = "Puntaje total PRS-11",
    fill     = "Grupo"
  ) +
  theme_pubr() +
  theme(legend.position = "top")

print(grafico)
ggsave("grafico_PRS.png", grafico, width = 7, height = 5, dpi = 300)

