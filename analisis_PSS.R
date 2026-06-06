# =============================================================
# ANÁLISIS PSS (Estrés Percibido) — VERSIÓN FINAL
# =============================================================
# Diseño: medidas repetidas (PRE / POST-inmediato / POST-2 semanas)
#         comparando grupo control vs experimental
#
# Paso 1: verificar normalidad (Shapiro-Wilk)
# Paso 2: según resultado → Friedman (no paramétrico) o ANOVA (paramétrico)
# =============================================================


# ---- 1. PAQUETES --------------------------------------------
# Descomentar la primera vez:
# install.packages("tidyverse")
# install.packages("rstatix")
# install.packages("ggpubr")

library(tidyverse)
library(rstatix)
library(ggpubr)


# ---- 2. CARGAR Y PREPARAR DATOS ----------------------------
datos_pss <- read.csv("C:/Users/HP/Documents/TESIS ANT/Cuestionarios/PSS/PSS_estres_percibido_scored.csv")

datos_pss <- datos_pss %>%
  select(ID, momento, grupo, pss_total) %>%
  filter(!is.na(pss_total),
         !is.na(grupo),
         grupo %in% c("control", "experimental"))

datos_pss$ID      <- factor(datos_pss$ID)
datos_pss$momento <- factor(datos_pss$momento,
                            levels = c("pre", "post_inmediato", "post_2semanas"),
                            labels = c("PRE", "POST-inmediato", "POST-2 semanas"))
datos_pss$grupo   <- factor(datos_pss$grupo,
                            levels = c("control", "experimental"))

cat("\n=== PARTICIPANTES POR GRUPO Y MOMENTO ===\n")
print(table(datos_pss$grupo, datos_pss$momento))


# ---- 3. ESTADÍSTICAS DESCRIPTIVAS --------------------------
cat("\n=== MEDIANA Y RANGO INTERCUARTÍLICO (RIC) ===\n")
descriptivos <- datos_pss %>%
  group_by(grupo, momento) %>%
  summarise(
    n       = n(),
    mediana = median(pss_total, na.rm = TRUE),
    RIC_25  = quantile(pss_total, 0.25, na.rm = TRUE),
    RIC_75  = quantile(pss_total, 0.75, na.rm = TRUE),
    .groups = "drop"
  )
print(descriptivos)


# ---- 4. VERIFICACIÓN DE NORMALIDAD (Shapiro-Wilk) ----------
cat("\n=== PRUEBA DE NORMALIDAD (Shapiro-Wilk) ===\n")
cat("p > 0.05 → normal | p < 0.05 → NO normal\n\n")
normalidad <- datos_pss %>%
  group_by(grupo, momento) %>%
  shapiro_test(pss_total)
print(normalidad)


# ---- 5. PRUEBA DE FRIEDMAN ----------------------------------
# Si algún grupo/momento NO es normal → usamos Friedman
# Si todos son normales → se puede usar ANOVA mixto

datos_completos <- datos_pss %>%
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
  friedman_test(pss_total ~ momento | ID)
print(friedman_control)

cat("\n--- Grupo EXPERIMENTAL ---\n")
friedman_experimental <- datos_completos %>%
  filter(grupo == "experimental") %>%
  mutate(ID = droplevels(ID)) %>%
  friedman_test(pss_total ~ momento | ID)
print(friedman_experimental)


# ---- 6. POST-HOC: WILCOXON PAREADO -------------------------
cat("\n=== POST-HOC: WILCOXON PAREADO + BONFERRONI ===\n")

cat("\n--- Grupo CONTROL ---\n")
wilcox_control <- datos_completos %>%
  filter(grupo == "control") %>%
  mutate(ID = droplevels(ID)) %>%
  pairwise_wilcox_test(
    pss_total ~ momento,
    paired          = TRUE,
    p.adjust.method = "bonferroni"
  )
print(wilcox_control)

cat("\n--- Grupo EXPERIMENTAL ---\n")
wilcox_experimental <- datos_completos %>%
  filter(grupo == "experimental") %>%
  mutate(ID = droplevels(ID)) %>%
  pairwise_wilcox_test(
    pss_total ~ momento,
    paired          = TRUE,
    p.adjust.method = "bonferroni"
  )
print(wilcox_experimental)


# ---- 7. MANN-WHITNEY ----------------------------------------
cat("\n=== MANN-WHITNEY: CONTROL vs EXPERIMENTAL POR MOMENTO ===\n")
mann_whitney <- datos_pss %>%
  group_by(momento) %>%
  wilcox_test(pss_total ~ grupo) %>%
  add_significance()
print(mann_whitney)


# ---- 8. GRÁFICO ---------------------------------------------
grafico <- ggplot(datos_pss, aes(x = momento, y = pss_total, fill = grupo)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 2) +
  scale_fill_manual(
    values = c("control" = "#E07B54", "experimental" = "#4A90D9"),
    labels = c("Control", "Experimental")
  ) +
  labs(
    title    = "Estrés Percibido (PSS) según grupo y momento",
    subtitle = "Mediana y rango intercuartílico — pruebas no paramétricas",
    x        = "Momento de medición",
    y        = "Puntaje total PSS (0–40)",
    fill     = "Grupo"
  ) +
  theme_pubr() +
  theme(legend.position = "top")

print(grafico)

# Para guardar el gráfico, descomentar:
# ggsave("grafico_PSS.png", grafico, width = 8, height = 5, dpi = 300)

# Cargar el archivo completo con todos los ítems
pss_completo <- read.csv("C:/Users/HP/Documents/TESIS ANT/Cuestionarios/PSS/PSS_estres_percibido_scored.csv")

library(tidyverse)

pss_completo <- read.csv("C:/Users/HP/Documents/TESIS ANT/Cuestionarios/PSS/PSS_estres_percibido_scored.csv")

pss_completo %>%
  filter(ID == 7) %>%
  select(ID, momento,
         pss_1_orig, pss_1,
         pss_2_orig, pss_2,
         pss_3_orig, pss_3,
         pss_4_orig, pss_4,
         pss_5_orig, pss_5,
         pss_6_orig, pss_6,
         pss_7_orig, pss_7,
         pss_8_orig, pss_8,
         pss_9_orig, pss_9,
         pss_10_orig, pss_10,
         pss_total)

