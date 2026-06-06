# =============================================================
# ANÁLISIS RYFF (Bienestar Psicológico) — VERSIÓN FINAL
# =============================================================
# Diseño: PRE vs POST-2 semanas
#         comparando grupo control vs experimental
#
# Nota: se excluye POST-inmediato porque el grupo control
# no tiene datos en ese momento.
#
# Muestra: 30 controles y 18 experimentales con datos completos
#
# Subescalas:
#   - Autoaceptación        (ítems 1,7,13,19,25,31)
#   - Relaciones positivas  (ítems 2,8,14,20,26,32)
#   - Autonomía             (ítems 3,9,15,21,27,33)
#   - Dominio del entorno   (ítems 4,10,16,22,28,34)
#   - Propósito en la vida  (ítems 5,11,17,23,29,35)
#   - Crecimiento personal  (ítems 6,12,18,24,30,36,37,38,39)
#
# Análisis:
#   1. Estadísticos descriptivos (mediana y RIC)
#   2. Normalidad: Shapiro-Wilk
#   3. Wilcoxon pareado: PRE vs POST-2sem por grupo (total y subescalas)
#   4. Tamaño del efecto: r de Wilcoxon
#   5. Mann-Whitney: control vs experimental por momento
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
datos_ryff <- read.csv("C:/Users/HP/Documents/TESIS ANT/Cuestionarios/Ryff/Ryff_scored_v2.csv")

subescalas <- c("ryff_autoaceptacion", "ryff_relaciones_positivas",
                "ryff_autonomia", "ryff_dominio_entorno",
                "ryff_proposito_vida", "ryff_crecimiento_personal")

datos_ryff <- datos_ryff %>%
  select(ID, momento, grupo, ryff_total, all_of(subescalas)) %>%
  filter(!is.na(ryff_total),
         !is.na(grupo),
         grupo %in% c("control", "experimental"))

datos_ryff$ID      <- factor(datos_ryff$ID)
datos_ryff$momento <- factor(datos_ryff$momento,
                             levels = c("PRE", "POST_2sem"),
                             labels = c("PRE", "POST-2 semanas"))
datos_ryff$grupo   <- factor(datos_ryff$grupo,
                             levels = c("control", "experimental"))

# Solo participantes con ambos momentos completos
datos_pareados <- datos_ryff %>%
  group_by(ID) %>%
  filter(n() == 2) %>%
  ungroup()

cat("=== MUESTRA ===\n")
print(table(datos_pareados$grupo, datos_pareados$momento))


# ---- 3. ESTADÍSTICOS DESCRIPTIVOS --------------------------
cat("\n=== DESCRIPTIVOS — PUNTAJE TOTAL ===\n")
datos_pareados %>%
  group_by(grupo, momento) %>%
  summarise(
    n   = n(),
    Mdn = median(ryff_total),
    P25 = quantile(ryff_total, 0.25),
    P75 = quantile(ryff_total, 0.75),
    .groups = "drop"
  ) %>% print()

cat("\n=== DESCRIPTIVOS — SUBESCALAS (medianas) ===\n")
datos_pareados %>%
  group_by(grupo, momento) %>%
  summarise(across(all_of(subescalas), ~ median(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  print()


# ---- 4. NORMALIDAD (Shapiro-Wilk) --------------------------
cat("\n=== NORMALIDAD — PUNTAJE TOTAL (Shapiro-Wilk) ===\n")
cat("p > .05 = distribución normal\n\n")
datos_pareados %>%
  group_by(grupo, momento) %>%
  shapiro_test(ryff_total) %>%
  select(grupo, momento, statistic, p) %>%
  print()


# ---- 5. WILCOXON PAREADO — PUNTAJE TOTAL -------------------
cat("\n=== WILCOXON PAREADO: PRE vs POST-2 semanas ===\n\n")

for (g in c("control", "experimental")) {
  cat(paste0("--- ", toupper(g), " ---\n"))
  sub <- datos_pareados %>%
    filter(grupo == g) %>%
    mutate(ID = droplevels(ID))

  w <- sub %>%
    rstatix::wilcox_test(ryff_total ~ momento, paired = TRUE) %>%
    add_significance()
  e <- sub %>%
    wilcox_effsize(ryff_total ~ momento, paired = TRUE)
  cat(sprintf("Total: W = %.1f, p = %.4f %s, r = %.3f (%s)\n\n",
              w$statistic, w$p, w$p.signif,
              e$effsize, e$magnitude))
}


# ---- 6. WILCOXON PAREADO — SUBESCALAS ----------------------
cat("=== WILCOXON PAREADO POR SUBESCALA ===\n\n")

nombres_sub <- c(
  "ryff_autoaceptacion"       = "Autoaceptación",
  "ryff_relaciones_positivas" = "Relaciones positivas",
  "ryff_autonomia"            = "Autonomía",
  "ryff_dominio_entorno"      = "Dominio del entorno",
  "ryff_proposito_vida"       = "Propósito en la vida",
  "ryff_crecimiento_personal" = "Crecimiento personal"
)

for (g in c("control", "experimental")) {
  cat(paste0("--- ", toupper(g), " ---\n"))
  sub <- datos_pareados %>%
    filter(grupo == g) %>%
    mutate(ID = droplevels(ID))

  for (var in subescalas) {
    w <- sub %>%
      rstatix::wilcox_test(reformulate("momento", response = var),
                           paired = TRUE) %>%
      add_significance()
    e <- sub %>%
      wilcox_effsize(reformulate("momento", response = var),
                     paired = TRUE)
    cat(sprintf("  %-25s W = %.1f, p = %.4f %s, r = %.3f (%s)\n",
                nombres_sub[var],
                w$statistic, w$p, w$p.signif,
                e$effsize, e$magnitude))
  }
  cat("\n")
}


# ---- 7. MANN-WHITNEY ----------------------------------------
cat("=== MANN-WHITNEY: CONTROL vs EXPERIMENTAL ===\n\n")

cat("Puntaje total:\n")
datos_pareados %>%
  group_by(momento) %>%
  rstatix::wilcox_test(ryff_total ~ grupo) %>%
  add_significance() %>%
  select(momento, statistic, p, p.signif) %>%
  print()


# ---- 8. GRÁFICOS --------------------------------------------

# Figura 1: Puntaje total
fig1 <- ggplot(datos_pareados,
               aes(x = momento, y = ryff_total, fill = grupo)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 2,
               width = 0.5, position = position_dodge(0.6)) +
  scale_fill_manual(
    values = c("control" = "#E07B54", "experimental" = "#4A90D9"),
    labels = c("Control", "Experimental")
  ) +
  labs(
    title = "Figura 1. Bienestar psicológico (Ryff) — puntaje total",
    x     = "Momento de medición",
    y     = "Bienestar psicológico\n(puntaje total Ryff, rango 39–234)",
    fill  = "Grupo"
  ) +
  theme_pubr() +
  theme(legend.position = "top",
        plot.title      = element_text(size = 11))

# Preparar datos para subescalas
datos_sub_long <- datos_pareados %>%
  pivot_longer(cols = all_of(subescalas),
               names_to  = "subescala",
               values_to = "puntaje") %>%
  mutate(subescala = recode(subescala, !!!nombres_sub))

# Figura 2a: Subescalas de 6 ítems (rango 6-36)
fig2a <- datos_sub_long %>%
  filter(subescala != "Crecimiento personal") %>%
  group_by(grupo, momento, subescala) %>%
  summarise(mediana = median(puntaje, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = momento, y = mediana, color = grupo, group = grupo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(~ subescala, ncol = 3) +
  scale_y_continuous(limits = c(6, 36), breaks = seq(6, 36, by = 6)) +
  scale_color_manual(
    values = c("control" = "#E07B54", "experimental" = "#4A90D9"),
    labels = c("Control", "Experimental")
  ) +
  labs(
    title = "Figura 2a. Subescalas Ryff (6 ítems c/u, rango 6–36)",
    x     = "Momento de medición",
    y     = "Bienestar psicológico\n(mediana por subescala, rango 6–36)",
    color = "Grupo"
  ) +
  theme_pubr() +
  theme(legend.position = "top",
        axis.text.x     = element_text(angle = 30, hjust = 1),
        strip.text      = element_text(size = 9),
        plot.title      = element_text(size = 11))

# Figura 2b: Crecimiento personal (9 ítems, rango 9-54)
fig2b <- datos_sub_long %>%
  filter(subescala == "Crecimiento personal") %>%
  group_by(grupo, momento, subescala) %>%
  summarise(mediana = median(puntaje, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = momento, y = mediana, color = grupo, group = grupo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_y_continuous(limits = c(9, 54), breaks = seq(9, 54, by = 9)) +
  scale_color_manual(
    values = c("control" = "#E07B54", "experimental" = "#4A90D9"),
    labels = c("Control", "Experimental")
  ) +
  labs(
    title = "Figura 2b. Crecimiento personal (9 ítems, rango 9–54)",
    x     = "Momento de medición",
    y     = "Bienestar psicológico\n(mediana crecimiento personal, rango 9–54)",
    color = "Grupo"
  ) +
  theme_pubr() +
  theme(legend.position = "top",
        axis.text.x     = element_text(angle = 30, hjust = 1),
        plot.title      = element_text(size = 11))

print(fig1)
print(fig2a)
print(fig2b)

# Para guardar en alta resolución:
ggsave("Fig1_Ryff_total.png",          fig1,  width = 7, height = 5, dpi = 300)
ggsave("Fig2a_Ryff_subescalas.png",    fig2a, width = 10, height = 7, dpi = 300)
ggsave("Fig2b_Ryff_crec_personal.png", fig2b, width = 5, height = 4, dpi = 300)
