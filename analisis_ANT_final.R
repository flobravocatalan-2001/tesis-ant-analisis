# =============================================================
# ANÁLISIS ANT (Attention Network Test) — VERSIÓN FINAL
# =============================================================
# Diseño: medidas repetidas (PRE vs POST)
#         comparando grupo control vs experimental
#
# Variables dependientes:
#   - Alerta:             RT(no cue) - RT(double cue)
#   - Orientación:        RT(center cue) - RT(spatial cue)
#   - Control ejecutivo:  RT(incongruent) - RT(congruent)
#   - RT global:          tiempo de reacción medio
#   - Accuracy:           proporción de respuestas correctas
#
# Análisis:
#   1. Limpieza conductual
#   2. Estadísticos descriptivos
#   3. Verificación de supuestos (normalidad, homogeneidad)
#   4. ANOVA mixto (tiempo × grupo) para cada red y RT/accuracy
#   5. Post-hoc si hay efectos significativos
#   6. Gráficos
# =============================================================


# ---- 1. PAQUETES --------------------------------------------
# Descomentar la primera vez:
# install.packages(c("tidyverse", "readr", "janitor", "ez", "car", "rstatix", "ggpubr", "coin"))

library(tidyverse)
library(readr)
library(janitor)
library(ez)
library(car)
library(coin)
library(rstatix)
library(ggpubr)


# ---- 2. CARGAR DATOS ----------------------------------------
setwd("C:/Users/HP/Documents/TESIS ANT/ANT/Conductuales ANT")

archivos <- list.files(pattern = "\\.csv$")

datos_todos <- purrr::map2_df(
  archivos, archivos,
  ~ read_csv(.x) %>% mutate(archivo_real = .y)
)

# Extraer tiempo (pre/post) y sujeto desde nombre de archivo
datos_todos <- datos_todos %>%
  mutate(
    tiempo = case_when(
      str_detect(str_to_lower(archivo_real), "_1_") ~ "pre",
      str_detect(str_to_lower(archivo_real), "_2_") ~ "post",
      TRUE ~ NA_character_
    ),
    sujeto = str_extract(str_to_upper(archivo_real), "S\\d+")
  )

cat("=== VERIFICACIÓN INICIAL ===\n")
cat("Distribución pre/post:\n")
print(table(datos_todos$tiempo, useNA = "always"))
cat("\nSujetos sin ID:", sum(is.na(datos_todos$sujeto)), "\n")


# ---- 3. LIMPIEZA CONDUCTUAL ---------------------------------
# Eliminar ensayos sin respuesta
datos_1 <- datos_todos %>% filter(!is.na(rt_sec))

# Eliminar RT extremos (< 200ms o > 1700ms)
datos_2 <- datos_1 %>%
  filter(rt_sec >= 0.200, rt_sec <= 1.700)

cat("\nEnsayos originales:", nrow(datos_todos))
cat("\nTras eliminar sin respuesta:", nrow(datos_1))
cat("\nTras eliminar RT extremos:", nrow(datos_2), "\n")

# Base para RT: solo respuestas correctas con cue y flanker codificados
datos_rt <- datos_2 %>%
  filter(response_correct == 1) %>%
  mutate(
    cue = case_when(
      cue_code == 11 ~ "no",
      cue_code == 12 ~ "center",
      cue_code == 13 ~ "double",
      cue_code == 14 ~ "spatial",
      TRUE ~ NA_character_
    ),
    flanker = case_when(
      target_code == 22 ~ "congruent",
      target_code == 23 ~ "incongruent",
      target_code == 21 ~ "neutral",
      TRUE ~ NA_character_
    )
  )


# ---- 4. ASIGNAR GRUPOS -------------------------------------
asignar_grupo <- function(sujeto_vec) {
  num <- as.numeric(str_extract(sujeto_vec, "\\d+"))
  case_when(
    num >= 1  & num <= 15 ~ "experimental",
    num >= 33 & num <= 48 ~ "experimental",
    num >= 16 & num <= 32 ~ "control",
    num >= 49 & num <= 64 ~ "control",
    TRUE ~ NA_character_
  )
}


# ---- 5. CALCULAR ÍNDICES DE REDES ANT ----------------------
base_condiciones <- datos_rt %>%
  filter(!is.na(cue), !is.na(flanker)) %>%
  group_by(sujeto, tiempo, cue, flanker) %>%
  summarise(rt = mean(rt_sec, na.rm = TRUE), .groups = "drop")

redes_ant <- base_condiciones %>%
  group_by(sujeto, tiempo) %>%
  summarise(
    alerta = ifelse(
      all(c("no", "double") %in% cue),
      mean(rt[cue == "no"]) - mean(rt[cue == "double"]),
      NA_real_
    ),
    orientacion = ifelse(
      all(c("center", "spatial") %in% cue),
      mean(rt[cue == "center"]) - mean(rt[cue == "spatial"]),
      NA_real_
    ),
    control = ifelse(
      all(c("incongruent", "congruent") %in% flanker),
      mean(rt[flanker == "incongruent"]) - mean(rt[flanker == "congruent"]),
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  mutate(grupo = asignar_grupo(sujeto))

# Solo sujetos con PRE y POST completos
redes_ant_completa <- redes_ant %>%
  group_by(sujeto) %>%
  filter(n() == 2) %>%
  ungroup()

cat("\n=== MUESTRA REDES ANT ===\n")
print(table(redes_ant_completa$grupo, redes_ant_completa$tiempo))


# ---- 6. DETECTAR Y EXCLUIR SUJETOS PROBLEMÁTICOS -----------
resumen_sujeto <- redes_ant_completa %>%
  group_by(sujeto) %>%
  summarise(
    alerta_media    = mean(alerta, na.rm = TRUE),
    orientacion_media = mean(orientacion, na.rm = TRUE),
    control_media   = mean(control, na.rm = TRUE),
    n_NA = sum(is.na(alerta) | is.na(orientacion) | is.na(control)),
    .groups = "drop"
  )

# Outliers (z > 3) o valores negativos sistemáticos
outliers <- redes_ant_completa %>%
  group_by(sujeto) %>%
  summarise(
    alerta_out    = any(abs(scale(alerta)) > 3, na.rm = TRUE),
    orientacion_out = any(abs(scale(orientacion)) > 3, na.rm = TRUE),
    control_out   = any(abs(scale(control)) > 3, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(alerta_out | orientacion_out | control_out)

problematicos <- resumen_sujeto %>%
  filter(
    n_NA > 1 |
      (alerta_media < 0 & orientacion_media < 0) |
      control_media < 0
  )

sujetos_excluir <- union(outliers$sujeto, problematicos$sujeto)

cat("\nSujetos problemáticos detectados:", paste(sujetos_excluir, collapse = ", "), "\n")

redes_ant_limpia <- redes_ant_completa %>%
  filter(!sujeto %in% sujetos_excluir)

cat("\nMuestra final tras exclusiones:\n")
print(table(redes_ant_limpia$grupo, redes_ant_limpia$tiempo))


# ---- 7. DESCRIPTIVOS REDES ANT -----------------------------
redes_largas <- redes_ant_limpia %>%
  pivot_longer(
    cols = c(alerta, orientacion, control),
    names_to  = "red",
    values_to = "indice"
  ) %>%
  mutate(
    tiempo = factor(tiempo, levels = c("pre", "post")),
    red    = factor(red, levels = c("alerta", "orientacion", "control"))
  )

cat("\n=== DESCRIPTIVOS — REDES ANT (ms) ===\n")
descriptivos_redes <- redes_largas %>%
  group_by(grupo, tiempo, red) %>%
  summarise(
    n    = n(),
    M    = round(mean(indice, na.rm = TRUE) * 1000, 1),
    SD   = round(sd(indice, na.rm = TRUE) * 1000, 1),
    .groups = "drop"
  )
print(descriptivos_redes)


# ---- 8. NORMALIDAD ------------------------------------------
cat("\n=== NORMALIDAD (Shapiro-Wilk) por red ===\n")
redes_largas %>%
  group_by(grupo, tiempo, red) %>%
  shapiro_test(indice) %>%
  select(grupo, tiempo, red, statistic, p) %>%
  print(n = 30)


# ---- 9. ANOVA MIXTO POR RED --------------------------------
cat("\n=== ANOVA MIXTO: TIEMPO × GRUPO ===\n")
cat("Reporta: F, p, ges (eta cuadrado generalizado)\n\n")

for (r in c("alerta", "orientacion", "control")) {
  cat(paste0("--- ", toupper(r), " ---\n"))

  base <- redes_largas %>%
    filter(red == r, !is.na(indice)) %>%
    group_by(sujeto) %>%
    filter(n() == 2) %>%
    ungroup() %>%
    mutate(sujeto = factor(sujeto),
           grupo  = factor(grupo),
           tiempo = factor(tiempo, levels = c("pre", "post")))

  anova_res <- ezANOVA(
    data     = base,
    dv       = .(indice),
    wid      = .(sujeto),
    within   = .(tiempo),
    between  = .(grupo),
    detailed = TRUE,
    type     = 3
  )

  tab <- anova_res$ANOVA %>%
    select(Effect, F, p, ges) %>%
    mutate(across(c(F, p, ges), ~ round(.x, 4)))

  print(tab)
  cat("\n")
}


# ---- 10. POST-HOC (si hay efectos significativos) -----------
# Wilcoxon pareado por grupo para cada red
cat("=== POST-HOC: WILCOXON PAREADO POR GRUPO ===\n\n")

for (r in c("alerta", "orientacion", "control")) {
  cat(paste0("--- ", toupper(r), " ---\n"))

  base <- redes_largas %>%
    filter(red == r, !is.na(indice)) %>%
    group_by(sujeto) %>%
    filter(n() == 2) %>%
    ungroup()

  for (g in c("control", "experimental")) {
    sub <- base %>%
      filter(grupo == g) %>%
      mutate(sujeto = droplevels(factor(sujeto)))

    w <- sub %>%
      rstatix::wilcox_test(indice ~ tiempo, paired = TRUE) %>%
      add_significance()
    e <- sub %>%
      wilcox_effsize(indice ~ tiempo, paired = TRUE)

    cat(sprintf("  %-14s W = %.1f, p = %.4f %s, r = %.3f (%s)\n",
                g, w$statistic, w$p, w$p.signif,
                e$effsize, e$magnitude))
  }
  cat("\n")
}


# ---- 11. RT GLOBAL Y ACCURACY ------------------------------
# RT global
rt_global <- datos_rt %>%
  group_by(sujeto, tiempo) %>%
  summarise(rt_medio = mean(rt_sec, na.rm = TRUE), .groups = "drop") %>%
  mutate(grupo = asignar_grupo(sujeto)) %>%
  group_by(sujeto) %>%
  filter(n_distinct(tiempo) == 2) %>%
  ungroup()

# Accuracy
accuracy <- datos_2 %>%
  group_by(sujeto, tiempo) %>%
  summarise(
    accuracy = mean(response_correct, na.rm = TRUE),
    errores  = sum(response_correct == 0, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(grupo = asignar_grupo(sujeto)) %>%
  group_by(sujeto) %>%
  filter(n_distinct(tiempo) == 2) %>%
  ungroup()

cat("=== DESCRIPTIVOS — RT GLOBAL (ms) ===\n")
rt_global %>%
  group_by(grupo, tiempo) %>%
  summarise(
    n  = n(),
    M  = round(mean(rt_medio) * 1000, 1),
    SD = round(sd(rt_medio) * 1000, 1),
    .groups = "drop"
  ) %>% print()

cat("\n=== DESCRIPTIVOS — ACCURACY ===\n")
accuracy %>%
  group_by(grupo, tiempo) %>%
  summarise(
    n        = n(),
    M_acc    = round(mean(accuracy), 3),
    SD_acc   = round(sd(accuracy), 3),
    M_err    = round(mean(errores), 1),
    .groups  = "drop"
  ) %>% print()

# ANOVA RT global
cat("\n=== ANOVA MIXTO — RT GLOBAL ===\n")
anova_rt <- ezANOVA(
  data     = rt_global %>%
    mutate(sujeto = factor(sujeto), grupo = factor(grupo),
           tiempo = factor(tiempo, levels = c("pre","post"))),
  dv       = .(rt_medio),
  wid      = .(sujeto),
  within   = .(tiempo),
  between  = .(grupo),
  detailed = TRUE, type = 3
)
print(anova_rt$ANOVA %>% select(Effect, F, p, ges) %>%
        mutate(across(c(F, p, ges), ~ round(.x, 4))))

# ANOVA accuracy
cat("\n=== ANOVA MIXTO — ACCURACY ===\n")
anova_acc <- ezANOVA(
  data     = accuracy %>%
    mutate(sujeto = factor(sujeto), grupo = factor(grupo),
           tiempo = factor(tiempo, levels = c("pre","post"))),
  dv       = .(accuracy),
  wid      = .(sujeto),
  within   = .(tiempo),
  between  = .(grupo),
  detailed = TRUE, type = 3
)
print(anova_acc$ANOVA %>% select(Effect, F, p, ges) %>%
        mutate(across(c(F, p, ges), ~ round(.x, 4))))


# ---- 12. GRÁFICO -------------------------------------------
etiquetas_red <- c(
  alerta     = "Alerta\n(no cue − double cue)",
  control    = "Control ejecutivo\n(incongruent − congruent)",
  orientacion = "Orientación\n(center − spatial)"
)

descriptivos_ms <- redes_largas %>%
  group_by(grupo, tiempo, red) %>%
  summarise(
    M  = mean(indice, na.rm = TRUE) * 1000,
    SE = (sd(indice, na.rm = TRUE) / sqrt(n())) * 1000,
    .groups = "drop"
  ) %>%
  mutate(tiempo = factor(tiempo, levels = c("pre", "post"),
                         labels = c("PRE", "POST")))

fig_ant <- ggplot(descriptivos_ms,
                  aes(x = tiempo, y = M,
                      group = grupo, color = grupo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE),
                width = 0.1, linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "gray50", linewidth = 0.5) +
  facet_wrap(~ red, ncol = 3,
             labeller = labeller(red = etiquetas_red)) +
  scale_color_manual(
    values = c("control" = "#E07B54", "experimental" = "#4A90D9"),
    labels = c("Control", "Experimental")
  ) +
  labs(
    title = "Figura 3. Índices atencionales ANT por grupo y momento",
    subtitle = "Media ± error estándar (ms)",
    x     = "Momento de medición",
    y     = "Índice atencional (ms)",
    color = "Grupo"
  ) +
  theme_pubr() +
  theme(legend.position = "top",
        strip.text      = element_text(size = 9),
        plot.title      = element_text(size = 11))

print(fig_ant)

# Para guardar:
# ggsave("Fig3_ANT_redes.png", fig_ant, width = 10, height = 5, dpi = 300)
