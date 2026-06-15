# =============================================================
# ANÁLISIS ANT (Attention Network Test) — VERSIÓN COMBINADA FINAL
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
#   2. Cálculo de índices ANT
#   3. Detección y exclusión de sujetos problemáticos
#   4. Estadísticos descriptivos
#   5. Verificación de supuestos (normalidad + Levene)
#   6. Modelos Mixtos Lineales - LMM (tiempo × grupo)
#   7. Post-hoc con medias marginales estimadas (emmeans)
#   8. Análisis de sensibilidad sin S13
#   9. RT global y accuracy
#  10. Gráficos
# =============================================================


# ---- 1. PAQUETES --------------------------------------------
# Descomentar la primera vez:
# install.packages(c("tidyverse", "readr", "janitor", "ez", "car", "rstatix", "ggpubr", "coin"))
install.packages(c("lme4", "lmerTest", "emmeans"))
install.packages("MuMIn")
library(MuMIn)
library(lme4)
library(lmerTest)
library(emmeans)
library(tidyverse)
library(readr)
library(janitor)
library(ez)
library(car)
library(coin)
library(rstatix)
library(ggpubr)


# ---- 2. CARGAR DATOS ----------------------------------------
setwd("C:/Users/HP/Documents/TESIS ANT/ANT/Conductuales/ANT")

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
cat("Sujetos sin ID:", sum(is.na(datos_todos$sujeto)), "\n")


# ---- 3. LIMPIEZA CONDUCTUAL ---------------------------------
datos_1 <- datos_todos %>% filter(!is.na(rt_sec))
datos_2 <- datos_1 %>% filter(rt_sec >= 0.200, rt_sec <= 1.700)

cat("\nEnsayos originales:          ", nrow(datos_todos))
cat("\nTras eliminar sin respuesta: ", nrow(datos_1))
cat("\nTras eliminar RT extremos:   ", nrow(datos_2), "\n")

# Solo respuestas correctas con cue y flanker codificados
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

# Número de ensayos válidos por sujeto/tiempo/condición
ensayos_validos <- datos_rt %>%
  filter(!is.na(cue), !is.na(flanker)) %>%
  group_by(sujeto, tiempo, cue, flanker) %>%
  summarise(n_ensayos = n(), .groups = "drop")

cat("\nResumen de ensayos válidos por celda:\n")
print(summary(ensayos_validos$n_ensayos))


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


# ---- 5. CALCULAR ÍNDICES ANT --------------------------------
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

cat("\n=== MUESTRA INICIAL (antes de exclusiones) ===\n")
print(table(redes_ant_completa$grupo, redes_ant_completa$tiempo))

# Sujetos con NAs en alguna red
cat("\nSujetos con NAs en alguna red:\n")
print(redes_ant_completa %>%
        filter(is.na(alerta) | is.na(orientacion) | is.na(control)) %>%
        select(sujeto, tiempo, alerta, orientacion, control))


# ---- 6. DETECCIÓN Y EXCLUSIÓN DE SUJETOS PROBLEMÁTICOS -----
# Criterio 1: outlier extremo (z > 3) en cualquier red
outliers <- redes_ant_completa %>%
  group_by(sujeto) %>%
  summarise(
    alerta_out      = any(abs(scale(alerta)) > 3, na.rm = TRUE),
    orientacion_out = any(abs(scale(orientacion)) > 3, na.rm = TRUE),
    control_out     = any(abs(scale(control)) > 3, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(alerta_out | orientacion_out | control_out)

# Criterio 2: control ejecutivo negativo (conceptualmente inválido)
# o alerta Y orientacion negativas simultáneamente
resumen_sujeto <- redes_ant_completa %>%
  group_by(sujeto) %>%
  summarise(
    alerta_media      = mean(alerta, na.rm = TRUE),
    orientacion_media = mean(orientacion, na.rm = TRUE),
    control_media     = mean(control, na.rm = TRUE),
    n_NA = sum(is.na(alerta) | is.na(orientacion) | is.na(control)),
    .groups = "drop"
  )

problematicos <- resumen_sujeto %>%
  filter(
    n_NA > 1 |
      (alerta_media < 0 & orientacion_media < 0) |
      control_media < 0
  )

sujetos_excluir <- union(outliers$sujeto, problematicos$sujeto)

cat("\n=== SUJETOS EXCLUIDOS ===\n")
cat("Por outlier extremo:", paste(outliers$sujeto, collapse = ", "), "\n")
cat("Por valores problemáticos:", paste(problematicos$sujeto, collapse = ", "), "\n")
cat("Total excluidos:", length(sujetos_excluir), "\n")

redes_ant_limpia <- redes_ant_completa %>%
  filter(!sujeto %in% sujetos_excluir)

cat("\n=== MUESTRA FINAL (tras exclusiones) ===\n")
print(table(redes_ant_limpia$grupo, redes_ant_limpia$tiempo))


# ---- 7. DESCRIPTIVOS ----------------------------------------
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
    n  = n(),
    M  = round(mean(indice, na.rm = TRUE) * 1000, 1),
    SD = round(sd(indice, na.rm = TRUE) * 1000, 1),
    SE = round((sd(indice, na.rm = TRUE) / sqrt(n())) * 1000, 1),
    .groups = "drop"
  )
print(descriptivos_redes)


# ---- 8. SUPUESTOS -------------------------------------------
cat("\n=== NORMALIDAD (Shapiro-Wilk) ===\n")
cat("p > .05 = distribución normal\n\n")
redes_largas %>%
  group_by(grupo, tiempo, red) %>%
  shapiro_test(indice) %>%
  select(grupo, tiempo, red, statistic, p) %>%
  print(n = 30)

etiquetas_red <- c(
  alerta      = "Alerta\n(no cue − double cue)",
  orientacion = "Orientación\n(center − spatial)",
  control     = "Control ejecutivo\n(incongruent − congruent)"
)


# Histograma con curva normal superpuesta
redes_largas %>%
  ggplot(aes(x = indice * 1000)) +  # *1000 para convertir a ms
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 15, 
                 fill = "#4A90D9", 
                 color = "white",
                 alpha = 0.7) +
  stat_function(fun = dnorm,
                args = list(
                  mean = mean(redes_largas$indice * 1000, na.rm = TRUE),
                  sd   = sd(redes_largas$indice * 1000, na.rm = TRUE)
                ),
                color = "red", linewidth = 1) +
  facet_grid(grupo + tiempo ~ red,
             labeller = labeller(red = etiquetas_red)) +
  labs(
    title = "Distribución de índices ANT por grupo y momento",
    x     = "Índice atencional (ms)",
    y     = "Densidad"
  ) +
  theme_pubr() +
  theme(strip.text = element_text(size = 8))

cat("\n=== HOMOGENEIDAD DE VARIANZAS (Levene) ===\n")
for (r in c("alerta", "orientacion", "control")) {
  cat(paste0("--- ", r, " ---\n"))
  base_pre  <- redes_largas %>% filter(red == r, tiempo == "pre")
  base_post <- redes_largas %>% filter(red == r, tiempo == "post")
  cat("  PRE:  ")
  print(leveneTest(indice ~ grupo, data = base_pre))
  cat("  POST: ")
  print(leveneTest(indice ~ grupo, data = base_post))
}


# ---- 9. Linear Mixed Models: TIEMPO × GRUPO --------------------------------
cat("\n=== MODELOS MIXTOS LINEALES (LMM): TIEMPO × GRUPO ===\n")
cat("Reporta: F, gl, p\n\n")

for (r in c("alerta", "orientacion", "control")) {
  cat(paste0("--- ", toupper(r), " ---\n"))
  
  base <- redes_largas %>%
    filter(red == r, !is.na(indice)) %>%
    group_by(sujeto) %>%
    filter(n() == 2) %>%
    ungroup() %>%
    mutate(
      sujeto = factor(sujeto),
      grupo  = factor(grupo),
      tiempo = factor(tiempo, levels = c("pre", "post"))
    )
  
  # El modelo:
  # indice ~ tiempo * grupo  → queremos saber si hay efecto de tiempo,
  #                             de grupo, y si interactúan entre sí
  # (1 | sujeto)             → cada persona tiene su propio punto de partida
  
  modelo <- lmer(indice ~ tiempo * grupo + (1 | sujeto), data = base)
  
  # Tabla de resultados con valores F y p
  print(anova(modelo, type = 3))
  
  # Tamaño del efecto (r²)
  cat("R² marginal (solo efectos fijos):",
      round(MuMIn::r.squaredGLMM(modelo)[1], 4), "\n")
  cat("R² condicional (modelo completo):",
      round(MuMIn::r.squaredGLMM(modelo)[2], 4), "\n\n")
}


# ---- 10. POST-HOC CON LMM -----------------------------------
cat("=== POST-HOC: COMPARACIONES PRE vs POST POR GRUPO ===\n\n")

for (r in c("alerta", "orientacion", "control")) {
  cat(paste0("--- ", toupper(r), " ---\n"))
  
  base <- redes_largas %>%
    filter(red == r, !is.na(indice)) %>%
    group_by(sujeto) %>%
    filter(n() == 2) %>%
    ungroup() %>%
    mutate(
      sujeto = factor(sujeto),
      grupo  = factor(grupo),
      tiempo = factor(tiempo, levels = c("pre", "post"))
    )
  
  modelo <- lmer(indice ~ tiempo * grupo + (1 | sujeto), data = base)
  
  # Comparar PRE vs POST dentro de cada grupo por separado
  comparaciones <- emmeans(modelo, ~ tiempo | grupo)
  print(pairs(comparaciones, adjust = "bonferroni"))
  cat("\n")
}

# ---- 11. ANÁLISIS DE SENSIBILIDAD SIN S13 -------------------
# S13 tiene alerta = 0.735 en PRE (outlier extremo que infla
# la media del grupo experimental)

cat("=== ANÁLISIS DE SENSIBILIDAD: SIN S13 ===\n\n")

redes_largas_sinS13 <- redes_ant_completa %>%
  filter(sujeto != "S13") %>%
  pivot_longer(
    cols = c(alerta, orientacion, control),
    names_to  = "red",
    values_to = "indice"
  ) %>%
  mutate(
    tiempo = factor(tiempo, levels = c("pre", "post")),
    red    = factor(red, levels = c("alerta", "orientacion", "control"))
  )

cat("Descriptivos alerta sin S13:\n")
redes_largas_sinS13 %>%
  filter(red == "alerta") %>%
  group_by(grupo, tiempo) %>%
  summarise(M = round(mean(indice, na.rm=TRUE)*1000,1),
            SD = round(sd(indice, na.rm=TRUE)*1000,1),
            n = n(), .groups="drop") %>%
  print()

cat("\nLMM alerta sin S13:\n")
base_alerta_sinS13 <- redes_largas_sinS13 %>%
  filter(red == "alerta", !is.na(indice)) %>%
  group_by(sujeto) %>% filter(n() == 2) %>% ungroup() %>%
  mutate(sujeto = factor(sujeto), grupo = factor(grupo),
         tiempo = factor(tiempo, levels = c("pre","post")))

modelo_sinS13 <- lmer(indice ~ tiempo * grupo + (1 | sujeto),
                      data = base_alerta_sinS13)

print(anova(modelo_sinS13, type = 3))

cat("R² marginal:", round(MuMIn::r.squaredGLMM(modelo_sinS13)[1], 4), "\n")
cat("R² condicional:", round(MuMIn::r.squaredGLMM(modelo_sinS13)[2], 4), "\n")


# ---- 12. RT GLOBAL Y ACCURACY ------------------------------
rt_global <- datos_rt %>%
  group_by(sujeto, tiempo) %>%
  summarise(rt_medio = mean(rt_sec, na.rm = TRUE), .groups = "drop") %>%
  mutate(grupo = asignar_grupo(sujeto)) %>%
  group_by(sujeto) %>%
  filter(n_distinct(tiempo) == 2) %>%
  ungroup()

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

cat("\n=== DESCRIPTIVOS — RT GLOBAL (ms) ===\n")
rt_global %>%
  group_by(grupo, tiempo) %>%
  summarise(n = n(),
            M  = round(mean(rt_medio)*1000, 1),
            SD = round(sd(rt_medio)*1000, 1),
            .groups = "drop") %>% print()

cat("\n=== DESCRIPTIVOS — ACCURACY ===\n")
accuracy %>%
  group_by(grupo, tiempo) %>%
  summarise(n = n(),
            M_acc  = round(mean(accuracy), 3),
            SD_acc = round(sd(accuracy), 3),
            M_err  = round(mean(errores), 1),
            .groups = "drop") %>% print()

cat("\n=== LMM — RT GLOBAL ===\n")
modelo_rt <- lmer(rt_medio ~ tiempo * grupo + (1 | sujeto),
                  data = rt_global %>%
                    mutate(sujeto = factor(sujeto),
                           grupo  = factor(grupo),
                           tiempo = factor(tiempo, levels = c("pre","post"))))
print(anova(modelo_rt, type = 3))
cat("R² marginal:", round(MuMIn::r.squaredGLMM(modelo_rt)[1], 4), "\n")
cat("R² condicional:", round(MuMIn::r.squaredGLMM(modelo_rt)[2], 4), "\n")

cat("\n=== LMM — ACCURACY ===\n")
modelo_acc <- lmer(accuracy ~ tiempo * grupo + (1 | sujeto),
                   data = accuracy %>%
                     mutate(sujeto = factor(sujeto),
                            grupo  = factor(grupo),
                            tiempo = factor(tiempo, levels = c("pre","post"))))
print(anova(modelo_acc, type = 3))
cat("R² marginal:", round(MuMIn::r.squaredGLMM(modelo_acc)[1], 4), "\n")
cat("R² condicional:", round(MuMIn::r.squaredGLMM(modelo_acc)[2], 4), "\n")


# ---- 13. GRÁFICO -------------------------------------------
etiquetas_red <- c(
  alerta      = "Alerta\n(no cue − double cue)",
  orientacion = "Orientación\n(center − spatial)",
  control     = "Control ejecutivo\n(incongruent − congruent)"
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
                  aes(x = tiempo, y = M, group = grupo, color = grupo)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "gray50", linewidth = 0.5) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE),
                width = 0.1, linewidth = 0.8) +
  facet_wrap(~ red, ncol = 3,
             labeller = labeller(red = etiquetas_red)) +
  scale_color_manual(
    values = c("control" = "#E07B54", "experimental" = "#4A90D9"),
    labels = c("Control", "Experimental")
  ) +
  labs(
    title    = "Figura 3. Índices atencionales ANT por grupo y momento",
    subtitle = "Media ± error estándar (ms)",
    x        = "Momento de medición",
    y        = "Índice atencional (ms)",
    color    = "Grupo"
  ) +
  theme_pubr() +
  theme(legend.position = "top",
        strip.text      = element_text(size = 9),
        plot.title      = element_text(size = 11))

print(fig_ant)

# Para guardar:
# ggsave("Fig3_ANT_redes.png", fig_ant, width = 10, height = 5, dpi = 300)
