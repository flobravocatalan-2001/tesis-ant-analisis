# =================================================================
# COMPARACIÓN: Estrés post-MIST vs. post-ANT, Día 1 vs. Día 3
# =================================================================
#
# OBJETIVO de este script:
#   Comparar el nivel de estrés (VAS) y ansiedad (STAI-6) en 4
#   momentos repetidos para la misma persona:
#     - Día 1, después del MIST   (dia1_post_mist)
#     - Día 1, después del ANT    (dia1_post_ant)
#     - Día 3, después del MIST   (dia3_post_mist)
#     - Día 3, después del ANT    (dia3_post_ant)
#
#   Esto permite responder preguntas como:
#     - ¿El estrés sube o baja entre hacer la MIST y hacer el ANT,
#       dentro del mismo día?
#     - ¿El estrés post-MIST (o post-ANT) cambia entre el día 1 y
#       el día 3 del retiro?
#
# Estructura:
#   0. Paquetes
#   1. Cargar datos y pasar a formato "ancho" (una fila por persona)
#   2. Quedarnos solo con quienes tienen las 4 mediciones completas
#   3. Normalidad (Shapiro-Wilk) en cada una de las 4 condiciones
#   4. Prueba de Friedman (ómnibus) para VAS y STAI-6
#   5. Comparaciones post-hoc de Wilcoxon con corrección de Bonferroni
#   6. Tabla descriptiva final
# =================================================================

library(readxl)
library(dplyr)
library(tidyr)


# -----------------------------------------------------------------
# 1. CARGAR DATOS Y PASAR A FORMATO ANCHO
# -----------------------------------------------------------------
ruta_archivo <- "C:/Users/HP/Documents/TESIS ANT/Cuestionarios/MIST/Stai_VAS.xlsx" 

datos <- read_excel(ruta_archivo)

# Creamos una sola columna que identifique la condición (combinando
# día + momento), para poder "abrir" los datos en formato ancho:
# una fila por persona, una columna por cada una de las 4 condiciones.
datos <- datos %>%
  mutate(condicion = paste0(dia_intervencion, "_", momento))

datos_ancho_vas <- datos %>%
  select(id, condicion, vas) %>%
  pivot_wider(names_from = condicion, values_from = vas)

datos_ancho_stai6 <- datos %>%
  select(id, condicion, stai6_total) %>%
  pivot_wider(names_from = condicion, values_from = stai6_total)

orden_condiciones <- c("dia1_post_mist", "dia1_post_ant", "dia3_post_mist", "dia3_post_ant")

# -----------------------------------------------------------------
# 2. ASIGNAR GRUPO (control/experimental) SEGÚN EL ID
# -----------------------------------------------------------------
# Misma lógica usada en tu script de análisis del ANT: cada retiro
# tuvo su propio bloque de códigos, y los retiros se alternaron
# entre modalidad experimental y control.
asignar_grupo <- function(id_vec) {
  num <- as.numeric(stringr::str_extract(id_vec, "\\d+"))
  dplyr::case_when(
    num >= 1  & num <= 15 ~ "EXPERIMENTAL",
    num >= 33 & num <= 48 ~ "EXPERIMENTAL",
    num >= 16 & num <= 32 ~ "CONTROL",
    num >= 49 & num <= 64 ~ "CONTROL",
    TRUE ~ NA_character_
  )
}

datos_ancho_vas <- datos_ancho_vas %>% mutate(grupo = asignar_grupo(id))
datos_ancho_stai6 <- datos_ancho_stai6 %>% mutate(grupo = asignar_grupo(id))


# -----------------------------------------------------------------
# 3. QUEDARNOS SOLO CON QUIENES TIENEN LAS 4 MEDICIONES COMPLETAS
# -----------------------------------------------------------------
datos_ancho_vas <- datos_ancho_vas %>% drop_na(all_of(orden_condiciones))
datos_ancho_stai6 <- datos_ancho_stai6 %>% drop_na(all_of(orden_condiciones))

cat("Personas con las 4 mediciones completas de VAS, por grupo:\n")
print(table(datos_ancho_vas$grupo))
cat("\nPersonas con las 4 mediciones completas de STAI-6, por grupo:\n")
print(table(datos_ancho_stai6$grupo))


# -----------------------------------------------------------------
# 4. FUNCIÓN REUTILIZABLE: corre todo el análisis para UN grupo
# -----------------------------------------------------------------
# En vez de copiar y pegar el mismo código dos veces (una para
# CONTROL y otra para EXPERIMENTAL), armamos una función que hace
# todo el proceso (Shapiro-Wilk + Friedman + post-hoc + descriptivos)
# y la llamamos una vez por grupo. Esto evita errores de copy-paste
# y hace más fácil revisar/modificar el análisis en un solo lugar.

analizar_grupo <- function(datos_ancho, nombre_variable, nombre_grupo) {
  cat("\n\n#####################################################\n")
  cat("### GRUPO:", nombre_grupo, "| VARIABLE:", nombre_variable, "\n")
  cat("#####################################################\n")
  
  sub <- datos_ancho %>% filter(grupo == nombre_grupo)
  cat("n =", nrow(sub), "\n")
  
  cat("\n--- Shapiro-Wilk por condición ---\n")
  for (cond in orden_condiciones) {
    resultado <- shapiro.test(sub[[cond]])
    cat(cond, ": W =", round(resultado$statistic, 3), ", p =", round(resultado$p.value, 4), "\n")
  }
  
  matriz <- as.matrix(sub[, orden_condiciones])
  cat("\n--- Friedman ---\n")
  print(friedman.test(matriz))
  
  cat("\n--- Post-hoc Wilcoxon (Bonferroni) ---\n")
  print(pairwise.wilcox.test(
    x = unlist(sub[, orden_condiciones]),
    g = rep(orden_condiciones, each = nrow(sub)),
    paired = TRUE,
    p.adjust.method = "bonferroni"
  ))
  
  cat("\n--- Descriptivos (mediana [P25-P75]) ---\n")
  largo <- sub %>% pivot_longer(cols = all_of(orden_condiciones), names_to = "condicion", values_to = "valor")
  print(largo %>%
          group_by(condicion) %>%
          summarise(n = n(), mediana = median(valor), p25 = quantile(valor, .25), p75 = quantile(valor, .75)))
}


# -----------------------------------------------------------------
# 5. CORRER EL ANÁLISIS PARA CADA GRUPO Y CADA VARIABLE
# -----------------------------------------------------------------
analizar_grupo(datos_ancho_vas, "VAS", "CONTROL")
analizar_grupo(datos_ancho_vas, "VAS", "EXPERIMENTAL")
analizar_grupo(datos_ancho_stai6, "STAI-6", "CONTROL")
analizar_grupo(datos_ancho_stai6, "STAI-6", "EXPERIMENTAL")


# =================================================================
# 6. MODELO LMM: GRUPO × CONDICIÓN (interacción formal)
# =================================================================
# Las pruebas de Friedman de arriba se corrieron POR SEPARADO para
# cada grupo, por lo que nunca testean directamente si la
# TRAYECTORIA de estrés/ansiedad a lo largo de los 4 momentos
# difiere realmente entre control y experimental (es decir, la
# interacción grupo × tiempo). Este bloque corre un modelo único
# (Linear Mixed Model) que sí testea esa interacción formalmente,
# con grupo como factor entre-sujetos y condición (los 4 momentos)
# como factor intra-sujetos.
#
# install.packages("lme4")
# install.packages("lmerTest")
library(lme4)
library(lmerTest)

preparar_largo <- function(datos_ancho, nombre_variable) {
  datos_ancho %>%
    filter(!is.na(grupo)) %>%
    pivot_longer(cols = all_of(orden_condiciones),
                 names_to = "condicion", values_to = "valor") %>%
    mutate(
      condicion = factor(condicion, levels = orden_condiciones),
      grupo     = factor(grupo),
      id        = factor(id)
    )
}

largo_vas   <- preparar_largo(datos_ancho_vas, "VAS")
largo_stai6 <- preparar_largo(datos_ancho_stai6, "STAI-6")

cat("\n\n#####################################################\n")
cat("### MODELO LMM — VAS: grupo * condicion ###\n")
cat("#####################################################\n")
modelo_vas <- lmer(valor ~ grupo * condicion + (1 | id), data = largo_vas)
print(anova(modelo_vas))

cat("\n\n#####################################################\n")
cat("### MODELO LMM — STAI-6: grupo * condicion ###\n")
cat("#####################################################\n")
modelo_stai6 <- lmer(valor ~ grupo * condicion + (1 | id), data = largo_stai6)
print(anova(modelo_stai6))

# Resultados obtenidos (para referencia, ya corridos):
#   VAS:    grupo:condicion  F(3,147) = 3.09, p = .029  (SIGNIFICATIVO)
#   STAI-6: grupo:condicion  F(3,147) = 2.09, p = .104  (no significativo)
#
# Interpretación: para VAS, las trayectorias de estrés de los dos
# grupos a lo largo de los 4 momentos SÍ son formalmente distintas.
# Para STAI-6, la tendencia va en la misma dirección pero no alcanza
# significancia estadística con el n disponible.

# =================================================================
# FIN DEL SCRIPT
# =================================================================
