# ==========================================================
# === ACTUALIZAR CLASIFICACIÓN CAMPEONATO PÁDEL 2026-2027 ===
# ==========================================================

library(readxl)
library(dplyr)
library(stringr)
library(openxlsx)
library(tidyr)

archivo <- "padel_2026.xlsx"

# === 1) LEER RESULTADOS ===

resultados <- read_excel(archivo, sheet = "resultados")
names(resultados) <- toupper(str_trim(names(resultados)))

resultados <- resultados %>%
  mutate(
    GRUPO = str_squish(as.character(GRUPO)),
    PAREJA1 = str_squish(as.character(PAREJA1)),
    PAREJA2 = str_squish(as.character(PAREJA2)),
    RESULTADO_P1P2 = as.character(RESULTADO_P1P2),
    RESULTADO_P2P1 = as.character(RESULTADO_P2P1)
  )

# Eliminar partidos pendientes (sin marcador completo)
resultados <- resultados %>%
  filter(
    !is.na(RESULTADO_P1P2),
    str_squish(RESULTADO_P1P2) != "",
    !is.na(RESULTADO_P2P1),
    str_squish(RESULTADO_P2P1) != ""
  )

# === 2) FUNCIONES AUXILIARES ===

# Contar sets ganados y perdidos
contar_sets <- function(txt) {
  
  if (is.na(txt) || txt == "") {
    return(c(SG = 0, SP = 0))
  }
  
  m <- str_match_all(txt, "(\\d+)[\\-–—](\\d+)")[[1]]
  
  if (nrow(m) == 0) {
    return(c(SG = 0, SP = 0))
  }
  
  a <- as.numeric(m[, 2])
  b <- as.numeric(m[, 3])
  
  c(
    SG = sum(a > b, na.rm = TRUE),
    SP = sum(a < b, na.rm = TRUE)
  )
}

# Contar juegos ganados y perdidos
contar_juegos <- function(txt) {
  
  if (is.na(txt) || txt == "") {
    return(c(JG = 0, JP = 0))
  }
  
  m <- str_match_all(txt, "(\\d+)[\\-–—](\\d+)")[[1]]
  
  if (nrow(m) == 0) {
    return(c(JG = 0, JP = 0))
  }
  
  a <- as.numeric(m[, 2])
  b <- as.numeric(m[, 3])
  
  c(
    JG = sum(a, na.rm = TRUE),
    JP = sum(b, na.rm = TRUE)
  )
}

# === 3) ESTADÍSTICAS POR PARTIDO ===

stats <- list()

for (i in seq_len(nrow(resultados))) {
  
  r <- resultados[i, ]
  
  sg1sp1 <- contar_sets(r$RESULTADO_P1P2)
  sg2sp2 <- contar_sets(r$RESULTADO_P2P1)
  
  jg1jp1 <- contar_juegos(r$RESULTADO_P1P2)
  jg2jp2 <- contar_juegos(r$RESULTADO_P2P1)
  
  # Pareja 1
  stats[[length(stats) + 1]] <- tibble(
    GRUPO = r$GRUPO,
    PAREJA = r$PAREJA1,
    PJ = 1L,
    PG = as.integer(sg1sp1["SG"] > sg1sp1["SP"]),
    PP = as.integer(sg1sp1["SG"] < sg1sp1["SP"]),
    PE = as.integer(sg1sp1["SG"] == sg1sp1["SP"]),
    SG = unname(sg1sp1["SG"]),
    SP = unname(sg1sp1["SP"]),
    JG = unname(jg1jp1["JG"]),
    JP = unname(jg1jp1["JP"]),
    PUNTOS = ifelse(
      sg1sp1["SG"] > sg1sp1["SP"], 3,
      ifelse(sg1sp1["SG"] == sg1sp1["SP"], 1, 0)
    )
  )
  
  # Pareja 2
  stats[[length(stats) + 1]] <- tibble(
    GRUPO = r$GRUPO,
    PAREJA = r$PAREJA2,
    PJ = 1L,
    PG = as.integer(sg2sp2["SG"] > sg2sp2["SP"]),
    PP = as.integer(sg2sp2["SG"] < sg2sp2["SP"]),
    PE = as.integer(sg2sp2["SG"] == sg2sp2["SP"]),
    SG = unname(sg2sp2["SG"]),
    SP = unname(sg2sp2["SP"]),
    JG = unname(jg2jp2["JG"]),
    JP = unname(jg2jp2["JP"]),
    PUNTOS = ifelse(
      sg2sp2["SG"] > sg2sp2["SP"], 3,
      ifelse(sg2sp2["SG"] == sg2sp2["SP"], 1, 0)
    )
  )
}

# === 4) SUMAR ESTADÍSTICAS POR PAREJA ===

if (length(stats) > 0) {
  
  clasificacion_auto <- bind_rows(stats) %>%
    mutate(
      GRUPO = tolower(str_squish(GRUPO)),
      PAREJA = str_squish(PAREJA)
    ) %>%
    group_by(GRUPO, PAREJA) %>%
    summarise(
      PJ = sum(PJ),
      PG = sum(PG),
      PP = sum(PP),
      PE = sum(PE),
      SG = sum(SG),
      SP = sum(SP),
      JG = sum(JG),
      JP = sum(JP),
      PUNTOS = sum(PUNTOS),
      .groups = "drop"
    )
  
} else {
  
  # Permite ejecutar el programa aunque aún no haya partidos
  clasificacion_auto <- tibble(
    GRUPO = character(),
    PAREJA = character(),
    PJ = integer(),
    PG = integer(),
    PP = integer(),
    PE = integer(),
    SG = numeric(),
    SP = numeric(),
    JG = numeric(),
    JP = numeric(),
    PUNTOS = numeric()
  )
}

# === 5) AÑADIR PAREJAS QUE TODAVÍA NO HAN JUGADO ===

if ("clasificacion" %in% excel_sheets(archivo)) {
  
  clasif_base <- read_excel(
    archivo,
    sheet = "clasificacion"
  )
  
  names(clasif_base) <- toupper(
    str_trim(names(clasif_base))
  )
  
  clasif_base_min <- clasif_base %>%
    transmute(
      GRUPO = tolower(str_squish(as.character(GRUPO))),
      PAREJA = str_squish(as.character(PAREJA))
    ) %>%
    filter(
      !is.na(GRUPO),
      !is.na(PAREJA),
      GRUPO != "",
      PAREJA != ""
    ) %>%
    distinct()
  
  clasificacion_auto <- clasif_base_min %>%
    full_join(
      clasificacion_auto,
      by = c("GRUPO", "PAREJA")
    )
  
}

# Rellenar con ceros las estadísticas de parejas sin partidos
clasificacion_auto <- clasificacion_auto %>%
  mutate(
    across(
      c(PUNTOS, PJ, PG, PE, PP, SG, SP, JG, JP),
      ~coalesce(.x, 0)
    )
  )

# === 6) CLASIFICACIÓN Y DESEMPATES ===
#
# ORDEN:
# 1. Más puntos
# 2. Más juegos ganados
# 3. Más partidos ganados
# 4. Mejor diferencia de sets
# 5. Mejor diferencia de juegos
# 6. Menos sets perdidos
# 7. Menos juegos perdidos
# 8. Orden alfabético
#
# Cada pareja tendrá una posición distinta.

clasificacion_auto <- clasificacion_auto %>%
  mutate(
    DIF_SETS = SG - SP,
    DIF_JUEGOS = JG - JP
  ) %>%
  arrange(
    GRUPO,
    desc(PUNTOS),
    desc(JG),
    desc(PG),
    desc(DIF_SETS),
    desc(DIF_JUEGOS),
    SP,
    JP,
    PAREJA
  ) %>%
  group_by(GRUPO) %>%
  mutate(
    CLASIFICACION = row_number()
  ) %>%
  ungroup() %>%
  select(
    GRUPO, CLASIFICACION, PAREJA,
    PUNTOS, PJ, PG, PE, PP,
    SG, SP, JG, JP
  )

# === 7) CREAR HISTORIAL DE PARTIDOS ===

historial <- resultados %>%
  pivot_longer(
    cols = c(PAREJA1, PAREJA2),
    names_to = "ROL",
    values_to = "PAREJA"
  ) %>%
  mutate(
    FECHA = Sys.Date(),
    PAREJA = str_squish(PAREJA),
    GRUPO = tolower(str_squish(GRUPO))
  )

# Calcular historial individual
if (nrow(historial) > 0) {
  
  historial <- historial %>%
    rowwise() %>%
    mutate(
      SG = if (ROL == "PAREJA1") {
        contar_sets(RESULTADO_P1P2)[["SG"]]
      } else {
        contar_sets(RESULTADO_P2P1)[["SG"]]
      },
      SP = if (ROL == "PAREJA1") {
        contar_sets(RESULTADO_P1P2)[["SP"]]
      } else {
        contar_sets(RESULTADO_P2P1)[["SP"]]
      },
      RESULTADO = case_when(
        SG > SP ~ "Gana",
        SG < SP ~ "Pierde",
        TRUE ~ "Empata"
      ),
      PUNTOS = case_when(
        RESULTADO == "Gana" ~ 3,
        RESULTADO == "Empata" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    ungroup() %>%
    select(
      FECHA, GRUPO, VUELTA, PAREJA,
      RESULTADO, SG, SP, PUNTOS
    ) %>%
    group_by(GRUPO, PAREJA) %>%
    arrange(FECHA, VUELTA, .by_group = TRUE) %>%
    mutate(
      PARTIDO = row_number(),
      PUNTOS_ACUM = cumsum(PUNTOS),
      PG = cumsum(RESULTADO == "Gana"),
      PE = cumsum(RESULTADO == "Empata"),
      PP = cumsum(RESULTADO == "Pierde")
    ) %>%
    ungroup()
  
} else {
  
  historial <- tibble(
    FECHA = as.Date(character()),
    GRUPO = character(),
    VUELTA = character(),
    PAREJA = character(),
    RESULTADO = character(),
    SG = numeric(),
    SP = numeric(),
    PUNTOS = numeric(),
    PARTIDO = integer(),
    PUNTOS_ACUM = numeric(),
    PG = integer(),
    PE = integer(),
    PP = integer()
  )
  
}

# === 8) PREPARAR EXCEL ===

wb <- loadWorkbook(archivo)

# Hoja clasificacion_auto
if ("clasificacion_auto" %in% names(wb)) {
  removeWorksheet(wb, "clasificacion_auto")
}

addWorksheet(wb, "clasificacion_auto")
writeData(wb, "clasificacion_auto", clasificacion_auto)

# === 9) CLASIFICACIÓN FINAL PARA STREAMLIT ===

clasificacion_final <- clasificacion_auto %>%
  mutate(
    GRUPO = case_when(
      GRUPO == "mediocre alto" ~ "Mediocre alto",
      GRUPO == "mediocre medio" ~ "Mediocre medio",
      GRUPO == "mediocre bajo" ~ "Mediocre bajo",
      TRUE ~ str_to_sentence(GRUPO)
    )
  ) %>%
  arrange(GRUPO, CLASIFICACION) %>%
  rename(
    `P. JUGADOS` = PJ,
    `P. GANADOS` = PG,
    `P. EMPATADOS` = PE,
    `P. PERDIDOS` = PP,
    `SET GANADOS` = SG,
    `SET PERDIDOS` = SP
  ) %>%
  select(
    GRUPO,
    CLASIFICACION,
    PAREJA,
    PUNTOS,
    `P. JUGADOS`,
    `P. GANADOS`,
    `P. EMPATADOS`,
    `P. PERDIDOS`,
    `SET GANADOS`,
    `SET PERDIDOS`
  )

if ("clasificacion" %in% names(wb)) {
  removeWorksheet(wb, "clasificacion")
}

addWorksheet(wb, "clasificacion")
writeData(wb, "clasificacion", clasificacion_final)

# === 10) GUARDAR HISTORIAL ===

if ("historial_partidos" %in% names(wb)) {
  removeWorksheet(wb, "historial_partidos")
}

addWorksheet(wb, "historial_partidos")
writeData(wb, "historial_partidos", historial)

# === 11) GUARDAR EXCEL ===

saveWorkbook(
  wb,
  archivo,
  overwrite = TRUE
)

cat(
  "✅ Clasificación actualizada correctamente.\n",
  "Desempate: puntos > juegos ganados > partidos ganados.\n",
  "Posiciones únicas para todas las parejas.\n"
)
