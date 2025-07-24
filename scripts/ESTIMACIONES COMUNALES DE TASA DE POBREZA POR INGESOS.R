### ESTIMACIONES COMUNALES DE TASA DE POBREZA POR INGESOS ###

# ==========================================
# PASO 1: Instalar y cargar paquetes
# ==========================================

# 1.1 Instalar paquetes

install.packages("tidyverse")
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggrepel")

# ------------------------------------------

# 1.2 Cargar las librerias 

library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)


# ==========================================
# PASO 2: Definir y verificar directorio de trabajo
# ==========================================

# 2.1 Ver directorio actual

getwd()

# ------------------------------------------

# 2.2 Cambiar directorio de trabajo (ajusta la ruta)

setwd("C:\\Users\\brook\\OneDrive\\tablas incidencias DHS")

# ------------------------------------------

# 2.3 Verificar que se cambió

getwd()


# ==========================================
# PASO 3: Leer archivo Excel
# ==========================================

# 3.1 Ver hojas dentro del archivo (opcional)

excel_sheets("planilla_estimaciones_comunales_tasa_pobreza_por_ingresos_2013.xlsx")
excel_sheets("planilla_estimaciones_comunales_tasa_pobreza_por_ingresos_multidimensional_2015.xlsx")
excel_sheets("planilla_estimaciones_comunales_tasa_pobreza_por_ingresos_multidimensional_2017.xlsx")
excel_sheets("estimaciones_de_tasa_de_pobreza_por_ingresos_por_comunas_2020_revisada2022_09.xlsx")
excel_sheets("estimaciones_tasa_pobreza_ingresos_comunas_2022.xlsx")

# ------------------------------------------

# 3.2 Cargar una hoja específica

pobreza_x_ingresos_2013 <- read_excel(
  "planilla_estimaciones_comunales_tasa_pobreza_por_ingresos_2013.xlsx", 
  sheet = "Pobreza Ingresos 2013")

pobreza_x_ingresos_2015 <- read_excel(
  "planilla_estimaciones_comunales_tasa_pobreza_por_ingresos_multidimensional_2015.xlsx", 
  sheet = "Ingresos2015")

pobreza_x_ingresos_2017 <- read_excel(
  "planilla_estimaciones_comunales_tasa_pobreza_por_ingresos_multidimensional_2017.xlsx", 
  sheet = "Ingresos 2017")

pobreza_x_ingresos_2020 <- read_excel(
  "estimaciones_de_tasa_de_pobreza_por_ingresos_por_comunas_2020_revisada2022_09.xlsx", 
  sheet = "Cifras 2020 revisadas en 2022")

pobreza_x_ingresos_2022 <- read_excel(
  "estimaciones_tasa_pobreza_ingresos_comunas_2022.xlsx", 
  sheet = "Estimaciones")


# ==========================================
# PASO 4: Visualizar los datos cargados
# ==========================================

# 4.1 Ver primeras filas

head(pobreza_x_ingresos_2013)
head(pobreza_x_ingresos_2015)
head(pobreza_x_ingresos_2017)
head(pobreza_x_ingresos_2020)
head(pobreza_x_ingresos_2022)

# ------------------------------------------

# 4.2 Ver estructura del dataframe

str(pobreza_x_ingresos_2013)
str(pobreza_x_ingresos_2015)
str(pobreza_x_ingresos_2017)
str(pobreza_x_ingresos_2020)
str(pobreza_x_ingresos_2022)

# ------------------------------------------

# 4.3 Ver resumen estadístico básico

summary(pobreza_x_ingresos_2013)
summary(pobreza_x_ingresos_2015)
summary(pobreza_x_ingresos_2017)
summary(pobreza_x_ingresos_2020)
summary(pobreza_x_ingresos_2022)

# ==========================================
# PASO 5: Limpiar y normalizar los datos
# ==========================================


# 5.1 Leer archivo saltando la 1ª y 2ª filas (vacías)

pobreza_x_ingresos_2013 <- read_excel(     
  "planilla_estimaciones_comunales_tasa_pobreza_por_ingresos_2013.xlsx",
  sheet = "Pobreza Ingresos 2013",
  skip = 2
)

pobreza_x_ingresos_2015 <- read_excel(
  "planilla_estimaciones_comunales_tasa_pobreza_por_ingresos_multidimensional_2015.xlsx", 
  sheet = "Ingresos2015",
  skip = 2
)

pobreza_x_ingresos_2017 <- read_excel(
  "planilla_estimaciones_comunales_tasa_pobreza_por_ingresos_multidimensional_2017.xlsx", 
  sheet = "Ingresos 2017",
  skip = 2
)

pobreza_x_ingresos_2020 <- read_excel(
  "estimaciones_de_tasa_de_pobreza_por_ingresos_por_comunas_2020_revisada2022_09.xlsx", 
  sheet = "Cifras 2020 revisadas en 2022",
  skip = 2
)  

pobreza_x_ingresos_2022 <- read_excel(
  "estimaciones_tasa_pobreza_ingresos_comunas_2022.xlsx", 
  sheet = "Estimaciones",
  skip = 2
)


# ------------------------------------------

# 5.2 Revisar que hay en la columna "Código"

unique(pobreza_x_ingresos_2013$Código) 
unique(pobreza_x_ingresos_2015$Código)
unique(pobreza_x_ingresos_2017$Código)
unique(pobreza_x_ingresos_2020$Código)
unique(pobreza_x_ingresos_2022$Código)

'''
Lo que haces es revisar metadatos, mayormente no numericos, desde la primera
columna
'''

# ------------------------------------------

#  5.3 Mantenemos solo filas con codigo numérico

pobreza_x_ingresos_2013 <- pobreza_x_ingresos_2013 %>%
  filter(grepl("^[0-9]+$", `Código`))

pobreza_x_ingresos_2020 <- pobreza_x_ingresos_2020 %>%
  filter(grepl("^[0-9]+$", `Código`))

pobreza_x_ingresos_2022 <- pobreza_x_ingresos_2022 %>%
  filter(grepl("^[0-9]+$", `Código`))


# 5.4 Transformamos la columnas a numérica según corresponda

pobreza_x_ingresos_2013 <- pobreza_x_ingresos_2013 %>%
  mutate(
    Código = as.numeric(`Código`)
  )

pobreza_x_ingresos_2020 <- pobreza_x_ingresos_2020 %>%
  mutate(
    Código = as.numeric(`Código`)
  )

pobreza_x_ingresos_2022 <- pobreza_x_ingresos_2022 %>%
  mutate(
    Código = as.numeric(`Código`)
  )


pobreza_x_ingresos_2013 <- pobreza_x_ingresos_2013 %>%
  mutate(
    `Límite inferior` = as.numeric(replace(`Límite inferior`, `Límite inferior` == "-", "0")),
    `Límite superior` = as.numeric(replace(`Límite superior`, `Límite superior` == "-", "0"))
  )

pobreza_x_ingresos_2022 <- pobreza_x_ingresos_2022 %>%
  mutate(
    Región = case_when(
      Región == "Atacama" ~ "III de Atacama",
      Región == "Coquimbo" ~ "IV de Coquimbo",
      TRUE ~ Región  # Mantiene cualquier otro valor igual
    )
  )

'''
Volvemos al PASO 4 para revisar la estructura de los datos
'''

# ==========================================
# PASO 6: Manipulación de datos
# ==========================================

# 6.1 Agregar columna Año 

pobreza_x_ingresos_2013 <- pobreza_x_ingresos_2013 %>% mutate(Año = 2013)
pobreza_x_ingresos_2015 <- pobreza_x_ingresos_2015 %>% mutate(Año = 2015)
pobreza_x_ingresos_2017 <- pobreza_x_ingresos_2017 %>% mutate(Año = 2017)
pobreza_x_ingresos_2020 <- pobreza_x_ingresos_2020 %>% mutate(Año = 2020)
pobreza_x_ingresos_2022 <- pobreza_x_ingresos_2022 %>% mutate(Año = 2022)

# ------------------------------------------

# 6.2 Filtramos por las filas (regiones) que vamos a trabajar

pobreza_x_ingresos_2013 <- pobreza_x_ingresos_2013 %>%
  filter(`Región` %in% c("III de Atacama", "IV de Coquimbo"))

pobreza_x_ingresos_2015 <- pobreza_x_ingresos_2015 %>%
  filter(`Región` %in% c("III de Atacama", "IV de Coquimbo"))

pobreza_x_ingresos_2017 <- pobreza_x_ingresos_2017 %>%
  filter(`Región` %in% c("III de Atacama", "IV de Coquimbo"))

pobreza_x_ingresos_2020 <- pobreza_x_ingresos_2020 %>%
  filter(`Región` %in% c("III de Atacama", "IV de Coquimbo"))

pobreza_x_ingresos_2022 <- pobreza_x_ingresos_2022 %>%
  filter(`Región` %in% c("III de Atacama", "IV de Coquimbo"))

# ------------------------------------------

# 6.3 Filtramos por las columnas (indicadores) que vamos a trabajar


# 6.3.1 Seleccionamos columnas relevantes

pobreza_x_ingresos_2013 <- pobreza_x_ingresos_2013 %>%
  select(
    Código,
    Región,
    `Nombre comuna`,
    `Número de personas* en situación de pobreza por ingresos 2013`,
    `Porcentaje de personas en situación de pobreza por ingresos 2013`,
    `Límite inferior`,
    `Límite superior`,
    Año
  )

pobreza_x_ingresos_2015 <- pobreza_x_ingresos_2015 %>%
  select(
    Código,
    Región,
    `Nombre comuna`,
    `Número de personas en situación de pobreza por ingresos`,
    `Porcentaje de personas en situación de pobreza por ingresos 2015`,
    `Límite inferior`,
    `Límite superior`,
    Año
  )

pobreza_x_ingresos_2017 <- pobreza_x_ingresos_2017 %>%
  select(
    Código,
    Región,
    `Nombre comuna`,
    `Número de personas en situación de pobreza por ingresos`,
    `Porcentaje de personas en situación de pobreza por ingresos 2017`,
    `Límite inferior`,
    `Límite superior`,
    Año
  )
pobreza_x_ingresos_2020 <- pobreza_x_ingresos_2020 %>%
  select(
    Código,
    Región,
    `Nombre comuna`,
    `Número de personas en situación de pobreza por ingresos (**)`,
    `Porcentaje de personas en situación de pobreza por ingresos 2020`,
    `Límite inferior`,
    `Límite superior`,
    Año
  )
pobreza_x_ingresos_2022 <- pobreza_x_ingresos_2022 %>%
  select(
    Código,
    Región,
    `Nombre comuna`,
    `Número de personas en situación de pobreza por ingresos (**)`,
    `Porcentaje de personas en situación de pobreza por ingresos 2022`,
    `Límite inferior\r\n (***)`,
    `Límite superior`,
    Año
  )

# ------------------------------------------

# 6.3.2  Renombramos las columnas seleccionadas

pobreza_x_ingresos_2013 <- pobreza_x_ingresos_2013 %>%
  rename(
    Numero_personas = `Número de personas* en situación de pobreza por ingresos 2013`,
    Porcentaje_pobreza = `Porcentaje de personas en situación de pobreza por ingresos 2013`,
    Limite_inferior = `Límite inferior`,
    Limite_superior = `Límite superior`
  )

pobreza_x_ingresos_2015 <- pobreza_x_ingresos_2015 %>%
  rename(
    Numero_personas = `Número de personas en situación de pobreza por ingresos`,
    Porcentaje_pobreza = `Porcentaje de personas en situación de pobreza por ingresos 2015`,
    Limite_inferior = `Límite inferior`,
    Limite_superior = `Límite superior`
  )

pobreza_x_ingresos_2017 <- pobreza_x_ingresos_2017 %>%
  rename(
    Numero_personas = `Número de personas en situación de pobreza por ingresos`,
    Porcentaje_pobreza = `Porcentaje de personas en situación de pobreza por ingresos 2017`,
    Limite_inferior = `Límite inferior`,
    Limite_superior = `Límite superior`
  )

pobreza_x_ingresos_2020 <- pobreza_x_ingresos_2020 %>%
  rename(
    Numero_personas = `Número de personas en situación de pobreza por ingresos (**)`,
    Porcentaje_pobreza = `Porcentaje de personas en situación de pobreza por ingresos 2020`,
    Limite_inferior = `Límite inferior`,
    Limite_superior = `Límite superior`
  )

pobreza_x_ingresos_2022 <- pobreza_x_ingresos_2022 %>%
  rename(
    Numero_personas = `Número de personas en situación de pobreza por ingresos (**)`,
    Porcentaje_pobreza = `Porcentaje de personas en situación de pobreza por ingresos 2022`,
    Limite_inferior = `Límite inferior\r\n (***)`,
    Limite_superior = `Límite superior`
  )

# ------------------------------------------

# 6.4 Unimos las tablas en una sola

pobreza_todos <- bind_rows(
  pobreza_x_ingresos_2013,
  pobreza_x_ingresos_2015,
  pobreza_x_ingresos_2017,
  pobreza_x_ingresos_2020,
  pobreza_x_ingresos_2022
)

# ------------------------------------------

# 6.5 Verificamos la información

glimpse(pobreza_todos)

View(pobreza_todos)

# ------------------------------------------

# 6.6 Creamos una copia para manipular en otra plataforma

write.csv(pobreza_todos, "pobreza_todos.csv", row.names = FALSE)


# ==========================================
# PASO 7: Resumen estadístico
# ==========================================

# 7.1 Revisión estructura básica

# 7.1.1 Estructura general

glimpse(pobreza_todos)

# ------------------------------------------

# 7.1.2 Primeras filas

head(pobreza_todos)

# ------------------------------------------

# 7.1.3 Resumen numérico rápido

summary(pobreza_todos)

# ------------------------------------------

# 7.2 Estadisticas claves por región y año

resumen_region_anio <- pobreza_todos %>%
  group_by(Región, Año) %>%
  summarise(
    Comunas = n(),
    Total_personas = sum(Numero_personas, na.rm = TRUE),
    Promedio_porcentaje = mean(Porcentaje_pobreza, na.rm = TRUE),
    Min_pobreza = min(Porcentaje_pobreza, na.rm = TRUE),
    Max_pobreza = max(Porcentaje_pobreza, na.rm = TRUE)
  ) %>%
  arrange(Región, Año)

print(resumen_region_anio)

# ------------------------------------------

# 7.3 Estadisticas descriptivas por comunas

resumen_comunas <- pobreza_todos %>%
  group_by(`Nombre comuna`) %>%
  summarise(
    Años = n(),
    Promedio_personas = mean(Numero_personas, na.rm = TRUE),
    Promedio_porcentaje = mean(Porcentaje_pobreza, na.rm = TRUE),
    Min_pobreza = min(Porcentaje_pobreza, na.rm = TRUE),
    Max_pobreza = max(Porcentaje_pobreza, na.rm = TRUE)
  ) %>%
  arrange(desc(Promedio_porcentaje))

print(resumen_comunas)

# ==========================================
# PASO 8: Estadistica Avanzada
# ==========================================

# 8.1 Variación porcentual 2013–2022 por comuna

pobreza_2013 <- pobreza_todos %>%
  filter(Año == 2013) %>%
  select(`Nombre comuna`, Porcentaje_pobreza)

pobreza_2022 <- pobreza_todos %>%
  filter(Año == 2022) %>%
  select(`Nombre comuna`, Porcentaje_pobreza)

variacion_comunal <- pobreza_2013 %>%
  inner_join(pobreza_2022, by = "Nombre comuna", suffix = c("_2013", "_2022")) %>%
  mutate(Diferencia = Porcentaje_pobreza_2022 - Porcentaje_pobreza_2013) %>%
  arrange(Diferencia)

# ------------------------------------------

# 8.2 Ranking comunal 2022

ranking_2022 <- pobreza_todos %>%
  filter(Año == 2022) %>%
  arrange(desc(Porcentaje_pobreza))

# ------------------------------------------

# 8.3 Promedio regional + bandas

prom_region <- pobreza_todos %>%
  group_by(Región, Año) %>%
  summarise(
    Promedio = mean(Porcentaje_pobreza, na.rm = TRUE),
    Min = min(Porcentaje_pobreza, na.rm = TRUE),
    Max = max(Porcentaje_pobreza, na.rm = TRUE)
  )

# ==========================================
# PASO 9: Visualización — Reformulado
# ==========================================

library(scales)  # Para formatear porcentajes bien

# Asegurar que Año sea entero
pobreza_todos$Año <- as.integer(pobreza_todos$Año)

# 9.1 Visualización exploratoria de los datos tabulados

ultima_medicion <- pobreza_todos %>%
  group_by(`Nombre comuna`) %>%
  filter(Año == max(Año))

ggplot(pobreza_todos, aes(x = Año, y = Porcentaje_pobreza * 100, color = Región)) +
  geom_point() +
  geom_line(aes(group = `Nombre comuna`), alpha = 0.4) +
  geom_text_repel(
    data = ultima_medicion,
    aes(label = `Nombre comuna`),
    size = 3,
    show.legend = FALSE
  ) +
  facet_wrap(~ Región) +
  labs(
    title = "Evolución de la pobreza por ingresos (2013–2022)",
    x = "Año",
    y = "Porcentaje de pobreza (%)",
    color = "Región"
  ) +
  scale_x_continuous(breaks = unique(pobreza_todos$Año)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  theme_minimal()

# ------------------------------------------

# 9.2 Visualización de estadísticas avanzadas

# 9.2.1 Evolución promedio regional

ggplot(prom_region, aes(x = Año, y = Promedio * 100, color = Región)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = Min * 100, ymax = Max * 100, fill = Región), alpha = 0.2, color = NA) +
  labs(
    title = "Evolución promedio de pobreza regional (Atacama y Coquimbo)",
    y = "Porcentaje de pobreza (%)",
    x = "Año"
  ) +
  scale_x_continuous(breaks = unique(pobreza_todos$Año)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  theme_minimal()

# ------------------------------------------

# 9.2.2 Ranking 2022

ggplot(ranking_2022, aes(x = reorder(`Nombre comuna`, Porcentaje_pobreza), y = Porcentaje_pobreza * 100)) +
  geom_col(fill = "#2c7fb8") +
  coord_flip() +
  labs(
    title = "Ranking comunal de pobreza 2022",
    x = "Comuna",
    y = "Porcentaje de pobreza (%)"
  ) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  theme_minimal()

# ------------------------------------------

# 9.2.3 Variación 2013–2022

variacion_comunal <- variacion_comunal %>%
  mutate(
    Diferencia_pct = Diferencia * 100,
    Status = ifelse(Diferencia < -0.05, "Disminuye notablemente",
                    ifelse(Diferencia > 0.05, "Aumenta notablemente", "Sin cambio significativo"))
  )

ggplot(variacion_comunal, aes(x = reorder(`Nombre comuna`, Diferencia_pct), y = Diferencia_pct, fill = Status)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Disminuye notablemente" = "#2ca25f",
      "Sin cambio significativo" = "#fdae61",
      "Aumenta notablemente" = "#e34a33"
    )
  ) +
  labs(
    title = "Variación de la pobreza por ingresos (2013–2022)",
    x = "Comuna",
    y = "Diferencia porcentual (%)",
    fill = "Tendencia"
  ) +
  theme_minimal()


# ==========================================
# PASO 10: Guardado de información
# ==========================================

# 10.1 Se guardan los resumenes 

write.csv(resumen_region_anio, "resumen_region_anio.csv", row.names = FALSE)

write.csv(resumen_comunas, "resumen_comunas.csv", row.names = FALSE)

write.csv(variacion_comunal, "variacion_comunal", row.names = FALSE)

write.csv(ranking_2022, "ranking_2022", row.names = FALSE)

write.csv(prom_region, "prom_región", row.names = FALSE)

ggsave("outputs/evolucion_promedio_regional.png", dpi = 300)

ggsave("outputs/ranking_2022.png", dpi = 300)

ggsave("outputs/variacion_comunal.png", dpi = 300)


# ==========================================
# FIN DE SCRIPT
# ==========================================
