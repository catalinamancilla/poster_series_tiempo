library(dplyr)
library(stringr)
library(readxl)
library(lubridate)
library(tidyr)
library(ggplot2)

# Base DEIS 1990-2023: defunciones históricas
# Base DEIS 2024-2026: actualización reciente
# Base INE nacional: denominadores para tasas nacionales
# Base INE regional: denominadores para tasas regionales y SMR

# Unidad de análisis: Defunciones registradas por ELA en Chile

# Denominador: Población residente estimada por INE

# A.1

# La enfermedad que le eligió corresponde a la Esclerosis Lateral Amiotrófica (ELA)
#una enfermedad neurodegenerativa que afecta las neuronas motoras superiores e inferiores,
#esto provoca en el paciente: debilidad muscular, pérdida de movilidad y finalmente una insuficiencia respiratoria.

#El ELA afecta aproximadamente a 7 de cada 100.000 personas en todo el mundo (https://medlineplus.gov/spanish/ency/article/000688.htm)
#enfermedad poco frecuente que presenta una elevada letalidad
#ya que el pronostico de vida es entre los 3 a 5 años y solo 1 de cada 4 personas sobrevive mas de 5 años aproximadamente

#Su relevancia en la salud pública radica en el gran impacto humano,
#su alto costo de cuidados y la gran dependencia que genera.
#para disminuir la intensidad de esta enfermedad, los sistemas sanitarios deben implementar manejos integrales como:
#cuidados paleativos tempranos, con el fin de mejorar la calidad de vida y el bienestar emocional de los pacientes y familiares.
#unidades especializadas para el seguimiento tanto neurológico, nutricionistas, terapeutas ocupacionales y una atencion respiratoria.
#https://www.minsal.cl/wp-content/uploads/2017/10/ELA.pdf



#### PARA SOLO EL ELA #####
enfermedad <- "Esclerosis Lateral Amiotrófica (ELA)"

# CIE-9
codigos_cie9 <- c("3352", "335.2")

# CIE-10
subcategoria_cie10 <- c("G122", "G12.2")


cat("Seleccione base DEIS 1990-2023\n") #base 4
ruta_deis_1990_2023 <- file.choose()

cat("Seleccione base DEIS 2024-2026\n") #base 1
ruta_deis_2024_2026 <- file.choose()

cat("Seleccione población nacional INE\n") #base 2
ruta_ine_nacional <- file.choose()

cat("Seleccione población regional INE\n") #base 3
ruta_ine_regional <- file.choose()


deis_1990_2023 <- read.csv2(
  ruta_deis_1990_2023,
  fileEncoding = "latin1",
  check.names = FALSE
)

deis_2024_2026 <- read.csv2(
  ruta_deis_2024_2026,
  fileEncoding = "latin1",
  check.names = FALSE
)

pob_nacional <- read_excel(ruta_ine_nacional)

pob_regional <- read.csv(
  ruta_ine_regional,
  check.names = FALSE
)

# UNIR BASES DE DEFUNCIÓN

defunciones <- bind_rows(deis_1990_2023, deis_2024_2026)

#Excluir 2026 porque el año aún no está cerrado

defunciones <- defunciones %>%filter(AÑO <= 2023)


#LIMPIEZA

#Limpiar año, sexo, edad y poblacion
defunciones <- defunciones %>%
  mutate(
    AÑO = as.integer(AÑO),
    
    FECHA_DEF_ORIGINAL = FECHA_DEF,
    
    FECHA_DEF = case_when(
      str_detect(FECHA_DEF_ORIGINAL, "^\\d{4}-\\d{2}-\\d{2}$") ~
        ymd(FECHA_DEF_ORIGINAL),
      
      str_detect(FECHA_DEF_ORIGINAL, "^\\d{2}-\\d{2}-\\d{4}$") ~
        dmy(FECHA_DEF_ORIGINAL),
      
      str_detect(FECHA_DEF_ORIGINAL, "^\\d{2}/\\d{2}/\\d{4}$") ~
        dmy(FECHA_DEF_ORIGINAL),
      
      str_detect(FECHA_DEF_ORIGINAL, "^\\d{4}/\\d{2}/\\d{2}$") ~
        ymd(FECHA_DEF_ORIGINAL),
      
      TRUE ~ suppressWarnings(
        parse_date_time(
          FECHA_DEF_ORIGINAL,
          orders = c("ymd", "dmy", "mdy")
        ) %>% as.Date()
      )
    ),
    
    SEXO_NOMBRE = str_trim(as.character(SEXO_NOMBRE)),
    
    NOMBRE_REGION = str_trim(as.character(NOMBRE_REGION)),
    
    COMUNA = str_trim(as.character(COMUNA)),
    
    LUGAR_DEFUNCION = str_trim(as.character(LUGAR_DEFUNCION)),
    
    DIAG1 = str_trim(str_to_upper(as.character(DIAG1))),
    
    CODIGO_CATEGORIA_DIAG1 = str_trim(str_to_upper(as.character(CODIGO_CATEGORIA_DIAG1))),
    
    CODIGO_SUBCATEGORIA_DIAG1 = str_trim(str_to_upper(as.character(CODIGO_SUBCATEGORIA_DIAG1))),
    
    GLOSA_CATEGORIA_DIAG1 = str_trim(as.character(GLOSA_CATEGORIA_DIAG1)),
    
    GLOSA_SUBCATEGORIA_DIAG1 = str_trim(as.character(GLOSA_SUBCATEGORIA_DIAG1)),
    
    EDAD_TIPO = as.numeric(EDAD_TIPO),
    
    EDAD_CANT = as.numeric(EDAD_CANT))


#CONVERSIÓN DE EDAD A AÑOS

defunciones <- defunciones %>%
  mutate(
    edad_anios = case_when(
      EDAD_TIPO == 1 ~ EDAD_CANT,
      EDAD_TIPO == 2 ~ EDAD_CANT / 12,
      EDAD_TIPO == 3 ~ EDAD_CANT / 365.25,
      EDAD_TIPO == 4 ~ EDAD_CANT / (24 * 365.25),
      TRUE ~ NA_real_
    )
  )

#GRUPO ETARIO

defunciones <- defunciones %>%
  mutate(
    grupo_etario = case_when(
      edad_anios < 20 ~ "<20",
      edad_anios >= 20 & edad_anios < 40 ~ "20-39",
      edad_anios >= 40 & edad_anios < 60 ~ "40-59",
      edad_anios >= 60 & edad_anios < 80 ~ "60-79",
      edad_anios >= 80 ~ "80+",
      TRUE ~ NA_character_
    )
  )

#PERIDOS DE ESTUDIO

defunciones <- defunciones %>%
  mutate(
    periodo = case_when(
      AÑO >= 1990 & AÑO <= 1999 ~ "1990-1999",
      AÑO >= 2000 & AÑO <= 2009 ~ "2000-2009",
      AÑO >= 2010 & AÑO <= 2019 ~ "2010-2019",
      AÑO >= 2020 & AÑO <= 2023 ~ "2020-2023",
      TRUE ~ NA_character_
    )
  )


#FILTRO ELA

ELA <- defunciones %>%
  filter(
    DIAG1 %in% codigos_cie9 |
      CODIGO_SUBCATEGORIA_DIAG1 %in% subcategoria_cie10
  ) %>%
  transmute(
    año = AÑO,
    
    fecha_def = FECHA_DEF,
    
    sexo = SEXO_NOMBRE,
    
    region = NOMBRE_REGION,
    
    comuna = COMUNA,
    
    edad = edad_anios,
    
    grupo_etario = grupo_etario,
    
    periodo = periodo,
    
    codigo_cie9 = DIAG1,
    
    categoria_cie10 =
      CODIGO_CATEGORIA_DIAG1,
    
    subcategoria_cie10 =
      CODIGO_SUBCATEGORIA_DIAG1,
    
    glosa_categoria =
      GLOSA_CATEGORIA_DIAG1,
    
    glosa_subcategoria =
      GLOSA_SUBCATEGORIA_DIAG1,
    
    lugar_defuncion =
      LUGAR_DEFUNCION
  )


ELA %>%
  count(
    subcategoria_cie10,
    glosa_subcategoria,
    sort = TRUE
  )


#cantidad registros
cat("Total registros ELA:", nrow(ELA),"\n")



# 4276 registros corresponden a ELA codificada en CIE-10 como G122. 433 registros corresponden a ELA
# codificada en CIE-9 como 3352. No aparecen otras subcategorías de G12.
# No se esta mezclando otras enfermedades de la motoneurona.

#DESCRIPCIÓN DE LA MUESTRA

#Defunciones por año
def_anio <- ELA %>%count(año, name = "defunciones")
print(def_anio)

#Sexo
sexo_ela <- ELA %>%count(sexo)
sexo_ela

#Región
region_ela <- ELA %>%count(region, sort = TRUE)
region_ela

#Grupo etario
grupo_edad_ela <- ELA %>%
  count(grupo_etario) %>%
  mutate(
    porcentaje = round(n/sum(n)*100,1)
  )
grupo_edad_ela




# RESUMEN DE EDAD

ELA %>%
  summarise(
    
    edad_min =
      min(edad, na.rm = TRUE),
    
    edad_media =
      mean(edad, na.rm = TRUE),
    
    edad_mediana =
      median(edad, na.rm = TRUE),
    
    edad_max =
      max(edad, na.rm = TRUE)
  )

# casos extremos

ELA %>%
  filter(edad < 18) %>%
  arrange(edad)


##GRÁFICOS DESCRIPTIVOS

plot(
  def_anio$año,
  def_anio$defunciones,
  type = "b",
  col = "#3D2A6B",
  pch = 16,
  lwd = 2,
  xlab = "Año",
  ylab = "Defunciones",
  main = "Mortalidad por ELA en Chile"
)

hist(ELA$edad, breaks = 20, main = "Edad al fallecer por ELA", xlab = "Edad (años)", col = "#B79CED", border = "#3D2A6B")


#POBLACIÓN NACIONAL

names(pob_nacional)

head(pob_nacional)

unique(pob_nacional$FECHA)[1:20]


# Poblacion nacional INE
pob_nacional <- pob_nacional %>% mutate(fecha = dmy(FECHA),
                                        año = year(fecha),
                                        mes = month(fecha))

# Utilizar sólo junio para evitar duplicación
# (enero y junio representan la misma población anual)

pob_nacional_junio <- pob_nacional %>% filter(mes == 6)

# Población nacional total por año

poblacion_anual <- pob_nacional_junio %>% group_by(año) %>%
  summarise(poblacion = sum(POBLACION, na.rm = TRUE),.groups = "drop")

poblacion_anual



#TASAS REGIONALES

# Equivalencia de regiones
equiv_regiones <- data.frame(
  REGION = 1:16,
  region = c(
    "De Tarapacá",
    "De Antofagasta",
    "De Atacama",
    "De Coquimbo",
    "De Valparaíso",
    "Del Libertador B. O'Higgins",
    "Del Maule",
    "Del Bíobío",
    "De La Araucanía",
    "De Los Lagos",
    "De Aisén del Gral. C. Ibáñez del Campo",
    "De Magallanes y de La Antártica Chilena",
    "Metropolitana de Santiago",
    "De Los Ríos",
    "De Arica y Parinacota",
    "De Ñuble"
  )
)

pob_regional_long <- pob_regional %>%
  pivot_longer(
    cols = matches("^a[0-9]{4}$"),
    names_to = "año",
    values_to = "poblacion"
  ) %>%
  mutate(
    año = as.numeric(sub("a", "", año))
  )


poblacion_region <- pob_regional_long %>%
  group_by(REGION, año) %>%
  summarise(
    poblacion = sum(poblacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(equiv_regiones, by = "REGION")


defunciones_region <- ELA %>%
  filter(año >= 2002, año <= 2023) %>%
  count(region, año, name = "defunciones")

tasa_region <- defunciones_region %>%
  left_join(
    poblacion_region,
    by = c("region", "año")
  ) %>%
  mutate(
    tasa_100k = defunciones / poblacion * 100000
  )
sum(is.na(tasa_region$tasa_100k))


ranking_regiones <- tasa_region %>%
  group_by(region) %>%
  summarise(
    tasa_promedio = mean(tasa_100k, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(tasa_promedio))
ranking_regiones


barplot(
  ranking_regiones$tasa_promedio,
  names.arg = ranking_regiones$region,
  las = 2,
  cex.names = 0.7,
  col = "#B79CED",
  border = "#3D2A6B",
  main = "Tasa promedio de mortalidad por ELA según región",
  ylab = "Tasa por 100.000 habitantes"
)



#################################################
# C
#################################################
#C1

#TASAS NACIONALES ANUALES

defunciones_anuales <- ELA %>% filter(año >= 1992, año <= 2023) %>%
  count(año, name = "defunciones")

tasa_anual <- defunciones_anuales %>% left_join(poblacion_anual,
                                                by = "año") %>% mutate(tasa_100k = defunciones / poblacion * 100000)
tasa_anual

sum(is.na(tasa_anual$tasa_100k))

# Grafico tasa nacional

plot(tasa_anual$año, tasa_anual$tasa_100k, type = "l", lwd = 3, col = "#3D2A6B", xlab = "Año",
     ylab = "Tasa por 100.000 habitantes", main = "Tasa nacional de mortalidad por ELA")

points(tasa_anual$año, tasa_anual$tasa_100k, pch = 16, col = "#B79CED", cex = 1.5)

#C2

#TASA POR SEXO ANUAL

poblacion_sexo <- pob_nacional_junio %>% mutate(sexo = case_when(
  SEXO == "H" ~ "Hombre",SEXO == "M" ~ "Mujer")) %>%
  group_by(año, sexo) %>%summarise(poblacion = sum(POBLACION, na.rm = TRUE),.groups = "drop")

defunciones_sexo <- ELA %>%filter(año >= 1992, año <= 2023) %>%
  count(año, sexo, name = "defunciones")

tasa_sexo <- defunciones_sexo %>%left_join(poblacion_sexo,
                                           by = c("año", "sexo")) %>%mutate(tasa_100k =
                                                                              defunciones / poblacion * 100000)

tasa_sexo
tasa_sexo %>% group_by(sexo) %>% summarise(tasa_promedio = mean(tasa_100k))


# Si muestra 0 significa que no hubo error al unir poblacion y defuncion
#sum(is.na(tasa_sexo$tasa_100k))
summary(tasa_sexo$tasa_100k)


# TASA ESPECIFICA POR GRUPO ETARIO

pob_nacional_junio <- pob_nacional_junio %>%
  mutate(grupo_etario = case_when(
    EDAD < 20 ~ "<20",
    EDAD >= 20 & EDAD < 40 ~ "20-39",
    EDAD >= 40 & EDAD < 60 ~ "40-59",
    EDAD >= 60 & EDAD < 80 ~ "60-79",
    EDAD >= 80 ~ "80+",
    TRUE ~ NA_character_))

poblacion_edad <- pob_nacional_junio %>%
  group_by(año, grupo_etario) %>%
  summarise(
    poblacion = sum(POBLACION, na.rm = TRUE),
    .groups = "drop"
  )

defunciones_edad <- ELA %>%
  filter(año >= 1992, año <= 2023) %>%
  count(año, grupo_etario, name = "defunciones")

tasa_edad <- defunciones_edad %>%
  left_join(
    poblacion_edad,
    by = c("año", "grupo_etario")
  ) %>%
  mutate(
    tasa_100k = defunciones / poblacion * 100000
  )


tasa_edad
# Si aparece 0 entonces la unión entre defunciones y población quedó correcta.
sum(is.na(tasa_edad$tasa_100k))



#C3

#TASA AJUSTADA POR EDAD

#poblacion estandar utilizando el promedio de la poblacion observada en cada grupo etario

poblacion_estandar <- poblacion_edad %>%
  group_by(grupo_etario) %>%
  summarise(
    poblacion_std = mean(poblacion),
    .groups = "drop"
  )

#calcular pesos de la población estándar

peso_std <- poblacion_estandar %>%
  mutate(
    peso = poblacion_std / sum(poblacion_std)
  ) %>%
  dplyr::select(grupo_etario, peso)

#Aplicar a las tasas especificas por edad

tasa_ajustada <- tasa_edad %>%
  left_join(
    peso_std,
    by = "grupo_etario"
  ) %>%
  mutate(
    contribucion = tasa_100k * peso
  ) %>%
  group_by(año) %>%
  summarise(
    tasa_ajustada =
      sum(contribucion,
          na.rm = TRUE),
    .groups = "drop"
  )
tasa_ajustada

# Si aparece 0 entonces la unión entre defunciones y población quedó correcta.
sum(is.na(tasa_ajustada$tasa_ajustada))

# Resumen de la tasa ajustada

summary(tasa_ajustada$tasa_ajustada)

# Gráfico

plot(
  tasa_ajustada$año,
  tasa_ajustada$tasa_ajustada,
  type = "l",
  lwd = 3,
  col = "#3D2A6B",
  xlab = "Año",
  ylab = "Tasa ajustada por 100.000 habitantes",
  main = "Tasa ajustada por edad de mortalidad por ELA"
)

points(
  tasa_ajustada$año,
  tasa_ajustada$tasa_ajustada,
  pch = 16,
  col = "#B79CED",
  cex = 1.5
)

#C4
# Modelo POISSON

modelo_poisson <- glm(
  defunciones ~ año,
  family = poisson(link = "log"),
  offset = log(poblacion),
  data = tasa_anual
)

summary(modelo_poisson)


# Evaluar sobredispersion

dispersion <- modelo_poisson$deviance / modelo_poisson$df.residual
dispersion

cat(
  "Indice de dispersion:",
  round(dispersion,2),
  "\n"
)

if(dispersion > 1.5){
  
  cat(
    "Existe sobredispersion.\n"
  )
  
  cat(
    "Por esta razon se utiliza un modelo quasi-Poisson para obtener errores estandar mas apropiados.\n"
  )
  
}else{
  
  cat(
    "No existe sobredispersion importante.\n"
  )
  
  cat(
    "El modelo Poisson resulta adecuado para los datos observados.\n"
  )
  
}


# Modelo QUASI-POISSON

tasa_anual_modelo <- tasa_anual %>% mutate(tiempo = año - min(año))

modelo_quasi <- glm(defunciones ~ tiempo + offset(log(poblacion)),
                    data = tasa_anual_modelo, family = quasipoisson(link = "log"))
summary(modelo_quasi)

APC <- 100 * (exp(coef(modelo_quasi)["tiempo"]) - 1)

APC

#Interpretacion APC
cat(
  "Cambio porcentual anual (APC):",
  round(APC, 2),
  "%\n"
)
if(APC > 0) {
  cat("La mortalidad por ELA presenta una tendencia creciente. \n")

} else if(APC < 0){
  cat("La mortalidad por ELA presenta una tendencia decreciente. \n")

} else {
  cat("La mortalidad por ELA se mantiene establa. \n")
}


#########################################################
#D
#########################################################

#D1
#Lee y transforma correctamente la población regional INE

pob_regional_long <- pob_regional %>%
  pivot_longer(
    cols = matches("^a[0-9]{4}$"),
    names_to = "año",
    values_to = "poblacion"
  ) %>%
  mutate(
    año = as.numeric(
      sub("a","", año)
    )
  ) %>%
  filter(
    año >= 2002,
    año <= 2023
  )


#D2
#Suma áreas urbana y rural y construye persona-años regionales

persona_anios_region <- pob_regional_long %>%
  group_by(
    REGION,
    año
  ) %>%
  summarise(
    persona_anios =
      sum(poblacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    equiv_regiones,
    by = "REGION"
  )


#D3
#Calcula tasas regionales brutas.

tasas_region <- ELA %>%
  filter(
    año >= 2002,
    año <= 2023
  ) %>%
  count(
    region,
    año,
    name = "defunciones"
  ) %>%
  left_join(
    persona_anios_region,
    by = c(
      "region",
      "año"
    )
  ) %>%
  mutate(
    tasa_100k =
      defunciones /
      persona_anios * 100000
  )

sum(is.na(tasas_region$tasa_100k))

#D4
#Calcula defunciones esperadas y SMR ajustado por
#año, sexo y edad.

tasa_nacional_ref <- ELA %>%
  filter(
    año >= 2002,
    año <= 2023
  ) %>%
  count(
    año,
    sexo,
    grupo_etario,
    name = "defunciones"
  ) %>%
  left_join(
    pob_nacional_junio %>%
      mutate(
        sexo = case_when(
          SEXO == "H" ~ "Hombre",
          SEXO == "M" ~ "Mujer"
        ),
        grupo_etario = case_when(
          EDAD < 20 ~ "<20",
          EDAD >= 20 & EDAD < 40 ~ "20-39",
          EDAD >= 40 & EDAD < 60 ~ "40-59",
          EDAD >= 60 & EDAD < 80 ~ "60-79",
          EDAD >= 80 ~ "80+",
          TRUE ~ NA_character_
        )
      ) %>%
      group_by(año, sexo, grupo_etario) %>%
      summarise(
        poblacion = sum(POBLACION, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("año", "sexo", "grupo_etario")
  ) %>%
  mutate(
    tasa_ref = defunciones / poblacion
  )

#poblacion regional por año, sexo y edad

poblacion_region_detalle <- pob_regional_long %>%
  mutate(
    sexo = case_when(
      SEXO == 1 | SEXO == "1" | SEXO == "H" ~ "Hombre",
      SEXO == 2 | SEXO == "2" | SEXO == "M" ~ "Mujer",
      TRUE ~ NA_character_
    ),
    grupo_etario = case_when(
      EDAD < 20 ~ "<20",
      EDAD >= 20 & EDAD < 40 ~ "20-39",
      EDAD >= 40 & EDAD < 60 ~ "40-59",
      EDAD >= 60 & EDAD < 80 ~ "60-79",
      EDAD >= 80 ~ "80+",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(
    REGION,
    año,
    sexo,
    grupo_etario
  ) %>%
  summarise(
    poblacion = sum(
      poblacion,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  left_join(
    equiv_regiones,
    by = "REGION"
  )

#defunciones esperadas

esperadas_region <- poblacion_region_detalle %>%
  left_join(
    tasa_nacional_ref %>%
      dplyr::select(
        año,
        sexo,
        grupo_etario,
        tasa_ref
      ),
    by = c(
      "año",
      "sexo",
      "grupo_etario"
    )
  ) %>%
  mutate(
    esperadas = poblacion * tasa_ref
  ) %>%
  group_by(region) %>%
  summarise(
    esperadas = sum(
      esperadas,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

sum(is.na(esperadas_region$esperadas))

#defunciones observadas
observadas_region <- ELA %>%
  filter(
    año >= 2002,
    año <= 2023
  ) %>%
  count(
    region,
    name = "observadas"
  )

#SMR (Corregido: iniciando desde esperadas_region para asegurar las 16 regiones)
smr_region <- esperadas_region %>%
  left_join(
    observadas_region,
    by = "region"
  ) %>%
  mutate(
    observadas = replace_na(observadas, 0),
    SMR = observadas / esperadas
  )

cat("\nResumen SMR regional (pre-intervalos):\n")
print(smr_region)
cat("\nEstadísticas SMR:\n")
print(summary(smr_region$SMR))

#intervalos de confianza 95% (Exacto basado en Poisson/Chi-cuadrado)

smr_region <- smr_region %>%
  mutate(
    IC_inf = case_when(
      observadas == 0 ~ 0,
      observadas > 0 ~ qchisq(0.025, 2 * observadas) / (2 * esperadas),
      TRUE ~ NA_real_
    ),
    IC_sup = case_when(
      observadas >= 0 ~ qchisq(0.975, 2 * (observadas + 1)) / (2 * esperadas),
      TRUE ~ NA_real_
    )
  )

cat("\nSMR regional con intervalos de confianza exactos Poisson:\n")
print(smr_region)

#interpretación
cat("\nInterpretación del SMR regional:\n")
smr_interpretacion <- smr_region %>%
  mutate(
    interpretacion = case_when(
      IC_inf > 1 ~ "Sobre lo esperado",
      IC_sup < 1 ~ "Bajo lo esperado",
      TRUE ~ "Similar a lo esperado"
    )
  )
print(smr_interpretacion)


# Guardar resultados y graficos

# Gráfico 1: Tasa nacional de mortalidad ajustada por edad (Entregable 5 - Gráfico 1)
p1 <- ggplot(tasa_ajustada, aes(x = año, y = tasa_ajustada)) +
  geom_line(color = "#3D2A6B", size = 1.2) +
  geom_point(color = "#B79CED", size = 3) +
  scale_x_continuous(breaks = seq(1992, 2023, by = 2)) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Tasa Anual de Mortalidad por ELA Ajustada por Edad",
    subtitle = "Chile, 1992-2023",
    x = "Año",
    y = "Tasa por 100.000 habitantes"
  ) +
  theme(
    plot.title = element_text(face = "bold", color = "#3D2A6B", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "#333333"),
    axis.title = element_text(face = "bold", color = "#3D2A6B"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

ggsave("tasamortalidadela.png", plot = p1, width = 8, height = 5, dpi = 300)

p1

# Tasa nacional específica por grupos de edad
p3 <- ggplot(tasa_edad, aes(x = año, y = tasa_100k, color = grupo_etario, group = grupo_etario)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = seq(1992, 2023, by = 4)) +
  scale_color_manual(
    values = c(
      "<20" = "#DCCFF8",
      "20-39" = "#CFC3E8",
      "40-59" = "#B79CED",
      "60-79" = "#3D2A6B",
      "80+" = "#1F1F1F"
    )
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Tasa de Mortalidad por ELA según Grupo Etario",
    subtitle = "Chile, 1992-2023 (Tasas Específicas por 100.000 hab.)",
    x = "Año",
    y = "Tasa por 100.000 habitantes",
    color = "Grupo Etario"
  ) +
  theme(
    plot.title = element_text(face = "bold", color = "#3D2A6B", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "#333333"),
    axis.title = element_text(face = "bold", color = "#3D2A6B"),
    legend.position = "bottom"
  )

ggsave("tasamortalidad_edad.png", plot = p3, width = 8, height = 5, dpi = 300)
p3

# Razón de Mortalidad Estandarizada (SMR) regional con IC 95%
p2 <- ggplot(smr_interpretacion, aes(x = reorder(region, SMR), y = SMR, fill = interpretacion)) +
  geom_col(alpha = 0.85) +
  geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup), width = 0.25, color = "#3D2A6B", size = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#3D2A6B", size = 0.8) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Sobre lo esperado" = "#3D2A6B",
      "Similar a lo esperado" = "#B79CED",
      "Bajo lo esperado" = "#DCCFF8"
    )
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Razón de Mortalidad Estandarizada (SMR) por ELA",
    subtitle = "SMR Regional Ajustado por Año, Sexo y Edad (Chile, 2002-2023)",
    x = "Región",
    y = "SMR",
    fill = "Mortalidad"
  ) +
  theme(
    plot.title = element_text(face = "bold", color = "#3D2A6B", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "#333333"),
    axis.title = element_text(face = "bold", color = "#3D2A6B"),
    legend.position = "bottom"
  )

ggsave("smr_regional_ela.png", plot = p2, width = 9, height = 6, dpi = 300)

p2



################################################
# Guardar la base de datos de ELA filtrada y limpia como archivo CSV
write.csv2(ELA, "base_datos_ELA.csv", row.names = FALSE, fileEncoding = "latin1")

smr_interpretacion <- smr_region %>%
  mutate(
    interpretacion = case_when(
      IC_inf > 1 ~ "Sobre lo esperado",
      IC_sup < 1 ~ "Bajo lo esperado",
      TRUE ~ "Similar a lo esperado"
    )
  )
print(smr_interpretacion)


##############################################
# Guarda las tablas de resultados


# 1. Tasas nacionales (con tasa ajustada)
write.csv2(tasa_ajustada, "resultados_tasa_nacional_ajustada.csv", row.names = FALSE, fileEncoding = "latin1")

# 2. Tasas por sexo
write.csv2(tasa_sexo, "resultados_tasa_por_sexo.csv", row.names = FALSE, fileEncoding = "latin1")

# 3. Tasas por edad
write.csv2(tasa_edad, "resultados_tasa_por_edad.csv", row.names = FALSE, fileEncoding = "latin1")

# 4. Resultados del modelo Quasi-Poisson
resultados_modelo <- data.frame(
  APC = APC,
  Coeficiente_tiempo = coef(modelo_quasi)["tiempo"],
  Std_Error = summary(modelo_quasi)$coefficients["tiempo", "Std. Error"],
  P_valor = summary(modelo_quasi)$coefficients["tiempo", "Pr(>|t|)"]
)
write.csv2(resultados_modelo, "resultados_modelo_quasi_poisson.csv", row.names = FALSE, fileEncoding = "latin1")

# 5. SMR regional
write.csv2(smr_interpretacion, "resultados_smr_regional.csv", row.names = FALSE, fileEncoding = "latin1")
