# Limpieza de Base

rm(list = ls())

library(tidyverse)
library(haven)

mod5 <- read.csv("C:/Users/uribe/OneDrive/Escritorio/2025 - 01/Microeconometría/TRABAJO MICROMETRIA/966-Modulo05/Enaho01a-2024-500.csv", fileEncoding = "Latin1")

# VARIABLES INDIVIDUALES

# Edad:
# View(mod5["P208A"])

mod5 <- mod5 %>%
  mutate(
    P208A = labelled(
      P208A,
      label = "Edad")) %>%
  rename(Edad = P208A)
         
# View(mod5["Edad"])

# Sexo
# view(mod5["P207"])

mod5 <- mod5 %>%
  mutate(P207 = case_when(
    P207 == 1 ~ 0,
    P207 == 2 ~ 1,
    TRUE ~ NA_real_ 
  )) %>%
  mutate(P207 = as.numeric(P207))

mod5 <- mod5 %>%
  mutate(P207 = labelled(
    P207,
    labels = c("Hombre" = 0, "Mujer" = 1),
    label = "Sexo"
  )) %>%
  rename(Sexo = P207)

# view(mod5["Sexo"])

# Estado Civil
# view(mod5["P209"])

mod5 <- mod5 %>%
  mutate(P209 = as.character(P209))

mod5 <- mod5 %>%
  mutate(
    P209 = case_when(
      P209 %in% c(1,2) ~ 1,
      P209 %in% c(3) ~ 2,
      P209 %in% c(4,5) ~ 3,
      P209 %in% c(6) ~ 4,
      TRUE ~ NA_real_
    ),
    P209 = labelled(
      P209,
      labels = c(
        "Casado/Conviviente" = 1,
        "Viudo" = 2,
        "Divorciado/Separado" = 3,
        "Soltero" = 4
      ),
      label = "Estado Civil"
    )
  ) %>%
  rename(Civil = P209)

# view(mod5["Civil"])

# Nivel educativo
# view(mod5["P301A"])

mod5 <- mod5 %>%
  mutate(P301A = as.numeric(P301A))

mod5 <- mod5 %>%
  mutate(
    P301A = case_when(
      P301A %in% c(1, 2, 12) ~ 1,  # Sin educación o básica especial
      P301A %in% c(3, 4) ~ 2,      # Primaria
      P301A %in% c(5, 6) ~ 3,      # Secundaria
      P301A %in% c(7, 8) ~ 4,      # Sup. no universitaria
      P301A %in% c(9, 10) ~ 5,      # Sup. universitaria 
      P301A %in% c(11) ~ 6,        # Maestria/Doctorado
      TRUE ~ NA_real_
    ),
    P301A = labelled(
      P301A,
      labels = c(
        "Sin educación" = 1,
        "Primaria" = 2,
        "Secundaria" = 3,
        "Sup. no universitaria" = 4,
        "Sup. universitaria" = 5,
        "Maestria/Doctorado" = 6
      ),
      label = "Nivel educativo"
    )
  ) %>%
  rename(Educ = P301A)

# view(mod5["Educ"])

# Raza
# view(mod5["P558C"])

mod5 <- mod5 %>%
  mutate(P558C = as.numeric(P558C))

mod5 <- mod5 %>%
  mutate(
    P558C = case_when(
      P558C %in% c(1, 2, 3, 4, 6, 9) ~ 1,  # Indigena
      P558C %in% c(5 , 7, 8) ~ 0,          # Otro
      TRUE ~ NA_real_
    ),
    P558C = labelled(
      P558C,
      labels = c(
        "Indigena" = 1,
        "Otro" = 0
      ),
      label = "Indigena"
    )
  ) %>%
  rename(Indigena = P558C)

# view(mod5["Indigena"])

# Jefe del hogar
# view(mod5["P203"])

mod5 <- mod5 %>%
  mutate(P203 = as.numeric(P203))

mod5 <- mod5 %>%
  mutate(
    P203 = case_when(
      P203 == 1 ~ 1,         # Jefe
      P203 %in% 2:11 ~ 0,    # Otro
      TRUE ~ NA_real_
    ),
    P203 = labelled(
      P203,
      labels = c(
        "Jefe" = 1,
        "Otro" = 0
      ),
      label = "Jefe de hogar"
    )
  ) %>%
  rename(Jefe = P203)

# view(mod5["Jefe"])

# Variables laborales

# PEA Ocupada (Para filtrar la base)
mod5 <- mod5 %>%
  mutate(
    PEAO = case_when(
      OCU500 %in% 1 ~ 1,          # PEA Ocupada
      OCU500 %in% 2:4 ~ 0,        # Otro
      TRUE ~ NA_real_
    ),
    PEAO = labelled(
      PEAO,
      labels = c(
        "PEA Ocupada" = 1,
        "Otro" = 0
      ),
      label = "PEA Ocupada"
    )
  )

# view(mod5["PEAO"])

# Informalidad

mod5 <- mod5 %>%
  mutate(
    Informal = case_when(
      P510A1 == 3 ~ 1,          # No registrado → informal
      P510A1 %in% 1:2 ~ 0,      # Registrado → formal
      TRUE ~ NA_real_
    ),
    Informal = labelled(
      Informal,
      labels = c("Formal" = 0, "Informal" = 1),
      label = "Condición de informalidad"
    )
  )

# view(mod5["Informal"])
# mod5 %>% count(Informal)

# Actividades Sectoriales
mod5 <- mod5 %>%
  mutate(
    Act_Sec = case_when(
      P506R4 %in% 0111:0322 ~ 1, # Agricultura, ganaderia, silvicultura y pesca
      P506R4 %in% 0510:0990 ~ 2, # Explotación de minas y canteras
      P506R4 %in% 1010:3320 ~ 3, # Industrias manufactureras
      P506R4 %in% 3510:3530 ~ 4, # Suministro de electricidad, gas, vapor y aire acondicionado
      P506R4 %in% 3600:3900 ~ 5, # Suministro de agua; evacuación de aguas residuales, gestión de desechos y descontaminación
      P506R4 %in% 4100:4390 ~ 6, # Construcción
      P506R4 %in% 4510:4799 ~ 7, # Comercio al por mayor y al por menor; reparación de vehículos automotores y motocicletas
      P506R4 %in% 4911:5320 ~ 8, # Transporte y almacenamiento
      P506R4 %in% 5510:5630 ~ 9, # Actividades de alojamiento y de servicio de comidas
      P506R4 %in% 5811:6399 ~ 10, # Información y comunicaciones
      P506R4 %in% 6411:6630 ~ 11, # Actividades financieras y de seguros
      P506R4 %in% 6810:6820 ~ 12, # Actividades inmobiliarias
      P506R4 %in% 6910:7500 ~ 13, # Actividades profesionales, científicas y técnicas
      P506R4 %in% 7710:8299 ~ 14, # Actividades de servicios administrativos y de apoyo
      P506R4 %in% 8411:8430 ~ 15, # Administración pública y defensa; planes de seguridad social de afiliación obligatoria
      P506R4 %in% 8510:8550 ~ 16, # Enseñanza
      P506R4 %in% 8610:8890 ~ 17, # Actividades de atención de la salud humana y de asistencia social
      P506R4 %in% 9000:9329 ~ 18, # Actividades artísticas, de entretenimiento y recreativas
      P506R4 %in% 9411:9609 ~ 19, # Otras actividades de servicios
      P506R4 %in% 9700:9820 ~ 20, # Actividades de los hogares como empleadores; actividades no diferenciadas de los hogares como productores de bienes y servicios para uso propio
      P506R4 %in% 9900 ~ 21, # Actividades de organizaciones y órganos extraterritoriales
      TRUE ~ NA_real_
    ),
    Act_Sec = labelled(
      Act_Sec,
      labels = c(
        "Agricultura, ganadería, silvicultura y pesca" = 1,
        "Explotación de minas y canteras" = 2,
        "Industrias manufactureras" = 3,
        "Suministro de electricidad, gas, vapor y aire acondicionado" = 4,
        "Suministro de agua; evacuación de aguas residuales, gestión de desechos y descontaminación" = 5,
        "Construcción" = 6,
        "Comercio al por mayor y al por menor; reparación de vehículos automotores y motocicletas" = 7,
        "Transporte y almacenamiento" = 8,
        "Actividades de alojamiento y de servicio de comidas" = 9,
        "Información y comunicaciones" = 10,
        "Actividades financieras y de seguros" = 11,
        "Actividades inmobiliarias" = 12,
        "Actividades profesionales, científicas y técnicas" = 13,
        "Actividades de servicios administrativos y de apoyo" = 14,
        "Administración pública y defensa; planes de seguridad social de afiliación obligatoria" = 15,
        "Enseñanza" = 16,
        "Actividades de atención de la salud humana y de asistencia social" = 17,
        "Actividades artísticas, de entretenimiento y recreativas" = 18,
        "Otras actividades de servicios" = 19,
        "Actividades de los hogares como empleadores; actividades no diferenciadas de los hogares como productores de bienes y servicios para uso propio" = 20,
        "Actividades de organizaciones y órganos extraterritoriales" = 21
      ),
      label = "Actividades Sectoriales"
    )
  )

# view(mod5["Act_Sec"])

# por sectores primarios, secunadrios y terciarios
mod5 <- mod5 %>% 
  mutate(
    Sector = case_when(
      Act_Sec %in% 1:2 ~ 1, #Sector Primario agropecuario
      Act_Sec %in% 3:6 ~ 2, #Sector Secundario manufactura
      Act_Sec %in% 7:21 ~ 3, #Sector Terciario servicios
      TRUE ~ NA_real_
    ),
    Sector = labelled(
      Sector,
      labels = c(
        "Sector Primario" = 1,
        "Sector Secundario" = 2,
        "Sector Terciario" = 3
      ),
      label = "Sector"
    )
  )

# view(mod5["Sector"])
# mod5 %>% count(Sector)

# Grupo ocupacional

mod5 <- mod5 %>%
  mutate(ciuo = as.integer(P505R4))

mod5 <- mod5 %>%
  mutate(
    ciou = case_when(
      P505R4 %in% 1111:1499 ~ 1, # Miembros del Poder Ejecutivo, Legislativo, Judicial y personal directivo de la administración pública y privada
      P505R4 %in% 2111:2656 ~ 2, # Profesionales científicos e intelectuales
      P505R4 %in% 3111:3523 ~ 3, # Profesionales técnicos
      P505R4 %in% 4110:4419 ~ 4, # Jefes y empleados administrativos
      P505R4 %in% 5111:5419 ~ 5, # Trabajadores de los servicios y vendedores de comercios y mercados
      P505R4 %in% 6111:6340 ~ 6, # Agricultores y trabajadores calificados agropecuarios, forestales y pesqueros
      P505R4 %in% 7111:7519 ~ 7, # Trabajadores de la construcción, edificación, productos artesanales, electricidad y las telecomunicaciones
      P505R4 %in% 8111:8352 ~ 8, # Operadores de maquinaria industrial, ensambladores y conductores de transporte
      P505R4 %in% 9111:9629 ~ 9, # Ocupaciones elementales
      P505R4 %in% c(111:131,211:231,311:331) ~ 0, # Ocupaciones militares y policiales
      TRUE ~ NA_real_
    ),
    ciou = labelled(
      ciou,
      labels = c(
        "Miembros del Poder Ejecutivo, Legislativo, Judicial y personal directivo de la administración pública y privada" = 1,
        "Profesionales científicos e intelectuales" = 2,
        "Profesionales técnicos" = 3,
        "Jefes y empleados administrativos" = 4,
        "Trabajadores de los servicios y vendedores de comercios y mercados" = 5,
        "Agricultores y trabajadores calificados agropecuarios, forestales y pesqueros" = 6,
        "Trabajadores de la construcción, edificación, productos artesanales, electricidad y las telecomunicaciones" = 7,
        "Operadores de maquinaria industrial, ensambladores y conductores de transporte" = 8,
        "Ocupaciones elementales" = 9,
        "Ocupaciones militares y policiales" = 0
      ),
      label = "Grupo Ocupacional"
    )
  )

# view(mod5["ciou"])

# Habilidad ocupacional

mod5 <- mod5 %>%
  mutate(
    Skill_ocu = case_when(
      ciou %in% 1:3 ~ 1, #High skill
      ciou %in% 4:8 ~ 2, #Medium skill
      ciou %in% 9 ~ 3, #Low skill
      ciou %in% 0 ~ 0 #Ocupaciones Fuerzas Armadas
    ),
    Skill_ocu = labelled(
      Skill_ocu,
      labels = c(
        "High skill" = 1,
        "Medium skill" = 2,
        "Low skill" = 3,
        "Fuerzas Armadas" = 0
      ),
      label = "Skill Ocupation"
    )
  )

# view(mod5["Skill_ocu"])

# ingreso mensual

mod5 <- mod5 %>%
  rowwise() %>%
  mutate(
    ing_ocu_pri = sum(c_across(c(I524A1, D529T, I530A, D536)), na.rm = TRUE),
    ing_ocu_sec = sum(c_across(c(I538A1, D540T, I541A, D543)), na.rm = TRUE),
    ing_extra   = D544T,
    ing_total   = sum(c_across(c(ing_ocu_pri, ing_ocu_sec, ing_extra)), na.rm = TRUE),
    ingreso     = ing_total / 12
  ) %>%
  ungroup()

# Variables geograficas

# Area (rural/urbano)

mod5 <- mod5 %>%
  mutate(
    area = case_when(
      ESTRATO %in% 1:5 ~ 1,
      ESTRATO %in% 6:8 ~ 0,
      TRUE ~ NA_real_
    ),
    area = labelled(
      area,
      labels = c("Urbano" = 1, "Rural" = 0),
      label = "Área Geográfica"
    )
  )

#Zona

mod5 <- mod5 %>%
  mutate(
    zona = case_when(
      DOMINIO %in% 1:3 ~ 1,
      DOMINIO %in% 4:6 ~ 2,
      DOMINIO == 7 ~ 3,
      DOMINIO == 8 ~ 4,
      TRUE ~ NA_real_
    ),
    zona = labelled(
      zona,
      labels = c(
        "Costa" = 1,
        "Sierra" = 2,
        "Selva" = 3,
        "Lima Metropolitana" = 4
      ),
      label = "Zona Geográfica"
    )
  )

# Zona geográfica
mod5 <- mod5 %>%
  mutate(
    zona_geo = case_when(
      zona == 1 & area == 1 ~ 1,
      zona == 1 & area == 0 ~ 2,
      zona == 2 & area == 1 ~ 3,
      zona == 2 & area == 0 ~ 4,
      zona == 3 & area == 1 ~ 5,
      zona == 3 & area == 0 ~ 6,
      zona == 4 ~ 7,
      TRUE ~ NA_real_
    ),
    zona_geo = labelled(
      zona_geo,
      labels = c(
        "Costa Urbano" = 1,
        "Costa Rural" = 2,
        "Sierra Urbano" = 3,
        "Sierra Rural" = 4,
        "Selva Urbano" = 5,
        "Selva rural" = 6,
        "Lima Metropolitana" = 7
      ),
      label = "Zona Geográfica"
    )
  )

# Base final
b2_2 <- mod5 %>% 
  filter(PEAO == 1) %>%
  select(Edad, Sexo, Civil, Educ, Indigena, Jefe, Sector, zona_geo, Informal, Skill_ocu, ingreso)

setwd("C:/Users/uribe/OneDrive/Escritorio/Limpieza bases")
write_dta(b2_2,"Base_Final.dta")


