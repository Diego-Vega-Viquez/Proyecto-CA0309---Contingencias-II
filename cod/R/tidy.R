library(dplyr)
library(readxl)
library(tidyr)
library(tidyverse)
library(writexl)

# Hombres

qx_h <- read_excel('../../data/data_raw.xlsx', sheet = 'qx_Hombres')
qx_h <- qx_h %>% 
  pivot_longer(cols = `2025`:`2150`,
               names_to = 'Año',
               values_to = 'qx_muerte')
colnames(qx_h) <- c('Año Nacimiento', 'Año', 'qx (Muerte)')
qx_h$`Año Nacimiento` <- as.integer(qx_h$`Año Nacimiento`)
qx_h$Año <- as.integer(qx_h$Año)

qx_h$Edad <- qx_h$Año- qx_h$`Año Nacimiento`

qx_h <- na.omit(qx_h)

qx_h$Sexo <- 'M'

view(qx_h)

# Mujeres

qx_m <- read_excel('../../data/data_raw.xlsx', sheet = 'qx_Mujeres')
qx_m <- qx_m %>% 
  pivot_longer(cols = `2025`:`2150`,
               names_to = 'Año',
               values_to = 'qx_muerte')
colnames(qx_m) <- c('Año Nacimiento', 'Año', 'qx (Muerte)')
qx_m$`Año Nacimiento` <- as.integer(qx_m$`Año Nacimiento`)
qx_m$Año <- as.integer(qx_m$Año)

qx_m$Edad <- qx_m$Año- qx_m$`Año Nacimiento`

qx_m <- na.omit(qx_m)

qx_m$Sexo <- 'F'

view(qx_m)

# Unido

qx_total <- bind_rows(qx_h, qx_m)

view(qx_total)

# Guardar

write_xlsx(qx_total,'../../data/qx_muertes.xlsx')
