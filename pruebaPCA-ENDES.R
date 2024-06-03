################################################################################
# Objetivo: Implementación del PCA con datos de la ENDES para el periodo 2007-2022
# Proyecto: GiZ Pobreza Urbana

# Estructura:
# 0. Librerías 
# 1. Generación de bases de datos
# 2. Creación de variables de interés
################################################################################

# 0. Librerías y direcciones ----
#install.packages("psych")
#install.packages("ggrepel")
#install.packages("stringi")
#install.packages("factoextra")
#install.packages("ROCR")

library(psych)
library(tidyverse)
library(ggrepel)
library(factoextra)
library(ROCR)
library(haven)

# 1. Generación de bases de datos -----

#dirEndes <- "C:/Users/User/OneDrive - MIGRACIÓN VIDENZA/1. Proyectos/1. Proyectos actuales/23. Artículos PDB/1. PDB - DIT/2. Data/ENDES/1. Bases"
dirEndes <- "C:/Users/Jennifer Prado/OneDrive - VIDENZA/Proyectos activos/1. PDB - DIT/2. Data/ENDES/1. Bases"
setwd(dirEndes)
baseHogares <- read_dta("baseHogaresENDES.dta")
baseNinosDIT <- read_dta("baseDIT.dta")

## 1.1. A nivel de hogares -----

baseHogares <- baseHogares %>%
  filter(area == 1) %>% 
  mutate(piso = case_when(hv213 == 10 | hv213 == 11 ~ 3,
                          hv213 == 20 | hv213 == 21 ~ 2,
                          hv213 == 30 | hv213 == 31 |hv213 == 32 |hv213 == 33 | hv213 == 34 ~ 1,
                          TRUE ~ NA),
         pared = case_when(hv214 == 10 | hv214 == 11 | hv214 == 12 | hv214 == 13 ~ 3,
                           hv214 == 20 | hv214 == 21 | hv214 == 22 | hv214 == 23 | hv214 == 24 ~ 2,
                           hv214 == 30 | hv214 == 31 | hv214 == 32 | hv214 == 33 ~ 1,
                           TRUE ~ NA),
         techo = case_when(hv215 == 10 | hv215 == 11 | hv215 == 12 ~ 3,
                           hv215 == 20 | hv215 == 21 | hv215 == 22 ~ 2,
                           hv215 == 30 | hv215 == 31 |hv215 == 32 |hv215 == 33 | hv215 == 34 ~ 1,
                           TRUE ~ NA),
         pctNinas = nNinas / nNinos,
         pctMujeres = nMujeres/mieperho) %>% 
  select(-c("nNinas", "nNinos", "nMujeres"))


## 1.2. A nivel de niños -----

var9a12 <-c("bord", "qi478", "qi478a", "qi478e1", "qi478e2", "qi478e3", "qi478e4", "qi478e5",
            "qi478e6", "qi478e7", "qi478e8", "qi478e9", "qi478e10", "caseid", "bidx", "id1",
            "b4", "v001", "v005", "v012", "v022", "v024", "v025", "v149", "v190", "sregion",
            "s119", "s108n", "e3conv", "e4conv", "e5conv", "e345", "r4_9_12m", "hv012",
            "mieperho", "dominio", "area", "region", "hv026", "altitud", "quintil", "riqueza",
            "agua", "tiempoAgua", "desague", "electricidad", "radio", "tv", "refrigerador",
            "bicicleta", "moto", "carro", "pisoBajaCalidad", "paredBajaCalidad", "techoBajaCalidad",
            "hacinamiento", "mujerJH", "edadJH", "combustibleCocina", "nActivosPrioritarios",
            "pesoNac", "tallaNac", "pesoNacBajo", "tallaNacBajo", "ira0a59", "eda0a59","e10conv")

baseDIT_9a12 <- baseNinosDIT %>% 
  filter(area == 1) %>% 
  filter(qi478 >= 9 & qi478 <= 12) %>% 
  select(all_of(var9a12)) %>% 
  mutate(sexo = case_when(b4 == 1 ~ 1,
                          TRUE ~ 0)) %>% 
  rename(edadMadre = v012)

baseDIT_13a18 <- baseNinosDIT %>% 
  filter(area == 1) %>% 
  filter(qi478 >= 13 & qi478 <= 18)

baseDIT_19a23 <- baseNinosDIT %>% 
  filter(area == 1) %>% 
  filter(qi478 >= 19 & qi478 <= 23)

baseDIT_24a36 <- baseNinosDIT %>% 
  filter(area == 1) %>% 
  filter(qi478 >= 24 & qi478 <= 36)

baseDIT_37a54 <- baseNinosDIT %>% 
  filter(area == 1) %>% 
  filter(qi478 >= 37 & qi478 <= 54)

baseDIT_55a71 <- baseNinosDIT %>% 
  filter(area == 1) %>% 
  filter(qi478 >= 55 & qi478 <= 71)


# Conjunto de variables numéricas a nivel de hogar
varNumDIT_9a12 <- c("qi478", "qi478e1", "qi478e2", "qi478e3", "qi478e4", "qi478e5",
                    "qi478e6", "qi478e7", "qi478e8", "qi478e9", "qi478e10", 
                    "sexo", "edadMadre", "v149", "s108n", "e3conv", "e4conv", "e5conv", "e10conv","e345", "r4_9_12m", "hv012",
                    "mieperho", "hv026", "altitud", "riqueza",
                    "agua", "tiempoAgua", "desague", "electricidad", "radio", "tv", "refrigerador",
                    "bicicleta", "moto", "carro", "pisoBajaCalidad", "paredBajaCalidad", "techoBajaCalidad",
                    "hacinamiento", "mujerJH", "edadJH", "combustibleCocina", "nActivosPrioritarios",
                    "pesoNac", "tallaNac", "pesoNacBajo", "tallaNacBajo")

# Bases de datos a nivel de Hogares
bPCA_9a12 <- baseDIT_9a12 %>% 
  subset(select = varNumDIT_9a12)

# Missings
missings_por_columna <- colSums(is.na(bPCA_9a12))

# Crear un dataframe con la información de los missings
df_missings <- data.frame(
  columna = names(missings_por_columna),
  missings = missings_por_columna)

df_missings <- df_missings %>% arrange(desc(missings))

bPCA_9a12 <- bPCA_9a12[complete.cases(bPCA_9a12) & apply(bPCA_9a12, 1, function(x) all(is.finite(x))), ]
bPCA_9a12 <- bPCA_9a12

# 3. PCA ----

variables_con_desviacion_cero <- names(bPCA_9a12)[apply(bPCA_9a12, 2, sd) == 0]
variables_con_desviacion_cero
# variables_con_desviacion_cero: "violenciaEsposoH" "algunSeguroH" "ira0a59H"

# Matriz de correlaciones
matrixcor <- cor(bPCA_9a12)
matrixcor
cor.plot(matrixcor)

# PCA
pc1 <- prcomp(x=bPCA_9a12,scale=TRUE, center=TRUE)
biplot(pc1, scale = 0)
summary(pc1)

# Scree graph
fviz_eig(pc1)

# Coeficiones del PCA
loadings1 <- pc1$rotation
loadings1

# Para visualización
loadings1t <- as.data.frame(t(loadings1))

View(loadings1t[abs(loadings1t$riqueza) > 0.2,])
# Análisis de alineamiento: solo componentes con peso de gasto > 0.2

# Ranking de las variables más importantes del PCA
pc1_loadings <- loadings1[, 1]
ranked_variables1 <- names(sort(abs(pc1_loadings), decreasing = TRUE))
ranked_variables1[1:21]

# Análisis visual de los 2 componentes principales
fviz_pca_var(pc1, geom = c("arrow"))
fviz_pca_var(pc1, geom = c("arrow", "text"))
fviz_pca_biplot(pc1, geom = c("point"))

