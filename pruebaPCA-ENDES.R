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

# 1. Generación de bases de datos -----

library(haven)
dirEnaho <- "/etc/data/base_trabajo"
setwd(dirEnaho)
baseHogares <- read_dta("baseHogaresENDES.dta")

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

# Conjunto de variables numéricas a nivel de hogar
varNumHog <- c("mieperho", "hacinamiento", "edadJH", "nActivosPrioritarios", "tiempoAgua",
               "violenciaEsposoH", "anemiaMujerH", "anemiaSevMujerH", "tuvoETSH", "algunSeguroH",
               "desnCrOmsH", "desnCrSevH", "anemiaNinosH", "anemiaSevNinosH", "desInfComH", "desInfEmoH", "desInfJueH", "tallaNacBajoH", "pesoNacBajoH", "ira0a59H", "eda0a59H",
               "riqueza", "piso", "pared", "techo")

# Bases de datos a nivel de Hogares
bHogPCA <- baseHogares %>% 
  subset(select = varNumHog)

# Missings
missings_por_columna <- colSums(is.na(bHogPCA))

# Crear un dataframe con la información de los missings
df_missings <- data.frame(
  columna = names(missings_por_columna),
  missings = missings_por_columna)

df_missings <- df_missings %>% arrange(desc(missings))

bHogPCA <- bHogPCA%>%
  select(-c(tiempoAgua, desInfComH, desInfEmoH, desInfJueH))

bHogPCA <- bHogPCA[complete.cases(bHogPCA) & apply(bHogPCA, 1, function(x) all(is.finite(x))), ]
bHogPCA1 <- bHogPCA

# 3. PCA ----

variables_con_desviacion_cero <- names(bHogPCA1)[apply(bHogPCA1, 2, sd) == 0]
variables_con_desviacion_cero
# variables_con_desviacion_cero: "violenciaEsposoH" "algunSeguroH" "ira0a59H"

bHogPCA1 <- bHogPCA%>%
  select(-c(violenciaEsposoH, algunSeguroH, ira0a59H))

# Matriz de correlaciones
matrixcor <- cor(bHogPCA1)
matrixcor
cor.plot(matrixcor)

# PCA
pc1 <- prcomp(x=bHogPCA1,scale=TRUE, center=TRUE)
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

