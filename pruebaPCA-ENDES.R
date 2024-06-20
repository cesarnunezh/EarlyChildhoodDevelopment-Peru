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
library(openxlsx)

# 1. Generación de bases de datos -----

#dirEndes <- "C:/Users/User/OneDrive - MIGRACIÓN VIDENZA/1. Proyectos/1. Proyectos actuales/23. Artículos PDB/1. PDB - DIT/2. Data/ENDES/1. Bases"
dirEndes <- "C:/Users/Jennifer Prado/OneDrive - VIDENZA/Proyectos activos/1. PDB - DIT/2. Data/ENDES/1. Bases"
setwd(dirEndes)
#baseHogares <- read_dta("baseHogaresENDES.dta")
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

var9a12 <-c("bord", "qi478","caseid", "bidx", "id1",
            "b4", "v001", "v005", "v012", "v022", "v024", "v025", "v149", "v190", "sregion",
            "s119", "s108n","r2",#"desnCrOms",
            "e3conv", "e4conv", "e5conv", "e345", "r4_9_12m", "hv012",
            "e7conv","e8conv", "e9conv","e10conv","e6f6conv","r4_9_12m",
            "mieperho", "dominio", "area", "region", "hv026", "altitud", "quintil", "riqueza",
            "agua", "desague", "electricidad", "radio", "tv", "refrigerador",
            "bicicleta", "moto", "carro", "pisoBajaCalidad", "paredBajaCalidad", "techoBajaCalidad",
            "hacinamiento", "mujerJH", "edadJH", "combustibleCocina", "nActivosPrioritarios",
            "pesoNac", "tallaNac", "pesoNacBajo", "tallaNacBajo", "ira0a59", "eda0a59",
            "e1camina_solo","e2conv","e6conv")

var13a18 <-c("bord", "qi478", "qi478a", "caseid", "bidx", "id1", 
             "b4", "v001", "v005", "v012", "v022", "v024", "v025", "v149", "v190","sregion","r4_13_18m",
             "s119", "s108n",
             "f2aconv", "f2bconv", "f2cconv", "f2dconv","f2econv","f3conv", "f4conv", "f5conv", "f345", "e6f6conv",
             "f1camina_solo", "f6conv",
             "hv012","mieperho", "dominio", "area", "region", "hv026", "altitud", "quintil", "riqueza",
             "agua", "desague", "electricidad", "radio", "tv", "refrigerador",
             "bicicleta", "moto", "carro", "pisoBajaCalidad", "paredBajaCalidad", "techoBajaCalidad",
             "hacinamiento", "mujerJH", "edadJH", "combustibleCocina", "nActivosPrioritarios",
             "pesoNac", "tallaNac", "pesoNacBajo", "tallaNacBajo", "ira0a59", "eda0a59")


var19a23 <-c("bord", "qi478", "qi478a", "caseid", "bidx", "id1", 
             "b4", "v001", "v005", "v012", "v022", "v024", "v025", "v149", "v190", "sregion",
             "s119", "s108n",
             "g1conv","g2aconv","g2bconv","g2cconv","g2abc","g3conv","g4conv","g345","r4_19_23m",
             "hv012","mieperho", "dominio", "area", "region", "hv026", "altitud", "quintil", "riqueza",
             "agua", "desague", "electricidad", "radio", "tv", "refrigerador",
             "bicicleta", "moto", "carro", "pisoBajaCalidad", "paredBajaCalidad", "techoBajaCalidad",
             "hacinamiento", "mujerJH", "edadJH", "combustibleCocina", "nActivosPrioritarios",
             "pesoNac", "tallaNac", "pesoNacBajo", "tallaNacBajo", "ira0a59", "eda0a59") 

var24a36 <-c("bord", "qi478", "qi478a", "caseid", "bidx", "id1", 
             "b4", "v001", "v005", "v012", "v022", "v024", "v025", "v149", "v190", "sregion",
             "s119", "s108n",
             "h1conv","h2conv","h3conv","h345","h4conv","h5conv","h6conv","h7conv","h567", "desInfJue" ,
             "h8aconv","h8bconv","h9conv", "h10conv","h11conv","h12conv","r4_24_36m",
             "hv012","mieperho", "dominio", "area", "region", "hv026", "altitud", "quintil", "riqueza",
             "agua", "desague", "electricidad", "radio", "tv", "refrigerador",
             "bicicleta", "moto", "carro", "pisoBajaCalidad", "paredBajaCalidad", "techoBajaCalidad",
             "hacinamiento", "mujerJH", "edadJH", "combustibleCocina", "nActivosPrioritarios",
             "pesoNac", "tallaNac", "pesoNacBajo", "tallaNacBajo", "ira0a59", "eda0a59") 

var37a54 <-c("bord", "qi478", "qi478a", "caseid", "bidx", "id1", 
             "b4", "v001", "v005", "v012", "v022", "v024", "v025", "v149", "v190", "sregion",
             "s119", "s108n",
             "desInfEmo", "i1conv", "i2conv","i3conv","i4aconv","i4bconv","i5conv","i6conv","i7conv","i8conv",
             "hv012","mieperho", "dominio", "area", "region", "hv026", "altitud", "quintil", "riqueza",
             "agua", "desague", "electricidad", "radio", "tv", "refrigerador",
             "bicicleta", "moto", "carro", "pisoBajaCalidad", "paredBajaCalidad", "techoBajaCalidad",
             "hacinamiento", "mujerJH", "edadJH", "combustibleCocina", "nActivosPrioritarios",
             "pesoNac", "tallaNac", "pesoNacBajo", "tallaNacBajo", "ira0a59", "eda0a59")

var55a71 <-c("bord", "qi478", "qi478a", "caseid", "bidx", "id1", 
             "b4", "v001", "v005", "v012", "v022", "v024", "v025", "v149", "v190", "sregion",
             "s119", "s108n",
             "desInfEmo", "j1conv","j2conv","j3conv","j4aconv", "j4bconv","j5conv","j6conv","j7conv","j8conv",
             "hv012","mieperho", "dominio", "area", "region", "hv026", "altitud", "quintil", "riqueza",
             "agua",  "desague", "electricidad", "radio", "tv", "refrigerador",
             "bicicleta", "moto", "carro", "pisoBajaCalidad", "paredBajaCalidad", "techoBajaCalidad",
             "hacinamiento", "mujerJH", "edadJH", "combustibleCocina", "nActivosPrioritarios",
             "pesoNac", "tallaNac", "pesoNacBajo", "tallaNacBajo", "ira0a59", "eda0a59")


baseDIT_9a12 <- baseNinosDIT %>% 
  filter(area == 1) %>% 
  filter(qi478 >= 9 & qi478 <= 12) %>% 
  select(all_of(var9a12)) %>% 
  mutate(sexo = case_when(b4 == 1 ~ 1,
                          TRUE ~ 0)) %>% 
  rename(edadMadre = v012)

baseDIT_13a18 <- baseNinosDIT %>% 
  filter(area == 1) %>% 
  filter(qi478 >= 13 & qi478 <= 18) %>% 
  select(all_of(var13a18)) %>% 
  mutate(sexo = case_when(b4 == 1 ~ 1,
                          TRUE ~ 0)) %>% 
  rename(edadMadre = v012)

baseDIT_19a23 <- baseNinosDIT %>% 
  filter(area == 1) %>% 
  filter(qi478 >= 19 & qi478 <= 23) %>% 
  select(all_of(var19a23)) %>% 
  mutate(sexo = case_when(b4 == 1 ~ 1,
                          TRUE ~ 0)) %>% 
  rename(edadMadre = v012)

baseDIT_24a36 <- baseNinosDIT %>% 
  filter(area == 1) %>% 
  filter(qi478 >= 24 & qi478 <= 36) %>% 
  select(all_of(var24a36)) %>% 
  mutate(sexo = case_when(b4 == 1 ~ 1,
                          TRUE ~ 0)) %>% 
  rename(edadMadre = v012)

baseDIT_37a54 <- baseNinosDIT %>% 
  filter(area == 1) %>% 
  filter(qi478 >= 37 & qi478 <= 54) %>% 
  select(all_of(var37a54)) %>% 
  mutate(sexo = case_when(b4 == 1 ~ 1,
                          TRUE ~ 0)) %>% 
  rename(edadMadre = v012)

baseDIT_55a71 <- baseNinosDIT %>% 
  filter(area == 1) %>% 
  filter(qi478 >= 55 & qi478 <= 71) %>% 
  select(all_of(var55a71)) %>% 
  mutate(sexo = case_when(b4 == 1 ~ 1,
                          TRUE ~ 0)) %>% 
  rename(edadMadre = v012)


# Conjunto de variables numéricas a nivel de hogar
varNumDIT_9a12 <- c("qi478", 
                    "sexo", "edadMadre", "v149", "s108n","e3conv", "e4conv", "e5conv", "e345", "r4_9_12m", "hv012",
                    "e7conv","e8conv", "e9conv","e10conv","r4_9_12m","r2","ira0a59", "eda0a59",
                    "mieperho", "hv026", "altitud", "riqueza",
                    "agua", "desague", "electricidad", "radio", "tv", "refrigerador",
                    "bicicleta", "moto", "carro",
                    "hacinamiento", "mujerJH", "edadJH", "combustibleCocina", "nActivosPrioritarios",
                    "pesoNac", "pesoNacBajo", 
                    "e1camina_solo","e2conv","e6conv")


varNumDIT_13a18 <-c("qi478",
                    "sexo", "edadMadre", "v149", "s108n","f2aconv", "f2bconv", "f2cconv", "f2dconv","f2econv","f3conv", "f4conv", "f5conv", "f345",
                    "f1camina_solo", "f6conv","r4_13_18m",
                    "mieperho", "hv026", "altitud", "riqueza",
                    "agua", "desague", "electricidad", "radio", "tv", "refrigerador",
                    "bicicleta", "moto", "carro", "pisoBajaCalidad", "paredBajaCalidad", "techoBajaCalidad",
                    "hacinamiento", "mujerJH", "edadJH", "combustibleCocina", "nActivosPrioritarios")

varNumDIT_19a23 <-c("qi478","qi478a", 
                    "sexo", "edadMadre", "v149", "s108n",
                    "g1conv","g2aconv","g2bconv","g2cconv","g2abc","g3conv","g4conv","g345","r4_19_23m",
                    "mieperho", "hv026", "altitud", "riqueza",
                    "agua", "desague", "electricidad", "radio", "tv", "refrigerador",
                    "bicicleta", "moto", "carro", "pisoBajaCalidad", "paredBajaCalidad", "techoBajaCalidad",
                    "hacinamiento", "mujerJH", "edadJH", "combustibleCocina", "nActivosPrioritarios")

varNumDIT_24a36 <-c("qi478","qi478a", 
                     "sexo", "edadMadre", "v149", "s108n",
                     "h1conv","h2conv","h3conv","h345","h4conv","h5conv","h6conv","h7conv","h567", "desInfJue" ,
                     "h8aconv","h8bconv","h9conv", "h10conv","h11conv","h12conv","r4_24_36m",
                     "mieperho", "hv026", "altitud", "riqueza",
                     "agua", "desague", "electricidad", "radio", "tv", "refrigerador",
                     "bicicleta", "moto", "carro", "pisoBajaCalidad", "paredBajaCalidad", "techoBajaCalidad",
                     "hacinamiento", "mujerJH", "edadJH", "combustibleCocina", "nActivosPrioritarios")

varNumDIT_37a54 <-c("qi478", 
                    "sexo","edadMadre","v149","s108n",
                    "desInfEmo", "i1conv", "i2conv","i3conv","i4aconv","i4bconv","i5conv","i6conv","i7conv","i8conv",
                    "mieperho", "hv026", "altitud", "riqueza",
                    "agua", "desague", "electricidad", "radio", "tv", "refrigerador",
                    "bicicleta", "moto", "carro", "pisoBajaCalidad", "paredBajaCalidad", "techoBajaCalidad",
                    "hacinamiento", "mujerJH", "edadJH", "combustibleCocina", "nActivosPrioritarios")

varNumDIT_55a71 <-c("qi478","qi478a", 
                    "sexo","edadMadre","v149","s108n",
                    "desInfEmo", "j1conv","j2conv","j3conv","j4aconv", "j4bconv","j5conv","j6conv","j7conv","j8conv",
                    "mieperho", "hv026", "altitud", "riqueza",
                    "agua", "desague", "electricidad", "radio", "tv", "refrigerador",
                    "bicicleta", "moto", "carro", "pisoBajaCalidad", "paredBajaCalidad", "techoBajaCalidad",
                    "hacinamiento", "mujerJH", "edadJH", "combustibleCocina", "nActivosPrioritarios")

# Bases de datos a nivel de Hogares
bPCA_9a12 <- baseDIT_9a12 %>% 
  subset(select = varNumDIT_9a12)

bPCA_13a18 <- baseDIT_13a18 %>% 
  subset(select = varNumDIT_13a18)

bPCA_19a23 <- baseDIT_19a23  %>% 
  subset(select = varNumDIT_19a23)

bPCA_24a36  <- baseDIT_24a36   %>% 
  subset(select = varNumDIT_24a36)

bPCA_37a54   <- baseDIT_37a54    %>% 
  subset(select = varNumDIT_37a54)

bPCA_55a71   <- baseDIT_55a71    %>% 
  subset(select = varNumDIT_55a71)

# Missings
listBases <- list(bPCA_9a12, bPCA_13a18, bPCA_19a23, bPCA_24a36, bPCA_37a54, bPCA_55a71)

for (i in seq_along(listBases)) {
  base <- listBases[[i]]
  missings_por_columna <- colSums(is.na(base))
  df_missings <- data.frame(
    columna = names(missings_por_columna),
    missings = missings_por_columna)
  
  df_missings <- df_missings %>% arrange(desc(missings))
  print(df_missings)
  listBases[[i]] <- base[complete.cases(base) & apply(base, 1, function(x) all(is.finite(x))), ]
}

# 3. PCA ----

### Definición de indicadores de interés:

ind9a12 <- c('e345',"r2",'anemiaNinos', 'desnCrOms')

#### PCA con variables relacionadas a apego
bPCA_9a12 <- listBases[[1]]

variables_con_desviacion_cero <- names(bPCA_9a12)[apply(bPCA_9a12, 2, sd) == 0]
variables_con_desviacion_cero
# variables_con_desviacion_cero: "ira0a59H"

##borramos variables con desviacion cero 

# Matriz de correlaciones
matrixcor_9a12 <- cor(bPCA_9a12)
matrixcor_9a12
matrixcor_9a12 <- as.data.frame(matrixcor_9a12)
write_xlsx(matrixcor_9a12, "matrixcor_9a12.xlsx")
#cor.plot(matrixcor)

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

View(loadings1t[abs(loadings1t$e345) > 0.2,])#con e345
# Análisis de alineamiento: solo componentes con peso de gasto > 0.2

# Ranking de las variables más importantes del PCA
pc1_loadings <- loadings1[, 1]
ranked_variables1 <- names(sort(abs(pc1_loadings), decreasing = TRUE))
ranked_variables1[1:21]

# Análisis visual de los 2 componentes principales
fviz_pca_var(pc1, geom = c("arrow"))
fviz_pca_var(pc1, geom = c("arrow", "text"))
fviz_pca_biplot(pc1, geom = c("point"))

# Realiza el PCA
pc1 <- prcomp(x = bPCA_9a12, scale = TRUE, center = TRUE)

# Abre una nueva ventana gráfica
dev.new()
# Realiza el biplot
biplot(pc1, scale = 0)
summary(pc1)

# Scree graph
fviz_eig(pc1)

# Coeficiones del PCA
loadings1 <- pc1$rotation
loadings1

# Para visualización
loadings1t <- as.data.frame(t(loadings1))

View(loadings1t[abs(loadings1t$e345) > 0.2,])#con e345
# Análisis de alineamiento: solo componentes con peso de gasto > 0.2

# Ranking de las variables más importantes del PCA
pc1_loadings <- loadings1[, 1]
ranked_variables1 <- names(sort(abs(pc1_loadings), decreasing = TRUE))
ranked_variables1[1:21]

# Análisis visual de los 2 componentes principales
fviz_pca_var(pc1, geom = c("arrow"))
fviz_pca_var(pc1, geom = c("arrow", "text"))
fviz_pca_biplot(pc1, geom = c("point"))

#########prueba grafico##########
# Realiza el PCA
pc1 <- prcomp(x = bPCA_9a12, scale = TRUE, center = TRUE)

# Abre una nueva ventana gráfica
dev.new()
# Realiza el biplot
biplot(pc1, scale = 0)
summary(pc1)

# Scree graph
fviz_eig(pc1)

# Coeficiones del PCA
loadings1 <- pc1$rotation
loadings1

# Para visualización
loadings1t <- as.data.frame(t(loadings1))

View(loadings1t[abs(loadings1t$r2) > 0.2,])#con r2

# Análisis de alineamiento: solo componentes con peso de gasto > 0.2

# Ranking de las variables más importantes del PCA
pc1_loadings <- loadings1[, 1]
ranked_variables1 <- names(sort(abs(pc1_loadings), decreasing = TRUE))
ranked_variables1[1:21]

# Análisis visual de los 2 componentes principales
fviz_pca_var(pc1, geom = c("arrow"))
fviz_pca_var(pc1, geom = c("arrow", "text"))
fviz_pca_biplot(pc1, geom = c("point"))

###### Poner resultados del PCA en una hoja de excel #####

archivo <- loadWorkbook("9a12.xlsx")

nombres_hojas <- names(archivo) 
print(nombres_hojas)

#nombres hojas
nuevos_nombres <- c("") 

#guardar nombres en las hojas y exportar
for(i in seq_along(nuevos_nombres)) {
  names(archivo)[i] <- nuevos_nombres[i]
}
saveWorkbook(archivo, "9a12.xlsx", overwrite = TRUE)





### De 13 a 18 meses 

variables_con_desviacion_cero <- names(bPCA_13a18)[apply(bPCA_13a18, 2, sd) == 0]
variables_con_desviacion_cero

# Matriz de correlaciones
matrixcor_13a18 <- cor(bPCA_13a18)
matrixcor_13a18
matrixcor_13a18 <- as.data.frame(matrixcor_13a18)
write_xlsx(matrixcor_13a18, "matrixcor_13a18.xlsx")


# PCA
pc2 <- prcomp(x=bPCA_13a18,scale=TRUE, center=TRUE)
biplot(pc2, scale = 0)
summary(pc2)

# Scree graph
fviz_eig(pc2)

# Coeficiones del PCA
loadings2<- pc2$rotation
loadings2

# Para visualización
loadings2t <- as.data.frame(t(loadings2))

View(loadings2t[abs(loadings1t$f345) > 0.2,]) #con f345

# Ranking de las variables más importantes del PCA
pc2_loadings <- loadings2[, 1]
ranked_variables2 <- names(sort(abs(pc2_loadings), decreasing = TRUE))
ranked_variables2[1:21]

# Análisis visual de los 2 componentes principales
fviz_pca_var(pc2, geom = c("arrow"))
fviz_pca_var(pc2, geom = c("arrow", "text"))
fviz_pca_biplot(pc2, geom = c("point"))

