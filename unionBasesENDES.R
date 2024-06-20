################################################################################
# Objetivo: Unión de los módulos de la ENDES para el periodo 2015-2022
# Proyecto:  GiZ Pobreza Urbana
# 
# Estructura:
# 0. Librerías y direcciones
# 1. Append de bases 2015-2022
# 2. Unión de bases a nivel de hogar y personas
# 3. Creación de variables de interés

################################################################################
# 0. Librerías y direcciones ----

bdEndes <- "C:/Users/Jennifer Prado/OneDrive - VIDENZA/Proyectos activos/1. PDB - DIT/2. Data/ENDES/0. Original"
#bdEndes <- "C:/Users/User/OneDrive - MIGRACIÓN VIDENZA/1. Proyectos/1. Proyectos actuales/23. Artículos PDB/1. PDB - DIT/2. Data/ENDES/0. Original"
bdTrabajo <- "C:/Users/Jennifer Prado/OneDrive - VIDENZA/Proyectos activos/1. PDB - DIT/2. Data/ENDES/1. Bases"
#bdTrabajo <- "C:/Users/User/OneDrive - MIGRACIÓN VIDENZA/1. Proyectos/1. Proyectos actuales/23. Artículos PDB/1. PDB - DIT/2. Data/ENDES/1. Bases"
library(dplyr)  
library(haven)

# 1. Append de bases 2019-2023 ----

data19_23 <- c("dit", "rec44" ,"rec43" , "rec42", "rec41", "rec21", "re758081", "re516171",
               "re223132", "programas_sociales_x_hogar", "rec82", "rec83", "rec84dv",
               "rec91", "rec94", "rec95", "rec0111", "rech0", "rech1", "rech4", 
               "rech5", "rech6", "rech23")

# Set working directory
setwd(bdEndes)

# Function
convert_labeled_to_numeric <- function(df) {
  for (col in names(df)) {
    if (inherits(df[[col]], "haven_labelled")) {
      df[[col]] <- as.numeric(df[[col]])
    }
  }
  return(df)
}

# Loop through each dataset and year
for (mod in data19_23) {
  for (x in 2019:2023) {
    # Read data
    data <- read_dta(paste0(mod, "_", x, ".dta"))
    colnames(data) <- tolower(colnames(data))
    data$id1 <- x
    data <- convert_labeled_to_numeric(data)
    
    # Store the combined data frame in the list
    if (x == 2019) {
      df <- data
    }
    else {
      dataNames <- colnames(data)
      dfNames <- colnames(df)
      common_cols <- intersect(dataNames, dfNames)
      df <- rbind(df[common_cols], data[common_cols])
      
    }
  }
  assign(paste0(mod), df)
  
}

rm(data, df)

# 2. Unión de bases a nivel de hogar y personas --------------------------------
# Base hogares----
baseHogaresENDES <- rech0 %>% 
  left_join(rech23, by = c("id1", "hhid")) %>% 
  rename(mieperho = hv013,
         nMujeres = hv010,
         nHombres = hv011,
         nNinos = hv014,
         estrato = hv022,
         dominio = hv023,
         region = hv024,
         area = hv025,
         altitud = hv040,
         quintil = hv270,
         riqueza = hv271,
         tvCable = sh61j,
         licuadora = sh61k,
         cocinaGas = sh61l,
         microndas = sh61n,
         lavadora = sh61o,
         computadora = sh61p,
         internet = sh61q,
         cortinas = sh76e,
         celular = hv243a) %>% 
  mutate(agua = case_when(hv201 == 11 | hv201 == 12 ~ 1,
                          hv201 == 99 ~ NA,
                          is.na(hv201) ~ NA,
                          TRUE ~ 0),
         tiempoAgua = case_when(hv204 == 996 | hv204 == 998 | hv204 == 999 ~ NA,
                                TRUE ~ hv204),
         desague = case_when(hv205 == 11 | hv205 == 12 ~ 1,
                             hv205 == 99 ~ NA,
                             is.na(hv205) ~ NA,
                             TRUE ~ 0),
         electricidad = case_when(hv206 == 1  ~ 1,
                                  hv206 == 9 ~ NA,
                                  is.na(hv206) ~ NA,
                                  TRUE ~ 0),
         radio = case_when(hv207 == 1  ~ 1,
                           hv207 == 9 ~ NA,
                           is.na(hv207) ~ NA,
                           TRUE ~ 0),
         tv = case_when(hv208 == 1  ~ 1,
                        hv208 == 9 ~ NA,
                        is.na(hv208) ~ NA,
                        TRUE ~ 0),
         refrigerador = case_when(hv209 == 1  ~ 1,
                                  hv209 == 9 ~ NA,
                                  is.na(hv209) ~ NA,
                                  TRUE ~ 0),
         bicicleta = case_when(hv210 == 1  ~ 1,
                               hv210 == 9 ~ NA,
                               is.na(hv210) ~ NA,
                               TRUE ~ 0),
         moto = case_when(hv211 == 1  ~ 1,
                          hv211 == 9 ~ NA,
                          is.na(hv211) ~ NA,
                          TRUE ~ 0),
         carro = case_when(hv212 == 1  ~ 1,
                           hv212 == 9 ~ NA,
                           is.na(hv212) ~ NA,
                           TRUE ~ 0),
         pisoBajaCalidad = case_when(hv213 == 11 | hv213 == 21 | hv213 == 96 ~ 1,
                                     is.na(hv213) ~ NA,
                                       TRUE ~ 0),
         paredBajaCalidad = case_when(hv214 < 29 | hv214 == 41 | hv214 == 96 ~ 1,
                                      is.na(hv214) ~ NA,
                                         TRUE ~ 0),
         techoBajaCalidad = case_when(hv215 < 29 | hv215 == 41 | hv215 == 96 ~ 1,
                                      is.na(hv215) ~ NA,
                                         TRUE ~ 0),
         hacinamiento = case_when(hv216 == 0 ~ 0,
                                  TRUE ~ mieperho / hv216),
         mujerJH = case_when(hv219 == 2 ~ 1,
                             is.na(hv219) ~ NA, 
                             TRUE ~ 0),
         edadJH = case_when(hv220 == 98 | hv220 == 99 ~ NA,
                            is.na(hv220) ~ NA, 
                            TRUE ~ hv220),
         combustibleCocina = case_when(hv226 >= 6 & hv226 < 95 ~ 1 ,
                                       is.na(hv226) ~ NA,
                                       TRUE ~ 0))

baseHogaresENDES <- baseHogaresENDES %>%
  mutate(nActivosPrioritarios = rowSums(select(., computadora, radio, lavadora, licuadora, microndas, refrigerador, tv)))

# Base personas----
basePersonasENDES <- rech4 %>% 
  rename(hvidx = idxh4) %>% 
  left_join(rech1, by = c("id1", "hhid", "hvidx")) %>% 
  rename(estadoCivil = hv115,
         estudia = hv110) %>% 
  mutate(mujer = case_when(hv104 == 2 ~ 1,
                           is.na(hv104) ~ NA,
                           TRUE ~ 0),
         edad = case_when(hv105 == 98 | hv105 == 99 | is.na(hv105) ~ NA,
                          TRUE ~ hv105),
         educ = case_when(hv109 == 8 | is.na(hv109) ~ NA,
                          TRUE ~ hv109),
         aniosEduc = case_when(hv108 == 97 | hv108 == 98 | is.na(hv108) ~ NA,
                              TRUE ~ hv108),
         noEstudia = case_when(hv129 == 0 | hv129 == 4 | hv129 == 5 ~ 1,
                               is.na(hv129) | hv129 == 9 ~ NA,
                               TRUE ~ 0),
         algunSeguro = case_when(sh11a ==1 | sh11b ==1 | sh11c ==1 | sh11d ==1 | sh11e ~ 1,
                                 TRUE ~ 0),
         pea = case_when(sh13 == 1 | sh13 == 2 | sh13 == 3 | sh13 == 4 | sh13 == 5 ~ 1,
                         TRUE ~ 0)
         )

# Base Mujeres ----

baseMujeresENDES <- rec91 %>%
  left_join(rec0111, by = c("id1", "caseid")) %>%
  left_join(re223132, by = c("id1", "caseid")) %>%
  left_join(rec42, by = c("id1", "caseid")) %>%
  left_join(re516171, by = c("id1", "caseid")) %>%
  left_join(rec84dv, by = c("id1", "caseid")) %>% 
  left_join(re758081, by = c("id1", "caseid"))

# Características de las mujeres
baseMujeresENDES <- baseMujeresENDES%>%
  mutate(nivEducMujer = case_when(v012 > 14 ~ v149,
                                  TRUE ~ NA),
         alfabetismoMujer = v155)

# Variables de violencia
## Violencia física ejercida por el esposo
baseMujeresENDES <- baseMujeresENDES %>%
  mutate(violenciaEsposoFEmpujo = case_when(d105a > 0 & d105a < 3 ~ 1,
                                           v044 == 0 | v502 == 0 ~ NA,
                                           TRUE ~ 0),
         violenciaEsposoFAbofeteo = case_when(d105b > 0 & d105b < 3 ~ 1,
                                             v044 == 0 | v502 == 0 ~ NA,
                                             TRUE ~ 0),
         violenciaEsposoFGolpe = case_when(d105c > 0 & d105c < 3 ~ 1,
                                          v044 == 0 | v502 == 0 ~ NA,
                                          TRUE ~ 0),
         violenciaEsposoFPatada = case_when(d105d > 0 & d105d < 3 ~ 1,
                                           v044 == 0 | v502 == 0 ~ NA,
                                           TRUE ~ 0),
         violenciaEsposoFAhorco = case_when(d105e > 0 & d105e < 3 ~ 1,
                                           v044 == 0 | v502 == 0 ~ NA,
                                           TRUE ~ 0),
         violenciaEsposoFArma1 = case_when(d105f > 0 & d105f < 3 ~ 1,
                                          v044 == 0 | v502 == 0 ~ NA,
                                          TRUE ~ 0),
         violenciaEsposoFArma2 = case_when(d105g > 0 & d105g < 3 ~ 1,
                                          v044 == 0 | v502 == 0 ~ NA,
                                          TRUE ~ 0),
         violenciaEsposoFBrazo = case_when(d105j > 0 & d105j < 3 ~ 1,
                                          v044 == 0 | v502 == 0 ~ NA,
                                          TRUE ~ 0)) %>% 
  mutate(violenciaEsposoFisica = case_when(v044 == 0 | v502 == 0  ~ 0,
                                           rowSums(select(., starts_with("violenciaEsposoF")), na.rm=TRUE) >0  ~ 1,
                                           TRUE ~ NA))

## Violencia sexual ejercida por el esposo
baseMujeresENDES <- baseMujeresENDES %>%
  mutate(violenciaEsposoSSex = case_when(d105h > 0 & d105h < 3 ~ 1,
                                        v044 == 0 | v502 == 0 ~ NA,
                                        TRUE ~ 0),
         violenciaEsposoSActSex = case_when(d105i > 0 & d105i < 3 ~ 1,
                                           v044 == 0 | v502 == 0 ~ NA,
                                           TRUE ~ 0)) %>% 
  mutate(violenciaEsposoSexual = case_when(v044 == 0 | v502 == 0  ~ 0,
                                           rowSums(select(., starts_with("violenciaEsposoS")), na.rm=TRUE) >0  ~ 1,
                                           TRUE ~ NA))

## Violencia psicológica y/o verbal ejercida por el esposo o compañero
baseMujeresENDES <- baseMujeresENDES %>%
  mutate(violenciaESposoPHumil = case_when(d103a > 0 & d103a < 3 ~ 1,
                                          v044 == 0 | v502 == 0 ~ NA,
                                          TRUE ~ 0),
         violenciaEsposoPCelo = case_when(v044 == 0 | v502 == 0 ~ 0,
                                         d101a == 1 ~ 1,
                                         TRUE ~ NA),
         violenciaEsposoPAcus = case_when(v044 == 0 | v502 == 0 ~ 0,
                                         d101b == 1 ~ 1,
                                         TRUE ~ NA),
         violenciaEsposoPContacto = case_when(v044 == 0 | v502 == 0 ~ 0,
                                             d101c == 1 | d101d == 1 ~ 1,
                                             TRUE ~ NA),
         violenciaEsposoPLugar = case_when(v044 == 0 | v502 == 0 ~ 0,
                                          d101e == 1 ~ 1,
                                          TRUE ~ NA),
         violenciaEsposoPDinero = case_when(v044 == 0 | v502 == 0 ~ 0,
                                           d101f == 1 ~ 1,
                                           TRUE ~ NA),
         violenciaEsposoPAmenaza1 = case_when(v044 == 0 | v502 == 0 ~ 0,
                                             d103b > 0 & d103b < 3 ~ 1,
                                             TRUE ~ NA),
         violenciaEsposoPAmenaza2 = case_when(v044 == 0 | v502 == 0 ~ 0,
                                             d103d > 0 & d103d < 3 ~ 1,
                                             TRUE ~ NA)) %>% 
  mutate(violenciaEsposoPsico = case_when(v044 == 0 | v502 == 0  ~ 0,
                                          rowSums(select(., starts_with("violenciaEsposoP")), na.rm=TRUE) >0  ~ 1,
                                          TRUE ~ NA))

## Cualquier tipo de violencia
baseMujeresENDES <- baseMujeresENDES %>%
  mutate(violenciaEsposo = case_when(v044 == 0 | v502 == 0  ~ 0,
                                    rowSums(select(., c('violenciaEsposoPsico','violenciaEsposoFisica', 'violenciaEsposoSexual')), na.rm=TRUE) >0  ~ 1,
                                    TRUE ~ NA))

# Uso actual de métodos anticonceptivos
baseMujeresENDES <- baseMujeresENDES %>%
  mutate(usaAnticonceptivo = case_when(v313 == 0 ~ 0,
                                       (v313 == 1 | v313 == 2 | v313 == 3) ~ 1,
                                       TRUE ~ NA),
         tipoAnticonceptivo = case_when(v313 == 0 ~ 0,
                                        (v313 == 1 | v313 == 2) ~ 1,
                                        v313 == 3 ~ 2,
                                        TRUE ~ NA))

#Anemia en mujeres de 15 a 49 años
baseMujeresENDES <- baseMujeresENDES %>%
  mutate(anemiaMujer = case_when(v457 > 3 & v012 > 14 ~ 0,
                                 v457 < 4 & v012 > 14 ~ 1,
                                 TRUE ~ NA),
         nivelAnemiaMujer = case_when(v457 <=3 & v012 > 14 ~ v457,
                                      TRUE ~ NA),
         anemiaSevMujer = case_when(v457 == 1 & v012 > 14 ~ 1,
                                    v457 > 1 & v457 <9 & v012 > 14 ~ 0,
                                    TRUE ~ NA))

baseMujeresENDES <- baseMujeresENDES %>% 
  mutate(tuvoETS = case_when(v763a == 1 ~ 1,
                             is.na(v763a) ~ NA,
                             TRUE ~ 0))
#Cobertura de seguro de salud para mujeres de 15 a 49 años
baseMujeresENDES <- baseMujeresENDES %>%
  mutate(seguroEssalud = case_when(v481e == 1 & v012 > 14 ~ 1,
                                 TRUE ~ 0),
         seguroSis = case_when(v481g == 1 & v012 > 14 ~ 1,
                                 TRUE ~ 0),
         seguroFap = case_when(v481f == 1 & v012 > 14 ~ 1,
                                 TRUE ~ 0),
         seguroEps = case_when(v481h == 1 & v012 > 14 ~ 1,
                                 TRUE ~ 0),
         seguroPriv = case_when(v481d ==1 & v012 > 14 ~ 1,
                                TRUE ~ 0),
         algunSeguro = case_when(rowSums(select(baseMujeresENDES, starts_with("v481")), na.rm = TRUE) > 1 & v012 > 14 ~ 1,
                                 TRUE ~ 0))

#Llevamos los indicadores mujeres a nivel hogar
baseMujeres1ENDES <- baseMujeresENDES %>%
  mutate(hhid = substr(caseid, 1, 15)) %>%
  select(id1, hhid, violenciaEsposo, anemiaMujer, anemiaSevMujer, tuvoETS, algunSeguro) %>%
  group_by(id1, hhid, .groups = 'drop') %>%
  summarize(violenciaEsposoH = ifelse(all(is.na(violenciaEsposo)), NA, max(violenciaEsposo, na.rm = TRUE)),
            anemiaMujerH = sum(anemiaMujer, na.rm = TRUE),
            anemiaSevMujerH = sum(anemiaSevMujer, na.rm = TRUE),
            tuvoETSH = sum(tuvoETS, na.rm = TRUE),
            algunSeguroH = sum(algunSeguro, na.rm = TRUE)) %>%
  select(- .groups)

baseHogaresENDES <- baseHogaresENDES %>%
  left_join(baseMujeres1ENDES, by = c("id1", "hhid")) %>%
  mutate(anemiaMujerH = anemiaMujerH / nMujeres,
         anemiaSevMujerH = anemiaSevMujerH / nMujeres,
         tuvoETSH = tuvoETSH / nMujeres,
         algunSeguroH = algunSeguroH / nMujeres)

rm(baseMujeres1ENDES)

# Base Niños----
baseNinosENDES <- rech6 %>% 
  rename(hvidx = hc0) %>% 
  left_join(basePersonasENDES, by = c("id1", "hhid", "hvidx")) %>% 
  mutate(edad = hc1,
         peso = case_when(is.na(hc2) ~ NA,
                          TRUE ~ hc2),
         talla = case_when(is.na(hc3) ~ NA,
                           TRUE ~ hc3),
         tallaEdad = case_when(is.na(hc5) ~ NA,
                               TRUE ~ hc5),
         pesoEdad = case_when(is.na(hc8) ~ NA,
                              TRUE ~ hc8),
         pesoTalla = case_when(is.na(hc11) ~ NA,
                               TRUE ~ hc11),
         mujer = case_when(hc27 == 2 ~ 1,
                           hc27 == 1 ~ 0),
         anemiaNinos = case_when(hc57 < 4 & edad <= 60 ~ 1,
                                 is.na(hc57) ~ NA,
                                 TRUE ~ 0),
         anemiaNivelNinos = case_when(hc57 == 1 ~ 4,
                                      hc57 == 2 ~ 3,
                                      hc57 == 3 ~ 2,
                                      hc57 == 4 ~ 1,
                                      is.na(hc57) ~ NA,
                                      TRUE ~ 0),
         anemiaSevNinos = case_when(anemiaNivelNinos == 4 ~ 1,
                                    anemiaNivelNinos < 4 ~ 0,
                                    TRUE ~ NA),
         educMadre = case_when(hc61 == 8 | hc61 == 9 ~ NA,
                               is.na(hc61) ~ NA,
                               TRUE ~ hc61),
         desnCrOms = case_when((hc70 < -200 & hv103 == 1) ~ 1,
                               (hc70 >= -200 & hc70 < 601 & hv103 == 1) ~ 0,
                               TRUE ~ NA),
         desnCrSev = case_when((hc70 < -300  & hv103 == 1) ~ 1,
                               (hc70 >= -300 & hc70 < 601 & hv103 == 1) ~ 0,
                               TRUE ~ NA))

baseNinos1ENDES <- baseNinosENDES %>%
  select(id1, hhid, mujer, desnCrOms, desnCrSev, anemiaNinos, anemiaSevNinos) %>%
  group_by(id1, hhid, .groups = 'drop')%>%
  summarize(nNinas = sum(mujer, na.rm = TRUE),
            desnCrOmsH = sum(desnCrOms, na.rm = TRUE),
            desnCrSevH = sum(desnCrSev, na.rm = TRUE),
            anemiaNinosH = sum(anemiaNinos, na.rm = TRUE),
            anemiaSevNinosH = sum(anemiaSevNinos, na.rm = TRUE)) %>%
  select(- .groups)

baseHogaresENDES <- baseHogaresENDES %>%
  left_join(baseNinos1ENDES, by = c("id1", "hhid")) %>%
  mutate(nNinas = nNinas / nNinos,
         desnCrOmsH = desnCrOmsH / nNinos,
         desnCrSevH = desnCrSevH / nNinos,
         anemiaNinosH = anemiaNinosH / nNinos,
         anemiaSevNinosH = anemiaSevNinosH / nNinos)

rm(baseNinos1ENDES)


#Desarrollo infantil --- Resultados DIT ---
baseNinosAuxENDES <- dit %>%
  left_join(rec21 %>% select("id1", "caseid", "bidx", "b4"), by = c("id1", "caseid", "bidx")) %>%
  left_join(rec0111 %>% select("id1", "caseid", "v001", "v005", "v012", "v022", "v024", "v025", "v149", "v190"), by = c("id1", "caseid")) %>%
  left_join(rec91 %>% select("id1", "caseid", "sregion", "s119", "s108n"), by = c("id1", "caseid")) %>%
  mutate(e3conv = case_when(qi478e3 == 1 ~ 1,
                            qi478e3 == 2 ~ 0,
                            TRUE ~ NA),
         e4conv = case_when(qi478e4 == 1 ~ 1,
                            qi478e4 == 2 ~ 0,
                            TRUE ~ NA),
         e5conv = case_when(qi478e5 == 1 ~ 1,
                            qi478e5 == 2 ~ 0,
                            TRUE ~ NA),
         e7conv = case_when(qi478e7 == 1 ~ 1,
                            qi478e7 == 2 ~ 0,
                            TRUE ~ NA),
         e8conv = case_when(qi478e8 == 1 ~ 1,
                            qi478e8 == 2 ~ 0,
                            TRUE ~ NA),
         e9conv = case_when(qi478e9 == 1 ~ 1,
                            qi478e9 == 2 ~ 0,
                            TRUE ~ NA),
         e10conv = case_when(qi478e10 == 1 | qi478e10 == 3 | qi478e10 == 4 ~ 1,
                             qi478e10 == 2 | qi478e10 == 5 ~ 0,
                             TRUE ~ NA),
         e345 = case_when(qi478a == 0 & (qi478 >= 9 & qi478 <= 12) ~ e3conv + e4conv + e5conv,
                          TRUE ~ NA),
         r2 = case_when(qi478a == 0 & (qi478 >= 9 & qi478 <= 12) ~ e7conv + e8conv + e9conv,
                        TRUE ~ NA),
         r4_9_12m = case_when(e345 < 3 ~ 0,
                              e345 == 3 ~ 1,
                              TRUE ~ NA),
         f3conv = case_when(qi478f3 == 1 ~ 1,
                            qi478f3 == 2 ~ 0,
                            TRUE ~ NA),
         f4conv = case_when(qi478f4 == 1 ~ 1,
                            qi478f4 == 2 ~ 0,
                            TRUE ~ NA),
         f5conv = case_when(qi478f5 == 1 ~ 1,
                            qi478f5 == 2 ~ 0,
                            TRUE ~ NA),
         f345 = case_when(qi478a == 0 & (qi478 >= 13 & qi478 <= 18) ~ f3conv + f4conv + f5conv,
                          TRUE ~ NA),
         r4_13_18m = case_when(f345 < 3 ~ 0,
                               f345 == 3 ~ 1,
                               TRUE ~ NA),
         g1conv = case_when(qi478g1 == 1 ~ 1,
                            qi478g1 == 2 ~ 0,
                            TRUE ~ NA),
         g2aconv = case_when(qi478g2_a == 1 ~ 1,
                             qi478g2_a == 2 ~ 0,
                             TRUE ~ NA),
         g2bconv = case_when(qi478g2_b == 1 ~ 1,
                             qi478g2_b == 2 ~ 0,
                             TRUE ~ NA),
         g2cconv = case_when(qi478g2_c == 1 ~ 1,
                             qi478g2_c == 2 ~ 0,
                             TRUE ~ NA),
         g3conv = case_when(qi478g3 == 1 ~ 1,
                            qi478g3 == 2 ~ 0,
                            TRUE ~ NA),
         g2abc = (g2aconv + g2bconv + g2cconv) / 3,
         g345 = case_when(qi478a == 0 & (qi478 >= 19 & qi478 <= 23) ~ g1conv + g2abc + g3conv,
                          TRUE ~ NA),
         r4_19_23m = case_when(g345 < 3 ~ 0,
                               g345 == 3 ~ 1,
                               TRUE ~ NA),
         h1conv = case_when(qi478h1 == 1 ~ 1,
                            qi478h1 == 2 ~ 0,
                            TRUE ~ NA),
         h2conv = case_when(qi478h2 == 1 ~ 1,
                            qi478h2 == 2 ~ 0,
                            TRUE ~ NA),
         h3conv = case_when(qi478h3 == 1 ~ 1,
                            qi478h3 == 2 ~ 0,
                            TRUE ~ NA),
         h345 = case_when(qi478a == 0 & (qi478 >= 24 & qi478 <= 36) ~ h1conv + h2conv + h3conv,
                          TRUE ~ NA),
         r4_24_36m = case_when(h345 < 3 ~ 0,
                               h345 == 3 ~ 1,
                               TRUE ~ NA),
         f2aconv = case_when(qi478f2_a == 1 ~ 1,
                             qi478f2_a == 2 ~ 0,
                             TRUE ~ NA),
         f2bconv = case_when(qi478f2_b == 1 ~ 1,
                             qi478f2_b == 2 ~ 0,
                             TRUE ~ NA),
         f2cconv = case_when(qi478f2_c == 1 ~ 1,
                             qi478f2_c == 2 ~ 0,
                             TRUE ~ NA),
         f2dconv = case_when(qi478f2_d == 1 ~ 1,
                             qi478f2_d == 2 ~ 0,
                             TRUE ~ NA),
         f2econv = case_when(qi478f2_e == 1 ~ 1,
                             qi478f2_e == 2 ~ 0,
                             TRUE ~ NA),
         e6f6conv = case_when(qi478e6 == 1 | qi478f6 == 1 ~ 1,
                              qi478e6 == 2 & qi478f6 == 2 ~ 0,
                              TRUE ~ NA),
         g4h4conv = case_when(qi478h4 == 1 | qi478g4 == 1 ~ 1,
                              qi478h4 == 2 | qi478g4 == 2 ~ 0,
                              TRUE ~ NA)) %>% 
  mutate(desInfCom = rowSums(select(., starts_with("r4_")), na.rm = TRUE),
         indEntorno2 = rowSums(select(., starts_with("f2")), na.rm = TRUE),
         r2 = case_when(qi478a == 0 & (qi478 >= 9 & qi478 <= 12) ~ e7conv + e8conv + e9conv,
                          TRUE ~ NA))


# Porcentaje de niñas y niños entre 24 y 71 meses de edad que tienen en casa materiales de juego estructurados y no estructurados
# H8I4J4A H8I4J4B

# Porcentaje de niñas y niños entre 24 y 71 meses de edad cuya madre no ejerce conductas de castigo físico hacia su hija/o
# H12I8J8

#Desarrollo infantil - Regulación de emociones
baseNinosAuxENDES <- baseNinosAuxENDES %>%
  mutate(desInfEmo = case_when(qi478a == 0 & (qi478 >= 24 & qi478 <= 36) ~ case_when(qi478h9 == 1 ~ 0,
                                                                                     qi478h9 == 2 ~ 1,
                                                                                     qi478h10 == 1 ~ 0,
                                                                                     qi478h10 == 2 ~ 1,
                                                                                     qi478h10 == 3 ~ 0,
                                                                                     qi478h11 == 1 ~ 0,
                                                                                     qi478h11 == 2 ~ 1,
                                                                                     TRUE ~ NA),
                               qi478a == 0 & (qi478 >= 37 & qi478 <= 54) ~ case_when(qi478i5 == 1 ~ 0,
                                                                                     qi478i5 == 2 ~ 1,
                                                                                     qi478i6 == 1 ~ 0,
                                                                                     qi478i6 == 2 ~ 1,
                                                                                     qi478i6 == 3 ~ 0,
                                                                                     qi478i7 == 1 ~ 0,
                                                                                     qi478i7 == 2 ~ 1,
                                                                                     TRUE ~ NA),
                               qi478a == 0 & (qi478 >= 55 & qi478 <= 71) ~ case_when(qi478j5 == 1 ~ 0,
                                                                                     qi478j5 == 2 ~ 1,
                                                                                     qi478j6 == 1 ~ 0,
                                                                                     qi478j6 == 2 ~ 1,
                                                                                     qi478j6 == 3 ~ 0,
                                                                                     qi478j7 == 1 ~ 0,
                                                                                     qi478j7 == 2 ~ 1,
                                                                                     TRUE ~ NA),
                               TRUE ~ NA))

#Desarrollo infantil - Juegan y dibujan
baseNinosAuxENDES <- baseNinosAuxENDES %>%
  mutate(h5conv = case_when(qi478h5 == 1 ~ 1,
                            qi478h5 == 2 ~ 0,
                            TRUE ~ NA),
         h6conv = case_when(qi478h6 == 1 ~ 1,
                            qi478h6 == 2 ~ 0,
                            TRUE ~ NA),
         h7conv = case_when(qi478h7 == 1 ~ 1,
                            qi478h7 == 2 ~ 0,
                            TRUE ~ NA),
         h567 = case_when(qi478a == 0 & (qi478 >= 24 & qi478 <= 36) ~ h5conv + h6conv + h7conv,
                          TRUE ~ NA),
         desInfJue = case_when(h567 %in% 0:2 ~ 0,
                        h567 == 3 ~ 1,
                        TRUE ~ NA))

varList <- c("id1", "hhid", "hv012", "mieperho", "dominio", "area", "region", "hv026", "altitud", "quintil", "riqueza", "agua", "tiempoAgua", 
             "desague", "electricidad", "radio", "tv", "refrigerador", "bicicleta", "moto", "carro", "pisoBajaCalidad", "paredBajaCalidad",
             "techoBajaCalidad", "hacinamiento", "mujerJH", "edadJH", "combustibleCocina", "nActivosPrioritarios")

baseNinosAuxENDES <- baseNinosAuxENDES %>% 
  mutate(hhid = substr(caseid, 1, 15)) %>% 
  left_join(baseHogaresENDES %>% select(all_of(varList)), by = c("id1", "hhid"))

#Talla y peso al nacer
baseNinosAux1ENDES <- rec0111 %>%
  left_join(rec42, by = c("id1", "caseid")) %>%
  left_join(rec91, by = c("id1", "caseid"))

rec43$midx <- rec43$hidx

baseNinosAux2ENDES <- rec21 %>%
  rename(midx = bidx) %>%
  left_join(rec41, by = c("id1", "caseid", "midx")) %>%
  left_join(rec43, by = c("id1", "caseid", "midx"))

baseNinosAux1ENDES <- baseNinosAux1ENDES %>%
  left_join(baseNinosAux2ENDES, by = c("id1", "caseid")) %>%
  mutate(edadM = v008 - b3,
         pesoNac = case_when(m19 < 2500 & v012 > 14 ~ 1,
                             (m19 > 2499 & m19 <= 8000) & v012 > 14 ~ 2,
                             m19 == 9996 & v012 > 14 ~ 3,
                             m19 == 9998 & v012 > 14 ~ 4,
                             TRUE ~ NA),
         tallaNac = case_when(m18 == 5 ~ 1,
                              m18 == 4 ~ 2,
                              m18 == 1 | m18 == 2 ~ 3,
                              m18 == 8 | m18 == 9 ~ 4,
                              TRUE ~ NA),
         pesoNacBajo = case_when(pesoNac == 1 ~ 1,
                                 pesoNac > 1 ~0,
                                 TRUE ~ NA),
         tallaNacBajo = case_when(tallaNac == 1 ~ 1,
                                  tallaNac > 1 ~ 0,
                                  TRUE ~ NA),
         ira0a59 = case_when((b5 == 1 & edadM < 60) ~ 0,
                             (b5 == 1 & edadM < 60 & h31b == 1) ~ 1,
                             TRUE ~ NA),
         eda0a59 = case_when((h11 == 0 | h11 == 8) & b5 == 1 & edadM < 60 & v012 > 14 ~ 0,
                             h11 == 2 & b5 == 1 & edadM < 60 & v012 > 14 ~ 1,
                             TRUE ~ NA))

#Variables DIT restantes de 9 - 12 meses 
baseNinosAuxENDES <- baseNinosAuxENDES %>%
  mutate(e1camina_solo = case_when(qi478e1 == 5 | qi478e1 == 6 ~ 1, #SE PONE DE PIE SIN AGARRARSE DE NADA y CAMINA SOLA /O CON SOLTURA
                                   qi478e1 == 1 | qi478e1 == 2| qi478e1 == 3 | qi478e1 == 4 ~ 0,
                              TRUE ~ NA),
         e2conv = case_when(qi478e2 == 1 ~ 1, #ESPACIO SIN OBJETOS 
                            qi478e2 == 2 ~ 0,
                            TRUE ~ NA),
         e6conv = case_when(qi478e6 == 1 ~ 1,
                            qi478e6 == 2 ~ 0,
                            TRUE ~ NA))

#Variables DIT restantes de 13 - 18 meses
baseNinosAuxENDES <- baseNinosAuxENDES %>%
  mutate(f1camina_solo = case_when(qi478f1 == 5 | qi478f1 == 6 ~ 1,
                                   qi478f1 == 1 | qi478f1 == 2| qi478f1 == 3 | qi478f1 == 4 ~ 0,
                                   TRUE ~ NA),
         f6conv = case_when(qi478f6 == 1 ~ 1,
                            qi478f6 == 2 ~ 0,
                            TRUE ~ NA))

#Variables DIT restantes de 19 - 23 meses
baseNinosAuxENDES <- baseNinosAuxENDES %>%
  mutate(g4conv =  case_when(qi478g4 == 1  ~ 1,
                             qi478g4 == 2 ~ 0,
                             TRUE ~ NA)) #participa” en las conversaciones con adultos

#Variables DIT restantes de 24 - 36 meses 
baseNinosAuxENDES <- baseNinosAuxENDES %>%
  mutate(h4conv = case_when(qi478h4 == 1 ~ 1,
                             qi478h4 == 2 ~ 0,
                             TRUE ~ NA),
         h8aconv = case_when(qi478h8_a == 1 ~ 1,
                              qi478h8_a == 2 ~ 0,
                              TRUE ~ NA),
         h8bconv = case_when(qi478h8_b == 1 ~ 1,
                             qi478h8_b == 2 ~ 0,
                             TRUE ~ NA),
         h9conv = case_when(qi478h9 == 1 ~ 1, 
                            qi478h9 == 2 ~ 0, 
                            TRUE ~ NA),
         h10conv = case_when(qi478h10 == 1 ~ 1,
                             qi478h10 == 2 ~ 2,
                             qi478h10 == 3 ~ 3,
                             TRUE ~ NA),
         h11conv = case_when(qi478h11 == 1 ~ 1, 
                             qi478h11 == 2 ~ 2,
                             TRUE ~ NA),
         h12conv = case_when(qi478h12 == 1 ~ 0,#ninguna vez le ha dado un palmazo, le ha jalado de los cabellos o la oreja o le ha golpeado con un objeto en cualquier parte de su cuerpo
                             qi478h12 == 2 | qi478h12 == 3 | qi478h12 == 4 ~ 1, #de 1 a más de 6 veces
                             TRUE ~ NA))


#Variables DIT restantes de 37 - 54 meses  
baseNinosAuxENDES <- baseNinosAuxENDES %>%
  mutate(i1conv = case_when(qi478i1 == 1 | qi478i1 == 2 ~ 0,
                            qi478i1 == 3 | qi478i1 == 4 | qi478i1 == 5 ~ 3,
                            TRUE ~ NA),
         i2conv = case_when(qi478i2 == 1 ~ 1,
                            qi478i2 == 2 ~ 0,
                            TRUE ~ NA),
         i3conv = case_when(qi478i3 == 1 ~ 1,
                            qi478i3 == 2 ~ 0,
                            TRUE ~ NA),
         i4aconv = case_when(qi478i4_a == 1 ~ 1,
                             qi478i4_a == 2 ~ 0,
                             TRUE ~ NA),
         i4bconv = case_when(qi478i4_b == 1 ~ 1,
                             qi478i4_b == 2 ~ 0,
                             TRUE ~ NA),
         i5conv = case_when(qi478i5 == 1 ~ 1,
                            qi478i5 == 2 ~ 0,
                             TRUE ~ NA),
         i6conv = case_when(qi478i6 == 1 |qi478i6 == 2 ~ 1, 
                            qi478i6 == 3 ~ 0,
                            TRUE ~ NA),
         i7conv = case_when(qi478i7 == 1 ~ 1,
                            qi478i7 == 2 ~ 0,
                            TRUE ~ NA),
         i8conv = case_when(qi478i8 == 1 ~ 0,#ninguna vez le ha dado un palmazo, le ha jalado de los cabellos o la oreja o le ha golpeado con un objeto en cualquier parte de su cuerpo
                            qi478i8 == 2 | qi478i8 == 3 | qi478i8 == 4 ~ 1, #de 1 a más de 6 veces
                            TRUE ~ NA))
         
#Variables DIT restantes de 55 - 71 meses
baseNinosAuxENDES <- baseNinosAuxENDES %>%
  mutate(j1conv = case_when(qi478j1 == 1 ~ 1,
                            qi478j1 == 2 ~ 0,
                            TRUE ~ NA),
         j2conv = case_when(qi478j2 == 1 ~ 1,
                            qi478j2 == 2 ~ 0,
                            TRUE ~ NA),
         j3conv = case_when(qi478j3 == 1 ~ 1,
                            qi478j3 == 2 ~ 0,
                            TRUE ~ NA),
         j4aconv = case_when(qi478j4_a == 1 ~ 1,
                             qi478j4_a == 2 ~ 0,
                             TRUE ~ NA),
         j4bconv = case_when(qi478j4_b == 1 ~ 1,
                             qi478j4_b == 2 ~ 0,
                             TRUE ~ NA), 
         j5conv = case_when(qi478j5 == 1 ~ 1,
                            qi478j5 == 2 ~ 0,
                            TRUE ~ NA),
         j6conv = case_when(qi478j6 == 1 | qi478j6 == 2 ~ 1, 
                            qi478j6 == 3 ~ 0,
                            TRUE ~ NA),
         j7conv = case_when(qi478j7 == 1 ~ 1,
                            qi478j7 == 2 ~ 0,
                            TRUE ~ NA),
         j8conv = case_when(qi478j8 == 1 ~ 0,#ninguna vez le ha dado un palmazo, le ha jalado de los cabellos o la oreja o le ha golpeado con un objeto en cualquier parte de su cuerpo
                            qi478j8 == 2 | qi478j8 == 3 | qi478j8 == 4 ~ 1, #de 1 a más de 6 veces
                            TRUE ~ NA))


baseNinosAux1ENDES <- baseNinosAux1ENDES %>% 
  rename(bidx = midx)

baseNinosAuxENDES <- baseNinosAuxENDES %>% 
  left_join(baseNinosAux1ENDES %>%  select("id1", "caseid", "bidx", "pesoNac", "tallaNac", "pesoNacBajo", "tallaNacBajo", "ira0a59", "eda0a59"), by = c("id1", "caseid", "bidx"))

baseNinosENDES %>% select("id1", "hhid", "hvidx", "anemiaNinos", "desnCrOms")

rm(baseNinosAux1ENDES, baseNinosAux2ENDES)

#dirBases <- "C:/Users/User/OneDrive - MIGRACIÓN VIDENZA/1. Proyectos/1. Proyectos actuales/23. Artículos PDB/1. PDB - DIT/2. Data/ENDES/1. Bases"
dirBases <- "C:/Users/Jennifer Prado/OneDrive - VIDENZA/Proyectos activos/1. PDB - DIT/2. Data/ENDES/1. Bases"
setwd(dirBases)
write_dta(data = baseNinosAuxENDES, "baseDIT.dta")

baseNinos1ENDES <- baseNinosAuxENDES %>%
  mutate(hhid = substr(caseid, 1, 15)) %>%
  select(id1, hhid, pesoNacBajo, tallaNacBajo, ira0a59, eda0a59) %>%
  group_by(id1, hhid, .groups = 'drop') %>%
  summarize(pesoNacBajoH = sum(pesoNacBajo, na.rm = TRUE),
            tallaNacBajoH = sum(tallaNacBajo, na.rm = TRUE),
            ira0a59H = sum(ira0a59, na.rm = TRUE),
            eda0a59H = sum(eda0a59, na.rm = TRUE)) %>%
  select(- .groups)

baseHogaresENDES <- baseHogaresENDES %>%
  left_join(baseNinos1ENDES, by = c("id1", "hhid")) %>%
  mutate(pesoNacBajoH = pesoNacBajoH / nNinos,
         tallaNacBajoH = tallaNacBajoH / nNinos,
         ira0a59H = ira0a59H / nNinos,
         eda0a59H = eda0a59H / nNinos)

rm(baseNinos1ENDES)

write_dta(data = baseHogaresENDES, "baseHogaresENDES.dta")


# REC95 Salud niños <- id: id1 caseid idx95
# DIT Desarrollo Infantil Temprano en niñas y niños menores de 6 años <- id: id1 caseid bidx
# REC43 Salud (niños) <- id: id1 caseid hidx
# REC44 Talla y peso <- id: id1 caseid hwidx