library(dplyr); library(stringr)

###############
## FROM GITHUB REPO (Works better so far)
#############
# Link a el repositorio de github
Link <- "https://github.com/carranco-sga/Mexico-COVID-19/archive/master.zip"
# Decargar los datos
download.file(Link, destfile = "Data/Mexico-COVID-19.zip")
# descomprimir el archivo
unzip("Data/Mexico-COVID-19.zip")
# cargar los datos en R
Cases <- read.csv("Mexico-COVID-19-master/Mexico_COVID19.csv") %>%
  mutate(Fecha = as.Date(Fecha, "%Y-%m-%d")) # formatear la fecha

EstCode <- read.csv("Data/EstCod.csv") # Cargar el codigo de los estados

#################
# Obtener casos por estado:
Cases_Estado <- Cases %>% select(1:ZAC_D) %>%
  tidyr::gather(Code, N, AGU_S:ZAC_D) %>% # convertir entre formato wide a long
  mutate(Estado = stringr::str_split_fixed(Code, "_", 2)[,1], # formatear variables
         tipo = stringr::str_split_fixed(Code, "_", 2)[,2],
         tipo = ifelse(tipo == "", "Total_I", tipo)) %>%
  select(-Code) %>% rename(Code = Estado) %>%
  tidyr::spread(tipo, N) %>% # transformar a wide de nuevo
  arrange(Code) %>%
  left_join(EstCode, by = "Code") # unir el codigo con el nombre del estado

write.csv(Cases_Estado, "Data/CasesEst.csv", row.names = F) # guardar los datos en el ordenador
#####################
# Casos totales
Cases <- Cases %>%
  mutate(prev = lead(Cases$Pos), new = lag(prev - Pos)) %>%
  select(Fecha, Casos = new, Cumulativo = Pos, Pos_rep, Susp_rep, IRAG_Test, Tested_tot, Recovered, Deceased)

write.csv(Cases, "Data/CasosTot.csv", row.names = F)

