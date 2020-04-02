library(dplyr); library(stringr)

# ####################################################################################################################
# ##Get the data
# ####################################################################################################################
###############
## FROM GITHUB REPO
#############
# Link al repositorio donde estan los datos
Link <- "https://github.com/carranco-sga/Mexico-COVID-19/archive/master.zip"
# Descargar el archivo
download.file(Link, destfile = "Data/Mexico-COVID-19.zip")
# extraer los archivos
unzip("Data/Mexico-COVID-19.zip")
# Cargar el numero de casos por entidad
Cases <- read.csv("Mexico-COVID-19-master/Mexico_COVID19.csv") %>%
  mutate(Fecha = as.Date(Fecha, "%Y-%m-%d")) # Formatear la variable fecha
# Datos con el codigo de los estados
EstCode <- read.csv("Data/EstCod.csv")
#################
### Obtener casos por estado:
Cases_Estado <- Cases %>% select(1:ZAC_D) %>%
  tidyr::gather(Code, N, AGU_S:ZAC_D) %>%
  mutate(Estado = stringr::str_split_fixed(Code, "_", 2)[,1],
         tipo = stringr::str_split_fixed(Code, "_", 2)[,2],
         tipo = ifelse(tipo == "", "Total_I", tipo)) %>%
  select(-Code) %>% rename(Code = Estado) %>%
  tidyr::spread(tipo, N) %>%
  arrange(Code) %>%
  left_join(EstCode, by = "Code")

# Exportar el archivo
write.csv(Cases_Estado, "App/CasesEst.csv", row.names = F)
#####################
### Casos totales
Cases <- Cases %>%
  mutate(prev = lead(Cases$Pos), new = lag(prev - Pos)) %>%
  select(Fecha, Casos = new, Cumulativo = Pos, Pos_rep, Susp_rep, IRAG_Test, Tested_tot, Recovered, Deceased)
# Exportar el archivo
write.csv(Cases, "Data/CasosTot.csv", row.names = F)

