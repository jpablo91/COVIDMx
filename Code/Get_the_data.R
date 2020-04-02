library(dplyr); library(stringr)

# ####################################################################################################################
# ##Get the data
# ####################################################################################################################
Link <- "https://www.gob.mx/cms/uploads/attachment/file/542104/Tabla_casos_positivos_COVID-19_resultado_InDRE_2020.03.18.pdf?fbclid=IwAR20Fu0lhBGVFoh2cDux5G1yHqN_IzEBs1iyEDJTSQlrst1XbrAMjhG1ILU"
# We use the function on page 1-3
out <- tabulizer::extract_tables(file = Link, pages = c(1, 2, 3), columns = list(8), method = "stream", encoding = "UTF-8")
# Extract columns that werent formatted correctly
dfst <- out[[1]][-c(1,2), 6] %>%
  str_split_fixed(string = ., pattern = " ", n = 4)
# Create a DF
MexDF <- out[[1]][-c(1,2), -6] %>% data.frame() %>%
  mutate(fecha = dfst[,1],
         status = dfst[,2],
         procedencia = dfst[,3],
         llegada = dfst[,4]) %>%
  select(-`X4`) %>%
  rename(N_Caso = X1, Estado = `X2`, Sexo = `X3`, Edad = `X5`) %>%
  mutate(Estado = iconv(Estado, from = "UTF-8", to = "ASCII//TRANSLIT"))
# DF for the Secod part
MDF2 <- rbind(out[[2]], out[[3]]) %>%
  data.frame()
# Fix names
names(MDF2) <- names(MexDF)
# Standardize
MDF2 <- MDF2 %>%
  mutate(Estado = iconv(Estado, from = "UTF-8", to = "ASCII//TRANSLIT"))
# Join everything
MexDF <- rbind(MexDF, MDF2) %>%
  mutate(fecha = as.Date(fecha, "%d/%m/%Y"))

write.csv(MexDF, "App/MexDF.csv", row.names = F)

# read.csv("App/MexDF.csv")


# Fri Mar 20 12:06:51 2020 ------------------------------

###############
## FROM GITHUB REPO (Works better so far)
#############
Link <- "https://github.com/carranco-sga/Mexico-COVID-19/archive/master.zip"
download.file(Link, destfile = "Data/Mexico-COVID-19.zip")
unzip("Data/Mexico-COVID-19.zip")

Cases <- read.csv("Mexico-COVID-19-master/Mexico_COVID19.csv") %>%
  mutate(Fecha = as.Date(Fecha, "%Y-%m-%d"))
EstCode <- read.csv("Data/EstCod.csv")
#################
# Obtener casos por estado:
Cases_Estado <- Cases %>% select(1:ZAC_D) %>%
  tidyr::gather(Code, N, AGU_S:ZAC_D) %>%
  mutate(Estado = stringr::str_split_fixed(Code, "_", 2)[,1],
         tipo = stringr::str_split_fixed(Code, "_", 2)[,2],
         tipo = ifelse(tipo == "", "Total_I", tipo)) %>%
  select(-Code) %>% rename(Code = Estado) %>%
  tidyr::spread(tipo, N) %>%
  arrange(Code) %>%
  left_join(EstCode, by = "Code")

write.csv(Cases_Estado, "App/CasesEst.csv", row.names = F)
# write.csv(Cases_Estado, "App/MexDF.csv", row.names = F)
#####################
# Casos totales
Cases <- Cases %>%
  mutate(prev = lead(Cases$Pos), new = lag(prev - Pos)) %>%
  select(Fecha, Casos = new, Cumulativo = Pos, Pos_rep, Susp_rep, IRAG_Test, Tested_tot, Recovered, Deceased)

write.csv(Cases, "Data/CasosTot.csv", row.names = F)

