##################################################################################################
#                                    Imputación Encuesta Beimar                                 ##
##################################################################################################

# Librerias ---------------------------------------------------------------
library(car)
library(dplyr)
library(plyr)
library(VIM)
library(Amelia)
library(mice)
library(dummies)
library(haven)
library(missForest)

# Parámetros -------------------------------------------------------------
directorio = "C:/Users/JuanCastillo/Desktop/Tarea beimar/Bases"

# Cambio de directorio
setwd(directorio) 
# Carga de bases  ---------------------------------------------------------

bd_tuclub.v2          <- read.csv("BD_Tuclub_CET 17-11-17_act.csv",header = T,
                                  sep = ";",skip = 1,na.strings=c("","NA"))

base.val              <- readRDS("DT_JyA_final.rds")

bd_tuclub.v2_filter   <- bd_tuclub.v2[,c(1,3,5,6,9,10,11:18,23:43)]

rm(bd_tuclub.v2)

# unión de bases  ---------------------------------------------------------
colnames(bd_tuclub.v2_filter)[1] <- c("id_persona")

# base de entrenamiento --------------------------------------------------
bd_tuclub.v2_filter$Genero <- as.character(bd_tuclub.v2_filter$Genero)

# Correción factores variable Género -------------------------------------
bd_tuclub.v2_filter$Genero <- ifelse(bd_tuclub.v2_filter$Genero=="f","F",bd_tuclub.v2_filter$Genero)
bd_tuclub.v2_filter$Genero <- ifelse(bd_tuclub.v2_filter$Genero=="m","M",bd_tuclub.v2_filter$Genero)
bd_tuclub.v2_filter$Genero <- as.factor(bd_tuclub.v2_filter$Genero)
levels(bd_tuclub.v2_filter$Genero)


# Correción variable Edad ------------------------------------------------
bd_tuclub.v2_filter$Edad   <- ifelse(bd_tuclub.v2_filter$Edad<=17,NA,bd_tuclub.v2_filter$Edad)
summary(bd_tuclub.v2_filter$Edad)

# cambio a variable dummies mascotas- cocina- salud_ --------

cols = c(7:14) 
bd_tuclub.v2_filter[,cols] <- apply(bd_tuclub.v2_filter[,cols], 2, function(x) as.character(x))
bd_tuclub.v2_filter[, 1:dim(bd_tuclub.v2_filter)[2]][bd_tuclub.v2_filter[, 1:dim(bd_tuclub.v2_filter)[2]]=="x"] <- "1"
bd_tuclub.v2_filter[,cols][is.na(bd_tuclub.v2_filter[,cols])] <- "0"

for(i in cols){
        bd_tuclub.v2_filter[, i]  <- as.factor(bd_tuclub.v2_filter[, i])
}

# VALORES NA PREGUNTAS --------------------------------------------------

cols_2 <- 15:35  
bd_tuclub.v2_filter[,cols_2] <- apply(bd_tuclub.v2_filter[,cols_2], 2, function(x) as.character(x))
bd_tuclub.v2_filter[,c(cols_2)][, 1:dim(bd_tuclub.v2_filter[,c(cols_2)])[2]][bd_tuclub.v2_filter[,c(cols_2)][, 1:dim(bd_tuclub.v2_filter[,c(cols_2)])[2]]=="0"] <- NA

for(i in cols_2){
        bd_tuclub.v2_filter[, i]  <- as.factor(bd_tuclub.v2_filter[, i])
}


# Codificación nombres variables "pregunta" -------------------------------
colnames(bd_tuclub.v2_filter)[15:35] <- paste0("P",1:21)

# base_validación ---------------------------------------------------------

base.val.filter           <- base.val %>% select(IdPersona,Categoria,SAL_BASICO,GF,Genero,Diferencia) 

colnames(base.val.filter) <- c("id_persona","Categoria","Slario","Grupo.familiar","Genero","Edad")

base.val.filter$Edad      <- floor(base.val.filter$Edad)

# patrón datos faltantes -------------------------------------------------
aggr(base_final_val ,prop=F,numbers=T)
missmap((base_final_val))

# NO Dummies --------------------------------------------------------------

str(bd_tuclub.v2_filter[,1:6])
str(base.val.filter[,1:6])

base.val.filter$Categoria <- as.factor(base.val.filter$Categoria)
base.val.filter$Grupo.familiar <- as.integer(base.val.filter$Grupo.familiar)
base.val.filter$Genero <- as.factor(base.val.filter$Genero)
base.val.filter$Edad <- as.integer(base.val.filter$Edad)

bd_tuclub.v2_filter$id_persona <- as.character(bd_tuclub.v2_filter$id_persona) 

# Fusión de bases ---------------------------------------------------------

#dummies
base_final_val$Grupo.familiar <- as.integer(base_final_val$Grupo.familiar)
base_final_val$Edad           <- as.integer(base_final_val$Edad)

data_final                    <- bind_rows(base_final,base_final_val)

#no dummies

data_final                    <- bind_rows(bd_tuclub.v2_filter,base.val.filter)


# Imputación Forest ---------------------------------------------------------


colnames(data_final) <- paste0("V",1:229)

imput_base <- missForest(data_final[,-c(1)],
                         ntree = 3,
                         variablewise = F,
                        maxiter = 2) 

write.csv(imput_base$ximp,"imputacionforest.csv")




