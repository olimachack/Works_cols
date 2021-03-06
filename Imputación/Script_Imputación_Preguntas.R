##################################################################################################
#                                    Imputaci�n Encuesta Beimar                                 ##
##################################################################################################

# Librerias ---------------------------------------------------------------

library(dplyr)
library(plyr)
library(VIM)
library(Amelia)
library(mice)
library(dummies)
library(haven)

# Par�metros -------------------------------------------------------------
directorio = "C:/Users/JuanCastillo/Desktop/Tarea beimar/Bases"

# Cambio de directorio
setwd(directorio) 
# Carga de bases  ---------------------------------------------------------

bd_tuclub             <- read.csv("BD_Tuclub_CET_17_11_17.csv",header = T,
                                  sep = ";",skip = 1,na.strings=c("","NA"))

bd_tuclub.v2          <- read.csv("BD_Tuclub_CET 17-11-17_act.csv",header = T,
                                  sep = ";",skip = 1,na.strings=c("","NA"))

bd_tuclub.v3          <- read.csv("BD_Tuclub_CET 15-08-18-SIN BIENVENIDA FINAL.csv",header = T,
                                     sep = ";",skip = 1,na.strings=c("","NA"))

base.val              <- readRDS("DT_JyA_final.rds")

cons_pers          <- read.csv("ConsolidadaPersona.csv",header = T,sep=",",na.strings=c("","NA")) %>% 
        arrange(id_persona, piramide2) %>% 
        group_by(id_persona) %>% 
        filter(row_number()==1)

bd_tuclub_filter       <- bd_tuclub[,c(1:12,17:37)]
bd_tuclub.v2_filter    <- bd_tuclub.v2[,c(1,4,8)]
cons_pers_filter       <- cons_pers[,c(2,(5:8),11,12,14)]

bd_tuclub.v3.filter           <- bd_tuclub.v3   %>% 
        select(id_persona , Edad.1, Genero.1, Salario, Grupo.familiar, categoria)

bd_tuclub.v3.filter $id_persona <- as.character(bd_tuclub.v3.filter $id_persona) 

bd_tuclub.v3.filter $id_persona <- gsub(pattern = "CC","",x =bd_tuclub.v3.filter $id_persona)
bd_tuclub.v3.filter $id_persona <- gsub(pattern = "CE","",x =bd_tuclub.v3.filter $id_persona)
bd_tuclub.v3.filter $id_persona <- gsub(pattern = "PA","",x =bd_tuclub.v3.filter $id_persona)
bd_tuclub.v3.filter $id_persona <- gsub(pattern = "RC","",x =bd_tuclub.v3.filter $id_persona)
bd_tuclub.v3.filter $id_persona <- gsub(pattern = "TI","",x =bd_tuclub.v3.filter $id_persona)

bd_tuclub.v3.filter$id_persona <- as.numeric(bd_tuclub.v3.filter$id_persona)

rm(bd_tuclub,bd_tuclub.v2,cons_pers)

# uni�n de bases  ---------------------------------------------------------

cons_pers_filter$id_persona <- as.character(cons_pers_filter$id_persona) 

cons_pers_filter$id_persona <- gsub(pattern = "CC","",x =cons_pers_filter$id_persona)
cons_pers_filter$id_persona <- gsub(pattern = "CE","",x =cons_pers_filter$id_persona)


# Definici�n de nombre (Llave) -------------------------------------------
colnames(bd_tuclub_filter)[1]    <- c("id_persona")
colnames(bd_tuclub.v2_filter)[1] <- c("id_persona")

cons_pers_filter$id_persona      <- as.numeric(cons_pers_filter$id_persona) 

# base de entrenamiento --------------------------------------------------
base_train <- left_join(bd_tuclub_filter,cons_pers_filter, by = c("id_persona"))
base_train <- left_join(base_train,bd_tuclub.v2_filter, by = c("id_persona"))

bd_tuclub.v3.imp <- left_join(bd_tuclub.v3.filter,cons_pers_filter, by = c("id_persona"))

bd_tuclub.v3.imp <- bd_tuclub.v3.imp %>% select(salario,categoria.y,genero,Edad,total_numero_grupo_familiar)
        
colnames(bd_tuclub.v3.imp)[c(2,3,5)] <- c("categoria","Genero","numero gupo familiar")

summary(bd_tuclub.v3.imp)
#asignaci�n valores "NA"
#base_train[, 1:dim(base_train)[2]][base_train[, 1:dim(base_train)[2]]==0] <- NA

base_train$Genero <- as.character(base_train$Genero)

# Correci�n factores variable G�nero -------------------------------------
base_train$Genero <- ifelse(base_train$Genero=="f","F",base_train$Genero)
base_train$Genero <- ifelse(base_train$Genero=="m","M",base_train$Genero)
base_train$Genero <- as.factor(base_train$Genero)

table(base_train$Genero)

# Correci�n variable Edad ------------------------------------------------
base_train$Edad   <- ifelse(base_train$Edad<=17,NA,base_train$Edad)

# cambio a variable dummies Genero- Edad- mascotas- cocina- salud_ --------

cols = 5:12    
base_train[,cols] <- apply(base_train[,cols], 2, function(x) as.character(x))
base_train[, 1:dim(base_train)[2]][base_train[, 1:dim(base_train)[2]]=="x"] <- "1"

base_train[,cols][is.na(base_train[,cols])] <- "0"

for(i in cols){
        base_train[, i]  <- as.factor(base_train[, i])
}

cols_2 <- 13:33  
base_train[,cols_2] <- apply(base_train[,cols_2], 2, function(x) as.character(x))
base_train[,c(cols_2)][, 1:dim(base_train[,c(cols_2)])[2]][base_train[,c(cols_2)][, 1:dim(base_train[,c(cols_2)])[2]]=="0"] <- "null"
#base_train[,c(13:33)][, 1:dim(base_train[,c(13:33)])[2]][base_train[,c(13:33)][, 1:dim(base_train[,c(13:33)])[2]]=="0"] <- NA

for(i in cols_2){
        base_train[, i]  <- as.factor(base_train[, i])
}

str(base_train)
names(base_train)
summary(base_train)

# Codificaci�n nombres variables "pregunta" -------------------------------
colnames(base_train)[13:33] <- paste0("P",1:21)
length(colnames(base_train)[13:33])
length( paste0("P",1:13))

# variables dummies -------------------------------------------------------
dummies_preg  <- dummy.data.frame(base_train[,13:33])

base_train$nivel_socioeconomico <- as.factor(base_train$nivel_socioeconomico)
base_train$Segmento             <- as.factor(base_train$Segmento)


dummies_pred                    <- dummy.data.frame(base_train[,c(3,35,37,38,39,40)])

base_final <- cbind(base_train[,c(1,2,4:12)],dummies_preg,dummies_pred,base_train$salario,base_train$total_numero_grupo_familiar)
rm(dummies_preg,dummies_pred)



#base 16.000
dummies_bd_tuclub.v3                 <- dummy.data.frame(bd_tuclub.v3.imp[,c(2,3)])

final_new <- cbind(bd_tuclub.v3.imp$salario,bd_tuclub.v3.imp$`numero gupo familiar`,dummies_bd_tuclub.v3 ,bd_tuclub.v3.imp$Edad)
colnames(final_new)[c(1,2,10)] <- c("salario","numero gupo familiar","Edad")
colnames(final_new) <- c('x45','x46','x7','x8','x9','x10','x4','x5','x6','x3')

# Renombrar variables -----------------------------------------------------

colnames(base_final) <- c('x1','x2','x3','y1','y2','y3','y4','y5','y6','y7','y8','y9','y10',
                          'y11','y12','y13','y14','y15','y16','y17','y18','y19','y20','y21',
                          'y22','y23','y24','y25','y26','y27','y28','y29','y30','y31','y32',
                          'y33','y34','y35','y36','y37','y38','y39','y40','y41','y42','y43',
                          'y44','y45','y46','y47','y48','y49','y50','y51','y52','y53','y54',
                          'y55','y56','y57','y58','y59','y60','y61','y62','y63','y64','y65',
                          'y66','y67','y68','y69','y70','y71','y72','y73','y74','y75','y76',
                          'y77','y78','y79','y80','y81','y82','y83','y84','y85','y86','y87',
                          'y88','y89','y90','y91','y92','y93','y94','y95','y96','y97','y98',
                          'y99','y100','y101','y102','y103','y104','y105','y106','y107',
                          'y108','y109','y110','y111','y112','y113','y114','y115','y116',
                          'y117','y118','y119','y120','y121','y122','y123','y124','y125',
                          'y126','y127','y128','y129','y130','y131','y132','y133','y134',
                          'y135','y136','y137','y138','y139','y140','y141','y142','y143',
                          'y144','y145','y146','y147','y148','y149','y150','y151','y152',
                          'y153','y154','y155','y156','y157','y158','y159','y160','y161',
                          'y162','y163','y164','y165','y166','y167','y168','y169','y170',
                          'y171','y172','y173','y174','y175','y176','y177','y178','y179',
                          'y180','y181','y182','y183','y184','y185','y186','y187','y188',
                          'y189','y190','y191','y192','y193','y194','y195','y196','y197',
                          'y198','y199','y200','y201','y202','y203','y204','y205','y206',
                          'y207','y208','y209','y210','y211','y212','y213','y214','y215',
                          'y216','y217','y218','y219','y220','y221','y222','y223','y224',
                          'y225','y226','y227','x4','x5','x6','x7','x8','x9','x10','x11',
                          'x12','x13','x14','x15','x16','x17','x18','x19','x20','x21',
                          'x22','x23','x24','x25','x26','x27','x28','x29','x30','x31',
                          'x32','x33','x34','x35','x36','x37','x38','x39','x40','x41',
                          'x42','x43','x44',"x45","x46")

# Base final exportaci�n train --------------------------------------------------
write.csv(base_final,"base_final.csv",row.names = F)

# base_validaci�n ---------------------------------------------------------

colnames(base.val)[c(35,36,39,42)] <- c("categoria","total_numero_grupo_familiar","Edad","salario")

base.val$Edad <- floor(base.val$Edad)

dummies.val   <- dummy.data.frame(base.val[,c(35,37)])

base.val      <- cbind(base.val[,-c(35,37)],dummies.val)

# Base final exportaci�n val --------------------------------------------------
colnames(base.val)[c(35,37,40:47)] <- c("x46" ,"x3","x45","x7","x8","x9","x10","x4","x5","x6")
write.csv(base.val,"base.val.final.csv",row.names = F)


# Base final 16000 --------------------------------------------------------
write.csv(final_new,"final_new.csv",row.names = F)


# patr�n datos faltantes -------------------------------------------------
aggr(base_final,prop=F,numbers=T)
missmap((base_final))
# Lectura salida del modelo Knn o Random Forest python ---------------------------

#imp       <- read.csv("imput_final_class_knn.csv",header = T)[,-1]
imp       <- read.csv("imput_final_class_16000.csv",header = T)[,-1]


# Reestructuraci�n de base dummy  -----------------------------------------
names.p   <- c("mascotas","cocina","salud_bienestar_belleza","decoracion","tecnologia","licores","ocio","vestuario")

names.P1  <- c("P1Bacteriologia y laboratorio Clinico","P1Capacitaci�n Nuevas Tecnolog�as","P1Contabilidad y finanzas ",
               "P1Enfermeria","P1Finanzas y comercio internacional","P1Ingenieria Administrativa",
               "P1Ingenieria de Producci�n","P1Ingenieria de Sistemas","P1Ingenieria de Sistemas y Computaciuon",
               "P1Ingenieria electrica","P1Ingenieria Electr�nica","P1Ingenieria Industrial","P1Ingenieria Informatica",
               "P1null","P1Relaciones Econ�micas Internacionales","P1Salud Ocupacional ")

names.P2  <- c("P2Bacteriologia y laboratorio Clinico","P2Capacitaci�n Nuevas Tecnolog�as",
               "P2Contabilidad y finanzas ","P2Enfermeria","P2Finanzas y comercio internacional",
               "P2Ingenieria Administrativa","P2Ingenieria de Producci�n","P2Ingenieria de Sistemas",
               "P2Ingenieria de Sistemas y Computaciuon","P2Ingenieria electrica","P2Ingenieria Electr�nica",
               "P2Ingenieria Industrial","P2Ingenieria Informatica","P2null","P2Relaciones Econ�micas Internacionales",
               "P2Salud Ocupacional")

names.P3  <- c("P3Contabilidad y finanzas",
               "P3Gesti�n integrada de la calidad medio ambiente, seguridad y salud ocupacional",
               "P3Gesti�n portuaria","P3Mantenimiento de equipo biom�dico","P3null","P3Operaci�n de cami�n minero",
               "P3Salud Ocupacional","P3T�cnicos")

names.P4  <- c("P4Administraci�n","P4Administrativos y comerciales",
               "P4Alimentos y bebidas","P4Banca","P4Burs�til","P4Construcci�n","P4Contabilidad","P4Dise�o",
               "P4Docencia","P4Educaci�n","P4Finanzas","P4Inform�tica","P4Ingenier�as","P4Marketing",
               "P4Mercadotecnia","P4null","P4Recursos humanos","P4Salud","P4Servicio al cliente",
               "P4Tecnolog�as de informaci�n")

names.P5  <- c("P5Conferencias o talleres sobre c�mo Autoestima",
               "P5Conferencias o talleres sobre c�mo vivir mejor",
               "P5Conferencias o talleres sobre Comunicaci�n Asertiva",
               "P5Conferencias o talleres sobre Trabajo en Equipo",
               "P5Conferencias y seminarios dirigidos por expertos (liderazgo, superaci�n personal, manejo de conflictos, entre otros) ",
               "P5null")

names.P6  <- c( "P6Analista","P6Bases de datos","P6Contabilidad y finanzas","P6Dise�o y arquitectura software",
                "P6Inform�tica","P6Mantenimiento de equipo","P6null","P6Office","P6T�cnicos","P6T�cnicos profesionales",
                "P6Tecnolog�as de informaci�n","P6Tecnol�gicos")

names.P7  <- c("P7Analista","P7Bases de datos","P7Contabilidad y finanzas",
               "P7Dise�o y arquitectura software","P7Inform�tica","P7Mantenimiento de equipo",
               "P7null","P7Office","P7T�cnicos","P7T�cnicos profesionales",
               "P7Tecnolog�as de informaci�n","P7Tecnol�gicos")

names.P8  <- c("P8Capacitaci�n Nuevas Tecnolog�as","P8Coordinador","P8Enfermeria","P8Gerente",
               "P8Gesti�n portuaria","P8Jefe de �rea","P8null","P8Practicante profesional",
               "P8Salud Ocupacional")

names.P9  <- c("P9null","P9Tu")

names.P10 <- c("P10null","P10Tu Grupo Familiar")

names.P11 <- c("P11Bacteriologia y laboratorio Clinico",
               "P11Capacitaci�n Nuevas Tecnolog�as","P11Contabilidad y finanzas ","P11Enfermeria","P11Estadistica",
               "P11Finanzas y comercio internacional","P11Geolog�a","P11Ingenieria Administrativa","P11Ingenieria de Minas",
               "P11Ingenieria de Producci�n","P11Ingenieria de Sistemas","P11Ingenieria de Sistemas y Computaciuon",
               "P11Ingenieria de Telecomunicaciones","P11Ingenieria electrica","P11Ingenier�a electromec�nica",
               "P11Ingenieria Electr�nica","P11Ingenieria Industrial","P11Ingenieria Informatica","P11Ingenieria Mecanica",
               "P11Medicina","P11null","P11Quimica farmaceutica","P11Relaciones Econ�micas Internacionales",
               "P11Salud Ocupacional ")

names.P12 <- c("P12Administraci�n","P12Administrativos y comerciales","P12Alimentos y bebidas",
               "P12Banca","P12Burs�til","P12Construcci�n","P12Contabilidad","P12Dise�o","P12Docencia","P12Educaci�n",
               "P12Finanzas","P12Inform�tica","P12Ingenier�as","P12Marketing","P12Mercadotecnia","P12null",
               "P12Recursos humanos","P12Salud","P12Servicio al cliente","P12Tecnolog�as de informaci�n")

names.P13 <- c("P13Administraci�n","P13Administrativos y comerciales","P13Alimentos y bebidas","P13Banca","P13Burs�til",
               "P13Construcci�n","P13Contabilidad","P13Dise�o","P13Docencia","P13Educaci�n","P13Finanzas","P13Inform�tica",
               "P13Ingenier�as","P13Marketing","P13Mercadotecnia","P13null","P13Recursos humanos","P13Salud",
               "P13Servicio al cliente","P13Tecnolog�as de informaci�n")

names.P14 <- c("P14Ingenieria Administrativa",
               "P14Ingenieria de Minas","P14Ingenieria de Producci�n","P14Ingenieria de Sistemas",
               "P14Ingenieria de Sistemas y Computaciuon","P14Ingenieria de Telecomunicaciones","P14Ingenieria electrica",
               "P14Ingenier�a electromec�nica","P14Ingenieria Electr�nica","P14Ingenieria Industrial",
               "P14Ingenieria Informatica","P14Ingenieria Mecanica","P14Ingeniero","P14null")

names.P15 <- c("P15Capacitaci�n empresarial",
               "P15Desarrollo de competencias (liderazgo, trabajo en equipo, etc)","P15Finanzas personales",
               "P15Formaci�n en conocimientos relacionados con su profesi�n","P15Formaci�n para mis hijos o grupo familiar",
               "P15Idiomas","P15Inform�tica y nuevas tecnolog�as","P15null")

names.P16 <- c("P16Educaci�n","P16null")

names.P17 <- c("P17Libre inversi�n y tarjeta de cr�dito","P17null")

names.P18 <- c("P18null","P18Tecnolog�a, muebles y electrodom�sticos")

names.P19 <- c("P19Actividades recreativas (caminatas ecol�gicas, ciclopaseos, actividades al aire libre, visita a parques)|Cursos, diplomados y programas de posgrados. |Torneos deportivos (f�tbol, basquetbol, voleibol, etc.)",
               "P19Actividades recreativas (caminatas ecol�gicas, ciclopaseos, actividades al aire libre, visita a parques)|Programas de apoyo, asesor�a y respaldo frente a problem�ticas personales y familiares (consultorios jur�dicos y psicol�gicos, servicio de guarder�a y",
               "P19Actividades recreativas (caminatas ecol�gicas, ciclopaseos, actividades al aire libre, visita a parques)|Programas internos para el descanso y esparcimiento (gimnasia laboral, espacios de descanso)",
               "P19Campa�as de salud preventiva, salud ocupacional y en general apoyo m�dico en tu lugar de trabajo.|Actividades recreativas (caminatas ecol�gicas, ciclopaseos, actividades al aire libre, visita a parques)|Cursos, diplomados y programas de posgrados. |Torneo",
               "P19Campa�as de salud preventiva, salud ocupacional y en general apoyo m�dico en tu lugar de trabajo.|Eventos culturales (teatro, visita a museos, concursos de talentos, conciertos, recitales)|Actividades recreativas (caminatas ecol�gicas, ciclopaseos, activi",
               "P19Conferencias y seminarios dirigidos por expertos (liderazgo, superaci�n personal, asertividad en la comunicaci�n, manejo de conflictos, entre otros)|Actividades recreativas (caminatas ecol�gicas, ciclopaseos, actividades al aire libre, visita a parques)",
               "P19Conferencias y seminarios dirigidos por expertos (liderazgo, superaci�n personal, asertividad en la comunicaci�n, manejo de conflictos, entre otros)|Campa�as de salud preventiva, salud ocupacional y en general apoyo m�dico en tu lugar de trabajo.|Cursos, ",
               "P19Conferencias y seminarios dirigidos por expertos (liderazgo, superaci�n personal, asertividad en la comunicaci�n, manejo de conflictos, entre otros)|Campa�as de salud preventiva, salud ocupacional y en general apoyo m�dico en tu lugar de trabajo.|Eventos ",
               "P19Conferencias y seminarios dirigidos por expertos (liderazgo, superaci�n personal, asertividad en la comunicaci�n, manejo de conflictos, entre otros)|Campa�as de salud preventiva, salud ocupacional y en general apoyo m�dico en tu lugar de trabajo.|Programa",
               "P19Conferencias y seminarios dirigidos por expertos (liderazgo, superaci�n personal, asertividad en la comunicaci�n, manejo de conflictos, entre otros)|Programas de apoyo, asesor�a y respaldo frente a problem�ticas personales y familiares (consultorios jur�d",
               "P19Cursos, diplomados y programas de posgrados. ",
               "P19Cursos, diplomados y programas de posgrados. |Torneos deportivos (f�tbol, basquetbol, voleibol, etc.)|Programas internos para el descanso y esparcimiento (gimnasia laboral, espacios de descanso)",
               "P19Eventos culturales (teatro, visita a museos, concursos de talentos, conciertos, recitales)|Actividades recreativas (caminatas ecol�gicas, ciclopaseos, actividades al aire libre, visita a parques)|Cursos, diplomados y programas de posgrados. |Programas int",
               "P19Eventos culturales (teatro, visita a museos, concursos de talentos, conciertos, recitales)|Actividades recreativas (caminatas ecol�gicas, ciclopaseos, actividades al aire libre, visita a parques)|Cursos, diplomados y programas de posgrados. |Torneos depor",
               "P19null")

names.P20 <- c("P20Centros de Formaci�n","P20null")

names.P21 <- c("P21Actividades deportivas, recreativas y de esparcimiento|Actividades que promuevan la integraci�n Familiar",
               "P21Actividades deportivas, recreativas y de esparcimiento|Actividades que promuevan la integraci�n Familiar|Convenios para acceder a cursos/programas educativos",
               "P21Actividades deportivas, recreativas y de esparcimiento|Actividades que promuevan la integraci�n Familiar|Visi�n del empleado como un ser integral (vida laboral + vida familiar + vida personal)",
               "P21Actividades deportivas, recreativas y de esparcimiento|Reconocimiento (felicitaciones, premios, incentivos, ascensos por sus logros)|Convenios para acceder a cursos/programas educativos",
               "P21Actividades deportivas, recreativas y de esparcimiento|Reconocimiento (felicitaciones, premios, incentivos, ascensos por sus logros)|Visi�n del empleado como un ser integral (vida laboral + vida familiar + vida personal)",
               "P21Actividades deportivas, recreativas y de esparcimiento|Salud (prevenci�n y atenci�n)|Reconocimiento (felicitaciones, premios, incentivos, ascensos por sus logros)","P21null")

names.alt     <- c(names.p,names.P1,names.P2,names.P3,names.P4,names.P5,names.P6,names.P7,names.P8,names.P9,
                   names.P10,names.P11,names.P12,names.P13,names.P14,names.P15,names.P16,names.P17,names.P18,
                   names.P19,names.P20,names.P21)

colnames(imp) <- names.alt

groups        <- list(names.P1,names.P2,names.P3,names.P4,names.P5,names.P6,names.P7,names.P8,names.P9,names.P10,
                      names.P11,names.P12,names.P13,names.P14,names.P15,names.P16,names.P17,names.P18,names.P19,
                      names.P20,names.P21)

# Dummies a factor --------------------------------------------------------

alm       <- as.data.frame(matrix(data = NA,nrow = dim(imp[,-c(1:8)])[1],length(groups)))

for(i in 1:length(groups)){
        
        temp               <- as.matrix(imp[,-c(1:8)][groups[[i]]])
        alm[,i]            <- factor((temp %*% (1:ncol(temp))))
}

alm.2 <- as.data.frame(matrix(data = NA,nrow = dim(imp[,-c(1:8)])[1],length(groups)))

for(i in 1:length(groups)){
        for(j in 1:dim(alm)[1]){
                
                alm.2[j,i]       <-  ifelse(as.numeric(as.character(alm[j,i]))==0,0,groups[[i]][as.numeric(as.character(alm[j,i]))])
                
        }
}

# Salida base final -------------------------------------------------------
base.beimar <- readRDS("DT_JyA_final.rds")

base.beimar <- cbind(base.beimar,imp[,c(1:8)],alm.2)

write.csv(base.beimar,"base.beimar.csv")



# Salida fina base 16000 --------------------------------------------------

bd_tuclub.v3             <- read.csv("BD_Tuclub_CET 15-08-18-SIN BIENVENIDA FINAL.csv",header = T,
                                     sep = ";",skip = 1,na.strings=c("","NA"))

base.beimar16 <- cbind(bd_tuclub.v3,imp[,c(1:8)],alm.2)

write.csv(base.beimar16,"base.beimar16.csv")
# Imputaci�n --------------------------------------------------------------

# imputed <- mice(base_train[1:20,][,3:12],seed=1220,m = 1)
# prueba  <- base_train[1:20,][,3:12]
# prueba$mascotas[c(1,10,20,5,9)] <- NA
# imputed <- mice(prueba,seed=1220,m = 1)
# filled  <- complete(imputed)
# head(filled)
