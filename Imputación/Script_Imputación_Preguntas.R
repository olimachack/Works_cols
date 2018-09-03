##################################################################################################
#                                    Imputación Encuesta Beimar                                 ##
##################################################################################################

# Librerias ---------------------------------------------------------------

library(dplyr)
library(plyr)
library(VIM)
library(Amelia)
library(mice)
library(dummies)
library(haven)

# Parámetros -------------------------------------------------------------
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

# unión de bases  ---------------------------------------------------------

cons_pers_filter$id_persona <- as.character(cons_pers_filter$id_persona) 

cons_pers_filter$id_persona <- gsub(pattern = "CC","",x =cons_pers_filter$id_persona)
cons_pers_filter$id_persona <- gsub(pattern = "CE","",x =cons_pers_filter$id_persona)


# Definición de nombre (Llave) -------------------------------------------
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
#asignación valores "NA"
#base_train[, 1:dim(base_train)[2]][base_train[, 1:dim(base_train)[2]]==0] <- NA

base_train$Genero <- as.character(base_train$Genero)

# Correción factores variable Género -------------------------------------
base_train$Genero <- ifelse(base_train$Genero=="f","F",base_train$Genero)
base_train$Genero <- ifelse(base_train$Genero=="m","M",base_train$Genero)
base_train$Genero <- as.factor(base_train$Genero)

table(base_train$Genero)

# Correción variable Edad ------------------------------------------------
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

# Codificación nombres variables "pregunta" -------------------------------
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

# Base final exportación train --------------------------------------------------
write.csv(base_final,"base_final.csv",row.names = F)

# base_validación ---------------------------------------------------------

colnames(base.val)[c(35,36,39,42)] <- c("categoria","total_numero_grupo_familiar","Edad","salario")

base.val$Edad <- floor(base.val$Edad)

dummies.val   <- dummy.data.frame(base.val[,c(35,37)])

base.val      <- cbind(base.val[,-c(35,37)],dummies.val)

# Base final exportación val --------------------------------------------------
colnames(base.val)[c(35,37,40:47)] <- c("x46" ,"x3","x45","x7","x8","x9","x10","x4","x5","x6")
write.csv(base.val,"base.val.final.csv",row.names = F)


# Base final 16000 --------------------------------------------------------
write.csv(final_new,"final_new.csv",row.names = F)


# patrón datos faltantes -------------------------------------------------
aggr(base_final,prop=F,numbers=T)
missmap((base_final))
# Lectura salida del modelo Knn o Random Forest python ---------------------------

#imp       <- read.csv("imput_final_class_knn.csv",header = T)[,-1]
imp       <- read.csv("imput_final_class_16000.csv",header = T)[,-1]


# Reestructuración de base dummy  -----------------------------------------
names.p   <- c("mascotas","cocina","salud_bienestar_belleza","decoracion","tecnologia","licores","ocio","vestuario")

names.P1  <- c("P1Bacteriologia y laboratorio Clinico","P1Capacitación Nuevas Tecnologías","P1Contabilidad y finanzas ",
               "P1Enfermeria","P1Finanzas y comercio internacional","P1Ingenieria Administrativa",
               "P1Ingenieria de Producción","P1Ingenieria de Sistemas","P1Ingenieria de Sistemas y Computaciuon",
               "P1Ingenieria electrica","P1Ingenieria Electrónica","P1Ingenieria Industrial","P1Ingenieria Informatica",
               "P1null","P1Relaciones Económicas Internacionales","P1Salud Ocupacional ")

names.P2  <- c("P2Bacteriologia y laboratorio Clinico","P2Capacitación Nuevas Tecnologías",
               "P2Contabilidad y finanzas ","P2Enfermeria","P2Finanzas y comercio internacional",
               "P2Ingenieria Administrativa","P2Ingenieria de Producción","P2Ingenieria de Sistemas",
               "P2Ingenieria de Sistemas y Computaciuon","P2Ingenieria electrica","P2Ingenieria Electrónica",
               "P2Ingenieria Industrial","P2Ingenieria Informatica","P2null","P2Relaciones Económicas Internacionales",
               "P2Salud Ocupacional")

names.P3  <- c("P3Contabilidad y finanzas",
               "P3Gestión integrada de la calidad medio ambiente, seguridad y salud ocupacional",
               "P3Gestión portuaria","P3Mantenimiento de equipo biomédico","P3null","P3Operación de camión minero",
               "P3Salud Ocupacional","P3Técnicos")

names.P4  <- c("P4Administración","P4Administrativos y comerciales",
               "P4Alimentos y bebidas","P4Banca","P4Bursátil","P4Construcción","P4Contabilidad","P4Diseño",
               "P4Docencia","P4Educación","P4Finanzas","P4Informática","P4Ingenierías","P4Marketing",
               "P4Mercadotecnia","P4null","P4Recursos humanos","P4Salud","P4Servicio al cliente",
               "P4Tecnologías de información")

names.P5  <- c("P5Conferencias o talleres sobre cómo Autoestima",
               "P5Conferencias o talleres sobre cómo vivir mejor",
               "P5Conferencias o talleres sobre Comunicación Asertiva",
               "P5Conferencias o talleres sobre Trabajo en Equipo",
               "P5Conferencias y seminarios dirigidos por expertos (liderazgo, superación personal, manejo de conflictos, entre otros) ",
               "P5null")

names.P6  <- c( "P6Analista","P6Bases de datos","P6Contabilidad y finanzas","P6Diseño y arquitectura software",
                "P6Informática","P6Mantenimiento de equipo","P6null","P6Office","P6Técnicos","P6Técnicos profesionales",
                "P6Tecnologías de información","P6Tecnológicos")

names.P7  <- c("P7Analista","P7Bases de datos","P7Contabilidad y finanzas",
               "P7Diseño y arquitectura software","P7Informática","P7Mantenimiento de equipo",
               "P7null","P7Office","P7Técnicos","P7Técnicos profesionales",
               "P7Tecnologías de información","P7Tecnológicos")

names.P8  <- c("P8Capacitación Nuevas Tecnologías","P8Coordinador","P8Enfermeria","P8Gerente",
               "P8Gestión portuaria","P8Jefe de área","P8null","P8Practicante profesional",
               "P8Salud Ocupacional")

names.P9  <- c("P9null","P9Tu")

names.P10 <- c("P10null","P10Tu Grupo Familiar")

names.P11 <- c("P11Bacteriologia y laboratorio Clinico",
               "P11Capacitación Nuevas Tecnologías","P11Contabilidad y finanzas ","P11Enfermeria","P11Estadistica",
               "P11Finanzas y comercio internacional","P11Geología","P11Ingenieria Administrativa","P11Ingenieria de Minas",
               "P11Ingenieria de Producción","P11Ingenieria de Sistemas","P11Ingenieria de Sistemas y Computaciuon",
               "P11Ingenieria de Telecomunicaciones","P11Ingenieria electrica","P11Ingeniería electromecánica",
               "P11Ingenieria Electrónica","P11Ingenieria Industrial","P11Ingenieria Informatica","P11Ingenieria Mecanica",
               "P11Medicina","P11null","P11Quimica farmaceutica","P11Relaciones Económicas Internacionales",
               "P11Salud Ocupacional ")

names.P12 <- c("P12Administración","P12Administrativos y comerciales","P12Alimentos y bebidas",
               "P12Banca","P12Bursátil","P12Construcción","P12Contabilidad","P12Diseño","P12Docencia","P12Educación",
               "P12Finanzas","P12Informática","P12Ingenierías","P12Marketing","P12Mercadotecnia","P12null",
               "P12Recursos humanos","P12Salud","P12Servicio al cliente","P12Tecnologías de información")

names.P13 <- c("P13Administración","P13Administrativos y comerciales","P13Alimentos y bebidas","P13Banca","P13Bursátil",
               "P13Construcción","P13Contabilidad","P13Diseño","P13Docencia","P13Educación","P13Finanzas","P13Informática",
               "P13Ingenierías","P13Marketing","P13Mercadotecnia","P13null","P13Recursos humanos","P13Salud",
               "P13Servicio al cliente","P13Tecnologías de información")

names.P14 <- c("P14Ingenieria Administrativa",
               "P14Ingenieria de Minas","P14Ingenieria de Producción","P14Ingenieria de Sistemas",
               "P14Ingenieria de Sistemas y Computaciuon","P14Ingenieria de Telecomunicaciones","P14Ingenieria electrica",
               "P14Ingeniería electromecánica","P14Ingenieria Electrónica","P14Ingenieria Industrial",
               "P14Ingenieria Informatica","P14Ingenieria Mecanica","P14Ingeniero","P14null")

names.P15 <- c("P15Capacitación empresarial",
               "P15Desarrollo de competencias (liderazgo, trabajo en equipo, etc)","P15Finanzas personales",
               "P15Formación en conocimientos relacionados con su profesión","P15Formación para mis hijos o grupo familiar",
               "P15Idiomas","P15Informática y nuevas tecnologías","P15null")

names.P16 <- c("P16Educación","P16null")

names.P17 <- c("P17Libre inversión y tarjeta de crédito","P17null")

names.P18 <- c("P18null","P18Tecnología, muebles y electrodomésticos")

names.P19 <- c("P19Actividades recreativas (caminatas ecológicas, ciclopaseos, actividades al aire libre, visita a parques)|Cursos, diplomados y programas de posgrados. |Torneos deportivos (fútbol, basquetbol, voleibol, etc.)",
               "P19Actividades recreativas (caminatas ecológicas, ciclopaseos, actividades al aire libre, visita a parques)|Programas de apoyo, asesoría y respaldo frente a problemáticas personales y familiares (consultorios jurídicos y psicológicos, servicio de guardería y",
               "P19Actividades recreativas (caminatas ecológicas, ciclopaseos, actividades al aire libre, visita a parques)|Programas internos para el descanso y esparcimiento (gimnasia laboral, espacios de descanso)",
               "P19Campañas de salud preventiva, salud ocupacional y en general apoyo médico en tu lugar de trabajo.|Actividades recreativas (caminatas ecológicas, ciclopaseos, actividades al aire libre, visita a parques)|Cursos, diplomados y programas de posgrados. |Torneo",
               "P19Campañas de salud preventiva, salud ocupacional y en general apoyo médico en tu lugar de trabajo.|Eventos culturales (teatro, visita a museos, concursos de talentos, conciertos, recitales)|Actividades recreativas (caminatas ecológicas, ciclopaseos, activi",
               "P19Conferencias y seminarios dirigidos por expertos (liderazgo, superación personal, asertividad en la comunicación, manejo de conflictos, entre otros)|Actividades recreativas (caminatas ecológicas, ciclopaseos, actividades al aire libre, visita a parques)",
               "P19Conferencias y seminarios dirigidos por expertos (liderazgo, superación personal, asertividad en la comunicación, manejo de conflictos, entre otros)|Campañas de salud preventiva, salud ocupacional y en general apoyo médico en tu lugar de trabajo.|Cursos, ",
               "P19Conferencias y seminarios dirigidos por expertos (liderazgo, superación personal, asertividad en la comunicación, manejo de conflictos, entre otros)|Campañas de salud preventiva, salud ocupacional y en general apoyo médico en tu lugar de trabajo.|Eventos ",
               "P19Conferencias y seminarios dirigidos por expertos (liderazgo, superación personal, asertividad en la comunicación, manejo de conflictos, entre otros)|Campañas de salud preventiva, salud ocupacional y en general apoyo médico en tu lugar de trabajo.|Programa",
               "P19Conferencias y seminarios dirigidos por expertos (liderazgo, superación personal, asertividad en la comunicación, manejo de conflictos, entre otros)|Programas de apoyo, asesoría y respaldo frente a problemáticas personales y familiares (consultorios juríd",
               "P19Cursos, diplomados y programas de posgrados. ",
               "P19Cursos, diplomados y programas de posgrados. |Torneos deportivos (fútbol, basquetbol, voleibol, etc.)|Programas internos para el descanso y esparcimiento (gimnasia laboral, espacios de descanso)",
               "P19Eventos culturales (teatro, visita a museos, concursos de talentos, conciertos, recitales)|Actividades recreativas (caminatas ecológicas, ciclopaseos, actividades al aire libre, visita a parques)|Cursos, diplomados y programas de posgrados. |Programas int",
               "P19Eventos culturales (teatro, visita a museos, concursos de talentos, conciertos, recitales)|Actividades recreativas (caminatas ecológicas, ciclopaseos, actividades al aire libre, visita a parques)|Cursos, diplomados y programas de posgrados. |Torneos depor",
               "P19null")

names.P20 <- c("P20Centros de Formación","P20null")

names.P21 <- c("P21Actividades deportivas, recreativas y de esparcimiento|Actividades que promuevan la integración Familiar",
               "P21Actividades deportivas, recreativas y de esparcimiento|Actividades que promuevan la integración Familiar|Convenios para acceder a cursos/programas educativos",
               "P21Actividades deportivas, recreativas y de esparcimiento|Actividades que promuevan la integración Familiar|Visión del empleado como un ser integral (vida laboral + vida familiar + vida personal)",
               "P21Actividades deportivas, recreativas y de esparcimiento|Reconocimiento (felicitaciones, premios, incentivos, ascensos por sus logros)|Convenios para acceder a cursos/programas educativos",
               "P21Actividades deportivas, recreativas y de esparcimiento|Reconocimiento (felicitaciones, premios, incentivos, ascensos por sus logros)|Visión del empleado como un ser integral (vida laboral + vida familiar + vida personal)",
               "P21Actividades deportivas, recreativas y de esparcimiento|Salud (prevención y atención)|Reconocimiento (felicitaciones, premios, incentivos, ascensos por sus logros)","P21null")

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
# Imputación --------------------------------------------------------------

# imputed <- mice(base_train[1:20,][,3:12],seed=1220,m = 1)
# prueba  <- base_train[1:20,][,3:12]
# prueba$mascotas[c(1,10,20,5,9)] <- NA
# imputed <- mice(prueba,seed=1220,m = 1)
# filled  <- complete(imputed)
# head(filled)
