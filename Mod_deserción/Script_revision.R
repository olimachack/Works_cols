###########################################################################
#                REVISIÓN ANÁLISIS EXPLORATORIO MODELO DE DESERCIÓN       #
###########################################################################

# #Instalación de paquetes ------------------------------------------------
#install.packages(c("ggplot2","dplyr","plyr"),dependencies = T)

# Librerias ---------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(plyr)
library(scales)
# #Parámetros -------------------------------------------------------------
directorio = "C:/Users/JuanCastillo/Desktop/Modelo de deserción/preprocesadas"
setwd(directorio) # Cambio de directorio

# Carga de bases  ---------------------------------------------------------
base_maestra       <- read.csv("Base Maestra Trabajadores para Retiros 3.csv",sep = ";")
cons_trabajadores  <- read.csv("CONSOLIDADO DATOS TRABAJADORES 7.csv",sep = ";")
agrupación         <- read.table("agrupacion.txt",header = T,sep = ";")

# Depuración base ---------------------------------------------------------

#variable Tipo_contrato
cons_trabajadores$Tipo_contrato <- as.character(cons_trabajadores$Tipo_contrato)

cons_trabajadores$Tipo_contrato <- ifelse(cons_trabajadores$Tipo_contrato=="FIJO                   "
                                          ,"FIJO",cons_trabajadores$Tipo_contrato)
cons_trabajadores$Tipo_contrato <- ifelse(cons_trabajadores$Tipo_contrato=="APRENDIZAJE            "
                                          ,"APRENDIZAJE",cons_trabajadores$Tipo_contrato)
cons_trabajadores$Tipo_contrato <- ifelse(cons_trabajadores$Tipo_contrato=="OBRA O LABOR           ",
                                          "OBRA O LABOR",cons_trabajadores$Tipo_contrato)
cons_trabajadores$Tipo_contrato <- ifelse(cons_trabajadores$Tipo_contrato=="PENSIONADO ACTIVO SS   ",
                                          "PENSIONADO ACTIVO SS",cons_trabajadores$Tipo_contrato)
cons_trabajadores$Tipo_contrato <- ifelse(cons_trabajadores$Tipo_contrato=="PENSIONADO ACTIVOS INTE   ",
                                          "PENSIONADO ACTIVOS INTE",cons_trabajadores$Tipo_contrato)
cons_trabajadores$Tipo_contrato <- ifelse(cons_trabajadores$Tipo_contrato=="PENSIONADO FIJO        ",
                                          "PENSIONADO FIJO",cons_trabajadores$Tipo_contrato)
cons_trabajadores$Tipo_contrato <- ifelse(cons_trabajadores$Tipo_contrato=="PENSIONADOS            ",
                                          "PENSIONADOS",cons_trabajadores$Tipo_contrato)
cons_trabajadores$Tipo_contrato <- ifelse(cons_trabajadores$Tipo_contrato=="PROFESORES             ",
                                          "PROFESORES",cons_trabajadores$Tipo_contrato)
cons_trabajadores$Tipo_contrato <- as.factor(cons_trabajadores$Tipo_contrato)


# Piramide Poblacional por edad ----------------------------------------------------
age_genero <- ddply(cons_trabajadores,.(Rango_Edad,Genero,Estado_Trab),summarise,conteo=length(Edad))
age_genero <- age_genero %>% filter(Estado_Trab == "ACTIVO" | Estado_Trab  == "RETIRADO")

colnames(age_genero)[2] <- c("Género")
age_genero$Rango_Edad   <- factor(age_genero$Rango_Edad, levels = c("< 20","21-30", "31-40", "41-50", "51-60", "61-70",">70"))


breaks.fn               <- function(lims)return(pretty_breaks(n=5)(c(as.numeric(lims[1]),as.numeric(lims[2]))))

plot.piramide <- ggplot(data = age_genero, 
                        mapping = aes(x = Rango_Edad, fill = Género, 
                                      y = ifelse(test = Género == "Masculino", 
                                                 yes = -conteo, no = conteo))) +
        geom_bar(color="black",stat = "identity",width = 0.6) +theme_bw()+ facet_wrap(~Estado_Trab,scales = "free")+
        scale_y_continuous(labels = abs, limits = max(age_genero$conteo) * c(-1,1)) +
        labs(y = "Empleados") +
        coord_flip()+
        scale_y_continuous(breaks = breaks.fn)+
        scale_fill_brewer(palette = "Set1")+
        labs(y="N° de empleados", x="Rango de Edad", title="Distribución por Género")+
        theme(plot.title = element_text(size=20,hjust = 0.5,face = "bold",colour = "black"))+
        theme( axis.title=element_text(size=14,face="bold"),axis.text.x = element_text(size = 12),
               axis.text.y = element_text(size = 12))+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        theme(strip.background=element_rect(fill="#05179F"))+  
        theme(strip.text=element_text(colour="white",face="bold",size=25))

print(plot.piramide)

# Estado_Trab vs hijos ----------------------------------------------------


hijos <- ddply(cons_trabajadores,.(No_hijos,Estado_Trab),summarise,conteo=length(No_hijos))
#colnames(hijos)[1] <- c("Género")
hijos <- hijos %>% filter(Estado_Trab == "ACTIVO" | Estado_Trab  == "RETIRADO")

colnames(cons_trabajadores)

breaks.fn.hijos <- function(lims)return(pretty_breaks(n=10)(c(as.numeric(lims[1]),as.numeric(lims[2]))))


plot.hijos <- ggplot(data=hijos,aes(x=as.factor(No_hijos),y = conteo))+geom_bar(color="black",stat = "identity",fill = c("red"))+ 
        theme_bw()+theme(legend.position="none")+facet_wrap(~Estado_Trab)+
        labs(y="EMPLEADOS", x="CANTIDAD", title="NÚMERO DE HIJOS")+
        #theme(axis.ticks.y=element_blank())+  
        theme(plot.title = element_text(size=20,hjust = 0.5,face = "bold",colour = "black"))+
        #scale_x_continuous(breaks =seq(2,6,1), labels=paste0("Estrato ",levels(as.factor(cajeros$Estrato))))+
        #scale_y_continuous(breaks =seq(0,15,2))+
        theme( axis.title=element_text(size=14,face="bold"),axis.text.x = element_text(size = 15),axis.text.y = element_text(size = 15))+
        theme(strip.background=element_rect(fill="#05179F"))+  
        theme(strip.text=element_text(colour="white",face="bold",size=25))+
        scale_y_continuous(breaks = breaks.fn.hijos)

print(plot.hijos)

# Género vs dependencia económica ----------------------------------------------------
contrato               <- ddply(cons_trabajadores,.(Estado_Trab,Tipo_contrato),
                                summarise,conteo=length(Tipo_contrato))

contrato  <- contrato %>% filter(Estado_Trab == "ACTIVO" | Estado_Trab  == "RETIRADO")

#hijos <- hijos %>% filter(Estado_Trab == "ACTIVO" | Estado_Trab  == "RETIRADO")

breaks.fn.contrato <- function(lims)return(pretty_breaks(n=10)(c(as.numeric(lims[1]),as.numeric(lims[2]))))

plot.contrato <- ggplot(data=contrato,aes(x=as.factor(Tipo_contrato),y = conteo))+geom_bar(color="black",stat = "identity",fill = c("red"),width = 0.4)+ 
        theme_bw()+theme(legend.position="none")+facet_wrap(~Estado_Trab,scales = "free")+
        labs(y="CANTIDAD", x="TIPO DE CONTRATO", title="")+
        #theme(axis.ticks.y=element_blank())+  
        theme(plot.title = element_text(size=20,hjust = 0.5,face = "bold",colour = "black"))+
        #scale_x_continuous(breaks =seq(2,6,1), labels=paste0("Estrato ",levels(as.factor(cajeros$Estrato))))+
        #scale_y_continuous(breaks =seq(0,15,2))+
        theme( axis.title=element_text(size=14,face="bold"),axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12))+
        theme(strip.background=element_rect(fill="#05179F"))+  
        theme(strip.text=element_text(colour="white",face="bold",size=25))+
        scale_y_continuous(breaks = breaks.fn.contrato)+coord_flip()

print(plot.contrato)

# Género vs escolaridad ----------------------------------------------------

escolaridad               <- ddply(cons_trabajadores,.(Estado_Trab,Nivel_Escolaridad,Genero),
                                   summarise,conteo=length(Tipo_contrato))

breaks.fn.escolaridad   <- function(lims)return(pretty_breaks(n=10)(c(as.numeric(lims[1]),as.numeric(lims[2]))))

plot.escolaridad  <- ggplot(data=escolaridad,aes(x=as.factor(Nivel_Escolaridad),y = conteo))+geom_bar(color="black",aes(fill=Genero),stat = "identity",width = 0.4)+ 
        theme_bw()+theme(legend.position="none")+facet_grid(Genero~Estado_Trab,scales = "free_x")+
        labs(y="CANTIDAD", x="ESCOLARIDAD", title="")+
        #theme(axis.ticks.y=element_blank())+  
        theme(plot.title = element_text(size=20,hjust = 0.5,face = "bold",colour = "black"))+
        #scale_x_continuous(breaks =seq(2,6,1), labels=paste0("Estrato ",levels(as.factor(cajeros$Estrato))))+
        #scale_y_continuous(breaks =seq(0,15,2))+
        theme( axis.title=element_text(size=14,face="bold"),axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12))+
        theme(strip.background=element_rect(fill="#05179F"))+  
        theme(strip.text=element_text(colour="white",face="bold",size=25))+
        scale_y_continuous(breaks = breaks.fn.escolaridad  )+coord_flip()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(plot.escolaridad)


# Género vs antiguedad ----------------------------------------------------

antiguedad               <- ddply(cons_trabajadores,.(Estado_Trab,Rango_antiguedad,Genero),
                                  summarise,conteo=length(Tipo_contrato))

antiguedad$Rango_antiguedad   <- factor(antiguedad$Rango_antiguedad , levels = c("1 - 2 MESES","2 - 6 MESES", "6 - 12 MESES", "1 - 3 AÑOS", "3 - 5 AÑOS", "> 5 AÑOS" ))

breaks.fn.antiguedad   <- function(lims)return(pretty_breaks(n=10)(c(as.numeric(lims[1]),as.numeric(lims[2]))))

plot.antiguedad  <- ggplot(data=antiguedad,aes(x=as.factor(Rango_antiguedad),y = conteo))+geom_bar(aes(fill=Genero),stat = "identity",width = 0.4,color="black")+ 
        theme_bw()+theme(legend.position="none")+facet_grid(Genero~Estado_Trab,scales = "free_x")+
        labs(y="CANTIDAD", x="RANGO DE ANTIGUEDAD", title="")+
        #theme(axis.ticks.y=element_blank())+  
        theme(plot.title = element_text(size=20,hjust = 0.5,face = "bold",colour = "black"))+
        #scale_x_continuous(breaks =seq(2,6,1), labels=paste0("Estrato ",levels(as.factor(cajeros$Estrato))))+
        #scale_y_continuous(breaks =seq(0,15,2))+
        theme( axis.title=element_text(size=14,face="bold"),axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12))+
        theme(strip.background=element_rect(fill="#05179F"))+  
        theme(strip.text=element_text(colour="white",face="bold",size=25))+
        scale_y_continuous(breaks = breaks.fn.antiguedad  )+coord_flip()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(plot.antiguedad)

# Género vs tipo de vivienda ----------------------------------------------------

vivienda              <- ddply(cons_trabajadores,.(Estado_Trab,Tipo_vivienda,Genero),
                               summarise,conteo=length(Tipo_contrato))

breaks.fn.vivienda   <- function(lims)return(pretty_breaks(n=10)(c(as.numeric(lims[1]),as.numeric(lims[2]))))

plot.vivienda <- ggplot(data=vivienda,aes(x=as.factor(Tipo_vivienda),y = conteo))+geom_bar(aes(fill=Genero),color="black",stat = "identity",width = 0.4)+ 
        theme_bw()+theme(legend.position="none")+facet_grid(Genero~Estado_Trab,scales = "free_x")+
        labs(y="CANTIDAD", x="VIVIENDA", title="")+
        #theme(axis.ticks.y=element_blank())+  
        theme(plot.title = element_text(size=20,hjust = 0.5,face = "bold",colour = "black"))+
        #scale_x_continuous(breaks =seq(2,6,1), labels=paste0("Estrato ",levels(as.factor(cajeros$Estrato))))+
        #scale_y_continuous(breaks =seq(0,15,2))+
        theme( axis.title=element_text(size=14,face="bold"),axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12))+
        theme(strip.background=element_rect(fill="#05179F"))+  
        theme(strip.text=element_text(colour="white",face="bold",size=25))+
        scale_y_continuous(breaks = breaks.fn.vivienda)+coord_flip()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(plot.vivienda)

# Género vs Denominación del motivo de med ----------------------------------------------------

motivo_med              <- ddply(cons_trabajadores,.(Estado_Trab,Denominacion_motivo_med,Genero),
                                 summarise,conteo=length(Tipo_contrato))

motivo_med              <- motivo_med[with(motivo_med, order(-conteo, Estado_Trab,Denominacion_motivo_med,Genero)), ]

motivo_med              <- motivo_med[motivo_med$conteo>=500,]

breaks.fn.med  <- function(lims)return(pretty_breaks(n=10)(c(as.numeric(lims[1]),as.numeric(lims[2]))))


plot.med <- ggplot(data=motivo_med,aes(x=reorder(as.factor(Denominacion_motivo_med),conteo),y = conteo))+geom_bar(aes(fill=Genero),stat = "identity",color="black",width = 0.4)+ 
        theme_bw()+theme(legend.position="none")+facet_grid(Genero~Estado_Trab,scales = "free_x")+
        labs(y="CANTIDAD", x="MOTIVO DE LA MEDIDA", title="")+
        #theme(axis.ticks.y=element_blank())+  
        theme(plot.title = element_text(size=20,hjust = 0.5,face = "bold",colour = "black"))+
        #scale_x_continuous(breaks =seq(2,6,1), labels=paste0("Estrato ",levels(as.factor(cajeros$Estrato))))+
        #scale_y_continuous(breaks =seq(0,15,2))+
        theme( axis.title=element_text(size=14,face="bold"),axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12))+
        theme(strip.background=element_rect(fill="#05179F"))+  
        theme(strip.text=element_text(colour="white",face="bold",size=25))+
        scale_y_continuous(breaks = breaks.fn.med)+coord_flip()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(plot.med)

# Género vs Estado Civil ----------------------------------------------------

estado_civil             <- ddply(cons_trabajadores,.(Estado_Trab,Estado_civil,Genero),
                                  summarise,conteo=length(Tipo_contrato))

breaks.fn.civil  <- function(lims)return(pretty_breaks(n=10)(c(as.numeric(lims[1]),as.numeric(lims[2]))))


plot.estado_civil <- ggplot(data=estado_civil,aes(x=reorder(as.factor(Estado_civil),conteo),y = conteo))+geom_bar(aes(fill=Genero),stat = "identity",color="black",width = 0.4)+ 
        theme_bw()+theme(legend.position="none")+facet_grid(Genero~Estado_Trab,scales = "free_x")+
        labs(y="CANTIDAD", x="ESTADO CIVIL", title="")+
        #theme(axis.ticks.y=element_blank())+  
        theme(plot.title = element_text(size=20,hjust = 0.5,face = "bold",colour = "black"))+
        #scale_x_continuous(breaks =seq(2,6,1), labels=paste0("Estrato ",levels(as.factor(cajeros$Estrato))))+
        #scale_y_continuous(breaks =seq(0,15,2))+
        theme( axis.title=element_text(size=14,face="bold"),axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12))+
        theme(strip.background=element_rect(fill="#05179F"))+  
        theme(strip.text=element_text(colour="white",face="bold",size=25))+
        scale_y_continuous(breaks = breaks.fn.civil)+coord_flip()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(plot.estado_civil)

# tiempo de supervivencia general Género -----------------------------------------
month <- function(x){
        m = 12*x/365
        return(m)
}

breaks.fn.super <- function(lims)return(pretty_breaks(n=15)(c(as.numeric(lims[1]),as.numeric(lims[2]))))

cons_trabajadores$Antiguedad_meses <- floor(month(cons_trabajadores$Antiguedad))

supervivencia            <- ddply(cons_trabajadores,.(Estado_Trab,Genero),
                                  summarise,conteo=Antiguedad_meses)

supervivencia   <- supervivencia  %>% filter(Estado_Trab  == "RETIRADO")

super_genero <- ggplot(data = supervivencia, aes(y=conteo)) + 
        geom_boxplot(aes(fill=Genero),width=0.5,outlier.size=-1)+
        scale_y_continuous(breaks = breaks.fn.super )+
        coord_flip()+theme_bw()+theme(strip.background=element_rect(fill="#05179F"))+  
        theme(strip.text=element_text(colour="white",face="bold",size=20))+
        labs(y="MESES (SUPERVIVENCIA)", x="", title="")+ theme(legend.text=element_text(size=15))+
        theme( axis.title=element_text(size=14,face="bold"),axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12,colour = "transparent"))


print(super_genero)

# tiempo de supervivencia NIVEL CARGO -----------------------------------------

supervivencia_cargo  <- ddply(cons_trabajadores,.(Estado_Trab,Nivel_cargo),
                              summarise,conteo=Antiguedad_meses)

supervivencia_cargo  <- supervivencia_cargo  %>% filter(Estado_Trab  == "RETIRADO")
supervivencia_cargo  <- supervivencia_cargo  %>% filter(Nivel_cargo  != "#N/A")

super_genero_cargo  <- ggplot(data = supervivencia_cargo, aes(x=Nivel_cargo, y=conteo)) + 
        geom_boxplot(aes(fill=Nivel_cargo),width=0.5,outlier.size=-1)+
        scale_y_continuous(breaks = breaks.fn.super )+
        coord_flip()+theme_bw()+theme(strip.background=element_rect(fill="#05179F"))+  
        theme(strip.text=element_text(colour="white",face="bold",size=20))+
        labs(y="MESES (SUPERVIVENCIA)", x="", title="")+ theme(legend.text=element_text(size=15))+
        theme( axis.title=element_text(size=14,face="bold"),axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12))+
        theme(legend.position="none")

print(super_genero_cargo)


# tiempo de supervivencia general rango edad -----------------------------------------

supervivencia_rango_edad   <- ddply(cons_trabajadores,.(Estado_Trab,Rango_Edad),
                                    summarise,conteo=Antiguedad_meses)

supervivencia_rango_edad   <- supervivencia_rango_edad   %>% filter(Estado_Trab  == "RETIRADO")

supervivencia_rango_edad$Rango_Edad<- factor(supervivencia_rango_edad$Rango_Edad, levels = c("< 20","21-30", "31-40", "41-50", "51-60", "61-70",">70"))


super_edad <- ggplot(data = supervivencia_rango_edad, aes(x=Rango_Edad, y=conteo)) + 
        geom_boxplot(aes(fill=Rango_Edad),width=0.5,outlier.size=-1)+
        scale_y_continuous(breaks = breaks.fn.super )+
        coord_flip()+theme_bw()+theme(strip.background=element_rect(fill="#05179F"))+  
        theme(strip.text=element_text(colour="white",face="bold",size=20))+
        labs(y="MESES (SUPERVIVENCIA)", x="RANGO DE EDAD", title="")+ theme(legend.text=element_text(size=15))+
        theme( axis.title=element_text(size=14,face="bold"),axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12))+
        theme(legend.position="none")

print(super_edad)

# Supervivencia por estrato -----------------------------------------------

supervivencia_estrato  <- cons_trabajadores %>% ddply(.(Estado_Trab,Estrato),
                                                      summarise,conteo=Antiguedad_meses)

super_est<- ggplot(data = supervivencia_estrato, aes(x=Estrato, y=conteo)) + 
        geom_boxplot(aes(fill=Estrato),width=0.5,outlier.size=-1)+
        scale_y_continuous(breaks = breaks.fn.super )+
        coord_flip()+theme_bw()+theme(strip.background=element_rect(fill="#05179F"))+  
        theme(strip.text=element_text(colour="white",face="bold",size=20))+
        labs(y="MESES (SUPERVIVENCIA)", x="ESTRATO", title="")+ theme(legend.text=element_text(size=15))+
        theme( axis.title=element_text(size=14,face="bold"),axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12))+
        theme(legend.position="none")

print(super_est)
