library(data.table)

library (tidyverse)

library(ggplot2)


#####este link siempre se debe cambiar a donde estàn los archivos
#####
setwd("C:/Users/52331/Dropbox/MARIELENA/claser_marielena/registroacumulado")#esto es de lo màs ùtil que encontraràn 
#para poder obtener informcion todos los archivos de una carpeta 



fi_files = list.files(pattern ="Su", recursive = T) #en fi_files se pide que se guarde de la lista de archivos todos 
#los que que tengan el patron "Su" en su contenido y que vea todos los archivos dentro del directorio
#por eso se le pone T= de verdadero....

# para buscar como usar algunas de las funcione sen r se puede usar: 
# #help(list.files) or help("list.files), or ?list.files or ?"list.files" 
# 

if_rat = lapply (fi_files, function(x) {    
  options(stringsAsFactors = F)  #https://developer.r-project.org/Blog/public/2020/02/16/stringsasfactors/
  #https://simplystatistics.org/2015/07/24/stringsasfactors-an-unauthorized-biography/
  #https://community.rstudio.com/t/what-does-stringsasfactors-in-r-mean/35626/2
  if_com = read.table(x, skip=2, na.strings = "NA", fill=TRUE, #llena espacios vacios con NA
                      col.names = paste0("V",seq_len(6))) #seq_len es el total de columnas que se usaràn en la tabla que se està formando
  
  
  #lapply() es un caso especial de apply() , diseñado para aplicar funciones a todos los elementos de una lista. 
  #La l de su nombre se refiere, precisamente, a lista. lapply() intentará coercionar a una lista el objeto 
  #que demos como argumento y después aplicará una función a todos sus elemento
  
  
  sujeto=as.character(if_com[3,2]) # extraer dato de sujeto
  fecha=as.character(if_com[1,3]) #fecha de la sesión
  group=as.character(if_com[5,2]) #grupo
  
  #el condicional que se presenta a continuación ayuda a que si en un apartado aparecen los datos que 
  #se muestran a continuación, como RP_BS O RP_LONG2 entonces seleccione el dato de otro apartado diferente
  
  #rpb es un nombre aleatorio del archivo, skip es para quitar todas las filas antes de la columna
  #que quiero en el dato.#na.strings=na--- pone NA donde no hay datos 
  
  
  #ubicar la letra en la tabla común a todos, hasta es el nombre que le doy ala columna donde esta
  #V
  
  hasta = which(if_com$V1=="L:") - 1 # lo qu hace es saltar lineas para recortarse hasta S
  inicio = which(if_com$V1 == "C:") + 1
  if.1 = slice(if_com, inicio:hasta) #If.1<-silice lo que realizo es borrar hasta donde sale V:, del 1 hasta hasta; el cual es la línea 
  #donde está V-1 que sería que no aparesca v.
  
  if.1[1]=NULL#quita la primera columna que se forma de lado izquierdo
  
  # Convertir caracteres a valores numericos
  
  for (c in 1:ncol(if.1)) {
    if.1[ ,c] = as.numeric(if.1[ ,c]) #cambia todos los elementos a numerico de la columna 2 de todas las filas 
  }
  
  
  if.1= na.omit(stack(if.1)) #quita los NA generados
  # if.1[2]=NULL #borra la comlumna 2 donde estaban las V1,V2...
  if.1=data.frame(val=sort(if.1$values)) #sort acomoda la columna values en orden..
  if1 = if.1 %>% mutate(sujeto = sujeto, #mutate crea, modifica o quita columnas
                        fecha = fecha,
                        group = group) %>%
    separate(val, c("t","evento"), sep="\\.",convert = T) %>% as.data.frame()   #convert 
  
  return(if1)
  
} 

) %>% bind_rows() #combina todoas las columnas de cada uno de los archivos

#para entender mejor que es un factor 
#https://www.youtube.com/watch?v=xkRBfy8_2MU 
#https://www.youtube.com/watch?v=pY-NuMEd2Bs

###########################################################


#### lar marcas se realizan para poder analizar tanto ensayos if como de pico en ensayos acumulativos
#ademàs de poder tener los reforzadores dentro del registro acumulado


#como buscar respuestas en R de los archivos de MED
#entradas de cabeza-- 8
#respuestas-- 1
#Inicio IF20 s --5 
##Fin IF20 s-- 3



if_rat$markIF <- ifelse(if_rat$evento == 5 | if_rat$evento == 3,1,0) #aqui le decimos que en archivo if_rat 
#se cree una columna y que si en la coluna de eveto el numero es un 5 y 3 entonces  se ponga un 1 
if_rat$markREF <- ifelse(if_rat$evento == 2,1,0)
if_rat$markentry <- ifelse(if_rat$evento == 8,1,0)


if_rat = if_rat[order(as.Date(if_rat$fecha, format="%m/%d/%Y")),]

setDT(if_rat)[ ,sesiones := rleid(fecha),by = .(sujeto)] #cambia todos los data frame a un data table que es mas facil
#para manejar y ràpido
#rleid lo que hace es fomar la fecha y siempre que tengan el mismo dìa pone un nùmero.... 1,2,3 
#dependiendo del dìa y en este caso lo hace por sujeto 
#
#
#write.csv(if_rat, "VPA_IF.csv", row.names = F) #escribe el archivo en un cvs
#esto es muy importante porque despuès solo pueden subir el archivo y usarlo




if_ratIF <- if_rat %>% #creo un nuevo archivo para que no se modifique el original 
  group_by(sujeto,sesiones,group) %>%
  mutate(id_trial = cumsum(markIF)) %>% #mutate crea una nueva variable de un grupo de datos
  ##cumsum suma solo las marcas correspondientes de markIF por ejemplo
  as.data.frame() #se guarda como un data frame 


if_ratIF = if_ratIF %>% 
  group_by(id_trial,sujeto,sesiones) %>%  
  filter(cumsum(evento ==5) !=0) %>% #filtra los eventos especìficos 
  as.data.frame()
########hace una sumatoria para que solo entren los ensayos de IF quitando los marcadores finales

if_ratIF <- if_ratIF %>%
  group_by(sesiones, sujeto, group) %>% 
  mutate(cumensayo = cumsum(evento == 5))
#####esta lìnea lo que realiza es realizar de nuevo una contabilizaciòn de los ensayos para que sean en orden 
#donde no sea una secuencia 1,3,7.... si no que sea continua 1,2,3,4,....

if_ratIF <- if_ratIF %>%
  group_by(sujeto,sesiones,group) %>%
  mutate(dt = c(0,diff(t))) #calcula la diferencia entre pares consecutivos de elementos  
####realiza una resta del tiempo con la respuesta anterior para posteriormente poder 
#realizar una contabilizaciòn en segundos en cada ensayo 

if_ratIF <- if_ratIF %>%
  group_by(sujeto,group,sesiones) %>%
  mutate(periodo = cumsum(dt)/10) # dividir entre 10 para convertir a segundos, el tiempo se separa por ensayos 

###a continuaciòn lo que se realiza es separar por sesiones en registro acumulativo ademàs de solo dejar los 
#reforzadores y las respuestas

if_ratIF_resp_times <- if_ratIF %>% 
  group_by(sujeto,group,sesiones) %>%
  filter(evento %in% (1:2)) 

if_ratIF_resp_times<-if_ratIF_resp_times%>% 
  group_by(sujeto,group,sesiones) %>%
  mutate(cumResp = cumsum(evento == 1))###la acumulación de respuestas sumar donde el evento sea 1 

#######lo que ralizo a continuaciòn es en la columna donde se marcaron los reforzadores poner el momento en 
#el cual se dieron estos dentro del registro acumulativo
if_ratIF_resp_times <- mutate(if_ratIF_resp_times, markREF = ifelse(markREF == 1,periodo,markREF))

######a continuaciòn se puso Na odonde no habìa 0 de la columna de los reforzadores. 
if_ratIF_resp_times$markREF[if_ratIF_resp_times$markREF==0]<-NA 

#######a continuacion selecciono a un sujeto para poder ver su registro acumulativo con claridad
#también puedo filtrar las sesiones para que solo se muestre una, sin embargo tendrà el periodo correspondiente
#a la gràfica general donde se encuentran todas las sesiones

if_ratIF_group<- if_ratIF_resp_times%>%
  filter(sujeto =="VPA1",sesiones ==1)%>%
  group_by(sujeto,group,sesiones)

#lo que grafico es el momento en el que se dieron las respuestas tomando en cuenta todas las sesiones y los 
#ensayos, porteriormente agrego los reforzadores 
REGISTROSUJETO<-ggplot(if_ratIF_group,aes(x = periodo,y = cumResp))+
  geom_step()+ #Ayuda a graficar una función acumulativa
  geom_point(data = subset(if_ratIF_group,evento == 2),
             aes( x = markREF, y = cumResp),shape = "\\",size = 2,color="red")+
  scale_x_continuous("Tiempo")+
  scale_y_continuous("Respuestas")+
  ggtitle("Registro Acumulativo sujeto VPA1") +
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size=16),
        axis.text.x = element_text(hjust=0.5)) #hjust donde se acomodan los numeros 


REGISTROSUJETO

#será necesario cambiar el nombre del archivo dependiendo del sujeto y la sesión
ggsave(filename = "registroacumulativoVPA1.pdf",plot = REGISTROSUJETO, 
       width =50 , height = 14.5, units = "cm", dpi = 600)




