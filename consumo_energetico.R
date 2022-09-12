## Indicadores Guerrero
# David A. Ortega - 12/07/22

# Preparar Entorno ----
pacman::p_load(tidyverse,janitor,sf)
dev.off()
rm(list=ls())
# Cargar Bases ----
permisos_cre_2207 <- read_csv("input/PermisosdeGeneracionVigentesporModalidad.csv",skip=1) %>% clean_names()


# Sociedades de Autoabasto ----
permisos_cre_2207 %>% filter(modalidad =="AUT.") %>% mutate(fecha_de_otorgamiento = as.Date(fecha_de_otorgamiento,tryFormat ="%d/%m/%Y")) %>% 
  filter(fecha_de_otorgamiento>"2000-01-01") %>% group_by(energetico_primario) %>% 
  summarise(mwh = sum(capacidad_autorizada_mw)) %>% 
  ggplot(aes(reorder(energetico_primario,mwh),mwh,fill=energetico_primario))+
  geom_col()+
  coord_flip()

##
permisos_cre_2207 %>% filter(modalidad =="AUT.") %>% mutate(fecha_de_otorgamiento = as.Date(fecha_de_otorgamiento,tryFormat ="%d/%m/%Y")) %>% 
  mutate(ano = format(fecha_de_otorgamiento,format = "%Y")) %>% arrange(ano) %>% group_by(ano) %>% count()  
  
  ###
permisos_cre_2207 %>% filter(modalidad =="AUT.") %>% mutate(fecha_de_otorgamiento = as.Date(fecha_de_otorgamiento,tryFormat ="%d/%m/%Y")) %>% 
  mutate(ano = format(fecha_de_otorgamiento,format = "%Y")) %>% arrange(ano) %>% filter(!str_detect(permisionario,"Pemex")) %>% 
  group_by(ano) %>% count()  %>% mutate(ano = as.numeric(ano)) %>% filter(ano>2011) %>% 
  ggplot(aes(ano,n))+
  geom_line()+
  geom_label(aes(label = n))

##
permisos_cre_2207 %>% filter(modalidad =="AUT.") %>% mutate(fecha_de_otorgamiento = as.Date(fecha_de_otorgamiento,tryFormat ="%d/%m/%Y")) %>% 
  mutate(ano = format(fecha_de_otorgamiento,format = "%Y")) %>% arrange(ano) %>% filter(!str_detect(permisionario,"Pemex")) %>% filter(ano>2011) %>% 
  filter(ano<2019) %>% 
  group_by(energetico_primario) %>% summarise(capacidad=sum(capacidad_autorizada_mw)) %>% ungroup() %>% mutate(porcentaje = capacidad/sum(capacidad)*100)
  ggplot(aes(reorder(energetico_primario,capacidad),capacidad, fill = energetico_primario))+
  geom_col()

# Analisis de Permisos General ----



  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  