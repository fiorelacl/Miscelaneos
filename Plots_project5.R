library(tidyverse)

DF <- read_csv('C:/Users/fvcas/OneDrive/Escritorio/ENSO_Winter_School/Practical/Project/test.csv') %>% 
  mutate(Source=factor(Source, levels=c('HaddIST','CESM2_HISTORICAL','CESM2_CTRL',
                                        'FLOR_HISTORICAL','FLOR_CTRL','ZC','INC',
                                        'XRO','XRO_CTRL','XRO_T301','NRO'))) %>% 
  mutate(Classification=factor(Classification,levels=c('SLN','DLN','TLN','4More','None')))

round(prop.table(table(DF$Source,DF$Classification),margin=1)*100,1)

# La Niña counts
DF1<- DF %>% filter(Classification %in% c('SLN','DLN','TLN','4More'))
#########

ggplot(DF1, aes(x = Preceeding, color = Classification)) +
  geom_density( size = 1.5) +
  geom_vline(xintercept = 1.5, linetype = "dotdash", color = "red")+ # umbral Niño fuerte
  theme_minimal() +
  labs(title = "Densidad de anomalía previa con medianas por tipo de La Niña",
       x = "Preceding Anomaly(°C)", y = "Density") +
  scale_color_brewer(palette = "Blues") +
  theme(legend.title = element_blank(), legend.position = "top")+
  facet_wrap(~Source)


DF1 %>% filter(Classification=='TLN') %>% ggplot(., aes(x = W1, y = Preceeding)) +
  geom_point(width = 0.1, height = 0.2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Relación entre intensidad inicial y duración del evento",
       x = "Anomalía inicial W1 (°C)",
       y = "Duración (nº de inviernos)") +
  scale_color_brewer(palette = "Blues") +
  facet_wrap(~Source)


##########

DF1 %>% group_by(Source,Classification) %>%
  summarise(n_LN=n()) %>% 
  mutate(freq = round(n_LN*100 / sum(n_LN),1)) %>% 
  ungroup() %>% 
  mutate(Source = factor(Source, levels=c('HaddIST','CESM2_HISTORICAL','CESM2_CTRL',
                                                         'FLOR_HISTORICAL','FLOR_CTRL','ZC','INC',
                                                         'XRO','XRO_CTRL','XRO_T301','NRO'))) %>% 
  ggplot(aes(x = Source, y = freq, fill = Classification)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(x = "", y = "Frequency (%)", fill = "Classification") +
  theme_bw() +
  theme(
    text = element_text(size = 14, family = "monospace"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "monospace"),
    axis.text.y = element_text(family = "monospace"),
    legend.text = element_text(family = "monospace"),
    legend.title = element_text(family = "monospace"),
    plot.title = element_text(family = "monospace"),
    strip.text = element_text(family = "monospace"),
    legend.position = c(0.98, 0.98),          # posición dentro del gráfico
    legend.justification = c("right", "top"), # anclaje en esquina superior derecha
    legend.background = element_rect(fill = "white", color = "black")
  )

# Observation per 3 intervals 1870-1920 // 1921-1971 // 1972-2024 (2014)

temp1 <- DF %>% 
  select(-c(1,4,6:8,10)) %>% 
  filter(Classification != 'None') %>% 
  filter(Source %in% c('HaddIST','CESM2_HISTORICAL','FLOR_HISTORICAL')) %>% 
  filter(Year>=1870) %>% 
  group_by(Source,Classification) %>%
  summarise(n_LN=n()) %>% 
  mutate(freq = round(n_LN*100 / sum(n_LN),1)) %>% ungroup() %>% 
  # pivot_wider(names_from=Classification,values_from = n_LN,values_fill = 0) %>% 
  mutate(Period='1870-Latest')
temp2 <- DF %>% 
  select(-c(1,4,6:8,10)) %>% 
  filter(Classification != 'None') %>% 
  filter(Source %in% c('HaddIST','CESM2_HISTORICAL','FLOR_HISTORICAL')) %>% 
  filter(Year>=1920) %>% 
  group_by(Source,Classification) %>%
  summarise(n_LN=n()) %>% 
  mutate(freq = round(n_LN*100 / sum(n_LN),1)) %>% ungroup() %>% 
  # pivot_wider(names_from=Classification,values_from = n_LN,values_fill = 0) %>% 
  mutate(Period='1920-Latest')
temp3 <- DF %>% 
  select(-c(1,4,6:8,10)) %>% 
  filter(Classification != 'None') %>% 
  filter(Source %in% c('HaddIST','CESM2_HISTORICAL','FLOR_HISTORICAL')) %>% 
  filter(Year>=1970) %>% 
  group_by(Source,Classification) %>%
  summarise(n_LN=n()) %>% 
  mutate(freq = round(n_LN*100 / sum(n_LN),1)) %>% ungroup() %>% 
  # pivot_wider(names_from=Classification,values_from = n_LN,values_fill = 0) %>% 
  mutate(Period='1970-Latest')

TEMP <- bind_rows(temp1,temp2,temp3)

TEMP %>%
  ggplot(aes(x = Source, y = freq, fill = Classification)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7)  +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    expansion(mult = c(0, 0.01))) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(x = "", y = "Frequency (%)") +
  theme_bw() +
  theme(
    text = element_text(size = 14, family = "monospace"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "monospace"),
    axis.text.y = element_text(family = "monospace"),
    plot.title = element_text(family = "monospace"),
    strip.text = element_text(family = "monospace")) +
  guides(fill = "none") +
  facet_wrap(~Period)

DF %>% 
  select(-c(1,4,6:8,10)) %>% 
  filter(Classification != 'None') %>% 
  filter(Source %in% c('HaddIST','CESM2_HISTORICAL','FLOR_HISTORICAL')) %>% 
  filter(Year>=1970) %>% 
  group_by(Source,Classification) %>%
  summarise(n_LN=n()) %>% 
  mutate(freq = round(n_LN*100 / sum(n_LN),1)) %>% 
  ggplot(aes(x = Source, y = freq, fill = Classification)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(title='1970-Latest',x = "", y = "Frequency (%)", fill = "Classification") +
  theme_bw() +
  theme(
    text = element_text(size = 14, family = "monospace"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "monospace"),
    axis.text.y = element_text(family = "monospace"),
    legend.text = element_text(family = "monospace"),
    legend.title = element_text(family = "monospace"),
    plot.title = element_text(family = "monospace"),
    strip.text = element_text(family = "monospace"),
    legend.position = c(0.98, 0.98),          # posición dentro del gráfico
    legend.justification = c("right", "top"), # anclaje en esquina superior derecha
    legend.background = element_rect(fill = "white", color = "black")
  ) +guides(fill = "none")


# Slides windows of counts
DF2 <- DF %>% mutate(Year=case_when(Source=='CESM2_HISTORICAL' ~ Year-1850,
                             Source=='FLOR_HISTORICAL' ~ Year-1850,
                             Source=='HaddIST' ~ Year-1870,
                             TRUE~Year)) %>% 
  select(-c(1,4,6:8,10)) %>% 
  mutate(Year=Year+1) %>% filter(Classification != 'None') %>% 
  mutate(Source=factor(Source),
         Classification=factor(Classification)) %>% 
  filter(!(Source %in% c('HaddIST','CESM2_HISTORICAL','FLOR_HISTORICAL')))

DF3 <- DF %>% mutate(Year=case_when(Source=='CESM2_HISTORICAL' ~ Year-1850,
                                    Source=='FLOR_HISTORICAL' ~ Year-1850,
                                    Source=='HaddIST' ~ Year-1870,
                                    TRUE~Year)) %>% 
  select(-c(1,4,6:8,10)) %>% 
  mutate(Year=Year+1) %>% filter(Classification != 'None') %>% 
  mutate(Source=factor(Source),
         Classification=factor(Classification)) %>% 
  filter(Source %in% c('HaddIST','CESM2_HISTORICAL','FLOR_HISTORICAL')) %>% 
  group_by(Source,Classification) %>%
  summarise(n_LN=n()) %>% 
  mutate(freq = round(n_LN*100 / sum(n_LN),1)) %>% 
  ungroup()

BD <- data.frame()

for (i in 1:6450){
  temp <- DF2 %>% 
    filter(Year>=i & Year<i+154) %>% 
    group_by(Source,Classification) %>%
    summarise(n_LN=n()) %>% 
    mutate(freq = round(n_LN*100 / sum(n_LN),1)) %>% 
    ungroup() %>% mutate(Period=paste0(i,'_',i+154)) 
  BD <- bind_rows(BD, temp)
}

BD <- as_data_frame(BD) 
BD2 <- bind_rows(BD,DF3)


write_csv(BD2,'C:/Users/fvcas/OneDrive/Escritorio/ENSO_Winter_School/Practical/Project/LN_events.csv')

BD3 <- BD2 %>%
  separate(Period,c('Y0','Yf'),sep='_') %>%
  filter(!(Source=='ZC' & Y0>845)) %>% 
  filter(!(Source=='INC' & Y0>2946)) %>% 
  filter(!(Source=='XRO' & Y0>6446)) %>% 
  filter(!(Source=='XRO_CTRL' & Y0>6446)) %>%
  filter(!(Source=='XRO_T301' & Y0>6446)) %>%
  filter(!(Source=='FLOR_CTRL' & Y0>3345)) %>% 
  filter(!(Source=='CESM2_CTRL' & Y0>1846)) %>% 
  filter(!(Source=='NRO' & Y0>6446))
  
  
 ############
 source_colors <- c(
   "HaddIST"           = "#2b2b2b",   # gris muy oscuro
   
   # CESM2
   "CESM2_HISTORICAL"  = "#4a90e2",   # azul elegante
   "CESM2_CTRL"        = "#a3c6f1",   # azul cielo
   
   # FLOR
   "FLOR_HISTORICAL"   = "#00acc1",   # turquesa
   "FLOR_CTRL"         = "#b2ebf2",   # turquesa pastel
   
   # ZC & INC
   "ZC"                = "#66bb6a",   # verde fresco
   "INC"               = "#dcedc8",   # verde claro
   
   # XRO group
   "XRO"               = "#ef5350",   # coral
   "XRO_CTRL"          = "#f48fb1",   # rosa suave
   "XRO_T301"          = "#f48fD5",   # rosa suave
   "NRO"               = "#d32f2f"    # rojo vino
 )
 
 BD3 %>% 
   group_by(Source, Classification) %>% 
   summarise(emean = mean(freq), esd = sd(freq), .groups = "drop") %>% 
   mutate(Source = factor(Source, levels = names(source_colors))) %>%  # para mantener orden
   ggplot(aes(x = Source, y = emean, fill = Source)) +
   geom_bar(stat = "identity", width = 0.6) +
   geom_errorbar(aes(ymin = emean - esd, ymax = emean + esd), width = 0.2) +
   scale_fill_manual(values = source_colors) +
   scale_y_continuous(
     expand = expansion(mult = c(0, 0.05)),
     limits = c(0, NA)
   ) +
   scale_x_discrete(expand = c(0, 0))+
   labs(
     title = "Type LN events / Total LN Events", 
     x = "", 
     y = "%"
   ) +
   theme_bw() +
   facet_wrap(~factor(Classification, levels = c("SLN", "DLN", "TLN", "4More")),
              nrow = 4, scales = "free_y") +
   theme(
     text = element_text(size = 14, family = "monospace"),
     axis.text.x = element_text(angle = 45, hjust = 1, family = "monospace"),
     axis.text.y = element_text(family = "monospace"),
     legend.position = c(0.98, 0.98),
     legend.justification = c("right", "top"),
     legend.background = element_rect(fill = "white", color = "black"),
     legend.title = element_blank(),
     legend.text = element_text(family = "monospace")
   )+
   guides(fill = "none")

