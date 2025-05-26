library(OpenLand)
library(tidyverse)
library(raster)
library(tmap)
library(lubridate)

############################ Archivos / Inputs ###############################

piura_utm <- shapefile("../Datos/SHAPES/CUENCA_PIURA_Alta_MediaBaja_UTM17.shp")

modis <- brick("./MODIS_Homogen_0123vf.tif") %>% mask(.,piura_utm)
# modis <- brick(modis) %>% mask(.,piura_utm)
names(modis) <- paste0("Y_",2001:2023)
modis <- modis[[1:22]]

cci <- brick("./CCI_Homogen_0122vf.tif") %>% mask(.,piura_utm)
# cci <- brick(cci) %>% mask(.,piura_utm)
names(cci) <- paste0("Y_",2001:2022)

categ <- c( 'Other',"Forest",'Non Forest Veg',"Cropland", "Urban","Water", "Bare Soil")
categ_ID <- seq(0,66,11)
col_cat <- c( 'lightgrey',"#1c6027","#d3e382", 
              "#f3d1ef","#ea071a", "#b3f1f7",'#f0e6c6')

table_cont <- function(ras,res,product){
  
  lc <- contingencyTable(ras, pixelresolution = res)
  lc$tb_legend <- lc$tb_legend %>% arrange(categoryValue)
  sel <- categ_ID %in% lc$tb_legend$categoryValue # Obtiene las categorias que se incluyen
  lc$tb_legend$categoryName <- factor(categ[sel],levels = categ[sel]) # Editando la categoria nombre
  lc$tb_legend$color <- col_cat[sel] # Colores en el mismo orden que la leyenda
  return(lc)
  
}

############################## RASTERS 22 years A MB ###############################

cci_mb <- table_cont(mask(cci[[c(1,11,22)]],piura_utm[piura_utm$Subcuencas == 'MediaBaja',]),300,'CCI')
cci_a <- table_cont(mask(cci[[c(1,11,22)]],piura_utm[piura_utm$Subcuencas == 'Alta',]),300,'CCI')

mod_mb <- table_cont(mask(modis[[c(1,11,22)]],piura_utm[piura_utm$Subcuencas == 'MediaBaja',]),500,'MODIS')
mod_a <- table_cont(mask(modis[[c(1,11,22)]],piura_utm[piura_utm$Subcuencas == 'Alta',]),500,'MODIS')

#################################### TEMPORAL GRAPHS ################################




################################## NET GROSS #########################################

## modis ##

netgrossplot(dataset = mod_mb$lulc_Onestep,
             legendtable = mod_mb$tb_legend,
             xlab = NULL,
             ylab = bquote("Area (" ~ km^2 ~ ")"),
             legend_title = NULL,
             changesLabel = c(GC = "Gross Changes", NG = "Net Gain", NL = "Net Loss"),
             color = c(GC = "#dcdcdc", NG = '#bdc473', NL = "#c47392"))+
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position = "top") 

ggsave('./MODIS0122NETOvf_GROSS_MB.png',fig1,width=12,height = 12,units = 'cm',dpi=300)

netgrossplot(dataset = mod_a$lulc_Onestep,
             legendtable = mod_a$tb_legend,
             xlab = NULL,
             ylab = bquote("Area (" ~ km^2 ~ ")"),
             legend_title = NULL,
             changesLabel = c(GC = "Gross Changes", NG = "Net Gain", NL = "Net Loss"),
             color = c(GC = "#dcdcdc", NG = '#bdc473', NL = "#c47392"))+
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position = "top") 
ggsave('./MODIS0122NETOvf_GROSS_ALTA.png',fig2,width=12,height = 12,units = 'cm',dpi=300)
## CCI ##
netgrossplot(dataset = cci_mb$lulc_Onestep,
             legendtable = cci_mb$tb_legend,
             xlab = NULL,
             ylab = bquote("Area (" ~ km^2 ~ ")"),
             legend_title = NULL,
             changesLabel = c(GC = "Gross Changes", NG = "Net Gain", NL = "Net Loss"),
             color = c(GC = "#dcdcdc", NG = '#bdc473', NL = "#c47392"))+
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position = "top") 
ggsave(paste0('./CCI0122NETOvf_GROSS_MB.png'),fig3,width=12,height = 12,units = 'cm',dpi=300)

netgrossplot(dataset = cci_a$lulc_Onestep,
             legendtable = cci_a$tb_legend,
             xlab = NULL,
             ylab = bquote("Area (" ~ km^2 ~ ")"),
             legend_title = NULL,
             changesLabel = c(GC = "Gross Changes", NG = "Net Gain", NL = "Net Loss"),
             color = c(GC = "#dcdcdc", NG = '#bdc473', NL = "#c47392")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position = "top") 
ggsave(paste0('./CCI0122NETOvf_GROSS_ALTA.png'),fig4,width=12,height = 12,units = 'cm',dpi=300)

########################################################################################
plot_lulc_g <- function(ras,basin,res,shp,prod){
  
  if (basin %in% c('Alta','MediaBaja')){
    cci_p <- table_cont(mask(ras,shp[shp$Subcuencas == basin,]), res)
    
  }else{
    cci_p <- table_cont(ras, res) # cci [[3:7]]
  }
  
  theme_set(theme_classic(base_size = 12))
  
  # n -> gains ;  m <- losses
  testSL <- intensityAnalysis(dataset = cci_p,
                              category_n = "Bare", category_m = "Cropland")
  
  df <- testSL$interval_lvl@intervalData
  coefi <- round(df$U[[1]],1)
  la<- rev(df$Period)
  fig2 <- df %>% mutate(Year=substr(Period,6,9),Changes=if_else(St>U,'Fast','Slow')) %>% #Progresivo / acelerado
    ggplot(.,aes(factor(Year,labels =la),St,fill=Changes))+
    geom_col()+
    scale_fill_manual(values=c('#c47392','#bdc473'))+
    geom_hline(yintercept = coefi,linetype='dotted')+ 
    annotate('text',label = paste0("U=",coefi,'%'),x = 5, y = coefi+0.5, size = 4, colour = "black")+
    #scale_y_continuous(breaks = seq(0,3,.5),expand = c(0,0))+
    scale_y_continuous(expand = c(0,0),limits=c(0,10),breaks = seq(0,10,1))+ #breaks = seq(0,15,2.5)
    labs(y='Areal Annual Change (%)',x='')+theme_classic()+
    theme(text=element_text(size= 14),
          axis.text.x = element_text(size=11,angle = 90),
          legend.position = c(.94, .8),
          legend.text = element_text (size=12),
          legend.title = element_text (size=14)) 
  
  ggsave(paste0('./',prod,'0122vf_','SERIE_',basin,'.png'),
         fig2,width=17,height = 10,units = 'cm',dpi=300)
}

#plot_lulc_g(modis[[1:22]],'ALL',500,piura_utm,'MODIS')
plot_lulc_g(modis,'Alta',500,piura_utm,'MODIS')
plot_lulc_g(modis,'MediaBaja',500,piura_utm,'MODIS')

#plot_lulc_g(cci,'ALL',300,piura_utm,'CCI')
plot_lulc_g(cci,'Alta',300,piura_utm,'CCI')
plot_lulc_g(cci,'MediaBaja',300,piura_utm,'CCI')

#######################################################################
######################### MODIS - PAPER ##########################################

# modis_p <- table_cont(mask(modis[[1:22]],piura_utm[piura_utm$Subcuencas == 'MediaBaja',]), 500)
modis_p <- table_cont(mask(modis[[1:22]],piura_utm), 500)

chordDiagramLand(dataset = modis_p$lulc_Onestep,
                 legendtable = modis_p$tb_legend,legendsize = 0.7,legposition = c(x=-1.3,y=-0.3)) 

cci_p <- table_cont(mask(cci[[1:22]],piura_utm), 300)

chordDiagramLand(dataset = cci_p$lulc_Onestep,
                 legendtable = modis_p$tb_legend,legendsize = 0.7,legposition = c(x=-1.3,y=-0.3))

ggsave('./MODIS/MODIS0122v2_chorddiagram.png',
       width=17,height = 17,units = 'cm',dpi=300)

dataset <- modis_p$lulc_Multistep
legendtable <- modis_p$tb_legend
area_km2 <- modis_p$totalArea[1,1]

datachange <- dataset %>% 
  left_join(legendtable, by = c(From = "categoryValue")) %>% 
  left_join(legendtable, by = c(To = "categoryValue")) %>% 
  dplyr::select(-c(From, To)) %>% 
  rename(From = "categoryName.x", To = "categoryName.y", colorFrom = "color.x", colorTo = "color.y")

#areaif <- ifelse(isTRUE(area_km2), "km2", "QtPixel")
areaif <- ifelse(isTRUE(area_km2), "QtPixel", "km2")

datanual_a <- datachange %>% 
  group_by(yearTo, To) %>% summarise(area = sum(!!as.name(areaif))) %>% 
  rename(Year = "yearTo", lulc = "To") %>% 
  rbind(datachange[datachange$yearFrom == first(datachange$yearFrom), ] %>% 
          group_by(yearFrom, From) %>% 
          summarise(area = sum(!!as.name(areaif))) %>% 
          rename(Year = "yearFrom", lulc = "From")) %>% 
  mutate(Cuenca='MediaBaja')

BD_LU <- datanual_a #%>% bind_rows(datanual_m) %>% bind_rows(datanual_b) 



DF <- BD_LU %>% group_by(lulc,Cuenca) %>% 
  mutate(anom_median=area-median(area),
         anom_avg=area-mean(area)) %>% 
  ungroup()

write_csv(DF,'../datos/BD_y_LULC_MODIS0120v2_basin.csv')

DF %>% filter(Cuenca=='MediaBaja' & anom_median!=0) %>% 
  ggplot(., aes(factor(Year), anom_median,fill=lulc)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~lulc,nrow = 8,scales = 'free_y')+
  scale_fill_manual(values = c("#55985E", "#E4B7C8","#BDC474","#C39D55")) +
  #  scale_fill_manual(values = c("#55985E", "#E4B7C8","#BDC474","#DA5457","#F0E1AB")) +
  #  scale_fill_manual(values = c("#55985E", "#E4B7C8","#BDC474","#75B5C6","#C39D55","#F0E1AB","#9CCDE4"))+
  xlab('') + ylab('Area km²') + 
  ggtitle('Upper Basin') + 
  theme_bw(base_size = 10) + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'none',
        axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5,size=10),
        axis.text.y = element_text(size=10)) -> fig_lc

ind <- indices %>% 
  mutate(Year=if_else(month>11,year+1,year)) %>%
  mutate(DJF=case_when(month %in% c(12,1,2) ~ 'DJF'),
         FMA=case_when(month %in% c(2:4) ~ 'FMA') ) %>% 
  filter(month %in% c(12,1,2,3,4)) %>%
  pivot_longer(DJF:FMA,names_to = 'periodos', values_to = 'periodo') %>% 
  group_by(Year,periodo) %>%
  summarise(Eid=mean(E_index),Cid=mean(C_index)) %>%
  filter(Year<2023) %>% 
  mutate(Year=as.integer(Year)) %>% 
  pivot_longer(Eid:Cid, values_to = 'Valor',names_to = 'Indice' ) %>% 
  mutate(val_pos = if_else(Valor>0,Valor,0),
         val_neg = if_else(Valor<0,Valor,0))

ind %>% mutate(sele=case_when(periodo=='DJF' & Indice=='Cid' ~ 'DJF C_index',
                              periodo=='FMA' & Indice=='Eid' ~ 'FMA E_index')) %>%
  filter(sele!='NA') %>% ungroup() %>% 
  ggplot(.) +
  geom_bar(aes(Year,val_pos),stat='identity', fill = "#c47373") +
  geom_bar(aes(Year,val_neg), stat='identity',fill = "#73a5c4") +
  scale_y_continuous(breaks = seq(-2,4,1)) +
  scale_x_continuous(breaks=2001:2022,expand=c(0,0)) +
  labs(x='',y='Index') + 
  facet_wrap(~sele, nrow=2)+
  theme_bw(base_size = 12) + 
  theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5,size=12),
        axis.text.y = element_text(size=10)) -> fig_inds

arr_plot <- grid.arrange(fig_lc, fig_inds, ncol = 1, heights = c(4, 2))
ggsave("Upper_Basin_med_Anomalies_LULC_ECindxs.png", arr_plot, dpi = 300)

aux <- ind %>% mutate(sele=case_when(periodo=='DJF' & Indice=='Cid' ~ 'DJF C_index',
                                     periodo=='FMA' & Indice=='Eid' ~ 'FMA E_index')) %>%
  filter(sele!='NA') %>% ungroup() %>% dplyr::select(Year,Valor,sele)

BD_AUX <- DF %>% full_join(aux,by='Year')

BD_AUX  %>% 
  ggplot(.,aes(anom_median,Valor,color=lulc)) + 
  geom_point()+
  facet_grid(Cuenca~sele,scales = 'free')+
  scale_color_manual(values = c(c( "#55985E", "#E4B7C8","#BDC474","#75B5C6", "#C39D55",'lightgrey', "#DA5457","#F0E1AB", "#9CCDE4")))+
  geom_hline(yintercept=0, color="#cfcfcf")+
  geom_text_repel(
    aes(label=Year),
    box.padding = 0.2,nudge_y = -0.02,nudge_x = -0.02)+
  theme_dark()




# Null Hypothesis (H0): The mean anomaly is equal to zero
# Alternative Hypothesis (H1): The mean anomaly is not equal to zero

# Significance Level (alpha)
alpha <- 0.05

# Sample Mean and Standard Deviation
mean_anomaly <- mean(anomalies)
sd_anomaly <- sd(anomalies)

# Calculate t-statistic and p-value
t_test_result <- t.test(anomalies, mu = 0)

# Print the results
cat("T-Test Results:\n")
cat("t-statistic:", t_test_result$statistic, "\n")
cat("p-value:", t_test_result$p.value, "\n")

# Make a Decision
if (t_test_result$p.value < alpha) {
  cat("Reject the null hypothesis: Mean anomaly is significantly different from zero.\n")
} else {
  cat("Fail to reject the null hypothesis: No significant difference from zero.\n")
}

modis_p <- table_cont(cci[[c(1,4,7,10,13,16,19,22)]], 300)


sankeyLand(dataset = modis_p$lulc_Multistep,
                 legendtable = modis_p$tb_legend) 

barplotLand(dataset = modis_p$lulc_Multistep, 
            legendtable = modis_p$tb_legend,
            xlab = "MODIS at Upper Basin",
            ylab = bquote("Area (" ~ km^2~ ")"),
            area_km2 = TRUE) 

####################################################################
######################## CAMBIOS AREALES CCI###############################
cci_a <- mask(cci,piura_utm)
testacc <- acc_changes(cci_a)

tmap_options(max.raster = c(plot = 41711112, view = 41711112))

acc_map <- tmap::tm_shape(testacc[[1]]) +
  tmap::tm_raster(
    style = "cat",
    labels = c(
      paste0(testacc[[2]]$PxValue[1],  " (", round(testacc[[2]]$Percent[1], 1), "%)"),
      paste0(testacc[[2]]$PxValue[2],  " (", round(testacc[[2]]$Percent[2], 1), "%)"),
      paste0(testacc[[2]]$PxValue[3], " (", round(testacc[[2]]$Percent[3], 1), "%)"),
      paste0(testacc[[2]]$PxValue[4], " (", round(testacc[[2]]$Percent[4], 1), "%)")
    ),
    palette = c("#ffffff",'#edd9a3','#edd9a3',
                '#ea4f88','#692a99'
    ), #'#c6ce00','#e63244'
    title = "# of changed pixels \n2001 - 2022"
  ) +
  tmap::tm_legend(
    position = c(1.01, 0.25),
    legend.title.size = 0.9,
    legend.title.fontface = "bold",
    legend.text.size = 0.8,
    legend.bg.color='white',
    legend.bg.alpha=0.8
  ) +
  tm_shape(piura_utm) + tm_borders(lwd=2)+
  tmap::tm_compass(type = "arrow",
                   position = c("left", "top"),
                   size = 2) +
  tmap::tm_scale_bar(
    breaks = c(seq(0, 30, 10)),
    position = c(0.74, 0.001),
    text.size = 0.8
  ) +
  # tmap::tm_credits(
  #   paste0(
  #     "Case of Study site",
  #     "\nAccumulate changes from 2002 to 2014",
  #     "\nData create with OpenLand package",
  #     "\nLULC derived from Embrapa Pantanal, Instituto SOS Pantanal, and WWF-Brasil 2015."
  #   ),
  #   size = 0.7,
  #   position = c(0.01, -0, 01)
  # ) +
  tmap::tm_graticules(
    n.x = 5,
    n.y = 5,
    lines = FALSE,
    #alpha = 0.1
    labels.rot = c(0, 90),
    labels.size=1.2
  ) +
  tmap::tm_layout(inner.margins = c(0.02, 0.02, 0.02, 0.02))

acc_map


tmap::tmap_save(acc_map,
                filename = paste0("./acc_cci_0122.png"),
                width = 12,
                height = 7)

###############################################################################
modis_a <- mask(modis,piura_utm)
testacc <- acc_changes(modis_a[[1:22]])

tmap_options(max.raster = c(plot = 41711112, view = 41711112))

acc_map <- tmap::tm_shape(testacc[[1]]) +
  tmap::tm_raster(
    style = "cat",
    labels = c(
      paste0(testacc[[2]]$PxValue[1],  " (", round(testacc[[2]]$Percent[1], 1), "%)"),
      paste0(testacc[[2]]$PxValue[2],  " (", round(testacc[[2]]$Percent[2], 1), "%)"),
      paste0(testacc[[2]]$PxValue[3], " (", round(testacc[[2]]$Percent[3], 1), "%)"),
      paste0(testacc[[2]]$PxValue[4], " (", round(testacc[[2]]$Percent[4], 1), "%)"),
      paste0(testacc[[2]]$PxValue[5], " (", round(testacc[[2]]$Percent[5], 1), "%)"),
      paste0(testacc[[2]]$PxValue[6], " (", round(testacc[[2]]$Percent[6], 1), "%)"),
      paste0(testacc[[2]]$PxValue[7], " (", round(testacc[[2]]$Percent[7], 1), "%)"),
      paste0(testacc[[2]]$PxValue[8], " (", round(testacc[[2]]$Percent[8], 1), "%)"),
      paste0(testacc[[2]]$PxValue[9], " (", round(testacc[[2]]$Percent[9], 1), "%)"),
      paste0(testacc[[2]]$PxValue[10], " (", round(testacc[[2]]$Percent[10], 1), "%)"),
      paste0(testacc[[2]]$PxValue[11], " (", round(testacc[[2]]$Percent[11], 1), "%)"),
      paste0(testacc[[2]]$PxValue[12], " (", round(testacc[[2]]$Percent[12], 1), "%)"),
      paste0(testacc[[2]]$PxValue[13], " (", round(testacc[[2]]$Percent[13], 1), "%)"),
      paste0(testacc[[2]]$PxValue[14], " (", round(testacc[[2]]$Percent[14], 1), "%)"),
      paste0(testacc[[2]]$PxValue[15], " (", round(testacc[[2]]$Percent[15], 1), "%)"),
      paste0(testacc[[2]]$PxValue[16], " (", round(testacc[[2]]$Percent[16], 1), "%)"),
      paste0(testacc[[2]]$PxValue[17], " (", round(testacc[[2]]$Percent[17], 1), "%)")
),
    palette = c("#ffffff",'#edd9a3','#edd9a3','#f2637f',
                '#f2637f','#f2637f','#692a99',
                '#692a99','#692a99','#692a99',
                '#692a99','#692a99','#692a99',
                '#692a99','#692a99','#692a99','#692a99'
    ), #'#c6ce00','#e63244'
    title = "# of changed pixels \n2001 - 2022"
  ) +
  tmap::tm_legend(
    position = c(1.01, 0.25),
    legend.title.size = 0.9,
    legend.title.fontface = "bold",
    legend.text.size = 0.8,
    legend.bg.color='white',
    legend.bg.alpha=0.8
  ) +
  tm_shape(piura_utm) + tm_borders(lwd=2)+
  tmap::tm_compass(type = "arrow",
                   position = c("left", "top"),
                   size = 2) +
  tmap::tm_scale_bar(
    breaks = c(seq(0, 30, 10)),
    position = c(0.76, 0.001),
    text.size = 0.8
  ) +
  # tmap::tm_credits(
  #   paste0(
  #     "Case of Study site",
  #     "\nAccumulate changes from 2002 to 2014",
  #     "\nData create with OpenLand package",
  #     "\nLULC derived from Embrapa Pantanal, Instituto SOS Pantanal, and WWF-Brasil 2015."
  #   ),
  #   size = 0.7,
  #   position = c(0.01, -0, 01)
  # ) +
  tmap::tm_graticules(
    n.x = 5,
    n.y = 5,
    lines = FALSE,
    #alpha = 0.1
    labels.rot = c(0, 90),
    labels.size=1.2
  ) +
  tmap::tm_layout(inner.margins = c(0.02, 0.02, 0.02, 0.02))

acc_map

tmap::tmap_save(acc_map,
                filename = paste0("./acc_modis_0122.png"),
                width = 12,
                height = 7)

summary_dir(raster::unstack(cci))

###############################################################

cci_mb <- table_cont(mask(cci,piura_utm[piura_utm$Subcuencas == 'MediaBaja',]),300)
cci_a <- table_cont(mask(cci,piura_utm[piura_utm$Subcuencas == 'Alta',]),300)

mod_mb <- table_cont(mask(modis[[1:22]],piura_utm[piura_utm$Subcuencas == 'MediaBaja',]),500)
mod_a <- table_cont(mask(modis[[1:22]],piura_utm[piura_utm$Subcuencas == 'Alta',]),500)

####################################################################
dataset <- mod_a$lulc_Multistep
legendtable <- mod_a$tb_legend
area_km2 <- mod_a$totalArea[1,1]

datachange <- dataset %>% 
  left_join(legendtable, by = c(From = "categoryValue")) %>% 
  left_join(legendtable, by = c(To = "categoryValue")) %>% 
  dplyr::select(-c(From, To)) %>% 
  rename(From = "categoryName.x", To = "categoryName.y", colorFrom = "color.x", colorTo = "color.y")

#areaif <- ifelse(isTRUE(area_km2), "km2", "QtPixel")
areaif <- ifelse(isTRUE(area_km2), "QtPixel", "km2")

datanual_a <- datachange %>% 
  group_by(yearTo, To) %>% summarise(area = sum(!!as.name(areaif))) %>% 
  rename(Year = "yearTo", lulc = "To") %>% 
  rbind(datachange[datachange$yearFrom == first(datachange$yearFrom), ] %>% 
          group_by(yearFrom, From) %>% 
          summarise(area = sum(!!as.name(areaif))) %>% 
          rename(Year = "yearFrom", lulc = "From")) %>% 
  mutate(Cuenca='Upper')

BD_MB <- datanual_a %>% mutate(Year=as.numeric(Year)) %>% arrange(Year)
BD_A <- datanual_a %>% mutate(Year=as.numeric(Year)) %>% arrange(Year)

BD_LU <- BD_MB %>% bind_rows(BD_A)

BD_LU %>% group_by(Cuenca,Year) %>% summarise(AT=sum(area)) %>% arrange(AT)

DF <- BD_LU %>% group_by(lulc,Cuenca) %>% 
  mutate(anom_median=area-median(area),
         anom_avg=area-mean(area)) %>% 
  ungroup()

write_csv(BD_LU,'LULC_PERIODOS_MODIS_AMBvf.csv')

########################## MODIS - IA CATEGORY PER BASIN #############################
# n -> gains ;  m <- losses

#ALTAAAA
#modis_a <- table_cont(mask(modis[[c(1:22)]],piura_utm[piura_utm$Subcuencas == 'Alta',]),500)
testSL <- intensityAnalysis(dataset = mod_a,
                            category_n = "Non Forest Veg", category_m = "Cropland")

theme_set(theme_classic(base_size = 16))

plot(testSL$category_lvlLoss,
     labels = c(leftlabel = bquote("Loss Area (" ~km^2~ ")"),
                rightlabel = "Intensity Loss (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform intensity"), 
     leg_curv = c(x = 5, y = .5), #x=1,y=.5
     fontsize_ui = 14)+theme_bw()


plot(testSL$category_lvlGain,
     labels = c(leftlabel = bquote("Gain Area (" ~km^2~ ")"),
                rightlabel = "Intensity Gain (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform intensity"), 
     leg_curv = c(x = 1, y = .5),
     fontsize_ui = 16)

# MEDIA-BAJA m> # MODIS>CROPLAND

#modis_mb <- table_cont(mask(modis[[c(1:22)]],piura_utm[piura_utm$Subcuencas == 'MediaBaja',]),500)
testSL <- intensityAnalysis(dataset = mod_mb,
                            category_n = "Bare Soil", category_m = "Cropland")

theme_set(theme_classic(base_size = 16))

plot(testSL$category_lvlLoss,
     labels = c(leftlabel = bquote("Loss Area (" ~km^2~ ")"),
                rightlabel = "Intensity Loss (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform intensity"), 
     leg_curv = c(x = 5, y = .5), #x=1,y=.5
     fontsize_ui = 14)+theme_bw()


plot(testSL$category_lvlGain,
     labels = c(leftlabel = bquote("Gain Area (" ~km^2~ ")"),
                rightlabel = "Intensity Gain (%)"),
     marginplot = c(.3, .5), labs = c("Categories", "Uniform intensity"), 
     leg_curv = c(x = 0.5, y = 1.1),
     fontsize_ui = 14)

########################## CCI - IA CATEGORY PER BASIN #############################
# n -> gains ;  m <- losses

#ALTAAAA
#cci_a <- table_cont(mask(cci[[c(1:22)]],piura_utm[piura_utm$Subcuencas == 'Alta',]),300)
testSL <- intensityAnalysis(dataset = cci_a,
                            category_n = "Forest", category_m = "Cropland")

theme_set(theme_classic(base_size = 16))

plot(testSL$category_lvlLoss,
     labels = c(leftlabel = bquote("Loss Area (" ~km^2~ ")"),
                rightlabel = "Intensity Loss (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform intensity"), 
     leg_curv = c(x = .3, y = .5), #x=1,y=.5
     fontsize_ui = 14)+theme_bw()


plot(testSL$category_lvlGain,
     labels = c(leftlabel = bquote("Gain Area (" ~km^2~ ")"),
                rightlabel = "Intensity Gain (%)"),
     marginplot = c(.3, .5), labs = c("Categories", "Uniform intensity"), 
     leg_curv = c(x = 0.4, y = 0.5),
     fontsize_ui = 14)

# MEDIA-BAJA m> #CCI>FOREST // MODIS>CROPLAND

#cci_mb <- table_cont(mask(cci[[c(1:22)]],piura_utm[piura_utm$Subcuencas == 'MediaBaja',]),300)
testSL <- intensityAnalysis(dataset = cci_mb,
                            category_n = "Bare Soil", category_m = "Cropland")

theme_set(theme_classic(base_size = 16))

plot(testSL$category_lvlLoss,
     labels = c(leftlabel = bquote("Loss Area (" ~km^2~ ")"),
                rightlabel = "Intensity Loss (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform intensity"), 
     leg_curv = c(x = .1, y = .5), #x=1,y=.5
     fontsize_ui = 14)+theme_bw()


plot(testSL$category_lvlGain,
     labels = c(leftlabel = bquote("Gain Area (" ~km^2~ ")"),
                rightlabel = "Intensity Gain (%)"),
     marginplot = c(.3, .5), labs = c("Categories", "Uniform intensity"), 
     leg_curv = c(x = 0.8, y = 1.1),
     fontsize_ui = 14)
##############################################################################
aa_a <- mod_a
testSL <- intensityAnalysis(dataset = aa_a,
                            category_n = "Bare Soil", category_m = "Cropland")
#sankeyLand(modis_a$lulc_Multistep,legendtable = modis_a$tb_legend)
barplotLand(aa_a$lulc_Multistep,legendtable = aa_a$tb_legend,ylab = 'Area (km2)',area_km2 = TRUE)
chordDiagramLand(a_a$lulc_Onestep,legendtable = aa_a$tb_legend)

plot(testSL$interval_lvl,
     labels = c(leftlabel = "Interval Change Area (%)",
                rightlabel = "Annual Change Area (%)"),
     marginplot = c(-8, 0), labs = c("Changes", "Uniform Rate"), 
     leg_curv = c(x = 2/10, y = 3/10))

##############################MODIS TRANSITIONS###################################
# mod_a <- table_cont(mask(modis[[c(1,4,7,10,13,16,19,22)]],piura_utm[piura_utm$Subcuencas == 'Alta',]),500)

# n -> gains ;  m <- losses
#ALTAAAA
#modis_a <- table_cont(mask(modis[[1:22]],piura_utm[piura_utm$Subcuencas == 'Alta',]),500)
testSL <- intensityAnalysis(dataset = mod_a,
                            category_n = "Non Forest Veg", category_m = "Cropland")
theme_set(theme_classic(base_size = 16))
plot(testSL$transition_lvlGain_n,
     labels = c(leftlabel = bquote("Gain of Non-Forest (" ~ km^2 ~ ")"),
                rightlabel = "Intensity Gain of Non-Forest (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"), 
     leg_curv = c(x = -2, y = -.7),
     fontsize_ui = 14)

plot(testSL$transition_lvlLoss_m,
     labels = c(leftlabel = bquote("Loss of Cropland (" ~ km^2 ~ ")"),
                rightlabel = "Intensity Loss of Cropland (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"), 
     leg_curv = c(x = -.06, y = .7),
     fontsize_ui = 14)

# MEDIA-BAJA m> #CCI>FOREST // MODIS>CROPLAND
mod_mb <- table_cont(mask(modis[[1:22]],piura_utm[piura_utm$Subcuencas == 'MediaBaja',]),500)

testSL <- intensityAnalysis(dataset = mod_mb,
                            category_n = "Bare Soil", category_m = "Cropland")

plot(testSL$transition_lvlGain_n,
     labels = c(leftlabel = bquote("Gain of Bare Soil (" ~ km^2 ~ ")"),
                rightlabel = "Intensity Gain of Bare Soil (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"), 
     leg_curv = c(x = -0.4, y = 0.3),
     fontsize_ui = 16)

plot(testSL$transition_lvlLoss_m,
     labels = c(leftlabel = bquote("Loss of Cropland (" ~ km^2 ~ ")"),
                rightlabel = "Intensity Loss of Cropland (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"), 
     leg_curv = c(x = 0.05, y = 5/10),
     fontsize_ui = 16)

################################# CCI TRANSITION ####################################
#cci_a <- table_cont(mask(cci[[1:22]],piura_utm[piura_utm$Subcuencas == 'Alta',]),300)

# n -> gains ;  m <- losses
#ALTAAAA
testSL <- intensityAnalysis(dataset = cci_a,
                            category_n = "Forest", category_m = "Cropland")

plot(testSL$transition_lvlGain_n,
     labels = c(leftlabel = bquote("Gain of Forest (" ~ km^2 ~ ")"),
                rightlabel = "Intensity Gain of Forest (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"), 
     leg_curv = c(x = 0.1, y = 5/10),
     fontsize_ui = 14)

plot(testSL$transition_lvlLoss_m,
     labels = c(leftlabel = bquote("Loss of Cropland (" ~ km^2 ~ ")"),
                rightlabel = "Intensity Loss of Cropland (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"), 
     leg_curv = c(x = 0.1, y = 5/10),
     fontsize_ui = 14)

# MEDIA-BAJA m> #CCI>FOREST // MODIS>CROPLAND
#cci_mb <- table_cont(mask(cci[[1:22]],piura_utm[piura_utm$Subcuencas == 'MediaBaja',]),300)

testSL <- intensityAnalysis(dataset = cci_mb,
                            category_n = "Bare Soil", category_m = "Non Forest Veg")

plot(testSL$transition_lvlGain_n,
     labels = c(leftlabel = bquote("Gain of Bare Soil (" ~ km^2 ~ ")"),
                rightlabel = "Intensity Gain of Bare Soil (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"), 
     leg_curv = c(x = 0.1, y = 5/10),
     fontsize_ui = 16)

plot(testSL$transition_lvlLoss_m,
     labels = c(leftlabel = bquote("Loss of Cropland (" ~ km^2 ~ ")"),
                rightlabel = "Intensity Loss of Cropland (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"), 
     leg_curv = c(x = 0.1, y = 5/10),
     fontsize_ui = 16)



testSL <- intensityAnalysis(dataset = mod_mb,
                            category_n = "Coastal Desert", category_m = "Cropland")

plot(testSL$transition_lvlLoss_m,
     labels = c(leftlabel = bquote("Loss of Cropland (" ~ km^2 ~ ")"),
                rightlabel = "Intensity Loss of Cropland (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"), 
     leg_curv = c(x = 5/10, y = 5/10))

#######################################################################################
mod_mb <- table_cont(mask(modis,piura_utm[piura_utm$Subcuencas == 'MediaBaja',]),500)

testmb <- intensityAnalysis(dataset = mod_mb,
                            category_n = "Bare Soil", 
                            category_m = "Cropland")
DF1 <- testmb$lulc_table %>% mutate(Producto='MODIS',Cuenca='Lower-Middle')

mod_a <- table_cont(mask(modis,piura_utm[piura_utm$Subcuencas == 'Alta',]),500)

testa <- intensityAnalysis(dataset = mod_a,
                            category_n = "Non Forest Veg", 
                            category_m = "Cropland")
DF2 <- testa$lulc_table %>% mutate(Producto='MODIS',Cuenca='Upper')

cci_mb <- table_cont(mask(cci,piura_utm[piura_utm$Subcuencas == 'MediaBaja',]),300)

IAcci_mb <- intensityAnalysis(dataset = cci_mb,
                            category_n = "Bare Soil", category_m = "Cropland")
DF3 <- IAcci_mb$lulc_table %>% mutate(Producto='CCI',Cuenca='Lower-Middle')

cci_a <- table_cont(mask(cci,piura_utm[piura_utm$Subcuencas == 'Alta',]),300)

IAcci_a <- intensityAnalysis(dataset = cci_a,
                            category_n = "Forest", category_m = "Cropland")
DF4 <- IAcci_a$lulc_table %>% mutate(Producto='CCI',Cuenca='Upper')

DF <- DF1 %>% bind_rows(DF2,DF3,DF4)

DF <- DF %>% separate('Period',into=c('Year0','Year1'),sep='-')

DF0 <- DF %>% group_by(Producto,Cuenca,Year0,From) %>% summarise(Area=sum(km2))
DFf <- DF %>% group_by(Producto,Cuenca,Year1,To) %>% summarise(Area=sum(km2))

DF0<-DF0 %>% bind_rows(DFf %>% filter(Year1=='2022') %>% rename(Year0=Year1,From=To))

DF0 <- DF0 %>% mutate(Year0=as.numeric(Year0)) %>% rename(year=Year0)
DF0 %>% group_by(Producto,Cuenca,year) %>% summarise(Total=sum(Area)) -> DFF

Ind <- read_csv('../Datos/datos_ind_E_C.csv') %>% filter(year>1998 & year<2024)

Ind <- Ind %>% mutate(year1=if_else(month==12,year+1,year))

Ind_DEF <- Ind %>% filter(month %in% c(12,1,2)) %>%
  group_by(year1) %>% summarise(E=mean(E_index),C=mean(C_index),
                                ICENr=mean(ICENr), ICEN=mean(ICEN), 
                                ICEN_9120=mean(ICEN_9120)) %>% 
  rename(year=year1)

Ind_y <- Ind %>% group_by(year) %>% summarise(E=mean(E_index),C=mean(C_index),
                                              ICENr=mean(ICENr), ICEN=mean(ICEN), 
                                              ICEN_9120=mean(ICEN_9120))
Clim <- read_csv('../Datos/PP_TX_TN_MediaBaja_Alta.csv') %>%
  pivot_longer(4:13,names_to = 'varclim',values_to = 'valclim') %>%
  separate(varclim,into = c('Var','Cuenca'),sep='_') %>% 
  mutate(Cuenca=if_else(Cuenca=='MediaBaja','Lower-Middle','Upper')) %>% 
  pivot_wider(names_from = Var,values_from = valclim)

Clim_season <- Clim %>% mutate(year1=if_else(month==12,year+1,year)) %>% 
  mutate(season=if_else(month>5 & month<12,'DRY','WET')) %>%
  group_by(year1,season,Cuenca) %>% 
  summarise(PP=sum(PP),Txx=mean(Txx),Txp=mean(Txp),Tnp=mean(Tnp),Tnn=mean(Tnn)) %>% 
  rename(year=year1)

DF0 <- DF0 %>% pivot_wider(names_from = From,values_from = Area)
Ind_y

DF0 %>% full_join(.,Ind_DEF,by='year') %>% full_join(.,Clim_season,by=c('year','Cuenca')) -> BD

BD %>% ungroup() %>% mutate(TOTAL = rowSums(dplyr::select(., Forest,`Non Forest Veg`, `Bare Soil`, Cropland, Urban, Water, Other), na.rm = TRUE)) -> Bd

write_csv(Bd,'BD_corr_hydroclimVF.csv')

Bd <- read_csv('BD_corr_hydroclimVF.csv')

# Calcular la proporción de cada categoría de cobertura respecto al área total de la cuenca
BD <- Bd %>%
  mutate(Forest_percent = (Forest / TOTAL) * 100,
         Cropland_percent = (Cropland / TOTAL) * 100,
         Bare_percent = (`Bare Soil` / TOTAL) * 100,
         Urban_percent = (Urban / TOTAL) * 100,
         Water_percent = (Water / TOTAL) * 100,
         NonForest_percent = (`Non Forest Veg` / TOTAL) * 100)

BD1 <- BD %>% filter(Producto=='CCI' & Cuenca=='Upper' & season=='WET') %>% 
  mutate(E_lag1 = lag(E, 1),
                E_lag2 = lag(E, 2),
                E_lead1 = lead(E, 1),
                E_lead2 = lead(E, 2),
                C_lag1 = lag(C, 1),
                C_lag2 = lag(C, 2),
                C_lead1 = lead(C, 1),
                C_lead2 = lead(C, 2),
         ICENr_lag1 = lag(ICENr, 1),
         ICENr_lag2 = lag(ICENr, 2),
         ICENr_lead1 = lead(ICENr, 1),
         ICENr_lead2 = lead(ICENr, 2),
         ICEN_9120_lag1 = lag(ICEN_9120, 1),
         ICEN_9120_lag2 = lag(ICEN_9120, 2),
         ICEN_9120_lead1 = lead(ICEN_9120, 1),
         ICEN_9120_lead2 = lead(ICEN_9120, 2),
                PP_lag1 = lag(PP, 1),
                PP_lag2 = lag(PP, 2),
                PP_lead1 = lead(PP, 1),
                PP_lead2 = lead(PP, 2),
                Txp_lag1 = lag(Txp, 1),
                Txp_lag2 = lag(Txp, 2),
                Txp_lead1 = lead(Txp, 1),
                Txp_lead2 = lead(Txp, 2),
                Tnp_lag1 = lag(Tnp, 1),
                Tnp_lag2 = lag(Tnp, 2),
                Tnp_lead1 = lead(Tnp, 1),
                Tnp_lead2 = lead(Tnp, 2))

# Calcular la matriz de correlación entre los valores DEF y las categorías de cobertura vegetal
BD2 <- BD1  %>%  # Unir los datos de índices DEF con los datos de cobertura vegetal
  dplyr::select(Forest_percent,NonForest_percent, Cropland_percent, Bare_percent,
                Urban_percent,Water_percent, 
                E,E_lag1,E_lag2,E_lead1,E_lead2,
                C,C_lag1,C_lag2,C_lead1,C_lead2,
                ICENr,ICENr_lag1,ICENr_lag2,ICENr_lead1,ICENr_lead2,
                ICEN_9120,ICEN_9120_lag1,ICEN_9120_lag2,ICEN_9120_lead1,ICEN_9120_lead2,
                PP,PP_lag1,PP_lag2,PP_lead1,PP_lead2,
                Txp,Txp_lag1,Txp_lag2,Txp_lead1,Txp_lead2,
                Tnp,Tnp_lag1,Tnp_lag2,Tnp_lead1,Tnp_lead2)


# correlation_matrix_DEF <- BD2 %>%
#   cor(method = "pearson", use = "pairwise.complete.obs")
#library(psych)
cor_matrix <- corr.test(BD2)
#write_csv(as.data.frame(cor_matrix$r),'VF_CORR_CCI_MB_DRY.csv')
# Extraer las correlaciones significativas con un valor p < 0.05
significant_correlations <- which(cor_matrix$p < 0.05, arr.ind = TRUE)
print(significant_correlations)


# Visualizar tendencias generales
BD %>%
  ungroup() %>%
  filter(year > 2000 & Producto!='NA') %>%
  filter(Producto=='CCI') %>% 
  ggplot(aes(x = year, y = Forest)) +  # Cambié color por fill para barras apiladas
  geom_bar(stat = "identity", position = "dodge") +   # Usar stat = "identity" para gráficos de barras con valores
  labs(title = "Cambios en el área de Bosque a lo largo del tiempo",
       x = "Año", y = "Área de Bosque (km²)") +
  facet_wrap(vars(Cuenca),scales = 'free_y',nrow=2) +                       # Divide el gráfico por el producto (MODIS/CCI)
  theme_minimal()

BD %>%
  ungroup() %>%
  filter(year > 2000 & Producto!='NA') %>%
  filter(Producto=='MODIS') %>% 
  ggplot(aes(x = year, y = Forest)) +  # Cambié color por fill para barras apiladas
  geom_bar(stat = "identity", position = "dodge") +   # Usar stat = "identity" para gráficos de barras con valores
  labs(title = "Cambios en el área de Bosque a lo largo del tiempo",
       x = "Año", y = "Área de Bosque (km²)") +
  facet_wrap(vars(Cuenca),scales = 'free_y',nrow=2) +                       # Divide el gráfico por el producto (MODIS/CCI)
  theme_minimal()

# Visualizar tendencias generales
BD %>%
  ungroup() %>%
  filter(year > 2000 & Producto!='NA') %>%
  filter(Producto=='CCI') %>% 
  ggplot(aes(x = year, y = Cropland)) +  # Cambié color por fill para barras apiladas
  geom_bar(stat = "identity", position = "dodge") +   # Usar stat = "identity" para gráficos de barras con valores
  labs(title = "Cambios en el área de Cropland a lo largo del tiempo",
       x = "Año", y = "Área de Cropland (km²)") +
  facet_wrap(vars(Cuenca),scales = 'free_y',nrow=2) +                       # Divide el gráfico por el producto (MODIS/CCI)
  theme_minimal()

BD %>%
  ungroup() %>%
  filter(year > 2000 & Producto!='NA') %>%
  filter(Producto=='MODIS' & season=='DRY') %>% 
  ggplot() +  # Cambié color por fill para barras apiladas
#  geom_bar(aes(x = year, y = PP),stat = "identity", position = "dodge") +
  geom_point(aes(x = year, y = C),stat = "identity", position = "dodge")+
  geom_path(aes(x = year, y = C)) + # Usar stat = "identity" para gráficos de barras con valores
  labs(title = "Cambios en el área de Cropland a lo largo del tiempo",
       x = "Año", y = "Área de Cropland (km²)") +
#  facet_grid(vars(Cuenca),scales = 'free_y') +                       # Divide el gráfico por el producto (MODIS/CCI)
  theme_minimal()


# Crear un gráfico de dispersión para evaluar la relación entre índice E y área de bosque
BD %>%
  ungroup() %>%
  filter(year > 2000 & Producto!='NA') %>%
  filter(Producto=='MODIS') %>%
  ggplot(., aes(x = E, y = Forest, color = season)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relación entre el Índice E y el Área de Bosque",
       x = "Índice E", y = "Área de Forest (km²)") +
  theme_minimal()

# Modelo lineal simple para analizar la relación entre PP y el área de Forest
model <- lm(Forest ~ PP + Txx + Txp + Tnp + Tnn, data = BD)
summary(model)

# Matriz de correlación
cor_data <- BD %>%
  ungroup() %>%
  filter(year > 1999 & Producto != 'NA') %>%
  filter(Producto == 'MODIS') %>% 
  filter(Cuenca == 'Lower-Middle' & season == 'WET') %>% 
  mutate(
    E_lag1 = lag(E, 1),       # Lag +1 (año anterior)
    E_lead1 = lead(E, 1),     # Lag -1 (año siguiente)
    C_lag1 = lag(C, 1),
    C_lead1 = lead(C, 1),
    PP_lag1 = lag(PP, 1),
    PP_lead1 = lead(PP, 1),
    Txx_lag1 = lag(Txx, 1),
    Txx_lead1 = lead(Txx, 1),
    Txp_lag1 = lag(Txp, 1),
    Txp_lead1 = lead(Txp, 1),
    Tnp_lag1 = lag(Tnp, 1),
    Tnp_lead1 = lead(Tnp, 1),
    Tnn_lag1 = lag(Tnn, 1),
    Tnn_lead1 = lead(Tnn, 1)
  ) %>% 
  dplyr::select(Forest, Cropland, `Coastal Desert`, Urban, `Water Body`, 
                E, E_lag1, E_lead1, 
                C, C_lag1, C_lead1, 
                PP, PP_lag1, PP_lead1, 
                Txx, Txx_lag1, Txx_lead1, 
                Txp, Txp_lag1, Txp_lead1, 
                Tnp, Tnp_lag1, Tnp_lead1, 
                Tnn, Tnn_lag1, Tnn_lead1)

cor_matrix <- cor(cor_data, use = "pairwise.complete.obs") %>% as.data.frame()
write_csv(cor_matrix,'MODIS_LOWERMIDDLE_WET.csv')
print(cor_matrix)

# Calcular la matriz de correlación con significancia utilizando Hmisc::rcorr
BD_lagged_matrix <- as.matrix(cor_data)  # Convertir el data frame a matriz
correlaciones <- rcorr(BD_lagged_matrix, type = "pearson")  # type puede ser "pearson" o "spearman"

# Extraer las matrices de correlación y de valores p
matriz_correlacion <- correlaciones$r
matriz_p_valores <- correlaciones$P

# Mostrar la matriz de correlación
print("Matriz de correlación:")
print(matriz_correlacion)

# Mostrar la matriz de valores p
print("Matriz de valores p:")
print(matriz_p_valores)

# Visualización de la matriz de correlación
library(corrplot)
corrplot(cor_matrix, method = "square",type='lower')



###############################################################################
changesLabel = c(GC = "Gross change",NG = "Net gain", NL = "Net loss")
color = c(GC = "gray70", NG = "#006400", NL = "#EE2C2C")

datachange <- (alta$lulc_Onestep %>% left_join(alta$tb_legend, by = c(From = "categoryValue")) %>% 
                 left_join(alta$tb_legend, by = c(To = "categoryValue")) %>% 
                 dplyr::select(-c(From, To)) %>% rename(From = "categoryName.x", 
                                                        To = "categoryName.y"))[c(1, 2, 3, 7, 9)]
areaif <- ifelse(isTRUE(TRUE), "km2", "QtPixel")
lulc_gain <- datachange %>% dplyr::filter(From != To)
lulc_loss <- lulc_gain %>% rename(To = "From", From = "To") %>% 
  mutate(km2 = -1 * km2, QtPixel = -1 * QtPixel)
lulc_gainloss_gross <- rbind(lulc_gain, lulc_loss) %>% mutate(changes = ifelse(QtPixel > 
                                                                                 0, "Gain", "Loss"))
lulc_gainLoss_net <- lulc_gainloss_gross %>% group_by(To) %>% 
  summarise(area = sum(!!as.name(areaif))) %>% mutate(changes = ifelse(area > 
                                                                         0, changesLabel[[2]], changesLabel[[3]]))

lulc_gainloss_gross <- lulc_gainloss_gross[c(1, 2, 4,5, 6)]
names(lulc_gainloss_gross) <- c("Period", "area_gross", 
                                "From", "To", "changes")
names(color) <- unname(changesLabel[c("GC", "NG", "NL")])
