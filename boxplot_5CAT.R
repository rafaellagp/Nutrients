#Analise exploratória:
#Dados de nutrientes de cultivos classificados pelo grau de dependencia conforme Klein et al 2007.
#Categorias de dependencia de polinizador biótico: essential, great, little, modest, ND

#library
library(cowplot)
library(rlang)
library(gridExtra)
library(ggplot2)

#Graphics style 'apatheme'
apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times'),
        legend.title=element_blank())

#upload data
DATA_N<-read.csv(file.choose())
View(DATA_N)
str(DATA_N)
summary(DATA_N)


#Boxplot com dados brutos, categorizados de 0 a 4

Wb<-ggplot(data=DATA_N, aes(x=Pollinator_dependence, y= Water,
                            colour=Pollinator_dependence)) 
g1b<-Wb + geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Water (mg)")+
  apatheme

Eb<-ggplot(data=DATA_N, aes(x=Pollinator_dependence, y= Energy,
                            colour=Pollinator_dependence)) 
g2b<-Eb + geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Energy (kcal)")+
  apatheme  

Pb<-ggplot(data=DATA_N, aes(x=Pollinator_dependence, y= Protein,
                            colour=Pollinator_dependence)) 
g3b<-Pb + geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Protein (g)")+
  apatheme

Lb<-ggplot(data=DATA_N, aes(x=Pollinator_dependence, y=  Lipid,
                            colour=Pollinator_dependence)) 
g4b<-Lb + geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Lipids (g)")+
  apatheme

Cb<-ggplot(data=DATA_N, aes(x=Pollinator_dependence, y= Carbohydrate,
                            colour=Pollinator_dependence)) 
g5b<-Cb + geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Carbohydrate (g)")+
  apatheme

F1b<-ggplot(data=DATA_N, aes(x=Pollinator_dependence, y= Fiber,
                             colour=Pollinator_dependence)) 
g6b<-F1b+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Fiber (g)")+
  apatheme

CAb<-ggplot(data=DATA_N, aes(x=Pollinator_dependence, y= Calcium,
                             colour=Pollinator_dependence)) 
g7b<-CAb+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Calcium (mg)")+
  apatheme

Ib<-ggplot(data=DATA_N, aes(x=Pollinator_dependence, y= Iron,
                            colour=Pollinator_dependence)) 
g8b<-Ib+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Iron (mg)")+
  apatheme

Mb<-ggplot(data=DATA_N, aes(x=Pollinator_dependence, y= Magnesium,
                            colour=Pollinator_dependence)) 
g9b<-Mb+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Magnesium (mg)")+
  apatheme

PHb<-ggplot(data=DATA_N, aes(x=Pollinator_dependence, y= Phosphorus,
                             colour=Pollinator_dependence)) 
g10b<-PHb+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Phosphorus (mg)")+
  apatheme

POb<-ggplot(data=DATA_N, aes(x=Pollinator_dependence, y= Potassium,
                             colour=Pollinator_dependence)) 
g11b<-POb+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Potassium (mg)")+
  apatheme

Sb<-ggplot(data=DATA_N, aes(x=Pollinator_dependence, y= Sodium,
                            colour=Pollinator_dependence)) 
g12b<-Sb+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Sodium (mg)")+
  apatheme

Zb<-ggplot(data=DATA_N, aes(x=Pollinator_dependence, y= Zinc,
                            colour=Pollinator_dependence)) 
g13b<-Zb+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Zinc (mg)")+
  apatheme

VCb<-ggplot(data=DATA_N, aes(x=Pollinator_dependence, y= VitaminC,
                             colour=Pollinator_dependence)) 
g14b<-VCb+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Vitamin C (mg)")+
  apatheme

Thb<-ggplot(data=DATA_N, aes(x=Pollinator_dependence, y= Thiamin,
                             colour=Pollinator_dependence)) 
g15b<-Thb+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Thiamin (mg)")+
  apatheme

Rb<-ggplot(data=DATA_N, aes(x=Pollinator_dependence, y= Riboflavin,
                            colour=Pollinator_dependence)) 
g16b<-Rb+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Riboflavin (mg)")+
  apatheme

NIb<-ggplot(data=DATA_N, aes(x=Pollinator_dependence, y= Niacin,
                             colour=Pollinator_dependence)) 
g17b<-NIb+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Niacin (mg)")+
  apatheme

VB6b<-ggplot(data=DATA_N, aes(x=Pollinator_dependence, y= VitaminB6,
                              colour=Pollinator_dependence)) 
g18b<-VB6b+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Vitamin B6 (mg)")+
  apatheme

FOb<-ggplot(data=DATA_N, aes(x=Pollinator_dependence, y= Folate,
                             colour=Pollinator_dependence)) 
g19b<- FOb+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Folate (mg)")+
  apatheme

VAb<-ggplot(data=DATA_N, aes(x=Pollinator_dependence, y= VitaminA,
                             colour=Pollinator_dependence)) 
g20b<-VAb+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Vitamin A (mg)")+
  apatheme

VEb<-ggplot(data=DATA_N, aes(x=Pollinator_dependence, y= VitaminE,
                             colour=Pollinator_dependence)) 
g21b<-VEb+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Vitamin E (mg)")+
  apatheme

VKb<-ggplot(data=DATA_N, aes(x=Pollinator_dependence, y= VitaminK,
                             colour=Pollinator_dependence)) 
g22b<-VKb+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Vitamin K (mg)")+
  apatheme



grid.arrange(g1b,g2b,g3b,g4b,g5b,g6b,g7b,g8b,g9b,g10b,g11b,g12b,g13b,g14b,g15b,g16b,g17b,g18b,g19b,g20b,g21b,g22b)

