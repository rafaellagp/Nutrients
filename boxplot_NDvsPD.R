

#Analise exploratória:
#Dados de nutrientes de cultivos que são dependentes de polinização (PD) e que não dependem de polinização biótica (ND)
#boxplot comparativo entre categorias PD e ND


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

#boxplot ND vs PD

W<-ggplot(data=DATA_N, aes(x=dependence, y= Water,
                           colour=dependence)) 
g1<-W + geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Water (mg)")+
  apatheme  

E<-ggplot(data=DATA_N, aes(x=dependence, y= Energy,
                           colour=dependence)) 
g2<-E + geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Energy (kcal)")+
  apatheme  

P<-ggplot(data=DATA_N, aes(x=dependence, y= Protein,
                           colour=dependence)) 
g3<-P + geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Protein (g)")+
  apatheme

L<-ggplot(data=DATA_N, aes(x=dependence, y=  Lipid,
                           colour=dependence)) 
g4<-L + geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Lipids (g)")+
  apatheme

C<-ggplot(data=DATA_N, aes(x=dependence, y= Carbohydrate,
                           colour=dependence)) 
g5<-C + geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Carbohydrate (g)")+
  apatheme

F1<-ggplot(data=DATA_N, aes(x=dependence, y= Fiber,
                            colour=dependence)) 
g6<-F1+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Fiber (g)")+
  apatheme

CA<-ggplot(data=DATA_N, aes(x=dependence, y= Calcium,
                            colour=dependence)) 
g7<-CA+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Calcium (mg)")+
  apatheme

I<-ggplot(data=DATA_N, aes(x=dependence, y= Iron,
                           colour=dependence)) 
g8<-I+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Iron (mg)")+
  apatheme

M<-ggplot(data=DATA_N, aes(x=dependence, y= Magnesium,
                           colour=dependence)) 
g9<-M+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Magnesium m(g)")+
  apatheme

PH<-ggplot(data=DATA_N, aes(x=dependence, y= Phosphorus,
                            colour=dependence)) 
g10<-PH+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Phosphorus (mg)")+
  apatheme

PO<-ggplot(data=DATA_N, aes(x=dependence, y= Potassium,
                            colour=dependence)) 
g11<-PO+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Potassium (mg)")+
  apatheme

S<-ggplot(data=DATA_N, aes(x=dependence, y= Sodium,
                           colour=dependence)) 
g12<-S+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Sodium (mg)")+
  apatheme

Z<-ggplot(data=DATA_N, aes(x=dependence, y= Zinc,
                           colour=dependence)) 
g13<-Z+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Zinc (mg)")+
  apatheme

VC<-ggplot(data=DATA_N, aes(x=dependence, y= VitaminC,
                            colour=dependence)) 
g14<-VC+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Vitamin C (mg)")+
  apatheme

Th<-ggplot(data=DATA_N, aes(x=dependence, y= Thiamin,
                            colour=dependence)) 
g15<-Th+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Thiamin (mg)")+
  apatheme

R<-ggplot(data=DATA_N, aes(x=dependence, y= Riboflavin,
                           colour=dependence)) 
g16<-R+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Riboflavin (mg)")+
  apatheme

NI<-ggplot(data=DATA_N, aes(x=dependence, y= Niacin,
                            colour=dependence)) 
g17<-NI+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Niacin (mg)")+
  apatheme

VB6<-ggplot(data=DATA_N, aes(x=dependence, y= VitaminB6,
                             colour=dependence)) 
g18<-VB6+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Vitamin B6 (mg)")+
  apatheme

FO<-ggplot(data=DATA_N, aes(x=dependence, y= Folate,
                            colour=dependence)) 
g19<- FO+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Folate (mg)")+
  apatheme

VA<-ggplot(data=DATA_N, aes(x=dependence, y= VitaminA,
                            colour=dependence)) 
g20<-VA+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Vitamin A (mg)")+
  apatheme

VE<-ggplot(data=DATA_N, aes(x=dependence, y= VitaminE,
                            colour=dependence)) 
g21<-VE+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Vitamin E (mg)")+
  apatheme

VK<-ggplot(data=DATA_N, aes(x=dependence, y= VitaminK,
                            colour=dependence)) 
g22<-VK+ geom_jitter() + geom_boxplot(size=0.3, alpha=0.5)+
  labs(x="", y="Vitamin K (mg)")+
  apatheme


grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,g21,g22)



