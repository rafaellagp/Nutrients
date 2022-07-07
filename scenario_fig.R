
#The following figure (cenarios_fig) show the Worst-case and best-case estimates based on a scenario of pollinator loss in terms of nutrient 
# production based on 45 leading crops cultivated in Brazil in 2017 (current scenario). 
# The degree of pollinator dependence is represented as essential, great, modest, little, 
# and non-dependent (ND) (sensu Klein et al., 2007 and Gallai and Vaissi`ere 2009). 
# Production of pollinator non-dependent crops and pollinator-dependent crops is shown as 
# horizontal bars on the left and right, respectively. Current status of cropland production 
# in Brazil (top panel). Best-case estimate (middle panel) indicates moderate impacts on nutrient
# production. Worst-case estimate (bottom panel) indicates further declines in nutrient production 
# and a complete collapse in the productivity of ‘essential’ pollinator-dependent crops. 
# Source: IBGE, 2020.


library(ggplot2)
library(scales)
library(reshape)
library(gridExtra)
library(cowplot)
library(rlang)
library(gridExtra)
library(ggplot2)


dt_current <- read.table(text = "			VitaminK	VitaminE	VitaminA	Folate	VitaminB6	Niacin	Riboflavin	Thiamin	VitaminC	Zinc	Sodium	Potassium	Phosphorus	Magnesium	Iron	Calcium	Fiber	Carbohydrate	Lipid	Protein
eND	-0.448564788	-2.176778061	-1.129391409	-1.616812236	-2.24450243	-3.441963394	-1.904053048	-2.385404039	-3.896676818	-2.966254104	-4.154250395	-5.509210341	-5.176063978	-4.780062502	-2.958991621	-3.847398487	-6.469450678	-7.505846521	-6.254995122	-6.684439473
dLittle	0.126425601	0.748314908	0.103019334	0.74503561	0.711864308	1.47797539	0.085660766	0.428620381	1.763155811	1.432545886	1.722610528	4.022037802	3.503751302	3.133334118	1.723549953	2.9511286	5.245974784	5.525546458	4.633681156	5.257963692
cModest	0.000915786	0.917841472	1.059112711	2.246640508	1.865861297	3.231663614	2.27159028	2.658093159	4.483626643	3.010899409	4.190145892	5.80838977	5.30233082	4.828856692	3.565919848	5.308470735	6.643615734	7.060359823	6.849144154	7.126933905
bGreat	0.056375528	1.492286832	0.167823569	0.100057499	0.337479228	0.937009967	0.106972841	0.230173721	2.239995969	0.786031231	1.818937672	3.497318404	2.778384112	2.458323348	0.889178968	2.14148078	4.224500027	4.816965292	4.985131393	4.304682643
aEssential	0	0.188250315	0.226428757	0.022157894	0.219199594	0.680569706	0.100142308	0.198414089	2.257109644	0.45515616	1.768880395	3.247271246	2.206591773	2.164440015	0.63298607	1.983766988	4.04757988	5.035770936	3.317870177	3.941878063 ", sep = "", header = TRUE)

dat_log10_current <- melt(cbind(dt_current, ind = rownames(dt_current)), id.vars = c('ind'))
g_current <- ggplot(dat_log10_current, aes(x = variable, y = ifelse(ind %in% 1:2,-value, value), fill = ind)) + 
  geom_col() +
  coord_flip() +
  theme_bw(base_size = 9, base_family = "Times") +
  labs(y= 'Nutrient production amount (tons)', x="", title ="Current scenario") +
  theme(plot.title = element_text(hjust= 0.01, face="bold", size= 10))+
  labs(fill = "Pollinator dependence") +
  theme(legend.position='none')+
  scale_fill_manual(values=c( "#B2182B", "#D6604D", "#F4A582","#FDDBC7","#ADD8E6"))+
  scale_y_continuous(limits=c(-8, 23), breaks=c(-10,0,10,20))
plot(g_current)


dt_optimist <- read.table(text = "			VitaminK	VitaminE	VitaminA	Folate	VitaminB6	Niacin	Riboflavin	Thiamin	VitaminC	Zinc	Sodium	Potassium	Phosphorus	Magnesium	Iron	Calcium	Fiber	Carbohydrate	Lipid	Protein
eND	-0.448564788	-2.176778061	-1.129391409	-1.616812236	-2.24450243	-3.441963394	-1.904053048	-2.385404039	-3.896676818	-2.966254104	-4.154250395	-5.509210341	-5.176063978	-4.780062502	-2.958991621	-3.847398487	-6.469450678	-7.505846521	-6.254995122	-6.684439473
dLittle	0.126425601	0.748314908	0.103019334	0.74503561	0.711864308	1.47797539	0.085660766	0.428620381	1.763155811	1.432545886	1.722610528	4.022037802	3.503751302	3.133334118	1.723549953	2.9511286	5.245974784	5.525546458	4.633681156	5.257963692
cModest	0.000824294	0.877875618	1.017546357	2.201156396	1.820760484	3.185934429	2.22609091	2.61244169	4.437870737	2.965188975	4.144391516	5.762632354	5.25657357	4.783099917	3.520175468	5.262713482	6.597858254	7.014602336	6.80338667	7.081176418
bGreat	0.034695356	1.279659283	0.108236371	0.062753134	0.231739666	0.747406107	0.067286937	0.152090683	2.019810117	0.609158376	1.601459794	3.275561769	2.557017386	2.23748122	0.703178366	1.921717333	4.002668543	4.595120955	4.763285639	4.082848249
aEssential	0	0.022947288	0.028747541	0.002267359	0.027615857	0.139645628	0.011119338	0.024449387	1.278211636	0.07379324	0.83079876	2.249477482	1.230227215	1.190413797	0.12369574	1.022557048	3.047930045	4.035806931	2.319746115	2.94232467 ", sep = "", header = TRUE)

dat_log10_optimist <- melt(cbind(dt_optimist, ind = rownames(dt_optimist)), id.vars = c('ind'))
g_optimist <- ggplot(dat_log10_optimist, aes(x = variable, y = ifelse(ind %in% 1:2, -value, value), fill = ind)) + 
  geom_col() +
  coord_flip() +
  theme_bw(base_size = 9, base_family = "Times") +
  labs(y= 'Nutrient production amount (tons)', x="", title ="Best-case estimate in a scenario of pollinator loss") +
  theme(plot.title = element_text(hjust= 0.01, face="bold", size= 10)) +
  labs(fill = "Pollinator dependence") +
  scale_fill_manual(values=c( "#B2182B", "#D6604D", "#F4A582","#FDDBC7","#ADD8E6"))+
  theme(legend.position = "none")+
  scale_y_continuous(limits=c(-8, 23), breaks=c(-10,0,10,20))
plot(g_optimist)


dt_pessimist <- read.table(text = "				VitaminK	VitaminE	VitaminA	Folate	VitaminB6	Niacin	Riboflavin	Thiamin	VitaminC	Zinc	Sodium	Potassium	Phosphorus	Magnesium	Iron	Calcium	Fiber	Carbohydrate	Lipid	Protein
eND	-0.448564788	-2.176778061	-1.129391409	-1.616812236	-2.24450243	-3.441963394	-1.904053048	-2.385404039	-3.896676818	-2.966254104	-4.154250395	-5.509210341	-5.176063978	-4.780062502	-2.958991621	-3.847398487	-6.469450678	-7.505846521	-6.254995122	-6.684439473
dLittle	0.116439857	0.714957192	0.094685796	0.711734999	0.679165785	1.438443363	0.078607018	0.403382797	1.722937591	1.393170886	1.682464691	3.981083277	3.462806159	3.092407107	1.683402362	2.910218058	5.20501642	5.484587978	4.592723547	5.217005322
cModest	0.000558859	0.735487438	0.868023562	2.033541042	1.654956209	3.017156294	2.058403272	2.444032701	4.268965596	2.796499942	3.975493648	5.593720037	5.087662039	4.614190645	3.351325116	5.093801935	6.428945632	6.845689682	6.634474028	6.912263761
bGreat	0.006571824	0.634202274	0.021969746	0.012204351	0.05279433	0.26516855	0.013142027	0.032167933	1.301152269	0.193704539	0.910617592	2.539827708	1.82559096	1.511777868	0.241112744	1.207529018	3.266102209	3.858411531	4.026560439	3.346249514
aEssential	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0 ", sep = "", header = TRUE)

dat_log10_pessimist <- melt(cbind(dt_pessimist, ind = rownames(dt_pessimist)), id.vars = c('ind'))
g_pessimist <- ggplot(dat_log10_pessimist, aes(x = variable, y = ifelse(ind %in% 1:2, -value, value), fill = ind)) + 
  geom_col() +
  coord_flip() +
  theme_bw(base_size = 9, base_family = "Times") +
  labs(y= 'Nutrient production amount (tons)', x="", title ="Worst-case estimate in a scenario of pollinator loss") +
  theme(plot.title = element_text(hjust= 0.01, face="bold", size= 10)) +
  labs(fill = "Pollinator dependence") +
  scale_fill_manual(values=c( "#B2182B", "#D6604D", "#F4A582","#FDDBC7","#ADD8E6"))+
  theme(legend.position = "none")+
  scale_y_continuous(limits=c(-8, 23), breaks=c(-10,0,10,20))

plot(g_pessimist)

df.Legenda <- read.table(text = "		VitaminK	VitaminE	VitaminA	Folate	VitaminB6	Niacin	Riboflavin	Thiamin	VitaminC	Zinc	Sodium	Potassium	Phosphorus	Magnesium	Iron	Calcium	Fiber	Carbohydrate	Lipid	Protein	
ND	-0.448564788	-2.176778061	-1.129391409	-1.616812236	-2.24450243	-3.441963394	-1.904053048	-2.385404039	-3.896676818	-2.966254104	-4.154250395	-5.509210341	-5.176063978	-4.780062502	-2.958991621	-3.847398487	-6.469450678	-7.505846521	-6.254995122	-6.684439473	
1.Little	0.126425601	0.748314908	0.103019334	0.74503561	0.711864308	1.47797539	0.085660766	0.428620381	1.763155811	1.432545886	1.722610528	4.022037802	3.503751302	3.133334118	1.723549953	2.9511286	5.245974784	5.525546458	4.633681156	5.257963692	
2.Modest	0.000915786	0.917841472	1.059112711	2.246640508	1.865861297	3.231663614	2.27159028	2.658093159	4.483626643	3.010899409	4.190145892	5.80838977	5.30233082	4.828856692	3.565919848	5.308470735	6.643615734	7.060359823	6.849144154	7.126933905	
3.Great	0.056375528	1.492286832	0.167823569	0.100057499	0.337479228	0.937009967	0.106972841	0.230173721	2.239995969	0.786031231	1.818937672	3.497318404	2.778384112	2.458323348	0.889178968	2.14148078	4.224500027	4.816965292	4.985131393	4.304682643	
4.Essential	0	0.188250315	0.226428757	0.022157894	0.219199594	0.680569706	0.100142308	0.198414089	2.257109644	0.45515616	1.768880395	3.247271246	2.206591773	2.164440015	0.63298607	1.983766988	4.04757988	5.035770936	3.317870177	3.941878063	
                         ", sep = "", header = TRUE)

dat_df.Legenda <- melt(cbind(df.Legenda, ind = rownames(df.Legenda)), id.vars = c('ind'))
df.legenda <- ggplot(dat_df.Legenda, aes(x = variable, y = ifelse(ind %in% 1:2,-value, value), fill = ind)) + 
  geom_col()+
  coord_flip() +
  theme_bw(base_size = 10, base_family = "Times") +
  labs(y= 'Nutrient production amount (tons)', x="", title ="Worst-case") +
  theme(plot.title = element_text(hjust= 0.01, face="bold", size= 10)) +
  labs(fill = "") +
  scale_fill_manual(labels = c("ND","Little", "Modest","Great", "Essential"), values=c("#ADD8E6", "#FDDBC7","#F4A582","#D6604D","#B2182B"))+
  theme(legend.position = "bottom")

leg <-get_legend (df.legenda)

plot(leg)

cenarios_fig<-grid.arrange(g_current, g_optimist, g_pessimist, ncol=1, bottom = leg)
tiff('cenarios_fig.tiff', units="cm", width=25, height=45, res=300, compression = 'lzw')
plot(cenarios_fig)
dev.off()
