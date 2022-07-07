
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(dplyr)
library(sf)
library(tidyverse)
library(scatterpie)
library(ggforce)
library(ggspatial)
library(ggsn)
library(mapproj)
library(ggplot2)
library(ggforce)
library(dplyr)
library(geobr)
library(crul)
library(sf)


install.packages("devtools")
devtools::install_github("rpradosiqueira/brazilmaps")

pie<- read.table(text= "sigla	Total	Essential	Great	Modest	Little	ND	lat	long	cat
DF	497885	47675	142713	15813	91684	200000	-15.83	-47.86	9.978827586
AP	768093	141755	0	20413	135235	470690	1.41	-51.77	12.39429708
RR	1657326	329443	146357	89239	429167	663120	1.99	-61.33	18.20618576
AC	3087129	404986	120600	180333	1184955	1196255	-8.77	-70.55	24.84805425
SE	4077543	386495	530458	287643	872947	2000000	-10.57	-37.45	28.5571112
MS	5371654	676833	472444	402203	1029344	2790830	-20.51	-54.54	32.77698583
TO	6695234	1169614	25067	515318	298658	4686577	-9.46	-48.26	36.5929884
AM	7085797	1620716	190574	522937	1751570	3000000	-3.47	-65.1	37.64517765
PI	7196930	1846219	814506	681064	362028	3493113	-6.6	-42.28	37.9392409
RJ	7396254	418389	2813480	746698	1417687	2000000	-22.25	-42.66	38.46102963
RO	7577092	1374326	1049163	622990	1485214	3045399	-10.83	-63.34	38.92837526
MA	7588152	1057007	483674	754070	767799	4525602	-5.42	-45.44	38.95677605
AL	9994659	1723341	2061354	377737	1832227	4000000	-9.62	-36.82	44.70941512
PE	10055745	1365600	2988002	617990	775329	4308824	-8.38	-37.86	44.84583593
RN	10330352	1705092	2346547	506914	2150216	3621583	-5.81	-36.59	45.45404712
MT	11028932	2187323	741007	1105366	1248334	5746902	-12.64	-55.42	46.96580032
PB	11866573	1051824	2656734	987584	1772958	5397473	-7.28	-36.72	48.71667682
PA	12604802	1991146	570211	1806896	2616357	5620192	-3.79	-52.48	50.20916649
ES	13195781	1434657	3690652	1066230	3313678	3690564	-19.19	-40.34	51.37271844
GO	15927998	997610	4006915	1432571	2084521	7406381	-15.98	-49.86	56.44111622
CE	17120475	2008879	4850553	1376264	4082755	4802024	-5.2	-39.53	58.51576711
BA	23357728	3600705	4873792	1656801	4679536	8546894	-13.29	-41.71	68.34870591
SC	31268036	2879745	6270396	1495222	3622673	17000000	-27.45	-50.95	79.07975215
PR	76830433	7450486	22610946	3147351	14621650	29000000	-24.89	-51.55	123.9600202
MG	81997557	6377663	25389432	4285569	11944893	34000000	-18.1	-44.38	128.0605771
RS	85808101	9438693	19283800	3609992	17475616	36000000	-30.17	-53.5	131.0023672
SP	89926590	5755329	24950980	3627004	28593277	27000000	-22.19	-48.79	134.1093509", sep = "", header = TRUE)

#Pie graph
str(pie)
name<-c("Total", "lat", "long")
pie[,name]<-lapply(pie[,name], as.numeric)
str(pie)

#Brazil map with states
library(geobr)
states <- read_state(year=2014)

#World map - South America map
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")
install.packages("rgeos")
library(rgeos)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
worldmap <- ne_countries(scale = "medium", returnclass = "sf")
head(worldmap[c('name', 'continent')])
samerica <- worldmap[worldmap$continent == 'South America',]

#Scatterpie + map
pie_map<- ggplot() +
  geom_sf(data = samerica, color = "grey90", fill = "white") + 
  geom_sf(data=states, fill="grey99", color="black", size=.02, show.legend = FALSE)+
  coord_sf(xlim = c(-80, -20), ylim = c(-35, 5)) +
  geom_scatterpie(data = pie, 
                  aes(long, lat, r = sqrt(Total)/5000),
                  cols = c("Essential", "Great", "Modest", "Little", "ND"), 
                  color=NA,
                  alpha = 0.9)+
  xlab("") + ylab("") +
  scale_fill_manual(
    breaks = c("Essential", "Great", "Modest", "Little", "ND"),
    labels = c("Essential", "Great", "Modest", "Little", "Non-dependent"),
    values = c("Essential"= "#B2182B",
               "Great"= "#D6604D",
               "Modest" = "#F4A582",
               "Little" = "#FDDBC7",
               "ND" = "#ADD8E6"))+
  labs(title = "",
       subtitle = "",
       fill = "Dependence") +
  theme_bw() +
  theme(plot.title = element_text(family = "Times"),
        legend.title = element_text(family = "Times"),
        legend.text = element_text(family = "Times"),
        legend.position = c(0.96, 0.02),
        legend.justification = c(1, 0))+
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.3, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-80, -20), ylim = c(-35, 5)) +
  geom_scatterpie_legend(sqrt(pie$Total)/3000, x=-25, y=-15, n=5)

#plot
pie_map 