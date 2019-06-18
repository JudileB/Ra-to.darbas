#########################################################
### Reikiamu paketu instaliavimas
#########################################################

if (!require(rsdmx)) install.packages('rsdmx'); require(rsdmx)
if (!require(eurostat)) install.packages('eurostat'); require(eurostat)
if (!require(tidyverse)) install.packages('tidyverse'); require(tidyverse)

#########################################################
### Duomenu apie infliacija is eurostat parsisiuntimas
#########################################################

prc_hicp_aind <- get_eurostat("prc_hicp_aind", stringsAsFactors = F)

#########################################################
### Duomenu aie infliacija apdorojimas. Lenteliu Lietuvos
### infliacijos ir europos infliacijos sudarymas. pakeiciami
### stulpialiu pavadinimai bei isrenkami tik
### reikalingi stulpeliai
### Duomenys imami nuo 2002 metu iki 2018
#########################################################

infliacija_LT <- prc_hicp_aind %>%
    filter(coicop == "CP00",
           geo == "LT",
           time >= "2002-01-01",
           unit == "RCH_A_AVG")%>%
  select(3, 4, 5)
names(infliacija_LT) <- c("Salis", "Laikotarpis", "Infliacijos_lygis")

infliacija_EU28 <- prc_hicp_aind %>%
  filter(coicop == "CP00",
         geo == "EU28",
         time >= "2002-01-01",
         unit == "RCH_A_AVG")%>%
  select(3, 4, 5)
names(infliacija_EU28) <- c("Salis", "Laikotarpis", "Infliacijos_lygis")


#########################################################
### Duomenu apie nedarba parsisiuntimas is Eurostat
#########################################################

lfsa_urgaed <- get_eurostat("lfsa_urgaed", stringsAsFactors = F,
                            filters = list(geo = c("LT", "EU28"),
                                           age = "Y15-74",
                                           isced11 = "TOTAL",
                                           sex = "T",
                                           unit = "PC"))

#########################################################
### Duomenu apie nedarba apdorijimas.
### Sukuriamos Lietuvos ir EU nedarbo lenteles. pakeiciami
### stulpialiu pavadinimai bei isrenkami tik
### reikalingi stulpeliai
### Duomenys imami nuo 2002 metu iki 2018
#########################################################

nedarbas_LT <- lfsa_urgaed %>%
  filter(time >= "2002-01-01",
         geo == "LT")%>%
  select(5, 6, 7)
names(nedarbas_LT) <- c("Salis", "Laikotarpis", "Nedarbo_lygis")

nedarbas_EU28 <- lfsa_urgaed %>%
  filter(time >= "2002-01-01", 
       geo == "EU28")%>%
select(5, 6, 7)
names(nedarbas_EU28) <- c("Salis", "Laikotarpis", "Nedarbo_lygis")

#########################################################
### Grafiko braizymas. Lietuvos nedarbo ir infliacijos
### grafikas. (Spausdinimas iskomentuotas, nuemus komentavima spausdins
### darbinej direktorijoj)
#########################################################
# png(filename = "Lietuvos_nedarbo_ir_infliacijos_lygis.png", width = 9, height = 4, units = "in", res = 200)
# dev.args=list(encoding="CP1257.enc")
ggplot()+
  geom_line(data = nedarbas_LT, aes(x=Laikotarpis, y=Nedarbo_lygis, col = "Nedarbas_LT"), size=1)+
  geom_line(data = infliacija_LT, aes(x=Laikotarpis, y=Infliacijos_lygis, col="Infliacija_LT"), size=1)+
  scale_color_manual(values = c("darkgoldenrod2", "darkcyan"))+
  theme(legend.title=element_blank())+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")+
  scale_y_continuous(breaks = seq(-2, 20, 1))+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  labs(title = "Lietuvos nedarbo ir infliacijos lygis",
       subtitle = "Šaltinis: Eurostat (lfsa_urgaed, prc_hicp_aind)",
       x="Laikotarpis",
       y="Reikšmės, %")
# dev.off()

#########################################################
### Grafiko braizymas. Europos sajungos nedarbo ir infliacijos
### grafikas. (Spausdinimas iskomentuotas, nuemus komentavima spausdins
### darbinej direktorijoj)
#########################################################
# png(filename = "EU_nedarbo_ir_infliacijos_lygis.png", width = 9, height = 4, units = "in", res = 200)
# dev.args=list(encoding="CP1257.enc")
ggplot()+
  geom_line(data = nedarbas_EU28, aes(x=Laikotarpis, y=Nedarbo_lygis, col = "Nedarbas_EU"), size=1)+
  geom_line(data = infliacija_EU28, aes(x=Laikotarpis, y=Infliacijos_lygis, col="Infliacija_EU"), size=1)+
  scale_color_manual(values = c("darkgoldenrod2", "darkcyan"))+
  theme(legend.title=element_blank())+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")+
  scale_y_continuous(breaks = seq(-2, 20, 1))+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  labs(title = "Europos Sąjungos nedarbo ir infliacijos lygis",
       subtitle = "Šaltinis: Eurostat (lfsa_urgaed, prc_hicp_aind)",
       x="Laikotarpis",
       y="Reikšmės, %")
# dev.off()

#########################################################
### Naujos lenteles, kurioje butu tik lietuvos nedarbo ir
### infliacijos rodikliai kartu sudarymas. Pagal R paskaiciavimus
### Phillips'o kreives braizymas
### (Spausdinimas iskomentuotas, nuemus komentavima spausdins
### darbinej direktorijoj)
#########################################################
LT_rodikliai <- left_join(infliacija_LT, nedarbas_LT, by=c("Laikotarpis", "Salis"))
dev.
# png(filename = "LT_phillips.png", width = 6, height = 4, units = "in", res = 200)
# dev.args=list(encoding="CP1257.enc")
ggplot(data=LT_rodikliai, aes(x=Nedarbo_lygis, y=Infliacijos_lygis))+
  geom_point()+
  geom_smooth()+
  scale_x_continuous(breaks = seq(4, 18, 1))+
  scale_y_continuous(breaks = seq(-2, 12, 1))+
  labs(title = "Phillips'o kreivė Lietuvoje ",
       subtitle = "Šaltinis: Eurostat (lfsa_urgaed, prc_hicp_aind)",
       x="Nedarbo lygis, %",
       y="Infliacijos lygis, %")
# dev.off()

#########################################################
### Naujos lenteles, kurioje butu tik europos sajungos nedarbo ir
### infliacijos rodikliai kartu sudarymas. Pagal R paskaiciavimus
### Phillips'o kreives braizymas
### (Spausdinimas iskomentuotas, nuemus komentavima spausdins
### darbinej direktorijoj)
#########################################################

EU_rodikiai <- left_join(infliacija_EU28, nedarbas_EU28, by=c("Laikotarpis", "Salis"))
# png(filename = "EU_phillips.png", width = 6, height = 4, units = "in", res = 200)
# dev.args=list(encoding="CP1257.enc")
ggplot(data=EU_rodikiai, aes(x=Nedarbo_lygis, y=Infliacijos_lygis))+
  geom_point()+
  geom_smooth()+
  scale_x_continuous(breaks = seq(4, 18, 1))+
  scale_y_continuous(breaks = seq(-2, 12, 1))+
  labs(title = "Phillips'o kreivė Europos Sąjungoje",
       subtitle = "Šaltinis: Eurostat (lfsa_urgaed, prc_hicp_aind)",
       x="Nedarbo lygis, %",
       y="Infliacijos lygis, %")
# dev.off()




  