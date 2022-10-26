library(tidyverse)
library(readxl)

#Load the data
df <- read_excel('Wnioskodawcy_w_konkursach_NCN_rozstrzygnietych_w_latach_2011-2021_wraz_z_panelami_dziedzinowymi_kopia.xlsx')


#make 3 dataframes and pivot them longer
df_złożone <- df %>% 
  select(c(Grupa_nauk, Panel_dziedzinowy, Nazwa_panelu_dziedzinowego, `Nazwa_jednostki_naukowej`,
           złozone_2011, zlozone_2012, złozone_2013, zlozone_2014, złozone_2015, zlozone_2016, złozone_2017,
           zlozone_2018, złozone_2019, zlozone_2020, złozone_2021)) %>% 
  pivot_longer(!c(Grupa_nauk, Panel_dziedzinowy, Nazwa_panelu_dziedzinowego, Nazwa_jednostki_naukowej),
               names_to = 'rok', values_to = 'liczba_złożonych_wniosków')
df_złożone$rok <- gsub(".*_",'',df_złożone$rok)


df_pozyskane <- df %>% 
  select(c(Grupa_nauk, Panel_dziedzinowy, Nazwa_panelu_dziedzinowego, Nazwa_jednostki_naukowej,
           pozyskane_2011, pozyskane_2012, pozyskane_2013, pozyskane_2014, pozyskane_2015, pozyskane_2016,
           pozyskane_2017,pozyskane_2018, pozyskane_2019, pozyskane_2020, pozyskane_2021)) %>% 
  pivot_longer(!c(Grupa_nauk, Panel_dziedzinowy, Nazwa_panelu_dziedzinowego, Nazwa_jednostki_naukowej),
               names_to = 'rok', values_to = 'liczba_pozyskanych_wniosków')
df_pozyskane$rok <- gsub(".*_",'',df_pozyskane$rok)




#merge 3 dataframes into one and delete institutions that submitted 0 proposals in a given year
#and calculate success rate for each intitution in a given panel each year
merged_df <- merge(df_złożone, df_pozyskane) %>% 
  subset(., liczba_złożonych_wniosków !=0) %>% 
  mutate(sukces=liczba_pozyskanych_wniosków/liczba_złożonych_wniosków)


by_inst <- merged_df %>% 
  select(c('Nazwa_jednostki_naukowej','liczba_pozyskanych_wniosków',
           'liczba_złożonych_wniosków')) %>% 
  filter(liczba_złożonych_wniosków>5) %>% 
  filter(Nazwa_jednostki_naukowej != 'Instytut Problemów Jądrowych im. Andrzeja Sołtana') %>% 
  group_by(Nazwa_jednostki_naukowej) %>% 
  summarise(across(everything(), sum)) %>% 
  mutate(sukces=liczba_pozyskanych_wniosków/liczba_złożonych_wniosków) %>% 
  ungroup() 


mean_sukces <- mean(by_inst$sukces)

best_inst <- by_inst %>% 
  filter(sukces>2*(mean_sukces))


best_inst %>% 
  ggplot(aes(y=reorder(Nazwa_jednostki_naukowej, sukces), x=sukces,))+
  geom_bar(stat = 'identity', fill='darkgray')+
  geom_vline(xintercept=mean_sukces, linetype='dashed', color='lightgray')+
  scale_x_continuous(labels = scales::percent, limits = c(0,0.63))+
  theme_bw()+
  ggtitle('Polish Research Facilities with the highest success rate in obtaining NCN Funds (2011-2021)')+
  ylab('Polish Research Facility')+
  xlab('Facility\'s Success Rate (2011-2021)')+
  theme(plot.title = element_text(hjust=1))

ggsave('best institutes.png', width=8, height = 4) 