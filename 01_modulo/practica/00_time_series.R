library(tidyverse)
library(fpp3)

data("global_economy")
data("tourism")
data("PBS")
global_economy
tourism
PBS
#### Operaciones Simples

mydata <- tsibble(
  year = 2015:2019, 
  y = c(123,39,78,52,110), 
  index = year
)

mydata


PBS %>% 
  filter(ATC2 == "A10") %>% 
  select(Month, Concession, Type, Cost) %>% 
  summarise(TotalC = sum(Cost)) %>% 
  mutate(Cost = TotalC / 1000000) -> a10


################################ Practica tsibble
data("olympic_running")

olympic_running

olympic_running %>% distinct(Sex)

a10

a10 %>% 
  autoplot(Cost) +
  labs(y = "$ (millons", title = "Australian antidiabetic drug sales")


a10 %>% 
  ggplot(aes(x = Month, y = Cost)) +
  geom_point()

a10 %>% 
  ggplot(aes(x = Month, y = Cost)) +
  geom_line()


a10 %>% autoplot(Cost)
a10 %>% autoplot(Cost) + geom_point()
a10 %>% autoplot(Cost) +
  labs(title = "Antidiabetic drug sales", 
       y = "$ Millon")

data("ansett")

ansett

ansett %>% 
  autoplot(Passengers)

ansett %>% distinct(Class)
ansett %>% distinct(Airports)

# 10 aereopuertos * 3 clases = 30 time series unicas

ansett %>% 
  filter(Class == 'Economy') %>% 
  autoplot()

## Filtrando series particulares

melsyd_economy <- ansett %>% 
  filter(Airports == 'MEL-SYD') %>% 
  select(-Airports)

melsyd_economy
melsyd_economy %>% 
  autoplot()

#### Todos los viajes de Melbourne hacia Sydney por la 3ra clase

melsyd_economy %>% 
  filter(Class == 'Economy') %>% 
  mutate(Passengers = Passengers /1000) %>% 
  autoplot(Passengers) +
  labs(title = 'Ansett airlines economy class', 
       subtitle = 'Melbourne-Sydney', 
       y = "Passengers ('0000)")
  
