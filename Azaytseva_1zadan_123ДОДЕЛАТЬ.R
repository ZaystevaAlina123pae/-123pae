setwd("~/магистратура/1курс/моделирование/1задание")
getwd ()
# устанавливаем пакеты
# install.packages ("тидиверс")
# install.packages ("rnoaa")
# открываем нужные нам пакеты
library(tidyverse)
library(rnoaa)
library(lubridate)

# # устанавливаем список метеостанций
# station_data = ghcnd_stations ()
# write.csv (station_data, file = "station_data.csv")
station_data = read.csv ( " station_data.csv " )

# # формируем список метеостанций
# создаем таблицу с именем региона и координатами его столицы
novosibirsk  =  data.frame ( id = "novosibirsk", latitude = 55.0415000,  longitude = 82.9346000 )
# выбираем метеостанции в фиксированном радиусе от Оренбурга,
# которые имеют необходимые данные для определенного периода
novosibirsk_around = meteo_nearby_stations(lat_lon_df = novosibirsk, 
                                           station_data = station_data, 
                                           limit = 25, 
                                           var = c( "TAVG"), 
                                           year_min = 2010, year_max = 2013)
# получили таблицу где указаны все индетификаторы метеостанций,
# отсортированных по удаленности от Новосибирска
# первый из низ будет индентификатор Новосибирска
# получаем индентификатор метеостанции Новосибирска
novosibirsk_id = novosibirsk_around [["novosibirsk"]][["id"]][1]
summary ( novosibirsk_id )
# для получения таблицы со всеми метеостанциями вокруг Оренбурга
# необходимо выбрать целиком первый объект из списка
novosibirsk_table = novosibirsk_around [[ 1 ]]
summary ( novosibirsk_table )

# выведем индетификаторы отфильрованных метеостанций
novosibirsk_table $ id
# # скачивание погодных данных для наших метеостанций
# чтобы получить вскрытие данных с 1 метеостанции используйте команду meteo_tidy_ghcnd
all_novosibirsk_data = meteo_tidy_ghcnd ( stationid  =  novosibirsk_id )
# посмотрим что мы скачали
summary ( all_novosibirsk_data )
# создать цикл, в котором бы скачивались нужные данные для всех метеостанций
# cоздадим объект, куда скачаем все данные всех метеостанций
all_novosibirsk_meteodata  =  data.frame ()
# создаем цикл для наших 25 метеостанций
stations_names = novosibirsk_table $ ID
stations_names = stations_names [ 1 : 25 ]

for ( sname  in  stations_names )
{ One_meteo = meteo_tidy_ghcnd ( stationid  =  SNAME ,
                                 date_min  =  " 2005-01-01 " ,
                                 date_max  =  " 2017-12-31 " )
  station_vars = names( one_meteo )
  if ( ! ( " tavg "  % в%  station_vars )) {
    if ( ! ( " tmax " % в%  station_vars )) {
      next ()
    }
    one_meteo = one_meteo % > % mutate ( tavg = ( tmax + tmin ) / 2 )}
  one_meteo = one_meteo % > % select ( id , date , tavg )
  one_meteo  =  one_meteo % > % mutate ( tavg = tavg / 10 )
  all_novosibirsk_meteodata = rbind ( all_novosibirsk_meteodata , one_meteo )}
# записываем полученные результаты
write.csv(all_novosibirsk_meteodata,"all_novosibirsk_meteodata.csv")
# считываем данные all_novosibirsk_meteodata.csv
all_novosibirsk_meteodata = read.csv("all_novosibirsk_meteodata.csv")
# смотрим что получилось
str ( all_novosibirsk_meteodata )

# добавим год, месяц, день
all_novosibirsk_meteodata = all_novosibirsk_meteodata %>% mutate ( year = year( date ),
                                                              month = month ( date ),
                                                              day = day ( date ))

#Отфильтруем данные за 2010-2013 год
years_novosibirsk_meteodata=filter(all_novosibirsk_meteodata, year %in% c(2010:2013))
# превратим NA в 0 и где tavg <5
years_novosibirsk_meteodata[is.na(years_novosibirsk_meteodata$tavg),"tavg"] = 0
years_novosibirsk_meteodata[years_novosibirsk_meteodata$tavg<5, "tavg"] = 0
summary ( all_novosibirsk_meteodata )

# сгруппируем метеостанции по id, месяцам и годам и проссумируем темперетатуру
# по этим группам, затем сгурппируем данные по месяцам и найдем среднее по месяцам
# для всех метеостанций
group_meteodata  = all_novosibirsk_meteodata %>% group_by (id,year,month)
sumT_group_meteodata  =  group_meteodata %>% summarize ( tsum = sum ( tavg ))
groups_month = sumT_group_meteodata %>% group_by ( month )
sumT_month = groups_month %>% summarize ( St = mean ( tsum ))
# # Подготовка к расчету по формуле Урожая ##
#Ввод констант:
afi=c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bfi=c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
di=c(0.00,0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)

#Коэффициент для экспозиции склона - считаем, что все поля идеально ровные
y = 1.0
#Коэффициент использования ФАР:
Kf=300
#Калорийность урожая культуры:
Qj=1600
#Сумма частей основной и побочной продукции:
Lj=2.2
#Стандартная влажность культуры:
Ej=25

# стандартная влажность культуры
# Рассчитаем Fi по месяцv
sumT_month  = sumT_month %>% mutate ( Fi=afi+bfi*y*St )
# Рассчитаем Yi
sumT_month  =  sumT_month %>% mutate ( Yi=((Fi*di)*Kf)/(Qj*Lj*(100-Ej)))
# # Расчитываем урожай
Yield = sum ( sumT_month $ Yi )
Yield
