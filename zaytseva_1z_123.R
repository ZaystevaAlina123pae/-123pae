# Задание 1

# Зайцева Алина 123ПАЭ

# Для региона 55 рассчитать урожайность пшеницы в 2013 году, 
# взяв для рассчета средние суммы
# активных температур за предыдущие 3 года, с метеостанций 
# на расстоянии от 90 до 180 км

#Проверка директории
setwd("~/магистратура/1курс/моделирование/1задание")
getwd ()

#Устанавливаем пакеты
#install.packages("tidyverse")
#install.packages("rnoaa")
#install.packages ( " lubridate " )

#подключаем пакеты
library(tidyverse)
library(rnoaa)
library(lubridate)

#скачиваем станции 
station_data = ghcnd_stations()

#сохраняем результат в отдельный файл 
write.csv(station_data, "stations.csv")
station_data = read.csv("stations.csv")

#После получения списка всех станций, получите список станций ближайших 
#к столице вашего региона,
#создав таблицу с именем региона и координатами его столицы
omsk = data.frame(id = "OMSK", latitude = 54.989342,  longitude = 73.368212)

#выберем станции, для этого введем необходимые переменные и дата, 
#также уберем лимит, тк в задании он не указан
omsk_around = meteo_nearby_stations(lat_lon_df = omsk, 
                                    station_data = station_data, 
                                    var = c("PRCP", "TAVG"), 
                                    year_min = 2010, year_max = 2012)

#omsk_around это список единственным элементом которого является таблица, 
#содержащая идентификаторы метеостанций отсортированных по их 
# удалленности от Омска, очевидно что первым элементом таблицы будет 
#идентификатор метеостанции Омска, его то мы и попытаемся получить
omsk_id = omsk_around[["OMSK"]][["id"]][1]

#получение всех данных с метеостанций
summary (omsk_id)
str(omsk_around)
all_omsk_data = meteo_tidy_ghcnd(stationid = omsk_id)

#чтобы получить таблицу всех метеостанций вокруг Омска нужно выбрать целиком первый объект из списка
omsk_table = omsk_around[[1]]
summary(omsk_table)


# отфильтруем все станции, на расстоянии от 90 до 180 км
#Это можно сделать или задав соответствующее условие в [,] или с помощью команды фильтр
omsk_table = filter (omsk_table, distance > 90 & distance < 180 )

#нужно убедится, что этот список включает нужные по условию задачи метеостанции
omsk_stations = omsk_table 
str(omsk_stations)

#Таким образом, мы сформировали список необходимых станций, посмотрим, что он содержит
omsk_stations$id

# получилось 9 станций в нужном нам километраже

#Для получения всех данных с 1 метеостанции, зная ее идентификатор, используйте 
#след. команду
all_omsk_data = meteo_tidy_ghcnd(stationid = omsk_id)

#посмотрим, что же скачивается
?meteo_tidy_ghcnd
summary(all_omsk_data)

#нам нужны среднесуточные температуры (tavg) выше 5 градусов c  2010 по 2012 гг.

###Нужно создать цикл, в котором бы скачивались  нужные данные для всех 
#метеостанций из созданного списка
#Создадим промежуточный объект, куда будем скачивать данные с конкретной метеостанции
all_i = data.frame()

#Создадим объект, куда скачаем все данные всех метеостанций
all_omsk_meteodata = data.frame()

#Цикл для всех метеостанций
#выберем нужные свойства с помощью [,c]
#с помощью команды rbind соединяем данные, полученные на предыдущих и данном
#этапах цикла
for(i in 1:9) 
{ 
  all_i  = meteo_tidy_ghcnd(stationid =  omsk_around[["OMSK"]][["id"]][i])
  
  all_i = all_i[ ,c("id","date","tavg")] 
  
  print(all_i)
  all_omsk_meteodata=rbind(all_omsk_meteodata, all_i)
}

#Записываем полученные результаты
write.csv(all_omsk_meteodata,"all_omsk_meteodata.csv")

# считываем данные из файла all_omsk_meteodata.csv
all_omsk_meteodata = read.csv("all_omsk_meteodata.csv")

#посмотрим на данные
str(all_omsk_meteodata)

# вытащить год
#проверим, что работает
y = year(all_omsk_meteodata$date); y
all_omsk_meteodata [,"year"]= year(all_omsk_meteodata$date)

#добавим месяц
all_omsk_meteodata [,"month"]= month(all_omsk_meteodata$date)

#вытащить день от начала года
all_omsk_meteodata [,"day_of_the_year"]= yday(all_omsk_meteodata$date)

#проверим результат
str(all_omsk_meteodata)    
#отфильтруем данные за 2010-2012
years_omsk_meteodata = filter (all_omsk_meteodata, year >= 2010 & year <= 2012 )   

#проверим результат
str(years_omsk_meteodata)
summary (years_omsk_meteodata)    

#Средняя (по годам и метеостанциям) сумма активных температур за месяц

#Изучаем формулу и видим, что единственное, что нужно расчитать
#- это сумму температур больше 5 град. по месячно, остальное в формуле-  константы

#### 1.  температурy нужно поделить на 10
years_omsk_meteodata[,"tavg"]= years_omsk_meteodata$tavg / 10
summary (years_omsk_meteodata)

#### 2. Превратим в нули все NA и где tavg больше 5 градусов
years_omsk_meteodata [is.na(years_omsk_meteodata$tavg), "tavg"] = 0
years_omsk_meteodata [years_omsk_meteodata$tavg<5, "tavg"] = 0

#проверяем, что температура получилась в или 0 или больше 5 градусов
summary(years_omsk_meteodata)

#### 3. суммарная температура за месяц за 3 года для всех станций 
# группируем по метеостанциям, годам и месяцам
alldays= group_by(years_omsk_meteodata,id,year,month)

#функция summarize применяет некоторые действия к отдельным группам, полученным
#с помощью функции group_by
#просуммируем температуру по этим группам с помощью sum
sumT_alldays_omsk = summarize(alldays, tsum = sum(tavg))

#Получилось - все года, все месяца присутствуют
summary(sumT_alldays_omsk)
755.6/30
# максимальная суммарная температура за месяц 755,6, то есть 755,6/30=25,18

#Сгруппирем данные по месяцам  
groups_omsk_months = group_by(sumT_alldays_omsk,month)
groups_omsk_months

#найду для всех метеостанций и ВСЕХ лет среднее по месяцам
sumT_months= summarize(groups_omsk_months , St = mean(tsum))
sumT_months

##Подготовка к расчету по формуле Урожая
###Ввод констант
afi=c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)
bfi=c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)
di=c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000)
y = 1.0 # - коэффициент для экспозиции склона - считаем, что все поля идеально ровные;
Kf = 300 # - коэффициент использования ФАР посевом;
Qj = 1600 # - калорийность урожая культуры; 
Lj = 2.2 #  - сумма частей основной и побочной продукции; 
Ej = 25 # - стандартная влажность культуры;

# Рассчитаем Fi по месяцам
#Fi= afi+bfi∗y∗(St>5℃)
sumT_months = mutate(sumT_months, Fi = afi+bfi*y*St)

#Рассчитаем Yi
sumT_months = mutate(sumT_months, Yi = 10^6*((Fi*di)*Kf)/(Qj*Lj*(100 - Ej)))

##  Расчитываем урожай как сумму по месяцам и думаем разумный ли он
Yield = sum(sumT_months$Yi)  
Yield

# Ответ: 17870937 г/га = 17,87 т/га = 178,7 ц/га. Урожайность превышена 