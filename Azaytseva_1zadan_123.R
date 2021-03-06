# Задание 1

# Зайцева Алина 123ПАЭ

# Для региона для региона 54 рассчитайте урожайность пшеницы 
# в период с 2010 по 2013 год взяв для расчёта средние 
# суммы активных температур за эти годы, с 25 ближайших метеостанций

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
#write.csv(station_data, "stations.csv")
station_data = read.csv("stations.csv")

#После получения списка всех станций, получите список станций ближайших 
#к столице вашего региона,
#создав таблицу с именем региона и координатами его столицы
novosibirsk = data.frame(id = "novosibirsk", latitude = 55.0415000,  longitude = 82.9346000)

#выберем станции, для этого введем необходимые переменные и дата, 
#также уберем лимит, тк в задании он не указан
novosibirsk_around = meteo_nearby_stations(lat_lon_df = novosibirsk, 
                                    station_data = station_data, 
                                    limit = 25, var = c("PRCP", "TAVG"), 
                                    year_min = 2010, year_max = 2013)

#novosibirsk_around это список единственным элементом которого является таблица, 
#содержащая идентификаторы метеостанций отсортированных по их 
# удалленности от Новосибирска, очевидно что первым элементом таблицы будет 
#идентификатор метеостанции Омска, его то мы и попытаемся получить
novosibirsk_id = novosibirsk_around [["novosibirsk"]][["id"]][1]

#получение всех данных с метеостанций
summary (novosibirsk_id)
str(novosibirsk_around)
all_novosibirsk_data = meteo_tidy_ghcnd(stationid = novosibirsk_id)

#чтобы получить таблицу всех метеостанций вокруг Омска нужно выбрать целиком первый объект из списка
novosibirsk_table = novosibirsk_around[[1]]
summary(novosibirsk_table)

#Для получения всех данных с 1 метеостанции, зная ее идентификатор, используйте 
#след. команду
all_novosibirsk_data = meteo_tidy_ghcnd(stationid = novosibirsk_id)

#нам нужны среднесуточные температуры (tavg) выше 5 градусов c  2010 по 2013 гг.

###Нужно создать цикл, в котором бы скачивались  нужные данные для всех 
#метеостанций из созданного списка
#Создадим промежуточный объект, куда будем скачивать данные с конкретной метеостанции
all_i = data.frame()

#Создадим объект, куда скачаем все данные всех метеостанций
all_novosibirsk_meteodata = data.frame()

#Цикл для всех метеостанций
for(i in 1:25)
{
  print(i)
  print(novosibirsk_id)
  #Выберем нужные свойства 
  all_i = meteo_tidy_ghcnd(stationid = novosibirsk_table$id [i])
  all_i=all_i[,c("id","date","tavg")]
  #Соединяем данные, полученные на предыдущих и данном этапах цикла
  all_novosibirsk_meteodata=rbind (all_novosibirsk_meteodata, all_i)
}

#Записываем полученные результаты
#write.csv(all_novosibirsk_meteodata,"all_novosibirsk_meteodata.csv")

# считываем данные из файла all_omsk_meteodata.csv
all_novosibirsk_meteodata = read.csv("all_novosibirsk_meteodata.csv")

#посмотрим на данные
str(all_novosibirsk_meteodata)

#Создаем строки год, месяц, день в таблице
all_novosibirsk_meteodata=mutate(all_novosibirsk_meteodata, 
                                 year=year(date),
                                 month=month(date),
                                 day=day(date))

#Посмотрим на данные
str(all_novosibirsk_meteodata)

#Отфильтруем данные за 2010-2013 год
years_novosibirsk_meteodata=filter(all_novosibirsk_meteodata, year %in% c(2010:2013))

#Проверим результат
str(years_novosibirsk_meteodata)
summary(years_novosibirsk_meteodata)

#Приводим среднюю сумму температур за все месяцы в подходящую для расчета форму, для этого делим на 10
years_novosibirsk_meteodata[,"tavg"]=years_novosibirsk_meteodata$tavg/10
summary(years_novosibirsk_meteodata)

#Превратим в нули все NA и где  tavg <5
years_novosibirsk_meteodata[is.na(years_novosibirsk_meteodata$tavg),"tavg"] = 0
years_novosibirsk_meteodata[years_novosibirsk_meteodata$tavg<5, "tavg"] = 0

#проверяем, что температура получилась в или 0 или больше 5 градусов
summary(years_novosibirsk_meteodata)

#Группируем по метеостанциям, годам и месяцам
alldays=group_by(years_novosibirsk_meteodata,id,year,month)

#Просуммируем температуру по этим группам с помощью sum
sumT_alldays_novosibirsk=summarize(alldays,tsum=sum(tavg))

#Максимальная суммарная температура за месяц 732.90, то есть 732.90/30=24,43
summary(sumT_alldays_novosibirsk)

#Сгруппируем данные по месяцам
groups_novosibirsk_months=group_by(sumT_alldays_novosibirsk,month)
groups_novosibirsk_months

#Найдем для всех метеостанций и всех лет среднее по месяцам
sumT_months=summarize(groups_novosibirsk_months,St=mean(tsum))
sumT_months

#Теперь можем произвести расчет урожая по формуле

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

#Рассчитаем Fi помесяца
sumT_months=mutate(sumT_months,Fi=afi+bfi*y*St)

#Рассчитаем Yi
sumT_months=mutate (sumT_months, Yi = 10^6*((Fi * di) * Kf) / (Qj * Lj * (100-Ej)))

#Расчитываем урожай как сумму по месяцам:
Yield = sum(sumT_months$Yi); Yield

# Ответ: 15390396 дг/га = 15,390396 ц/га

# Урожайность пшеницы в Новосибирской области 
# составила 15,390396 ц/га, предположительно верно







 
