#Задание 2

# Зайцева Алина ДХ123ПАЭ

# создайте модель множественной линейной регрессии дневных потоков 
# углекислого газа за летний период 2013 года по данным измерений 
# методом турбулентной пульсации

# зададим дирректорию
rm(list=ls())
setwd("~/магистратура/1курс/моделирование/др")
getwd()

# подключим пакеты
library("tidyr")
library("tibble")
library("tidyverse") 
library("stringr")    
library("dplyr")      
library("ggplot2")  

#считаем данные и удалим ненужные строчки
eddypro = read_csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"), comment=c("["))

# Удаляем ненужную пустую первую строку
eddypro = eddypro[-1, ]

# Удаляем ненужный пустой столбец "roll"
eddypro = select(eddypro, -(roll))

# Преобразуем в факторы (factor)столбы типа char (символ)
eddypro = eddypro %>% mutate_if(is.character, factor)

#Заменим спец. символы в названии стобцов на допустимые для переменных имена
names(eddypro) = names(eddypro) %>% 
  str_replace_all("[!]", "_emph_") %>% 
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_div_") %>%
  str_replace_all("[%]", "_perc_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]", "L_")

#Возвратим столбцы таблицы в виде векторов для проверки
glimpse(eddypro)

# Удалим строки в которых содержится NA, так как они содержат неполные данные и только мешают
eddypro = drop_na(eddypro)

# отберем данные за летний период (1 июня по 31 августа 2013) и по дневному времени
eddypro = filter(eddypro, DOY >= 152 & DOY < 244)%>%filter(daytime)

# Получим таблицу, состоящую только из чисел. Будем работать с ней
eddypro_numeric = eddypro[,sapply(eddypro,is.numeric) ]

# Получим таблицу, содержащую остальные колонки
eddypro_non_numeric = eddypro[,!sapply(eddypro,is.numeric) ]

# создадим матрицу для 
# корелляционного анализа и преобразовываем ее в таблиу с нужным нам столбцом 
# потоки углекислого газа
cor_tdl = cor(eddypro_numeric)
cor_tdl
cor_tdl = cor(drop_na(eddypro_numeric)) %>% as.data.frame %>% select(co2_flux)

#выберем имена переменных (строк) 
#с коэффициентом детерминации больше 0.1
vars = row.names(cor_tdl)[cor_tdl$co2_flux^2 > .1] %>% na.exclude

# Блок создания модели регрессивного анализа

# создадим обучающую и тестирующую выборки
row_numbers = 1:length(eddypro_numeric$co2_flux)
teach = sample(row_numbers, floor(length(eddypro_numeric$co2_flux)*.7))
test = row_numbers[-teach]

#Обучающая выборка
teaching_tbl = eddypro_numeric[teach,]

#Тестирующая выборка
testing_tbl = eddypro_numeric[test,]

# Создадим модель 1, добавив в нее все переменные с помощью "(.)" 
# и используя обучающую выборку
mod1 = lm(co2_flux~ (.) , data = teaching_tbl)

# Получим информацию о моделе и коэффициенты
summary(mod1)
coef(mod1)
resid(mod1)
confint(mod1)

# Проанализируем переменные по значимости
anova(mod1)

#Выведем графики
plot(mod1) 

# Создадим модель 2 добавив в неё значимые переменные из результатов 
# функции anova()(со значимостью до 0.01, соответственно ***,** и * пометки)
mod2 = lm(co2_flux~DOY + Tau + qc_Tau + rand_err_Tau + H + qc_H
          + rand_err_H + LE + qc_LE + rand_err_LE + rand_err_co2_flux
          + qc_co2_flux + h2o_flux + rand_err_h2o_flux + H_strg + h2o_v_minus_adv
          + h2o_molar_density + h2o_mole_fraction + h2o_mixing_ratio + co2_molar_density + co2_mole_fraction
          + co2_mixing_ratio + h2o_time_lag + sonic_temperature + air_temperature
          + air_pressure + air_density + air_heat_capacity + air_molar_volume + water_vapor_density + e + es
          + specific_humidity + RH + VPD + Tdew + u_unrot + v_unrot + w_unrot + u_rot + v_rot
          + w_rot + max_speed + wind_dir + yaw + pitch + u_star_ + TKE + L + L_z_minus_dL__div_L 
          + T_star_ + x_peak + x_offset + x_10_perc_ + x_30_perc_ + x_50_perc_ + x_70_perc_ + x_90_perc_ + un_Tau
          + Tau_scf + un_H + H_scf + un_LE + LE_scf + un_co2_flux + u_spikes + ts_var
          + co2_var + w_div_h2o_cov + co2_signal_strength_7200 + flowrate, data = teaching_tbl)

# Получим информацио о моделе и коэффициенты
summary(mod2)
coef(mod2)
resid(mod2)
confint(mod2)

# Проанализируем переменные по значимости
anova(mod2)

# Сравним с предыдущей моделью, не ухудшилась ли она
anova(mod2, mod1)

#Выведем графики
plot(mod2)

# Создадим модель 3, повторив отбрасывание
mod3 = lm(co2_flux~ DOY + Tau + qc_Tau + rand_err_Tau + H + qc_H 
          + rand_err_H + LE + qc_LE + rand_err_LE + rand_err_co2_flux
          + qc_co2_flux + h2o_flux + rand_err_h2o_flux + H_strg + h2o_v_minus_adv
          + h2o_molar_density + h2o_mole_fraction + h2o_mixing_ratio + co2_molar_density + co2_mole_fraction
          + co2_mixing_ratio + h2o_time_lag + sonic_temperature + air_temperature 
          + air_pressure + air_density + air_heat_capacity + air_molar_volume + water_vapor_density + e + es 
          + specific_humidity + RH + VPD + Tdew + u_unrot + v_unrot + w_unrot + u_rot + v_rot
          + w_rot + max_speed + wind_dir + yaw + pitch + u_star_ + TKE + L + L_z_minus_dL__div_L 
          + T_star_ + x_peak + x_offset + x_10_perc_ + x_30_perc_ + x_50_perc_ + x_90_perc_ + un_Tau 
          + Tau_scf + un_H + H_scf + un_LE + LE_scf + un_co2_flux + u_spikes + ts_var
          + co2_var + w_div_h2o_cov + flowrate, data = teaching_tbl)

# Получим информацио о модели и коэффициенты
summary(mod3)
coef(mod3)
resid(mod3)
confint(mod3)

# Проанализируем переменные по значимости
anova(mod3)

# Сравним с предыдущей моделью, не ухудшилась ли она
anova(mod3, mod2)

# Выведем графики
plot(mod3)

# Проведем корреляционный анализ переменных

# Выберем из таблицы только участвующие у линейной модели переменные 
cor_teaching_tbl = select(teaching_tbl, co2_flux, DOY, Tau, qc_Tau, rand_err_Tau, H, qc_H, rand_err_H,
                          LE, qc_LE, rand_err_LE, rand_err_co2_flux, qc_co2_flux, h2o_flux, rand_err_h2o_flux,
                          H_strg, h2o_v_minus_adv, h2o_molar_density, h2o_mole_fraction, h2o_mixing_ratio, co2_molar_density, 
                          co2_mole_fraction, co2_mixing_ratio, h2o_time_lag, sonic_temperature, air_temperature, air_pressure, 
                          air_density, air_heat_capacity, air_molar_volume, water_vapor_density, e, es, 
                          specific_humidity, RH, VPD, Tdew, u_unrot, v_unrot, w_unrot, u_rot, v_rot,
                          w_rot, max_speed, wind_dir, yaw, pitch, u_star_, TKE, L, L_z_minus_dL__div_L, 
                          T_star_, x_peak, x_offset, x_10_perc_, x_30_perc_, x_50_perc_, x_90_perc_, un_Tau, 
                          Tau_scf, un_H, H_scf, un_LE, LE_scf, un_co2_flux, u_spikes, ts_var,
                          co2_var, w_div_h2o_cov, flowrate)


# Получаем таблицу коэффициентов корреляций. И подправляем модель 3, 
# убирая из модели одну из двух коррелирующих между собой переменных 
# (начиная от коэффициента >= 0.7)
cor_tdl = cor(cor_teaching_tbl) %>% as.data.frame


# Графики по полученной моделе

# Проверка модели
# Построим точки h2o_flux от h2o_flux на значениях ОБУЧАЮЩЕЙ выборки. 
# Наложим предсказанные значения по модели 3 на ОБУЧАЮЩЕЙ выборке сверху в виде линии
# В идеале линия должна  пройти через все точки.  
# А так как у нас график h2o_flux от самой себя, то он должен идти под 45градусов
qplot(co2_flux , co2_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod3, teaching_tbl)))

# Теперь сделаем тоже самое на ТЕСТИРУЮЩЕЙ выборе
qplot(co2_flux , co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))

# Так как у нас модель зависит от множества переменных, мы можем вывести 
# много графиков зависимостей h2o_flux от учитываемых в моделе параметров
# В идеале предсказанная линия должна пройти через все точки, или как можно 
# ближе к ним на ТЕСТИРУЮЩЕЙ выборке
# Примеры
qplot(DOY, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(Tau, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(h2o_flux, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))

