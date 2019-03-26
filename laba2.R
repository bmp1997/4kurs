# загрузка пакетов ----
library('data.table')          # работаем с объектами "таблица данных"
library('moments')             # коэффициенты асимметрии и эксцесса 
library('lattice')
library('ggplot2')

# загружаем файл с данными по импорту масла в РФ (из прошлой практики) ----
fileURL <- 'https://raw.githubusercontent.com/aksyuk/R-data/master/COMTRADE/040510-Imp-RF-comtrade.csv'
# создаём директорию для данных, если она ещё не существует:
if (!file.exists('./data')) {
  dir.create('./data')
}
# создаём файл с логом загрузок, если он ещё не существует:
if (!file.exists('./data/download.log')) {
  file.create('./data/download.log')
}
# загружаем файл, если он ещё не существует,
#  и делаем запись о загрузке в лог:
if (!file.exists('./data/040510-Imp-RF-comtrade.csv')) {
  download.file(fileURL, './data/040510-Imp-RF-comtrade.csv')
  # сделать запись в лог
  write(paste('Файл "040510-Imp-RF-comtrade.csv" загружен', Sys.time()), 
        file = './data/download.log', append = T)
}
# читаем данные из загруженного .csv во фрейм, если он ещё не существует
if (!exists('DT')){
  DT.import <- data.table(read.csv('./data/040510-Imp-RF-comtrade.csv', 
                                   stringsAsFactors = F))
}
# предварительный просмотр ----
dim(DT.import)            # размерность таблицы
str(DT.import)            # структура (характеристики столбцов)
DT.import          # удобный просмотр объекта data.table


# сколько NA в каждом из оставшихся столбцов? ----
na.num <- apply(DT.import, 2, function(x) length(which(is.na(x))))
# выводим только положительные и по убыванию
sort(na.num[na.num > 0], decreasing = T)
# явное преобразование типа, чтобы избежать проблем 
#  при заполнении пропусков
DT.import[, Netweight.kg := as.double(Netweight.kg)]
# считаем медианы и округляем до целого, как исходные данные
DT.import[, round(median(.SD$Netweight.kg, na.rm = T), 0),
          by = Year]
# сначала копируем все значения
DT.import[, Netweight.kg.median := round(median(.SD$Netweight.kg,
                                                na.rm = T), 0),
          by = Year]
# затем заменяем пропуски на медианы
DT.import[!is.na(Netweight.kg), Netweight.kg.median := Netweight.kg]
# смотрим результат
DT.import[, Netweight.kg, Netweight.kg.median]
DT.import[is.na(Netweight.kg), Year, Netweight.kg.median]


# фильтр ----
DT.import$Aug_2014 <- 'до_2014'
DT.import$Aug_2014[DT.import$Period > 201408] <- 'после_2014'
unique(DT.import$Aug_2014)
mnth.f <- as.factor(unique(DT.import$Aug_2014))
cls <- palette(rainbow(2))


# Пакет "base" ----
x <- DT.import$Trade.Value.USD
y <- DT.import$Netweight.kg.median 
y[y == 0] <- NA

# оценка регрессии с помощью МНК
fit <- lm(y ~ x)
# график разброса с линией регрессии
png('Pic-01.png', width = 500, height = 500)
plot(x, y,
     xlab = 'Стоимость поставки, долл.США',
     ylab = 'Масса поставки, кг',
     main = 'Пакет base',
     pch = 21, col = rgb(0, 0, 0, alpha = 0.4),
     bg = cls[as.factor(DT.import$Aug_2014)])

# 2. добавляем прямую регрессии на график
abline(fit, col = rgb(0, 0, 1), lwd = 2)
# легенда
legend('topright', legend = mnth.f, fill = cls[mnth.f])
dev.off()


# Пакет "lattice" ----
png('Pic-02.png', width = 500, height = 500)
# графики разброса с линиями регрессий
xyplot(y ~ x | DT.import$Aug_2014, data = DT.import,
       ylab = 'Масса поставки, кг',
       xlab = 'Стоимость поставки, долл.США',
       main = 'Пакет lattice',
       panel = function(x, y, ...) {
         # вызов функции по умолчанию (график разброса)
         panel.xyplot(x, y, ...)
         # затем накладываем линии регрессии
         panel.lmline(x, y, col = 'red')
         
       })
dev.off()

# Пакет "ggplot2" ----

png('Pic-03.png', width = 500, height = 500)
gp <- ggplot(data = DT.import, aes(x = Trade.Value.USD, y = Netweight.kg.median))
gp <- gp + geom_point()
gp <- gp + facet_grid(. ~ Aug_2014)
gp <- gp + geom_smooth(method = 'lm')
gp <- gp + xlab('Стоимость поставки, долл.США')
gp <- gp + ylab('Масса поставки, кг')
gp <- gp + ggtitle('Пакет ggplot2')
gp
dev.off()
