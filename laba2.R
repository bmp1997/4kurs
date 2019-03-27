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


# Пакет "base" ----
x <- DT.import$Trade.Value.USD
y <- DT.import$Netweight.kg.median 
y[y == 0] <- NA
# график разброса с линией регрессии
png('Pic-01.png', width = 500, height = 500)

coplot(y ~ x | DT.import$Aug_2014,
       xlab = 'Стоимость поставки, долл.США',
       ylab = 'Масса поставки, кг',
       panel = function(x,y,...){
         points(x,y,...)
abline(lm(y~x))})
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
