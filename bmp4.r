# загрузка пакетов
library('R.utils')               # gunzip() для распаковки архивов 
library('sp')                    # функция spplot()
library('ggplot2')               # функция ggplot()
library('RColorBrewer')          # цветовые палитры
require('rgdal')                 # функция readOGR()
library('broom')                 # функция tidy()
require('dplyr')                 # функция join()
library('scales')                # функция pretty_breaks()
library('mapproj')
library('gpclib')
library('maptools')

# Объем перевезок за 2017 год ----
gpclibPermit()

ShapeFileURL <- "https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_RUS_shp.zip"
if (!file.exists('./data')) dir.create('./data')
if (!file.exists('./data/gadm36_RUS_shp.zip')) {
  download.file(ShapeFileURL, destfile = './data/gadm36_RUS_shp.zip')
}
# распаковать архив
unzip('./data/gadm36_RUS_shp.zip', exdir = './data/gadm36_RUS_shp')
# посмотреть список файлов распакованного архива
dir('./data/gadm36_RUS_shp')

# прочитать данные уровней 0, 1, 2
Regions0 <- readOGR("./data/gadm36_RUS_shp/gadm36_RUS_0.shp")
Regions1 <- readOGR("./data/gadm36_RUS_shp/gadm36_RUS_1.shp")
Regions2 <- readOGR("./data/gadm36_RUS_shp/gadm36_RUS_2.shp")

# делаем фактор из имён областей (т.е. нумеруем их)
Regions1@data$NAME_1 <- as.factor(Regions1@data$NAME_1)

# загружаем статистику с показателями по регионам
fileURL <- 'https://raw.githubusercontent.com/bmp1997/4kurs/laba4/population1.csv'
stat.Regions <- read.csv2(fileURL, stringsAsFactors = F)
stat.Regions$population_2017 <- as.numeric(stat.Regions$population_2017)

# вносим данные в файл карты
Regions1@data <- merge(Regions1@data, stat.Regions,
                       by.x = 'NAME_1', by.y = 'Region')

# задаём палитру
mypalette <- colorRampPalette(c('whitesmoke', 'coral3'))


spplot(Regions1, 'population_2017', main = 'Население России по регионам (тыс. человек)',
       col.regions = mypalette(10), # цветовая шкала
       # (10 градаций)
       col = 'coral4', # цвет контурных линий
       par.settings = list(axis.line = list(col = NA)) # без
       # осей
)


# Население Северной Осетии за 2018 год ----
gpclibPermit()

fileURL <- 'https://raw.githubusercontent.com/bmp1997/4kurs/laba4/population.csv'
Regions2@data$NAME_2 <- as.factor(Regions2@data$NAME_2)
stat.Regions <- read.csv2(fileURL, stringsAsFactors = F)

Regions <- readOGR(dsn = './data/gadm36_RUS_shp', # папка
                   layer = 'gadm36_RUS_2') # уровень 
Regions@data$id <- Regions@data$NAME_2
Regions <- Regions[grepl('^RU.NO.', Regions$HASC_2), ]
Regions.points <- fortify(Regions, region = 'id')
Regions.df <- merge(Regions.points, Regions@data, by = 'id')
stat.Regions$id <- stat.Regions$Districts
Regions.df <- merge(Regions.df,
                    stat.Regions[, c('id',
                                     'Population')],
                    by = 'id')

centroids.df <- as.data.frame(coordinates(Regions))
centroids.df$id <- Regions@data$id
colnames(centroids.df) <- c('long', 'lat', 'id')


gp <- ggplot() +
  geom_polygon(data = Regions.df,
               aes(long, lat, group = group,
                   fill = Population)) +
  geom_path(data = Regions.df,
            aes(long, lat, group = group),
            color = 'coral4') +
  coord_map(projection = 'gilbert') +
  scale_fill_distiller(palette = 'OrRd',
                       direction = 1,
                       breaks = pretty_breaks(n = 5)) +
  labs(x = 'Долгота', y = 'Широта',
       title = "Население Северной Осетии за 2018 год") +
  geom_text(data = centroids.df,
            aes(long, lat, label = id))
gp