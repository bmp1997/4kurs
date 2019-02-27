# Загрузка пакетов
library('XML')                 # разбор XML-файлов
library('RCurl')               # работа с HTML-страницами
library('rjson')               # чтение формата JSON
library('rvest')     # работа с DOM сайта
library('dplyr')     # инструменты трансформирования данных

#Соберем данные по 100 лучшим фильмам за 2017 год по версии Кинопоиска

#загружаем URL

url <- 'https://www.kinopoisk.ru/top/navigator/m_act[year]/2015/m_act[num_vote]/100/m_act[rating]/1%3A/m_act[gross]/%3A800/m_act[gross_type]/domestic/order/budget/#results'

# читаем HTML страницы
webpage <- read_html(url)

# отбор названий фильмов по селектору
title_data <- html_nodes(webpage,'div.name a') %>% html_text
head(title_data)
str(title_data)

#оценка с сайта IMDb
imdb <- html_nodes(webpage,'div.imdb') %>% html_text
head(imdb)
imdb <- gsub('IMDb: ','',imdb)
imdb <- gsub('\\s\\d*','',imdb)
imdb <- as.numeric(imdb) #final version for this
head(imdb)
str(imdb)

#количество оценивших на IMDb
Imdb_num <- html_nodes(webpage,'div.imdb') %>% html_text 
head(Imdb_num)
Imdb_num <- gsub('IMDb: \\d.\\d{2}','',Imdb_num)
Imdb_num <- gsub('\\s', '', Imdb_num)
Imdb_num <- as.numeric(Imdb_num)
head(Imdb_num)
str(Imdb_num)

#информация по фильмам
info <- html_nodes(webpage, 'div.name span') %>% html_text
head(info)

#названия на английском если имеются
eng <- gsub('\\s\\(.*','',info)
head(eng)
str(eng)

#продолжительность фильмов
time <- gsub('.*\\)\\s', '',info)
head(time)
time <- as.numeric(gsub('\\sмин.', '',time))
head(time)
str(time)

#режиссер
shef <- html_nodes(webpage, 'span i') %>% html_text()
shef <- gsub('реж. ', '',shef)
head(shef)
str(shef)

#оценки фильма с сайта кинопоиск
kino_mark <- html_nodes(webpage, '.numVote') %>% html_text()
head(kino_mark)
str(kino_mark)

#числовое выражение оценки
mark <- gsub('\\s\\(.*\\)','',kino_mark)
mark <- as.numeric(mark)
head(mark)
str(mark)

#количество поставивших оценку
per_mark <- gsub('\\d.\\d*\\s\\(', '',kino_mark)
per_mark <- gsub('\\)','',per_mark)
per_mark <- gsub('\\s','', per_mark)
per_mark <- as.numeric(per_mark)
head(per_mark)
str(per_mark)

#бюджет фильма
cost <- html_nodes(webpage, '.gray3') %>% html_text()
head(cost)
cost <- gsub('\\s', '',cost)
cost <- gsub('[$]','', cost)
head(cost)
str(cost)


DF.movies <- data.frame('Title'=title_data,
                        'English title'=eng,
                        'Runtime'=time,
                        'Producer'=shef,
                        'Cost'=cost,
                        'Kinopoisk mark'=mark,
                        'Marks from kinopoisk'=per_mark,
                        'IMDb mark'=imdb,
                        'Marks from IMDb'=Imdb_num)
dim(DF.movies)
str(DF.movies)

#записать файл csv
write.csv(DF.movies, file = "../data_movies_2017.csv", row.names = F)

