library('shiny')       # загрузка пакетов
library('data.table')

df <- data.table(read.csv('data_2015.csv', 
                          stringsAsFactors = F))


# размещение всех объектов на странице
shinyUI(
    # создать страницу с боковой панелью
    # и главной областью для отчётов
    pageWithSidebar(
        # название приложения:
        headerPanel('Бюджет фильмов за 2015 год на портале kinopoisk'),
        # боковая панель:
        sidebarPanel(
            sliderInput('Rating.range', 'Оценка кинопоиска:',
                        min = min(df$Kinopoisk.mark), max = max(df$Kinopoisk.mark), value = c(min(df$Kinopoisk.mark), max(df$Kinopoisk.mark))),
            sliderInput('Rating1.range', 'Оценка IMDB:',
                        min = min(df$IMDb.mark), max = max(df$IMDb.mark), value = c(min(df$IMDb.mark), max(df$IMDb.mark))),
            
            sliderInput('Runtime.range', 'Продолжительность:',
                        min = min(df$Runtime), max = max(df$Runtime), value = c(min(df$Runtime), max(df$Runtime))),
            sliderInput(               # слайдер: кол-во интервалов гистограммы
              'int.hist',                       # связанная переменная
              'Количество интервалов гистограммы:', # подпись
              min = 2, max = 10,                    # мин и макс
              value = floor(1 + log(50, base = 2)), # базовое значение
              step = 1)                             # шаг
        ),
        # главная область
        mainPanel(
            # текстовый объект для отображения
            textOutput('text1'),
            textOutput('text2'),
            textOutput('text3'),
            textOutput('text4'),
            textOutput('text5'),
            # гистограммы переменных
            plotOutput('hist1')
            )
        )
    )