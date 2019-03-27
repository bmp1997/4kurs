library('shiny')       # загрузка пакетов
library('lattice')
library('plyr')
library('data.table')

df <- data.table(read.csv('data_2015.csv', 
                          stringsAsFactors = F))

shinyServer(function(input, output) {
  output$text1 <- renderText({
    paste0('Вы выбрали оценку kinopoisk с ', 
           input$Rating.range[1], ' по ', input$Rating.range[2]
    )
  })
    output$text2 <- renderText({
      paste0('Вы выбрали оценку IMDB с ',
             input$Rating1.range[1], ' по ', input$Rating1.range[2]
             )
    })
    output$text3 <- renderText({
      paste0('Вы выбрали продолжительность с ',
             input$Runtime.range[1], ' по ', input$Runtime.range[2]
             )
    })
    output$gn.text4 <- renderText({
      paste0('Всего фильмов - ', nrow(df)
      )
    })
    # строим гистограммы переменных
    output$hist1 <- renderPlot({
        # сначала фильтруем данные
        DF <- df[between(df$Kinopoisk.mark, input$Rating.range[1], input$Rating.range[2])]
        DF <- DF[between(DF$IMDb.mark, input$Rating1.range[1], input$Rating1.range[2])]
        DF <- DF[between(DF$Runtime, input$Runtime.range[1], input$Runtime.range[2])]
        
    output$text5 <- renderText({
      paste0('Отобранных фильмов - ', nrow(DF)
             )
      })

        # затем строим график
        histogram( ~ Cost, 
                   data = DF,
                   xlab = '',
                  breaks = seq(min(DF$Cost), max(DF$Cost), 
                               length = input$int.hist + 1)
                   )
    })
})
