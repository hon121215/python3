#setwd("C://r_temp")
pacotes = c("shiny", "shinydashboard", "shinythemes", "plotly", "shinycssloaders","tidyverse","formattable",
            "scales", "knitr", "kableExtra", "ggfortify","dplyr","plotly","FNN","leaflet","ggplot2","readxl")

package.check <- lapply(pacotes, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})
load("BJH.RData")

# UI -------
ui <- navbarPage("Kleague & KBO",
                 tabPanel("Stadium",
                          fluidPage(
                            titlePanel("Stadium"),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("sport_select", "경기 종류", choices = c("전체", "축구", "야구")),
                                selectInput("stadium_select", "경기장 선택", choices = NULL),
                                actionButton("show_map_btn", "지도 표시")
                              ),
                              mainPanel(
                                leafletOutput("map_output", width = "100%", height = "800px")
                              )
                            )
                          )),
                 tabPanel("K-league",
                          tabsetPanel(
                            tabPanel("K-Team",
                                     tabsetPanel(
                                       tabPanel("Attack",
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    selectInput("attack_team", "Attack Team", choices = k_league_11$소속팀)
                                                  ),
                                                  mainPanel(
                                                    plotOutput("attack_radarPlot", height = "900px", width = "1000px")
                                                  )
                                                )),
                                       tabPanel("Defense",
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    selectInput("defense_team", "Defense Team", choices = k_league_12$소속팀)
                                                  ),
                                                  mainPanel(
                                                    plotOutput("defense_radarPlot", height = "900px", width = "1000px")
                                                  )
                                                ))
                                     )),
                            tabPanel("K-Player",
                                     tabsetPanel(
                                       tabPanel("K_Player",
                                                tabsetPanel(
                                                  tabPanel("GK",sidebarLayout(sidebarPanel(selectInput("player_1", "선수명", choices = unique(GK$선수명)),
                                                                                           selectInput("year_1", "년도", choices = unique(GK$년도))
                                                  ),mainPanel(plotOutput("radarChart_1", height = "900px", width = "1000px")))),
                                                  tabPanel("DF",sidebarLayout(sidebarPanel(selectInput("player_2", "선수명", choices = unique(DF$선수명)),
                                                                                           selectInput("year_2", "년도", choices = unique(DF$년도))
                                                  ),mainPanel(plotOutput("radarChart_2", height = "900px", width = "1000px")))),
                                                  tabPanel("MF",sidebarLayout(sidebarPanel(selectInput("player_3", "선수명", choices = unique(MF$선수명)),
                                                                                           selectInput("year_3", "년도", choices = unique(MF$년도))
                                                  ),mainPanel(plotOutput("radarChart_3", height = "900px", width = "1000px")))),
                                                  tabPanel("FW",sidebarLayout(sidebarPanel(selectInput("player_4", "선수명", choices = unique(FW$선수명)),
                                                                                           selectInput("year_4", "년도", choices = unique(FW$년도))
                                                  ),mainPanel(plotOutput("radarChart_4", height = "900px", width = "1000px")
                                                  )
                                                  )
                                                  )
                                                ))
                                     )
                            ))),
                 tabPanel("KBO",
                          tabsetPanel(
                            tabPanel("HITTER STAT",
                                     tabsetPanel(
                                       tabPanel("ALL SEASON",
                                                fluidPage(titlePanel("KBO Hitter Analysis"),
                                                          sidebarLayout(
                                                            sidebarPanel(),
                                                            mainPanel(
                                                              tabsetPanel(
                                                                tabPanel("Histogram", plotOutput("histogram")),
                                                                tabPanel("GPA Top 10", formattableOutput("gpa_top10")),
                                                                tabPanel("Batting Average Top 10", formattableOutput("avg_top10")),
                                                                tabPanel("Team Count", formattableOutput("team_count")),
                                                                tabPanel("Team Home Runs", formattableOutput("team_hr")),
                                                                tabPanel("Home Run Ratio", formattableOutput("hr_ratio")),
                                                                tabPanel("Max Home Runs by Team", formattableOutput("hr_max")),
                                                                tabPanel("Batting Distribution by Team", plotOutput("batting_boxplot"))))))),
                                       tabPanel("2023 SEASON",
                                                fluidPage(titlePanel("KBO Hitter 2023"),
                                                          sidebarLayout(
                                                            sidebarPanel(),
                                                            mainPanel(
                                                              tabsetPanel(
                                                                tabPanel("GPA Top Players", formattableOutput("gpa_table_1")),
                                                                tabPanel("Batting Average Top Players", formattableOutput("avg_table_1")),
                                                                tabPanel("Team Player Count", formattableOutput("team_count_1")),
                                                                tabPanel("Team Home Runs", formattableOutput("team_hr_1")),
                                                                tabPanel("Home Run Ratio", formattableOutput("hr_ratio_1")),
                                                                tabPanel("Top Home Run Players", formattableOutput("hr_top_1")),
                                                                tabPanel("Batting Distribution", plotOutput("batting_hist_1")),
                                                                tabPanel("Average Hits by Team", plotOutput("hits_boxplot_1"))))))))),
                            tabPanel("PITCHER STAT",
                                     tabsetPanel(
                                       tabPanel("ALL SEASON",
                                                fluidPage(titlePanel("KBO PITCHER Analysis"),
                                                          sidebarLayout(
                                                            sidebarPanel(),
                                                            mainPanel(
                                                              tabsetPanel(
                                                                tabPanel("All Starters - Top 10",fluidRow(column(12, formattableOutput("allf_top_10_table")))),
                                                                tabPanel("All Relievers - Top 10",fluidRow(column(12, formattableOutput("alls_top_10_table")))),
                                                                tabPanel("Past Starters - Top 10",fluidRow(column(12, formattableOutput("pastf_top_10_table")))),
                                                                tabPanel("Past Relievers - Top 10",fluidRow(column(12, formattableOutput("pasts_top_10_table"))))
                                                              )
                                                            )
                                                          )
                                                )
                                       ),
                                       tabPanel("2023 SEASON",
                                                fluidPage(titlePanel("KBO PITCHER Analysis"),
                                                          sidebarLayout(
                                                            sidebarPanel(),
                                                            mainPanel(
                                                              tabsetPanel(
                                                                tabPanel("Current Starters - Top 10",fluidRow(column(12, formattableOutput("nowf_top_10_table")))),
                                                                tabPanel("Current Middle Relief - Top 10",fluidRow(column(12, formattableOutput("now_middle_top_10_table")))),
                                                                tabPanel("Current Closers - Top 10",fluidRow(column(12, formattableOutput("now_last_top_10_table"))))
                                                              )
                                                            )
                                                          )
                                                )
                                       )
                                     )))),
                 tabPanel("Data",
                          tabsetPanel(
                            tabPanel("K-league",
                                     fluidPage(titlePanel("Kleague"),
                                               mainPanel(dataTableOutput(outputId = "myTable")))),
                            tabPanel("KBO-hitter",
                                     fluidPage(titlePanel("KBO_hitter"),
                                               mainPanel(dataTableOutput(outputId = "kbohitter")))),
                            tabPanel("KBO-pitcher",
                                     fluidPage(titlePanel("KBO_pitcher"),
                                               mainPanel(dataTableOutput(outputId = "kbopitcher"))))))
                 
)


server <- function(input, output,session) {
  output$myTable <- renderDataTable(data)
  output$kbohitter <- renderDataTable(kbo_hitter)
  output$kbopitcher <- renderDataTable(kbo_pitcher)
  
  selected_sport <- reactive({
    if (input$sport_select == "전체") {
      return(c)
    } else if (input$sport_select == "축구") {
      return(a)
    } else {
      return(b)
    }
  })
  
  observe({
    stadiums <- unique(selected_sport()$stadium)
    updateSelectInput(session, "stadium_select", choices = c("전체", stadiums))
  })
  
  observeEvent(input$show_map_btn, {
    output$map_output <- renderLeaflet({
      if (input$stadium_select == "전체" & input$sport_select == "축구") {
        leaflet(a) %>%
          setView(lng = 127.3249, lat = 36.36530, zoom = 6) %>%
          addTiles() %>%
          fitBounds(lng1 = 125.5, lat1 = 34, lng2 = 128, lat2 = 38) %>%
          addMarkers(data = markerData_s, lng = ~LON, lat = ~LAT, 
                     popup = lapply(seq_len(nrow(markerData_s)), function(i) {
                       paste0("<a href='", markerData_s$linkUrl[i], "' target='_blank'>", markerData_s$iconUrl[i], "</a>")
                     }), icon = makeIcon(iconUrl = ~iconUrl, iconWidth = 50, iconHeight = 50))
      } else if (input$stadium_select == "전체" & input$sport_select == "야구") {
        leaflet(b) %>%
          setView(lng = 127.3249, lat = 36.36530, zoom = 6) %>%
          addTiles() %>%
          fitBounds(lng1 = 125.5, lat1 = 34, lng2 = 128, lat2 = 38) %>%
          addMarkers(data = markerData_b, lng = ~LON, lat = ~LAT,
                     popup = lapply(seq_len(nrow(markerData_b)), function(i) {
                       paste0("<a href='", markerData_b$linkUrl[i], "' target='_blank'>", markerData_b$iconUrl[i], "</a>")
                     }),
                     icon = makeIcon(iconUrl = ~iconUrl, iconWidth = 50, iconHeight = 50))
      } else if (input$stadium_select == "전체" & input$sport_select == "전체") {
        leaflet(c) %>%
          setView(lng = 127.3249, lat = 36.36530, zoom = 6) %>%
          addTiles() %>%
          fitBounds(lng1 = 125.5, lat1 = 34, lng2 = 128, lat2 = 38) %>%
          addMarkers(data = markerData_c, lng = ~LON, lat = ~LAT, 
                     popup = lapply(seq_len(nrow(markerData_c)), function(i) {
                       paste0("<a href='", markerData_c$linkUrl[i], "' target='_blank'>", markerData_c$iconUrl[i], "</a>")
                     }),
                     icon = makeIcon(iconUrl = ~iconUrl, iconWidth = 50, iconHeight = 50))
      } else {
        data <- selected_sport()[selected_sport()$stadium == input$stadium_select, ]
        lng <- data$LON[1]
        lat <- data$LAT[1]
        zoom <- 12
        
        leaflet() %>%
          setView(lng = lng, lat = lat, zoom = 16) %>%
          addTiles() %>%
          addMarkers(data = markerData_c, lng = ~LON, lat = ~LAT,
                     popup = lapply(seq_len(nrow(markerData_c)), function(i) {
                       paste0("<a href='", markerData_c$linkUrl[i], "' target='_blank'>", markerData_c$iconUrl[i], "</a>")
                     }),
                     icon = makeIcon(iconUrl = ~iconUrl,
                                     iconWidth = 70,
                                     iconHeight = 70))
      }
    })
  })
  
  output$attack_radarPlot <- renderPlot({
    team_selected_11 <- k_league_11[k_league_11$소속팀 == input$attack_team, -1]
    max_values_11 <- rep(100, 5)
    min_values_11 <- rep(0, 5)
    chart_data_11 <- rbind(max_values_11, min_values_11, team_selected_11)
    colnames(chart_data_11) <- c("패스성공", "지상경합성공", "공중경합성공", "드리블성공", "공격진영패스성공")
    
    radarchart(chart_data_11, axistype = 1, seg = 5, plty = 1, pcol = 2, plwd = 3, pfcol = NA,
               title = paste(input$attack_team, "Attack Chart"))
  })
  
  output$defense_radarPlot <- renderPlot({
    team_selected_12 <- k_league_12[k_league_12$소속팀 == input$defense_team, -1]
    max_values_12 <- rep(100, 5)
    min_values_12 <- rep(0, 5)
    chart_data_12 <- rbind(max_values_12, min_values_12, team_selected_12)
    colnames(chart_data_12) <- c("패스성공", "수비진영패스성공", "지상경합성공", "공중경합성공", "태클성공")
    
    radarchart(chart_data_12, axistype = 1, seg = 5, plty = 1, pcol = 2, plwd = 3, pfcol = NA,
               title = paste(input$defense_team, "Defense Chart"))
  })
  
  output$radarChart_1 <- renderPlot({
    # 데이터 필터링
    filtered_data_1 <- subset(GK, 선수명 == input$player_1 & 년도 == input$year_1)
    
    # 데이터 전처리
    k_league_GK <- filtered_data_1 %>%
      group_by(선수명,년도) %>%
      summarise(
        평균_패스 = mean(패스성공),
        평균_클리어링 = mean(클리어링),
        평균_수비진영패스성공 = mean(수비진영패스성공)
      )
    
    score_3 <- k_league_GK[1, -c(1, 2)]
    max.score_3 <- rep(100, 5)
    min.score_3 <- rep(0, 5)
    ds_3 <- rbind(max.score_3, min.score_3, score_3)
    colnames(ds_3) <- c("패스성공", "클리어링", "수비진영패스성공")
    
    # 레이더 차트 그리기
    radarchart(ds_3,axistype = 1,seg = 5,plty = 1,plwd = 2,pcol = 2,pfcol = NA,title = "GK")
  })
  
  output$radarChart_2 <- renderPlot({
    # 데이터 필터링
    filtered_data_2 <- subset(DF, 선수명 == input$player_2 & 년도 == input$year_2)
    
    # 데이터 전처리
    k_league_DF <- filtered_data_2 %>%
      group_by(선수명, 년도) %>%
      summarise(
        평균_패스 = mean(패스성공),
        평균_지상경합성공 = mean(지상경합성공),
        평균_공중경합성공 = mean(공중경합성공),
        평균_태클성공 = mean(태클성공),
        평균_인터셉트 = mean(인터셉트)
      )
    
    score_4 <- k_league_DF[1,-c(1,2)]
    score_4
    max.score_4 <- rep(100,5)
    min.score_4 <- rep(0,5)
    ds_4 <- rbind(max.score_4,min.score_4,score_4)
    ds_4 <- data.frame(ds_4)
    colnames(ds_4) <- c("패스성공", "지상경합성공", "공중경합성공","드리블성공", "공격진영패스성공")
    
    # 레이더 차트 그리기
    radarchart(ds_4,axistype = 1,seg = 5,plty = 1,plwd = 2,pcol = 2,pfcol = NA,title = "DF")
  })
  
  output$radarChart_3 <- renderPlot({
    # 데이터 필터링
    filtered_data_3 <- subset(MF, 선수명 == input$player_3 & 년도 == input$year_3)
    
    # 데이터 전처리
    k_league_MF <- filtered_data_3 %>%
      group_by(선수명, 년도) %>%
      summarise(
        평균_드리블 = mean(드리블성공),
        평균_패스성공 = mean(패스성공),
        평균_공격진영패스성공 = mean(공격진영패스성공),
        평균_수비진영패스성공 = mean(수비진영패스성공),
        평균_지상경합성공 = mean(지상경합성공)
      )
    
    score_5 <- k_league_MF[1,-c(1,2)]
    score_5
    max.score_5 <- rep(100,5)
    min.score_5 <- rep(0,5)
    ds_5 <- rbind(max.score_5,min.score_5,score_5)
    ds_5 <- data.frame(ds_5)
    colnames(ds_5) <- c("드리블성공", "패스성공", "공격진영패스성공","수비진영패스성공", "지상경합성공")
    
    # 레이더 차트 그리기
    radarchart(ds_5,axistype = 1,seg = 5,plty = 1,plwd = 2,pcol = 2,pfcol = NA,title = "MF")
  })
  
  output$radarChart_4 <- renderPlot({
    # 데이터 필터링
    filtered_data_4 <- subset(FW, 선수명 == input$player_4 & 년도 == input$year_4)
    
    # 데이터 전처리
    k_league_FW <- filtered_data_4 %>%
      group_by(선수명, 년도) %>%
      summarise(
        평균_득점 = mean(득점),
        평균_유효슈팅 = mean(유효슈팅),
        평균_드리블성공 = mean(드리블성공),
        평균_패스성공 = mean(패스성공),
        평균_공격진영패스성공 = mean(공격진영패스성공)
      )
    
    score_6 <- k_league_FW[1,-c(1,2)]
    max.score_6 <- rep(100,5)
    min.score_6 <- rep(0,5)
    ds_6 <- rbind(max.score_6,min.score_6,score_6)
    ds_6 <- data.frame(ds_6)
    colnames(ds_6) <- c("득점", "유효슈팅", "드리블성공","패스성공", "공격진영패스성공")
    
    # 레이더 차트 그리기
    radarchart(ds_6,axistype = 1,seg = 5,plty = 1,plwd = 2,pcol = 2,pfcol = NA,title = "FW")
  })
  kbo_hitter_2023 <- filter(kbo_hitter, 년도 == 2023)
  
  # Filter data for players with minimum at bats
  kbo_hitter10 <- filter(kbo_hitter_2023, 타석 > 10)
  
  # GPA Top Players
  output$gpa_table_1 <- renderFormattable({
    GPA_top_2023 <- select(kbo_hitter10, 선수명, 년도, 팀명, 홈런, 장타, GPA) %>% 
      filter(GPA >= 0.3) %>% arrange(desc(GPA))
    formattable(
      GPA_top_2023,
      align = c("l", "l", "r", "r", "r"),
      list(
        GPA = color_tile("white", "orange")
      )
    )
  })
  
  # Batting Average Top Players
  output$avg_table_1 <- renderFormattable({
    avg_top_2023 <- select(kbo_hitter10, 선수명, 년도, 팀명, 타수, 안타, 타율) %>% 
      filter(타율 >= 0.325) %>% arrange(desc(타율))
    formattable(
      avg_top_2023,
      align = c("l", "l", "r", "r", "r"),
      list(
        타율 = color_bar("lightblue"),
        `선수명` = formatter("span", style = ~ style(color = "blue", font.weight = "bold"))
      )
    )
  })
  
  # Team Player Count
  output$team_count_1 <- renderFormattable({
    Team_count_2023 <- kbo_hitter10 %>% 
      group_by(팀명) %>% 
      summarise(선수 = n()) %>% 
      arrange(desc(x = 선수))
    formattable(
      Team_count_2023,
      align = c("l", "r"),
      list(
        `선수` = color_bar("lightgreen")
      )
    )
  })
  
  # Team Home Runs
  output$team_hr_1 <- renderFormattable({
    Team_HR_2023 <- kbo_hitter10 %>% 
      group_by(팀명) %>% 
      summarise(팀홈런 = sum(홈런)) %>% 
      arrange(desc(x = 팀홈런))
    formattable(
      Team_HR_2023,
      align = c("l", "r"),
      list(
        `팀홈런` = color_tile("white", "skyblue")
      )
    )
  })
  
  # Home Run Ratio
  output$hr_ratio_1 <- renderFormattable({
    HR_ratio_2023 <- kbo_hitter10 %>% 
      group_by(팀명) %>% 
      summarise(평균홈런 = sum(홈런) / n()) %>% 
      arrange(desc(x = 평균홈런))
    formattable(
      HR_ratio_2023,
      align = c("l", "r"),
      list(
        `평균홈런` = color_tile("white", "pink")
      )
    )
  })
  
  # Top Home Run Players
  output$hr_top_1 <- renderFormattable({
    HR_max_2023 <- kbo_hitter10 %>% 
      group_by(팀명, 선수명) %>% 
      summarise(홈런 = max(홈런)) %>% 
      arrange(desc(x = 홈런))
    HR_top_2023 <- head(HR_max_2023, 10)
    formattable(
      HR_top_2023,
      align = c("l", "r"),
      list(
        `홈런` = color_bar("lightblue")
      )
    )
  })
  
  # Batting Distribution
  output$batting_hist_1 <- renderPlot({
    breaks_2023 <- seq(from = 0, to = 200, by = 10)
    hist(kbo_hitter10$타석, freq = TRUE, breaks = breaks_2023,
         col = 'gray30', main = 'Batting Histogram', xlab = 'At Bats', ylab = 'Frequency')
  })
  
  # Average Hits by Team
  output$hits_boxplot_1 <- renderPlot({
    boxplot(안타 ~ 팀명, data = kbo_hitter10,
            main = 'Average Hits by Team', ylab = 'Hits', family = 'serif')
    abline(h = median(x = kbo_hitter10$안타), col = 'red', lwd = 2)
  })
  output$histogram <- renderPlot({
    kbo_hitter100 <- kbo_hitter %>% filter(타석 > 100)
    breaks <- seq(0, 700, by = 50)
    hist(
      x = kbo_hitter100$타석,
      freq = TRUE,
      breaks = breaks,
      xlim = c(0, 700),
      ylim = c(0, 250),
      col = 'gray30',
      border = 'gray50',
      labels = TRUE,
      main = 'Histogram of Players with 100 or More Plate Appearances',
      xlab = 'Plate Appearances',
      ylab = 'Frequency'
    )
  })
  
  # GPA 상위 10명 테이블
  output$gpa_top10 <- renderFormattable({
    kbo_hitter100 <- kbo_hitter %>% filter(타석 > 100)
    gpa_top10 <- kbo_hitter100 %>%
      select(선수명, 팀명, 홈런, 장타, GPA) %>%
      filter(GPA >= 0.35) %>%
      arrange(desc(GPA))
    formattable(
      gpa_top10,
      align = c("l", "l", "r", "r", "r"),
      list(
        GPA = color_tile("white", "orange")
      )
    )
  })
  
  # 타율 상위 10명 테이블
  output$avg_top10 <- renderFormattable({
    kbo_hitter100 <- kbo_hitter %>% filter(타석 > 100)
    avg_top10 <- kbo_hitter100 %>%
      select(선수명, 팀명, 타수, 안타, 타율) %>%
      filter(타율 >= 0.362) %>%
      arrange(desc(타율))
    formattable(
      avg_top10,
      align = c("l", "l", "r", "r", "r"),
      list(
        타율 = color_bar("lightblue"),
        `선수명` = formatter("span", style = ~ style(color = "blue", font.weight = "bold"))
      )
    )
  })
  
  # 구단별 100타석 이상인 선수 수 테이블
  output$team_count <- renderFormattable({
    kbo_hitter100 <- kbo_hitter %>% filter(타석 > 100)
    team_count <- kbo_hitter100 %>%
      group_by(팀명) %>%
      summarise(선수 = n()) %>%
      arrange(desc(x = 선수))
    formattable(
      team_count,
      align = c("l", "r"),
      list(
        `선수` = color_bar("lightgreen")
      )
    )
  })
  
  # 구단별 100타석 이상인 선수들의 홈런 합 테이블
  output$team_hr <- renderFormattable({
    kbo_hitter100 <- kbo_hitter %>% filter(타석 > 100)
    team_hr <- kbo_hitter100 %>%
      group_by(팀명) %>%
      summarise(팀홈런 = sum(홈런)) %>%
      arrange(desc(x = 팀홈런))
    formattable(
      team_hr,
      align = c("l", "r"),
      list(
        `팀홈런` = color_tile("white", "skyblue")
      )
    )
  })
  
  # 구단별 평균 홈런 비율 테이블
  output$hr_ratio <- renderFormattable({
    kbo_hitter100 <- kbo_hitter %>% filter(타석 > 100)
    hr_ratio <- kbo_hitter100 %>%
      group_by(팀명) %>%
      summarise(평균홈런 = sum(홈런) / n()) %>%
      arrange(desc(x = 평균홈런))
    formattable(
      hr_ratio,
      align = c("l", "r"),
      list(
        `평균홈런` = color_tile("white", "pink")
      )
    )
  })
  
  # 구단별 최다홈런 테이블
  output$hr_max <- renderFormattable({
    kbo_hitter100 <- kbo_hitter %>% filter(타석 > 100)
    hr_max <- kbo_hitter100 %>%
      group_by(팀명) %>%
      summarise(최다홈런 = max(홈런)) %>%
      arrange(desc(x = 최다홈런))
    formattable(
      hr_max,
      align = c("l", "r"),
      list(
        `최다홈런` = color_bar("lightblue")
      )
    )
  })
  
  # 팀별 평균 안타수 박스 플롯
  output$batting_boxplot <- renderPlot({
    kbo_hitter100 <- kbo_hitter %>% filter(타석 > 100)
    boxplot(
      안타 ~ 팀명,
      data = kbo_hitter100,
      main = 'Distribution of Hits by Team',
      ylab = 'Hits'
    )
    abline(
      h = median(x = kbo_hitter100$안타),
      col = 'red',
      lwd = 2
    )
  })
  output$allf_top_10_table <- renderFormattable({
    allf_top_10 <- head(subset(arrange(kbo_pitcher, kbo_pitcher$평균자책점), 선발 > 5), 10) %>%
      select("선수명", "팀명", "년도", "평균자책점", "경기", "승", "패", "선발승", "승률")
    formattable(allf_top_10,align = c("l", "r"),list(`평균자책점` = color_bar("lightgreen")))
  })
  
  output$alls_top_10_table <- renderFormattable({
    alls_top_10 <- head(subset(arrange(kbo_pitcher, kbo_pitcher$평균자책점), 선발 < 5), 10) %>%
      select("선수명", "팀명", "년도", "평균자책점", "경기", "승", "패", "홀드", "세이브")
    formattable(alls_top_10,align = c("l", "r"),list(`평균자책점` = color_bar("green")))
  })
  
  output$pastf_top_10_table <- renderFormattable({
    allf_top_10 <- head(subset(arrange(kbo_pitcher, kbo_pitcher$평균자책점), 선발 > 5), 10) %>%
      select("선수명", "팀명", "년도", "평균자책점", "경기", "승", "패", "선발승", "승률")
    pastf_top_10 <- subset(allf_top_10, 년도 < 2023) %>%
      select("선수명", "팀명", "년도", "평균자책점", "경기", "승", "패", "선발승", "승률")
    formattable(pastf_top_10,align = c("l", "r"),list(`평균자책점` = color_bar("yellow")))
  })
  
  output$pasts_top_10_table <- renderFormattable({
    alls_top_10 <- head(subset(arrange(kbo_pitcher, kbo_pitcher$평균자책점), 선발 < 5), 10) %>%
      select("선수명", "팀명", "년도", "평균자책점", "경기", "승", "패", "홀드", "세이브")
    pasts_top_10 <- head(subset(alls_top_10, 년도 < 2023), 10) %>%
      select("선수명", "팀명", "년도", "평균자책점", "경기", "승", "패", "홀드", "세이브")
    formattable(pasts_top_10,align = c("l", "r"),list(`평균자책점` = color_bar("orange")))
  })
  
  output$nowf_top_10_table <- renderFormattable({
    nowf_top_10 <- head(subset(arrange(pitcher_2023, pitcher_2023$평균자책점), 선발 > 5), 10) %>%
      select("선수명", "팀명", "년도", "평균자책점", "경기", "승", "패", "선발승", "승률")
    formattable(nowf_top_10,align = c("l", "r"),list(`평균자책점` = color_bar("skyblue")))
  })
  
  output$now_middle_top_10_table <- renderFormattable({
    now_middle_top_10 <- head(subset(arrange(pitcher_2023, desc(pitcher_2023$홀드), pitcher_2023$평균자책점), 이닝 < 30), 10) %>%
      select("선수명", "팀명", "년도", "평균자책점", "경기", "승", "패", "홀드", "세이브")
    formattable(now_middle_top_10,align = c("l", "r"),list(`평균자책점` = color_bar("purple")))
  })
  
  output$now_last_top_10_table <- renderFormattable({
    now_last_top_10 <- head(subset(arrange(pitcher_2023, desc(pitcher_2023$세이브), pitcher_2023$평균자책점), 이닝 < 30), 10) %>%
      select("선수명", "팀명", "년도", "평균자책점", "경기", "승", "패", "홀드", "세이브")
    formattable(now_last_top_10,align = c("l", "r"),list(`평균자책점` = color_bar("pink")))
  })
}

shinyApp(ui,server)

