library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(deSolve)
library(tidyverse)

source("code/SEIR_Sim.R")

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .NewConfirm_Fig {
        text-align: center;
        margin-left: 60px;
        display: block;
      }
      .Cum_Fig {
        text-align: center;
        display: block;
      }
      body {
        background-color: #ffffff;
      }
    "))
  ),

  # Header
  fluidRow(
    box(div(img(src = "NJMU.png", height = "50px", style = "margin-left: 8px; margin-top: 3px;"),
      img(src = "Main.png", height = "70px", style = "position: relative; top: -2px; margin-left: 17px;"),
      style = "text-align:left; font-size:12px; color: black; background-color: #f5f5f7;"
    )),
    style = "background-color: #f5f5f7; padding-top: 20px; padding-bottom: 17px;"
  ),
  div(
    h1("新发传染病预测系统", align = "center", style = "color: black;"),
    style = "margin: 0 auto; max-width: 62%;"
  ),
  fluidRow(
    box(
      div(
        p("参数输入"),
        style = "text-align:left; font-size:16px; color: black;"
      )
    )
  ),
  sidebarLayout(
    sidebarPanel(
      sliderInput("N", "总人口", min = 1e6, max = 1e7, value = 1e6, step = 1e6),
      sliderInput("I", "初始感染人数", min = 1, max = 100, value = 10),
      sliderInput("R", "初始具有免疫力人数", min = 0, max = 1000000, value = 0),
      sliderInput("incubation_period", "潜伏期(天)", min = 1, max = 30, value = 5),
      sliderInput("infectious_period", "感染期(天)", min = 1, max = 30, value = 7),
      sliderInput("simulation_time", "模拟时间(天)", min = 30, max = 365, value = 100)
    ),
    mainPanel(
      div(
        h3("每日新增病例预测", align = "center", style = "color: black; font-size:20px"),
        div(
          class = "NewConfirm_Fig", align = "center", plotOutput("NewConfirm_Fig", width = "100%", height = "300px"),
          style = "margin: 0 auto; max-width: 95%;"
        )
      ),
      br(),
      div(
        h3("累计确诊病例预测", align = "center", style = "color: black; font-size:20px"),
        div(
          class = "Cum_FIg", align = "center", plotOutput("Cum_FIg", width = "100%", height = "300px"),
          style = "margin: 0 auto; max-width: 95%;"
        )
      ),
      br()
    )
  ),


  # Footnote
  tags$hr(style = "margin-top: 0px; margin-bottom: 0px; background-color: #f5f5f7; color: #444445;"), # 移除上下外边距
  fluidRow(
    box(
      width = 12,
      div(
        p("免责声明：本工具所包含和生成的所有信息仅用于健康教育目的。这些信息不应被用于任何健康问题或疾病的诊断或治疗，
                   也不能以任何方式取代临床判断或指导个体患者的治疗。"),
        style = "text-align:left; font-size:12px; color: black; background-color: #f5f5f7;"
      )
    ),
    style = "background-color: #f5f5f7; padding-top: 20px; padding-bottom: 10px"
  ),
  tags$hr(style = "margin-top: 0px; margin-bottom: 0px; background-color: #f5f5f7; color: #444445;"),
  fluidRow(
    box(
      width = 12,
      div(
        p("版权所有：南京医科大学，传染病流行病学与建模团队"),
        style = "text-align:center; font-family: times; font-size:10px; color: black; background-color: #f5f5f7;"
      )
    ),
    style = "background-color: #f5f5f7; padding-top: 10px; padding-bottom: 20px;"
  )
)

# Server
server <- function(input, output) {
  output$NewConfirm_Fig <- renderPlot(RunModel(input)[[1]])
  output$Cum_FIg <- renderPlot(RunModel(input)[[2]])
}

shinyApp(ui = ui, server = server)
