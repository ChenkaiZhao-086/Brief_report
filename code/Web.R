library(shiny)
library(shinydashboard) 
library(shinyWidgets) 

### UI --------------------------------------------------------------------------
UI <- fluidPage(
  tags$head(
    tags$style(HTML("
      .JSPlot1 {
        text-align: center;
        margin-left: 60px;
        display: block;
      }
      .PiePlot {
        text-align: center;
        display: block;
      }
      body {
        background-color: #ffffff;
      }
    "))
  ),
  
  # Header
  fluidRow(box(div(img(src = "NJMU.png", height = "50px"),
                   img(src = "Main.png", height = "50px"),
                 style = "text-align:left; font-size:12px; color: black; background-color: #f5f5f7;" 
               )),
           style = "background-color: #f5f5f7; padding-top: 20px; padding-bottom: 10px;"),

  # First section: 新冠病毒序列概况
  div(
    h1("新冠病毒监测报告", align = "center", style = "color: black;"),
    h2("1、新冠病毒序列概况", align = "left", style = "color: black; font-size:24px"),
    style = "margin: 0 auto; max-width: 62%;"
  ),
  div(
    textOutput("Chapter1"), 
    align = "left", style = "margin: 0 auto; color: black; font-size:20px; max-width: 62%;"
  ),
  br(),
  div(class = "JSPlot1", align = "center", plotOutput("JSPlot1", width = "100%"), 
      style = "margin: 0 auto; max-width: 70%;"),
  br(),
  div(
    h3("主要新冠病毒亚型流行时间及构成占比", align = "center", style = "color: black; font-size:20px"),
    div(
      tableOutput("Table"), align = "center", 
      style = "margin: 0 auto; color: black; font-size:18px; max-width: 62%;")
    ),
  br(),
  
  # Second section: 入境感染者新冠病毒序列概况
  div(
    h2("2、入境感染者新冠病毒序列概况", align = "left", style = "color: black; font-size:24px"),
    style = "margin: 0 auto; max-width: 62%;"
  ),
  div(
    textOutput("Chapter2"), 
    align = "left", style = "margin: 0 auto; color: black; font-size:20px; max-width: 62%;"
  ),
  br(),
  div(class = "PiePlot", align = "center", plotOutput("PiePlot", width = "100%"), 
      style = "margin: 0 auto; max-width: 70%;"),
  br(),
  div(class = "PiePlot", align = "center", plotOutput("LocalImportComp", width = "100%", height = "800px"), 
      style = "margin: 0 auto; max-width: 70%;"),
  
  # Third section: 重症及死亡病例测序情况
  div(
    h2("3、重症及死亡病例测序情况", align = "left", style = "color: black; font-size:24px"),
    style = "margin: 0 auto; max-width: 62%;"
  ),
  div(
    textOutput("Chapter3"), 
    align = "left", style = "margin: 0 auto; color: black; font-size:20px; max-width: 62%;"
  ),
  br(style = "margin-top: 10px; margin-bottom: 10px; "),
  
  
  # Footnote
  tags$hr(style = "margin-top: 0px; margin-bottom: 0px; background-color: #f5f5f7; color: #444445;"), # 移除上下外边距
  fluidRow(box(width = 12,
               div(
                 p("免责声明：本工具所包含和生成的所有信息仅用于健康教育目的。这些信息不应被用于任何健康问题或疾病的诊断或治疗，
                   也不能以任何方式取代临床判断或指导个体患者的治疗。"),
                 style = "text-align:left; font-size:12px; color: black; background-color: #f5f5f7;" 
               )),
           style = "background-color: #f5f5f7; padding-top: 20px; padding-bottom: 10px;"),
  
  tags$hr(style = "margin-top: 0px; margin-bottom: 0px; background-color: #f5f5f7; color: #444445;"), 
  fluidRow(box(width = 12,
               div(
                 p("版权所有：南京医科大学，传染病流行病学与建模团队"),
                 style = "text-align:center; font-family: times; font-size:10px; color: black; background-color: #f5f5f7;"
               )),
           style = "background-color: #f5f5f7; padding-top: 10px; padding-bottom: 20px;")
)

### Server --------------------------------------------------------------------------
Server <- function(input, output) {
  output$Chapter1 <- renderText({
    paste0(Text.Date(JSDat), "全省共获得本士感染者新冠病毒基因组有效序列", length(LocalCases$Lab3Count), 
           "株，均为奥密克戎变异株，其中重组变异株XBB型后代谱系", LocalV1[Lab1 == "XBB", 2], 
           "株（占", LocalV1[Lab1 == "XBB", 3], "%，包括XBB.1.5及其后代谱系, ", LocalV2[Lab3 == "XBB.1.5", 6], 
           "株，XBB.1.9及其后代谱系", LocalV2[Lab3 == "XBB.1.9", 6], "株，XBB.1.16及其后代谱系", LocalV2[Lab3 == "XBB.1.16", 6], 
           "株，XBB.1.22及其后代谱系", LocalV2[Lab3 == "XBB.1.22", 6], "株，其它XBB型及其后代谱系", 
           LocalV2[Lab3 == "XBB", 6]+LocalV2[Lab3 == "XBB.1", 6], 
           "株），FY.3型及其后代谱系", unique(LocalV2[Lab2 == "FY.3", 5]), "株, EG.5型及其后代谱系", 
           unique(LocalV2[Lab2 == "EG.5", 5]), "株。")
  })
  
  output$Chapter2 <- renderText({
    paste0(Text.Date(JSDat), "全省共发现入境新冠病毒感染者", length(ImportCases$Lab3Count), 
           "例，结果发现重组变异株XBB型后代谱系", ImportV1[Lab1 == "XBB", 2], 
           "株（占", ImportV1[Lab1 == "XBB", 3], "%，包括XBB.1.5型及其后代谱系", ImportV2[Lab3 == "XBB.1.5", 6], 
           "株，XBB.1.9型及其后代谱系", ImportV2[Lab3 == "XBB.1.9", 6], "株，XBB.1.16型及其后代谱系", 
           ImportV2[Lab3 == "XBB.1.16", 6], "株，XBB.1.22及其后代谱系", ImportV2[Lab3 == "XBB.1.22", 6], 
           "株，其它XBB型后代谱系", ImportV2[Lab3 == "XBB", 6]+ImportV2[Lab3 == "XBB.1", 6],
           "株）；FY型及其后代谱系", ImportV1[Lab1 == "FY",2], "株", "；EG型及其后代谱系", ImportV1[Lab1 == "EG", 2], "株。")
  })
  
  output$Chapter3 <- renderText({
    paste0(Text.Date(JSDat), "全省累计重症病例", HeavyCount, "例，其中XBB.1.22型及其后代谱系1例，XBB.1.5型及其后代谱系1例，XBB.1.9型及其后代谱系1例，XBB.1.16型及其后代谱系1例，BN.1型及其后代谱系1例。无死亡病例。")
  })
  
  output$Table <- renderTable({
    Table$流行峰值时间 <- format(Table$流行峰值时间, "%Y-%m-%d")
    Table$亚型报告病例数 <- format(Table$亚型报告病例数, big.mark = ",")
    Table$累积病例数 <- format(Table$累积病例数, big.mark = ",")
    Table
    })
  
  output$JSPlot1 <- renderPlot({JSPlot[[1]]})
  
  output$PiePlot <- renderPlot(PiePlot)
  
  output$LocalImportComp <- renderPlot(LocalImportComp)
}

### APP --------------------------------------------------------------------------
shinyApp(ui = UI, server = Server)

