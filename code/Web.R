library(shiny)
library(shinydashboard) 
library(shinyWidgets) 

UI <- fluidPage(
  tags$div(
    tags$img(src = "www/Main.png", height = "50px"),
    tags$img(src = "Main.png", height = "50px"),
    style = "position: absolute; top: 10px; left: 10px;"
  ),
  
  tags$h1("新冠病毒监测报告", align = "center", style = "color: black;"),
  
  # First section: 新冠病毒序列概况
  tags$h2("1、新冠病毒序列概况", align = "left", style = "color: black; font-size:24px"),
  tags$div(
    textOutput("Chapter1"), 
    align = "left", style = "color: black; font-size:20px"
  ),
  # tableOutput("virusTable"),
  # plotOutput("virusPlot"),
  br(),
  
  # Second section: 入境感染者新冠病毒序列概况
  tags$h2("2、入境感染者新冠病毒序列概况", align = "left", style = "color: black; font-size:24px"),
  tags$div(
    textOutput("Chapter2"), 
    align = "left", style = "color: black; font-size:20px"
  ),
  br(),
  
  # Third section: 重症及死亡病例测序情况
  tags$h2("3、重症及死亡病例测序情况", align = "left", style = "color: black; font-size:24px"),
  tags$div(
    textOutput("Chapter3"), 
    align = "left", style = "color: black; font-size:20px"
  ),
  
  
  # Footnote
  hr(),
  fluidRow(box(width = 12,
               div(
                 p("免责声明：本工具所包含和生成的所有信息仅用于健康教育目的。这些信息不应被用于任何健康问题或疾病的诊断或治疗，
                   也不能以任何方式取代临床判断或指导个体患者的治疗。"),
                 style = "text-align:left; font-size:12px;" 
               ))),
  
  hr(),
  fluidRow(box(width = 12,
               "版权所有：南京医科大学，传染病流行病学与建模团队",
               style = "text-align:center; font-family: times;font-size:10px;"))
)

Server <- function(input, output) {
  output$Chapter1 <- renderText({
    paste0(Text.Date(JSDat), "全省共获得本士感染者新冠病毒基因组有效序列", length(LocalCases$Lab3Count), 
           "株，均为奥密克戎变异株，其中重组变异株XBB型后代谱系", LocalV1[Lab1 == "XBB", 2], 
           "株（占", LocalV1[Lab1 == "XBB", 3], "%，包括XBB.1.5及其后代谱系, ", LocalV2[Lab3 == "XBB.1.5", 6], 
           ", 株，XBB.1.9及其后代谱系", LocalV2[Lab3 == "XBB.1.9", 6], "株，XBB.1.16及其后代谱系", LocalV2[Lab3 == "XBB.1.16", 6], 
           "株，XBB.1.22及其后代谱系", LocalV2[Lab3 == "XBB.1.22", 6], "株，其它XBB型及其后代谱系", 
           LocalV2[Lab3 == "XBB", 6]+LocalV2[Lab3 == "XBB.1", 6], 
           "株），FY.3型及其后代谱系", unique(LocalV2[Lab2 == "FY.3", 5]), "株, EG.5型及其后代谱系", 
           unique(LocalV2[Lab2 == "EG.5", 5]), "株。")
  })
  
  output$Chapter2 <- renderText({
    paste0(Text.Date(JSDat), "全省共获得本士感染者新冠病毒基因组有效序列", 2959, "株，均为奥密克戎变异株，其中重组变异株XBB型后代谱系", 1675, "株（占56.6%，包括XBB.1.5及其后代谱系442株，XBB.1.9及其后代谱系446株，XBB.1.16及其后代谱系210株，XBB.1.22及其后代谱系490株，其它XBB型及其后代谱系87株），BA.5.2 型及其后代谱系656株（占22.2%）；BF.7型及其后代谱系563株（占19.0%）；其他重点关注变异株65株，（占2.2%）。")
  })
  
  output$Chapter3 <- renderText({
    paste0(Text.Date(JSDat), "全省共获得本士感染者新冠病毒基因组有效序列", 2959, "株，均为奥密克戎变异株，其中重组变异株XBB型后代谱系", 1675, "株（占56.6%，包括XBB.1.5及其后代谱系442株，XBB.1.9及其后代谱系446株，XBB.1.16及其后代谱系210株，XBB.1.22及其后代谱系490株，其它XBB型及其后代谱系87株），BA.5.2 型及其后代谱系656株（占22.2%）；BF.7型及其后代谱系563株（占19.0%）；其他重点关注变异株65株，（占2.2%）。")
  })
  
  output$virusTable <- renderTable({ generateTable() })
  output$virusPlot <- renderPlot({ generatePlot() })
}


shinyApp(ui = UI, server = Server)



calculateVirusPrevalence <- function() {
  # 模拟一些数据
  return(0.05) # 5% 流行率
}

# 函数生成表格的示例
generateTable <- function() {
  data.frame(
    Category = c("Category 1", "Category 2"),
    Value = c(123, 456)
  )
}

# 函数生成图片的示例
generatePlot <- function() {
  plot(1:10, 1:10) # 一个简单的线图
}

ui <- fluidPage(
  tags$div(
    tags$img(src = "path_to_first_image.jpg", height = "50px"),
    tags$img(src = "path_to_second_image.jpg", height = "50px"),
    style = "position: absolute; top: 10px; left: 10px;"
  ),
  tags$h1("新冠病毒监测报告", align = "center", style = "color: black;"),
  tags$div(
    textOutput("virusPrevalence"), 
    align = "center", style = "color: black;"
  ),
  tableOutput("virusTable"),
  plotOutput("virusPlot")
)

server <- function(input, output) {
  output$virusPrevalence <- renderText({
    prevalence <- calculateVirusPrevalence() * 100
    paste("当前新冠病毒流行率为", prevalence, "%。请继续关注更新。")
  })
  
  output$virusTable <- renderTable({ generateTable() })
  output$virusPlot <- renderPlot({ generatePlot() })
}

shinyApp(ui = ui, server = server)













rm(list=ls())

# library(shinyjs)

todays_date <- as.Date(Sys.Date())

hos <- read.csv("summaryData_hosORs.csv")
poc <- read.csv("summaryData_hosPoorORs.csv")
HosProb <- read.csv("summaryData_hosProp.csv")
RiskPercent <- read.csv("summaryData_forHospRiskClass.csv") 

#split the files into risk factors and results
hos_rows<- hos[c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]
hos_results<-hos[c(17,18,19)]

prob_rows<- HosProb[c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]
prob_results<-HosProb[c(20,21,22)]
names(prob_results)[1] <- "est"
names(prob_results)[2] <- "lci"
names(prob_results)[3] <- "uci"

RiskPercent_rows <- RiskPercent[c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]
RiskPercent_results<-RiskPercent[c("Percent")]

poc_rows<- poc[c(2,3,4)]
poc_results<-poc[c(5,6,7)]

# UI----
#used navbarPage function instead of 2 different shiny apps for ease in server hosting 
ui<-navbarPage(
  "", 
  tags$img(src = "NJMU-1.png", height = "40px", width = "140px",
           style = "position:absolute;top:8px;left:15px;z-index:9999;"), #add logo and adjust size and position
  
  tags$head(
    tags$style(type="text/css", 
               "label{  display: table-cell; text-align: left; } .form-group { display: table; } 
                 #resultsheader{ font-size: 16px;} #preventionheader{font-size: 16px;} #resultsheader2{font-size: 16px;} #preventionheader2{font-size: 16px;}
                                 .navbar .navbar-nav {float: right}
                                 .navbar .navbar-header {float: right}
                                 .navbar-default { background-color: #FFFFFF}
                            ")
  ),
  
  tabPanel("English (英语)", 
           
           tags$style(HTML(
             '.background-container {
      background-image: url("background.png");
      background-size:  350px;
      background-size:  cover;
      background-repeat: no-repeat;
      background-position: center top;
      /*background-attachment: fixed; */
    }
    
    .page-content {
/*background-color: rgba(255, 255, 255, 0.8);  半透明背景颜色 */
        padding: 20px;
        margin: 20px; /* 用于分隔文字与背景图 */
        border-radius: 10px;
    }

.my-button {
        background-color: #FF0000; /* 设置背景颜色为红色 */
        color: #FFFFFF; /* 设置字体颜色为白色 */
        font-weight: bold; /* 设置字体加粗 */
        font-size: 16px; /* 设置字体大小为16像素 */
        padding: 10px 20px; /* 设置内边距 */
      }
    
      ')),

tags$body(
  div(class = "background-container",
      div(class = "page-content",
          br(), br(),br(),
          p(strong("RESPIRATORY SYNCYTIAL VIRUS-RELATED HOSPITALISATION RISK PREDICTION TOOL"),br(), br(),style="text-align:left;color:white;font-size: 35px;", 
          ),
      )
  )),
hr(), # 添加水平线

fluidRow( 
  box(width = 12, 
      tags$p(br(),
             tags$b("Instructions for use:"), style="text-align:left;color:#4169E1;font-size: 20px;"),
      p("Respiratory syncytial virus (RSV) is the most common pathogen identified in children under five years with acute lower respiratory infections (ALRI), accounting for about 30% of all respiratory pathogens and affecting children's health.",br(),
        "The risk for hospitalisation with RSV-ALRI and poor outcomes after hospitalisation differs by individual.",br(), 
        "This tool, developed by a research team from Nanjing Medical University, aims to predict the risks associated with RSV-ALRI in children under five years in China.",
        style="text-align:left;color:blcak;font-size: 16px;",
        br()
      ),
      style = "background-color: light-blue;"
  )),


fluidRow(
  box(width = 12,
      tags$p(
        br(),
        tags$b("Please enter child’s information below:"),  # Based on the provided information, the tool can predict: 
        style="text-align:left;color:#4169E1;font-size: 20px;"
      ))),
# RF----
fluidRow(
  box(width =12,
      dateInput("dob", "1. Date of Birth: ", width = "320px", value = "2022-09-01", startview = "year"), #need to figure out how to make this look better
      numericInput("weight", "2. Birthweight (kg): ", value=2.5, min=0, max=20,step=0.1),
      prettyRadioButtons("prematurity", "3. Gestational age: ",# "margin-right: 20px;",  style = "primary",
                         # "3. Prematurity (i.e., gestational age < 37 weeks):", 
                         choices = list("<28 weeks"=1, "28 to <32 weeks "=2, "32 to <37 weeks"=3,"≥ 37weeks"=0), selected = 0, shape="round", inline= T),
      # prettyRadioButtons("BD", "5. Bronchopulmonary dysplasia:", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
      prettyRadioButtons("down_syndrome", "4. Down syndrome: ", choices = list("Yes"=1, "No"=0),selected = 0, shape="round", inline= T),
      prettyRadioButtons("chd", "5. Congenital heart disease: ", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
      prettyRadioButtons("Cystic_fibrosis", "6. Cystic fibrosis: ", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
      prettyRadioButtons("Previous_asthma", "7. Previous asthma: ", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
      prettyRadioButtons("hiv", "8. HIV infection:", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
      
      prettyRadioButtons("passive_smoking", "9. Passive smoking: ", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
      prettyRadioButtons("Not_exclusive_breastfeeding", "10. Not exclusive breastfeeding: ", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
      prettyRadioButtons("Attendance_at_daycare_center", "11. Attendance at daycare center: ", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
      prettyRadioButtons("siblings", "12. Number of siblings (defined as those living together with the child): ", 
                         choices= list("0"= 0, "≥1"=1), selected = 0, shape="round", inline=T),
      numericInput("pregnancy_age", "13. Maternal childbearing age: ", value=25, min=0, max=60,step=1),
      prettyRadioButtons("pregnancy_smoking", "14. Maternal smoking during pregnancy: ", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T)
      
  )),

fluidRow(box(actionButton("action", "Submit",class = "my-button"))),
br(),

fluidRow(
  box(width = 12,
      tags$p(br(),
             tags$b("Based on the provided information, the tool can predict:"), 
             style="text-align:left;color:#4169E1;font-size: 20px;",
             br()))),

fluidRow( 
  column(
    br(),
    div(
      strong("Risk of hospitalisation "), # "you will find alternatives to learn all these technical aspects independently.",
      style="text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px;font-size: 16px;",
      htmlOutput("res_combiOR.hos"),
      "times that of his/ her peers*"),
    width=3,
    br()),
  
  column(    
    br(),
    div(strong("Probability of hospitalisation"),
        style="text-align:center;color:black;background-color:papayawhip;padding:15px;border-radius:10px;font-size: 16px;",
        htmlOutput("Res_hosProb"),
        "within the next year"),
    width=3,
    br()),
  
  column(    
    br(),
    div(strong("Children with an RSV hospitalisation risk higher than X%"), #The proportion of children with higher hospitalisation risks than him/her
        style="text-align:center;color:black;background-color:rgba(117, 166, 215, 0.73);padding:15px;border-radius:10px;font-size: 16px;",
        htmlOutput("Res_hosRiskPercent")
    ),
    width=3,
    br()),
  
  column(    
    br(),
    div(strong("Risk of poor outcomes† after hospitalisation"),
        style="text-align:center;color:black;background-color:#BFF7BB;padding:15px;border-radius:10px;font-size: 16px;",
        htmlOutput("Res_CombiOR.poc"),
        "times that of his/ her peers"),
    width=3,
    br())
),

br(),

fluidRow( 
  box(width = 12, 
      tags$p(# br(),
        "* those children who do not have any risk factors"))),

fluidRow( 
  box(width = 12, 
      tags$p(# br(),
        "† need for prolonged hospital stay, oxygen supplementation, mechanical ventilation, or ICU admission"))),

fluidRow(
  box(width =12,
      div(htmlOutput("preventionheader")))),
br(),
fluidRow(
  box(width =12,   
      div(textOutput("prevention")))),


hr(),
fluidRow(box(width = 12,
             div(
               p("Disclaimer: All information contained and generated by this tool is only used for health education. This information should not be used in the diagnosis or treatment for any health issues or diseases, nor should it in any way replace clinical judgment or guidance for the treatment of individual patients."),
               style = "text-align:left; font-size:12px;"
             ))),


hr(),
fluidRow(box(width = 12,
             "© The Infectious Diseases Epidemiology & Modelling group, Nanjing Medical University, Nanjing, China",
             style = "text-align:center; font-family: times;font-size:10px;"))
  ), 

tabPanel("Chinese（中文）",
         
         tags$style(HTML(
           '.background-container {
      background-image: url("background.png");
      background-size:  350px;
      background-size:  cover;
      background-repeat: no-repeat;
      background-position: center top;
      /*background-attachment: fixed; */
    }
    
    .page-content {
/*background-color: rgba(255, 255, 255, 0.8);  半透明背景颜色 */
        padding: 20px;
        margin: 20px; /* 用于分隔文字与背景图 */
        border-radius: 10px;
    }

.my-button {
        background-color: #FF0000; /* 设置背景颜色为红色 */
        color: #FFFFFF; /* 设置字体颜色为白色 */
        font-weight: bold; /* 设置字体加粗 */
        font-size: 16px; /* 设置字体大小为16像素 */
        padding: 10px 20px; /* 设置内边距 */
      }

      ')),

tags$body(
  div(class = "background-container",
      div(class = "page-content",
          br(), br(),br(),
          # div(class = "text-container",
          p(strong("呼吸道合胞病毒相关住院风险预测工具"),br(), br(),style="text-align:left;color:white;font-size: 35px;", 
          ),
      )
  )),
hr(), # 添加水平线

fluidRow( 
  box(width = 12, 
      tags$p(br(),
             tags$b("使用指导："), style="text-align:left;color:#4169E1;font-size: 20px;"),#  br(), # 用于显示加粗的文本
      p("呼吸道合胞病毒（RSV）是导致5岁以下儿童下呼吸道感染的最常见病原体，约占所有呼吸道病原体的30%，严重危害儿童健康。",br(),
        "不同儿童RSV下呼吸道感染住院风险及住院后发生不良结局的风险存在一定差异。",br(), 
        "本工具由南京医科大学科研团队开发，可用于预测中国5岁以下儿童发生RSV下呼吸道感染住院的相关风险。",
        style="text-align:left;color:blcak;font-size: 16px;",
        br()  # ,br(),br(),br(),br()),
      ),
      style = "background-color: light-blue;"
      # 有效颜色：Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  )),
# fluidRow(box(width =12, title="呼吸道合胞病毒感染风险预测工具", solidHeader = TRUE, br(),
#              tags$p("呼吸道合胞病毒（RSV）是导致5岁以下儿童下呼吸道感染的最常见病原体，约占所有呼吸道病原体的30%，严重危害儿童健康。不同儿童RSV下呼吸道感染住院风险及住院后发生不良结局的风险存在一定差异。本工具由南京医科大学科研团队开发，可用于预测中国5岁以下儿童发生RSV下呼吸道感染的相关风险。",br(),
#                     "请在下方输入儿童信息，本工具可以根据这些信息预测: ",br(),"1. 儿童RSV下呼吸道感染住院的风险", br(), "2. 儿童未来一年内RSV下呼吸道感染住院的可能性；  ", br(), "3. 儿童RSV下呼吸道感染住院后，发生不良结局（需要延长住院时间、吸氧、使用呼吸机或转入ICU）的风险。",style = "font-size:16px;",br(),br() ))),
# 
fluidRow(
  box(width = 12, 
      tags$p( 
        br(),
        tags$b("请在下方输入儿童信息："),  
        style="text-align:left;color:#4169E1;font-size: 20px;"
      ))),

# RF----
fluidRow(
  box(width =12,
      dateInput("dob2", "1. 出生日期: ", width = "320px", value = "2022-09-01", startview = "year",language = "zh-CN"), #need to figure out how to make this look better
      numericInput("weight2", "2.出生体重 (公斤): ", value=2.5, min=0, max=20,step=0.1),
      prettyRadioButtons("prematurity2", "3. 胎龄: ", 
                         # "3. Prematurity (i.e., gestational age < 37 weeks):", 
                         choices = list("<28 周"=1, "28 到 <32 周"=2, "32 到 <37 周"=3,"≥ 37 周"=0), selected = 0, shape="round", inline= T),
      # prettyRadioButtons("BD", "5. Bronchopulmonary dysplasia:", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
      prettyRadioButtons("down_syndrome2", "4. 是否患有唐氏综合征: ", choices = list("Yes"=1, "No"=0),selected = 0, shape="round", inline= T),
      prettyRadioButtons("chd2", "5. 是否患有先天性心脏病: ", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
      prettyRadioButtons("Cystic_fibrosis2", "6. 是否患有囊性纤维化: ", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
      prettyRadioButtons("Previous_asthma2", "7. 既往是否患有哮喘: ", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
      prettyRadioButtons("hiv2", "8. 是否感染艾滋病病毒:", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
      
      prettyRadioButtons("passive_smoking2", "9. 被动吸烟（家中是否有家属吸烟）: ", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
      prettyRadioButtons("Not_exclusive_breastfeeding2", "10. 出生后头六个月为纯母乳喂养: ", choices = list("Yes"=0, "No"=1), selected = 0, shape="round", inline= T),
      prettyRadioButtons("Attendance_at_daycare_center2", "11. 是否上托幼机构/幼儿园: ", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
      prettyRadioButtons("siblings2", "12. 兄弟姐妹数量（共同居住，不含儿童本人）: ", 
                         choices= list("0"= 0, "≥1"=1), selected = 0, shape="round", inline=T),
      numericInput("pregnancy_age2", "13. 母亲生育时的年龄: ", value=25, min=0, max=60,step=1),
      prettyRadioButtons("pregnancy_smoking2", "14. 母亲孕期是否吸烟: ", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T)
      
      
  )),


fluidRow(box(actionButton("action2", "提交",class = "my-button"))),
br(),

fluidRow(
  box(width = 12,
      tags$p(br(),
             tags$b("本工具可以根据这些信息预测:"),  
             style="text-align:left;color:#4169E1;font-size: 20px;",
             br()))),

fluidRow( # 按照列布局
  column(
    br(),
    div(
      strong("RSV下呼吸道感染住院的风险"), 
      style="text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px;font-size: 16px;",
      htmlOutput("res_combiOR.hos2"),
      "倍相比于一般儿童*"),
    width=3,
    br()),
  
  column(    
    br(),
    div(strong("RSV下呼吸道感染住院的可能性"),
        style="text-align:center;color:black;background-color:papayawhip;padding:15px;border-radius:10px;font-size: 16px;",
        htmlOutput("Res_hosProb2"),
        "在未来一年内"),
    width=3,
    br()),
  
  column(    
    br(),
    div(strong("RSV住院风险高于X%的儿童"), # 住院相对风险高于他/她的儿童在五岁下儿童的占比
        # The proportion of children with higher hospitalisation risks than him/her"
        style="text-align:center;color:black;background-color:rgba(117, 166, 215, 0.73);padding:15px;border-radius:10px;font-size: 16px;",
        htmlOutput("Res_hosRiskPercent2")
    ),
    width=3,
    br()),
  
  column(    
    br(),
    div(strong("RSV下呼吸道感染住院后，发生不良结局†的风险"),
        style="text-align:center;color:black;background-color:#BFF7BB;padding:15px;border-radius:10px;font-size: 16px;",
        htmlOutput("Res_CombiOR.poc2"),
        "倍相比于一般儿童"),
    width=3,
    br())
),

fluidRow( 
  box(width = 12, 
      tags$p(# br(),
        "* 指任何危险因素都不存在的那些儿童"))),

fluidRow( 
  box(width = 12, 
      tags$p(# br(),
        "† 需要延长住院时间，吸氧，使用呼吸机或转入重症监护室（ICU）"))),

br(),
fluidRow(
  box(width =12,
      div(htmlOutput("preventionheader2")))),
br(),
fluidRow(
  box(width =12,   
      div(textOutput("prevention2")))),

hr(),
fluidRow(box(width = 12,
             div(
               p("免责声明：本工具所包含和生成的所有信息仅用于健康教育目的。这些信息不应被用于任何健康问题或疾病的诊断或治疗，也不能以任何方式取代临床判断或指导个体患者的治疗。"),
               style = "text-align:left; font-size:12px;" 
             ))),

hr(),
fluidRow(box(width = 12,
             "版权所有：南京医科大学，传染病流行病学与建模团队",
             style = "text-align:center; font-family: times;font-size:10px;"))

))

#The server code needs to be copied twice - once for the English version and once for the Chinese version
server <- function(input, output) {
  #English version =============================================================================================
  
  output$res_combiOR.hos <- renderUI({ 
    req(input$action)
    
    
    prematurity1<-0
    prematurity2<-0 
    prematurity3<-0
    
    if (isolate(input$prematurity) == 1){
      prematurity1<-1}
    else if (isolate(input$prematurity)==2){
      prematurity2<-1} 
    else if (isolate(input$prematurity)==3){
      prematurity3<-1}
    
    if (isolate(input$weight)<2.5){
      weight<-1
    } else {weight <-0}
    
    if (isolate(input$pregnancy_age)<25){
      pregnancy_age<-1
    } else {pregnancy_age <-0}  
    
    masterlist<- isolate(c(input$chd, input$Cystic_fibrosis, input$Attendance_at_daycare_center, input$down_syndrome,input$hiv,
                           input$Not_exclusive_breastfeeding, weight, pregnancy_age, input$pregnancy_smoking,
                           input$passive_smoking,prematurity3,prematurity1,prematurity2, input$Previous_asthma,input$siblings)) 
    
    row<-which(colSums(t(hos_rows) == masterlist) == ncol(hos_rows)) 
    hosest<- hos_results[row, "est"] 
    
    HTML(paste0("<b><span style='font-weight: bold;font-size: 28px;font-family:Helvetica;color:#EE4000;'>", 
                paste(round(hosest, digits=2)), "</span></b>"))  
    
  })
  
  output$Res_hosProb<- renderUI({
    #copy the code from output$res_combiOR.hos
    req(input$action)
    
    
    prematurity1<-0 # <28w RF12
    prematurity2<-0 # 28-32w RF13
    prematurity3<-0 # 33-36w RF11
    
    if (isolate(input$prematurity) == 1){
      prematurity1<-1}
    else if (isolate(input$prematurity)==2){
      prematurity2<-1} 
    else if (isolate(input$prematurity)==3){
      prematurity3<-1}
    
    if (isolate(input$weight)<2.5){
      weight<-1
    } else {weight <-0}
    
    
    if (isolate(input$pregnancy_age)<25){
      pregnancy_age<-1
    } else {pregnancy_age <-0}  
    
    # masterlist<- isolate(c(siblings2, siblings1, input$BD, weight, input$down_syndrome,input$prematurity, input$chd, input$hiv, input$sex, input$smoking))
    masterlist<- isolate(c(input$chd, input$Cystic_fibrosis, input$Attendance_at_daycare_center, input$down_syndrome,input$hiv,
                           input$Not_exclusive_breastfeeding, weight, pregnancy_age, input$pregnancy_smoking,
                           input$passive_smoking,prematurity3,prematurity1,prematurity2, input$Previous_asthma,input$siblings))
    
    row<-which(colSums(t(prob_rows) == masterlist) == ncol(prob_rows))
    probest<- prob_results[row, "est"]
    probest<- probest*1000
    # isolate(paste("2. The probability of hospitalisation for RSV-ALRI within the next year for the child would be  ", round(probest, digits = 2), "%"))
    HTML(paste0("<b><span style='font-weight: bold;font-size: 28px;font-family:Helvetica;color:#EE4000;'>", 
                paste(round(probest, digits=2),"‰",sep="") , "</span></b>"))  
    
  })
  # ----
  output$Res_hosRiskPercent <- renderUI({
    #copy the code from output$res_combiOR.hos
    req(input$action)
    
    prematurity1<-0 
    prematurity2<-0 
    prematurity3<-0 
    
    if (isolate(input$prematurity) == 1){
      prematurity1<-1}
    else if (isolate(input$prematurity)==2){
      prematurity2<-1} 
    else if (isolate(input$prematurity)==3){
      prematurity3<-1}
    
    
    if (isolate(input$weight)<2.5){
      weight<-1
    } else {weight <-0}
    
    
    if (isolate(input$pregnancy_age)<25){
      pregnancy_age<-1
    } else {pregnancy_age <-0}  
    
    
    masterlist<- isolate(c(input$chd, input$Cystic_fibrosis, input$Attendance_at_daycare_center, input$down_syndrome,input$hiv,
                           input$Not_exclusive_breastfeeding, weight, pregnancy_age, input$pregnancy_smoking,
                           input$passive_smoking,prematurity3,prematurity1,prematurity2, input$Previous_asthma,input$siblings)) 
    
    row<-which(colSums(t(RiskPercent_rows) == masterlist) == ncol(RiskPercent_rows))
    RiskPercent<- RiskPercent_results[row, "Percent"]
    # RiskPercent<- RiskPercent*10000
    # isolate(paste("2. The probability of hospitalisation for RSV-ALRI within the next year for the child would be  ", round(probest, digits = 2), "%"))
    HTML(paste0("<b><span style='font-weight: bold;font-size: 28px;font-family:Helvetica;color:#EE4000;'>", 
                paste(RiskPercent,"%",sep="") , "</span></b>"))   #％        ‱    ‰ro und(RiskPercent, digits=2),"‱",s
    
  })
  
  output$Res_CombiOR.poc<-renderUI({
    req(input$action)
    
    calc_dob<-isolate(as.Date(input$dob))
    
    diff_date<-difftime(todays_date, calc_dob, units = "weeks")
    diff_date <- as.numeric(diff_date)
    
    if (diff_date > 26){
      age<-0
    } else {age<-1}
    
    
    
    if (isolate(input$prematurity) %in% c(1,2,3)){
      prematurity1<-1}
    else {
      prematurity1<-0} 
    
    
    
    masterlist<-isolate(c(prematurity1, input$chd, age))
    row<-isolate(which(colSums(t(poc_rows) == masterlist) == ncol(poc_rows)))
    pocest<- isolate(poc_results[row, "est"])
    
    # isolate(paste("3. The risk of poor outcomes after being hospitalised for RSV-ALRI would be ", round(pocest, digits=2), "times that of his / her peers."))
    HTML(paste0("<b><span style='font-weight: bold;font-size: 28px;font-family:Helvetica;color:#EE4000;'>", 
                paste(round(pocest, digits=2)), "</span></b>"))  
    
  })
  
  output$resultsheader<-renderText({ 
    #just to check that the combination list is correct
    #return(selected_var())
    req(input$action)
    isolate(HTML(paste("<b>[Results]<b>")))
  })
  
  output$preventionheader<-renderText({
    
    req(input$action)
    isolate(HTML(
      paste0("<b><span style='font-weight: bold;font-size: 20px;font-family:Helvetica;color:#4169E1;'>", 
             "How to prevent RSV infection:", "</span></b>")))
  })
  
  
  output$prevention<-renderText({
    req(input$action)
    isolate(paste("To date, there are no vaccines for preventing RSV infection, and prophylactic monoclonal antibodies have not yet been marketed in China. Nonetheless, RSV infection can be effectively prevented by strengthening personal precautions, including avoiding visiting crowded places in winter and spring when RSV is most active and wearing masks at public venues; paying attention to hand hygiene, including washing hands frequently, and avoiding touching eyes, mouth, and nose with unwashed hands; and using personal protective equipment when attending hospitals to prevent hospital-acquired infections."))
  })
  
  #Chinese version ======================================================================================
  #all variables end with 2
  
  output$res_combiOR.hos2 <- renderUI({
    req(input$action2)
    
    prematurity11<-0
    prematurity21<-0 
    prematurity31<-0 
    
    if (isolate(input$prematurity2) == 1){
      prematurity11<-1}
    else if (isolate(input$prematurity2)==2){
      prematurity21<-1} 
    else if (isolate(input$prematurity2)==3){
      prematurity31<-1}
    
    
    if (isolate(input$weight2)<2.5){
      weight2<-1
    } else {weight2 <-0}
    
    if (isolate(input$pregnancy_age2)<25){
      pregnancy_age2<-1
    } else {pregnancy_age2 <-0}  
    
    masterlist<- isolate(c(input$chd2, input$Cystic_fibrosis2, input$Attendance_at_daycare_center2, input$down_syndrome2,input$hiv2,
                           input$Not_exclusive_breastfeeding2, weight2, pregnancy_age2, input$pregnancy_smoking2,
                           input$passive_smoking2,prematurity31,prematurity11,prematurity21, input$Previous_asthma2,input$siblings2))
    row<-which(colSums(t(hos_rows) == masterlist) == ncol(hos_rows))
    hosest<- hos_results[row, "est"]
    
    # isolate(paste(" 1.	该儿童RSV下呼吸道感染住院的风险是一般儿童的", round(hosest, digits=2), "倍"))
    HTML(paste0("<b><span style='font-weight: bold;font-size: 28px;font-family:Helvetica;color:#EE4000;'>", 
                paste(round(hosest, digits=2)), "</span></b>"))  
    
  })
  
  output$Res_hosProb2<- renderUI({
    #copy the code from output$res_combiOR.hos
    req(input$action2)
    
    prematurity11<-0 
    prematurity21<-0 
    prematurity31<-0 
    
    if (isolate(input$prematurity2) == 1){
      prematurity11<-1}
    else if (isolate(input$prematurity2)==2){
      prematurity21<-1} 
    else if (isolate(input$prematurity2)==3){
      prematurity31<-1}
    
    
    if (isolate(input$weight2)<2.5){
      weight2<-1
    } else {weight2 <-0}
    
    
    if (isolate(input$pregnancy_age2)<25){
      pregnancy_age2<-1
    } else {pregnancy_age2 <-0}  
    
    
    masterlist<- isolate(c(input$chd2, input$Cystic_fibrosis2, input$Attendance_at_daycare_center2, input$down_syndrome2,input$hiv2,
                           input$Not_exclusive_breastfeeding2, weight2, pregnancy_age2, input$pregnancy_smoking2,
                           input$passive_smoking2,prematurity31,prematurity11,prematurity21, input$Previous_asthma2,input$siblings2))
    row<-which(colSums(t(prob_rows) == masterlist) == ncol(prob_rows))
    probest2<- prob_results[row, "est"]
    probest2<- probest2*1000
    
    # isolate(paste("2.	该儿童未来一年内发生RSV下呼吸道感染住院的可能性是百分之 ", round(probest2, digits = 2), "%"))
    HTML(paste0("<b><span style='font-weight: bold;font-size: 28px;font-family:Helvetica;color:#EE4000;'>", 
                paste(round(probest2, digits=2),"‰",sep="") , "</span></b>"))  
    
  })
  # ----
  output$Res_hosRiskPercent2 <- renderUI({
    #copy the code from output$res_combiOR.hos
    req(input$action2)
    
    
    
    prematurity11<-0 
    prematurity21<-0 
    prematurity31<-0 
    
    if (isolate(input$prematurity2) == 1){
      prematurity11<-1}
    else if (isolate(input$prematurity2)==2){
      prematurity21<-1} 
    else if (isolate(input$prematurity2)==3){
      prematurity31<-1}
    
    if (isolate(input$weight2)<2.5){
      weight2<-1
    } else {weight2 <-0}
    
    
    if (isolate(input$pregnancy_age2)<25){
      pregnancy_age2<-1
    } else {pregnancy_age2 <-0}  
    
    masterlist<- isolate(c(input$chd2, input$Cystic_fibrosis2, input$Attendance_at_daycare_center2, input$down_syndrome2,input$hiv2,
                           input$Not_exclusive_breastfeeding2, weight2, pregnancy_age2, input$pregnancy_smoking2,
                           input$passive_smoking2,prematurity31,prematurity11,prematurity21, input$Previous_asthma2,input$siblings2))
    row<-which(colSums(t(RiskPercent_rows) == masterlist) == ncol(RiskPercent_rows))
    RiskPercent<- RiskPercent_results[row, "Percent"]
    # RiskPercent<- RiskPercent*10000
    
    # isolate(paste("2.	该儿童未来一年内发生RSV下呼吸道感染住院的可能性是百分之 ", round(probest2, digits = 2), "%"))
    HTML(paste0("<b><span style='font-weight: bold;font-size: 28px;font-family:Helvetica;color:#EE4000;'>", 
                paste(RiskPercent,"%",sep="") , "</span></b>"))  # round(RiskPercent, digits=2),"‱",s
    
  })
  
  output$Res_CombiOR.poc2<-renderUI({
    req(input$action2)
    
    calc_dob<-isolate(as.Date(input$dob))
    
    diff_date<-difftime(todays_date, calc_dob, units= "weeks")
    diff_date <- as.numeric(diff_date)
    
    if (diff_date > 26){
      age2<-0
    } else {age2<-1}
    
    
    
    if (isolate(input$prematurity) %in% c(1,2,3)){
      prematurity4<-1}
    else {
      prematurity4<-0} 
    
    masterlist<-isolate(c(prematurity4, input$chd2, age2))
    row<-isolate(which(colSums(t(poc_rows) == masterlist) == ncol(poc_rows)))
    pocest<- isolate(poc_results[row, "est"])
    
    # isolate(paste("3.	该儿童因RSV下呼吸道感染住院后发生不良结局的风险是一般儿童的   ", round(pocest, digits=2), "倍"))
    HTML(paste0("<b><span style='font-weight: bold;font-size: 28px;font-family:Helvetica;color:#EE4000;'>", 
                paste(round(pocest, digits=2)), "</span></b>"))  
    
  })
  
  output$resultsheader2<-renderText({
    
    req(input$action2)
    isolate(HTML(paste("<b>【RSV风险预测结果】<b>")))
  })
  
  output$preventionheader2<-renderText({
    
    req(input$action2)
    # isolate(HTML(paste("<b>【如何预防RSV感染】<b>")))
    
    isolate(HTML(
      paste0("<b><span style='font-weight: bold;font-size: 20px;font-family:Helvetica;color:#4169E1;'>", 
             "如何预防RSV感染：", "</span></b>")))
  })
  
  output$prevention2<-renderText({
    req(input$action2)
    isolate(paste("目前暂无可用疫苗，国内单抗尚未上市。可通过加强个人防护等措施有效预防感染，包括在RSV好发的冬春季节，尽量避免去人群聚集场所，外出时规范佩戴口罩；注意手卫生，勤洗手，接触公共物品后不用手触碰眼、口、鼻；在儿童医院等医疗机构就诊要做好儿童个人防护，防止院内交叉感染。"))
  })
}

shinyApp(ui = ui, server = server)
# getwd()
