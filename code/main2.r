library(data.table)
library(tidyverse)
library(readxl)
library(ggstream)
library(viridis)
library(doParallel)
library(foreach)
library(ISOweek)

# library(httpgd)
# hgd()
# hgd_view()

source("code/fun2.r")

CurDat <- as.Date("2023-12-28") # Sys.Date()

### Our world in data ----------------------------------------------------------------------------------
# loca <- c(
#   "Hong Kong", "Singapore", "Japan", "South Korea", "United Kingdom",
#   "Australia", "United States", "South Africa"
# )
# 
# Area <- fread(paste0(CurDat, "/covid-variants-area.csv"))
# Bar <- fread(paste0(CurDat, "/covid-variants-bar.csv"))
# 
# Area <- Area[Day >= CurDat - 365, -"Code"]
# colnames(Area) <- c(
#   "Entity", "Day", "Alpha", "Beta", "Gamma", "Delta", "BA.2", "BA.1", "BA.5",
#   "BA.4", "BA.2.12.1", "BA.2.75", "BQ.1", "XBB", "XBB.1.5", "XBB.1.16",
#   "CH.1.1", "XBB.1.9", "XBB.2.3", "EG.5.1", "Recombinant", "non_who"
# )
# FctLevel <- c(
#   "Alpha", "Beta", "Gamma", "Delta", "BA.2", "BA.1", "BA.5", "BA.4",
#   "BA.2.12.1", "BA.2.75", "BQ.1", "XBB", "XBB.1.5", "XBB.1.16", "CH.1.1",
#   "XBB.1.9", "XBB.2.3", "EG.5.1", "Others"
# )
# 
# Plot.Stream.Global (Area, CurDat, FctLevel, ReportCountry = F)
# 
# # For each country
# cl <- makeCluster(detectCores() - 1)
# registerDoParallel(cl)
# CountryListArea <- foreach(
#   df = split(Area, Area$Entity),
#   .packages = c("data.table", "tidyverse", "ggstream", "viridis")
# ) %dopar% {
#   Plot.Stream.Global (df, CurDat, FctLevel, ReportCountry = T)
# }
# names(CountryListArea) <- dput(names(split(Area, Area$Entity)))
# stopCluster(cl)
# 
# 
# Bar <- Bar[Day >= CurDat - 365, -"Code"]
# colnames(Bar) <- c(
#   "Entity", "Day", "Alpha", "Beta", "Gamma", "Delta", "BA.2", "BA.1", "BA.5",
#   "BA.4", "BA.2.12.1", "BA.2.75", "BQ.1", "XBB", "XBB.1.5", "XBB.1.16",
#   "CH.1.1", "XBB.1.9", "XBB.2.3", "EG.5.1", "Recombinant", "non_who"
# )
# Plot.Stream.Global (Bar, CurDat, FctLevel, ReportCountry = F)
# 
# 
### CoVariant --------------------------------------------------------------------------------
# CoVariant <- read.csv("json.csv") %>% as.data.table()
# colnames(CoVariant) <- c("total_sequences", "Day", "country", "Label", "cases", "year", 
#                          "month", "TotalYear", "PercYear", "percDay")
# CoVariant <- CoVariant[, ":="(Day = as.Date(Day))][, check := sum(cases), by = Label] # 找到完全没有病例的亚型
# a <- CoVariant[check > 0, ]
# unique(a$Label)
# 
# # Virus name
# VirName <- c("a_20A/S:439K", "Beta", "Alpha", "Gamma", "Delta21A", "Delta21J", "BA.1", 
#              "BA.2", "BA.4", "BA.5", "BA.2.12.1", "BA.2.75", "BQ.1", "XBB", "XBB.1.5", 
#              "XBB.1.16", "CH.1.1", "XBB.1.9", "XBB.2.3", "EG.5.1", "Recombinant")
# 
# CoVariant_Global <- copy(CoVariant)
# CoVariant_Global <- CoVariant_Global[, TotlaWeek := sum(total_sequences), by = .(Day)
# ][, cases := sum(cases), by = .(Day, type)
# ][, ":="(country = NULL, total_sequences = NULL, TotalYear = NULL, percDay = NULL, PercYear = NULL)]
# CoVariant_Global <- unique(CoVariant_Global, by = c("Day", "type"))
# 
# CoVariant_Global <- CoVariant_Global[, TotalPerDay := sum(cases), by = Day
# ][, PercDay := cases / TotalPerDay][, ":="(total_sequences = NULL, TotalYear = NULL )]
# 
# 
# dput(colnames(CoVariant))
# 
# fill_label <- unique(datPeak$Label)
# fill_label <- fill_label[fill_label != "Others"]
# fill_label <- c(fill_label, "Others")
# 
# fill_color <- viridis(n = length(fill_label) - 1, option = "A")
# fill_color <- c(fill_color, "grey50")
# names(fill_color) <- unique(fill_label)
# 
# axis_break <- seq.Date(min(dat$Day), max(dat$Day), by = "3 months")
# 
# datLabel <- dat[Day == max(Day), .SD
# ][, ":="(PercLabel = paste0(Label, " (", sprintf("%.2f", PercDay * 100), "%)"),
#          Label = factor(Label, levels = FctLevel))
# ][order(Label)][, index := (1:length(Label)) * 0.1]
# 
# dat[, Label := factor(Label, levels = FctLevel)]
# 
# fig1 <- CoVariant_Global %>% as.data.frame() %>% 
#   ggplot() +
#   geom_stream(aes(x = week, y = PercDay, fill = type), type = "proportional") +
#   geom_label(data = datLabel, mapping = aes(x = Day + 15, y = index, label = PercLabel),
#              color = "black", fill = "#FCFDBFFF", label.size = NA, size = 8, hjust = 0) +
#   scale_fill_manual(values = fill_color) +
#   scale_x_date(expand = expansion(mult = c(0.1, 0.3)), breaks = axis_break, date_labels = "%b %Y") +
#   scale_y_continuous(expand = c(0, 0), labels = scales::percent_format(scale = 100), breaks = seq(0, 1, 0.1),
#                      sec.axis = sec_axis(trans = ~., labels = scales::percent_format(scale = 100), breaks = seq(0, 1, 0.1))) +
#   theme_classic() +
#   theme(
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14),
#     axis.line.x = element_blank(),
#     legend.position = "bottom",
#     legend.spacing.y = unit(50, "pt"),
#     legend.key.height = unit(0.3, "cm"),
#     legend.key.width = unit(0.7, "cm"),
#     legend.text = element_text(size = 14),
#     plot.title = element_text(hjust = 1, face = "bold"),
#     plot.subtitle = element_text(hjust = 1, face = "bold"),
#     plot.caption = element_text(hjust = 0),
#     plot.margin = margin(10, 10, 10, 10))
# 
# fig2 <- CoVariant_Global %>% as.data.frame() %>% 
#   ggplot() +
#   geom_line(aes(x = week, y = PercDay, color = type), linewidth = 1) +
#   scale_x_date(expand = c(0.01, 0.5), breaks = axis_break, date_labels = "%b %Y") +
#   scale_y_continuous(expand = c(0,0)) +
#   theme_classic() +
#   scale_color_manual(values = fill_color) +
#   theme(
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14),
#     legend.position = "bottom",
#     legend.spacing.y = unit(50, "pt"),
#     legend.key.height = unit(0.3, "cm"),
#     legend.key.width = unit(0.7, "cm"),
#     legend.text = element_text(size = 14),
#     plot.title = element_text(hjust = 1, face = "bold"),
#     plot.subtitle = element_text(hjust = 1, face = "bold"),
#     plot.caption = element_text(hjust = 0),
#     plot.margin = margin(10, 10, 10, 10))
# 

## CDC data -------------------------------------------------------------------------------------
xlsx_files <- list.files("data", pattern = "\\.xlsx$", full.names = TRUE)
merged_data <- data.frame()

for (file in xlsx_files) {
  data <- read_xlsx(file,
    col_types = c(
      "text", "date", "text", "text", "text", "numeric", "text",
      "text", "text", "text", "date", "date", "date", "text",
      "text", "text", "numeric", "numeric", "text", "text",
      "text", "text", "text", "text"
    )
  )
  colnames(data) <- c(
    "useless_1", "ReportDate", "useless_2", "useless_3", "sex", "age",
    "PatientType", "SourceLocation", "hospital", "useless_4", "SymptomDate",
    "useless_4", "SampelDate", "ClinicalType", "useless_6", "useless_7",
    "Ct_ORFlab", "Ct_N", "useless_8", "useless_9", "varient", "var1",
    "var2", "var3"
  )
  data <- data %>%
    select(!starts_with("useless")) %>%
    mutate(
      ReportDate = as.Date(ReportDate),
      SymptomDate = as.Date(SymptomDate),
      SampelDate = as.Date(SampelDate)
    )
  merged_data <- rbind(merged_data, data)
}

merged_data <- as.data.table(merged_data)

JSDat <- copy(merged_data)
# 亚型分类规则：如果v1小于30，归类为Others，如果v2小于30，替换为v1，如果v3小于30，替换为v2
JSDat <- JSDat[, ":="(ReportDate = as.Date(ReportDate), 
                      sex = fcase(sex == "男", 1, sex == "女", 0), 
                      SymptomDate = as.Date(SymptomDate), 
                      SampelDate = as.Date(SampelDate), 
                      case = 1, 
                      week = isoweek(SampelDate), 
                      PatientType = fifelse(grepl("本土", PatientType), "Local", "Imported"),
                      ClinicalType = fcase(grepl("重型", ClinicalType), "Heavy", grepl("死", ClinicalType), "Death", default = "Normal"))
               ][week >= 16 & week <31
                 ][, v1sum := sum(case), by = var1
                 ][, v2sum := sum(case), by = var2
                 ][, v3sum := sum(case), by = var3
                 ][, Lab1 := fifelse(v1sum < 30, "Others", var1)
                 ][, Lab2 := fifelse(v2sum < 30, Lab1, var2)
                 ][, Lab3 := fifelse(v3sum < 30, Lab2, var3)
                   ][, TotalWeek := sum(case), by = week
                   ][, TotalVarWeek := sum(case), by = .(week, Lab3)
                     ][, PercWeek := TotalVarWeek / TotalWeek
                       ][order(week)]
JSDat_Unique <- unique(JSDat, by = c("week", "Lab3"))
JSDat_Unique <- JSDat_Unique[, c("SampelDate", "week", "Lab3", "PercWeek", "TotalWeek", "TotalVarWeek")]
setnames(JSDat_Unique, "Lab3", "Label")

FctLevelJS <- sort(dput(unique(JSDat_Unique$Label)))
FctLevelJS <- FctLevelJS[FctLevelJS != "Others"]
FctLevelJS <- c(FctLevelJS, "Others")

JSPlot <- Plot.Stream.JS(JSDat_Unique, FctLevelJS)
Table <- JSPlot[[3]][1:10] %>% as.data.frame()

PiePlotDat <- JSDat[, PatientCount := sum(case), by = PatientType
                 ][, .(PatientCount, PatientType)]
PiePlotDat <- unique(PiePlotDat)
PiePlotDat <- PiePlotDat[, Fraction := PatientCount / sum(PatientCount)
                   ][, Percentage := paste0(round(Fraction * 100, 1), "%")]

PiePlot <- ggplot(PiePlotDat, aes(x = "", y = PatientCount, fill = PatientType)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#fe3e06", "#e2dacf"), labels = c("输入病例", "本土病例")) +
  geom_text(aes(label = Percentage), position = position_stack(vjust = 0.5), size = 8) +
  theme_void() +
  theme(axis.line.x = element_blank(),
        legend.position = "bottom",
        legend.spacing.y = unit(50, "pt"),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.7, "cm"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0, face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.margin = margin(10, 10, 10, 10)) + 
  labs(title = NULL, # "本土及输入病例构成",
       x = NULL, y = NULL, fill = "病例来源")


LocalCases <- JSDat[PatientType == "Local"]
LocalV1 <- LocalCases[, .N, by = .(Lab1)][, percentage := round((N / sum(N) * 100), 1)]
LocalV2 <- LocalCases[, Lab1Count := .N, by = .(Lab1)
                      ][, Lab2Count := .N, by = .(Lab2)
                        ][, Lab3Count := .N, by = .(Lab3)
                          ][, c("Lab1", "Lab2", "Lab3", "Lab1Count", "Lab2Count", "Lab3Count")
                            ][, ":="(Lab2Perc = round((Lab2Count / Lab1Count * 100), 1), 
                                     Lab3Perc = round((Lab3Count / Lab1Count * 100), 1))
                              ][order(Lab1)]

LocalV2 <- unique(LocalV2, by = c("Lab1", "Lab2", "Lab3"))


ImportCases <- JSDat[PatientType == "Imported"]
ImportV1 <- ImportCases[, .N, by = .(Lab1)][, percentage :=  round((N / sum(N) * 100), 1)]
ImportV2 <- ImportCases[, Lab1Count := .N, by = .(Lab1)
                        ][, Lab2Count := .N, by = .(Lab2)
                          ][, Lab3Count := .N, by = .(Lab3)
                            ][, c("Lab1", "Lab2", "Lab3", "Lab1Count", "Lab2Count", "Lab3Count")
                              ][, ":="(Lab2Perc = round((Lab2Count / Lab1Count * 100), 1), 
                                       Lab3Perc = round((Lab3Count / Lab1Count * 100), 1))
                                ][order(Lab1)]

ImportV2 <- unique(ImportV2, by = c("Lab1", "Lab2", "Lab3"))

a1 <- ImportCases[order(SampelDate, var3)]
unique(a1, by = c("var3"))
a2 <- LocalCases[order(SampelDate, var3)]
unique(a2, by = c("var3"))

LocalImportComp <- ggplot() +
  geom_point(data = a2, aes(x = SampelDate, y = var3, color = "本土病例"), alpha = 0.3) +
  geom_point(data = a1, aes(x = SampelDate, y = var3, color = "输入病例"), alpha = 0.9, size = 2) +
  scale_color_manual(name = "病例来源", values = c("本土病例" = "#fe3e06", "输入病例" = "#044569"),
                     labels = c("本土病例", "输入病例")) +
  scale_x_date(date_labels = "%Y-%m", expand = expansion(mult = c(0.01, 0.1)))+
  theme_bw() +
  theme(axis.text.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.line.x = element_blank(),
        legend.position = "bottom",
        legend.spacing.y = unit(50, "pt"),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.7, "cm"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0, face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.margin = margin(10, 10, 10, 10)) +
  labs(x = NULL, y = NULL, title = NULL, # "本土及输入病例对新冠病毒传播的影响",
       subtitle = paste0("报告时间：", min(a2$SampelDate), " — ", max(a2$SampelDate)))


HeavyCases <- JSDat[ClinicalType == "Heavy"]
HeavyCount <- HeavyCases[, sum(case)]
table(HeavyCases$var2)
