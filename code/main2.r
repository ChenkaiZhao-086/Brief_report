library(data.table)
library(tidyverse)
library(readxl)
library(ggstream)
library(viridis)
library(doParallel)
library(foreach)
library(ISOweek)

library(httpgd)
hgd()
hgd_view()

source("code/fun2.r")


CurDat <- as.Date("2023-12-28") # Sys.Date()

### Our world in data ----------------------------------------------------------------------------------
loca <- c(
  "Hong Kong", "Singapore", "Japan", "South Korea", "United Kingdom",
  "Australia", "United States", "South Africa"
)

Area <- fread(paste0(CurDat, "/covid-variants-area.csv"))
Bar <- fread(paste0(CurDat, "/covid-variants-bar.csv"))

Area <- Area[Day >= CurDat - 365, -"Code"]
colnames(Area) <- c(
  "Entity", "Day", "Alpha", "Beta", "Gamma", "Delta", "BA.2", "BA.1", "BA.5",
  "BA.4", "BA.2.12.1", "BA.2.75", "BQ.1", "XBB", "XBB.1.5", "XBB.1.16",
  "CH.1.1", "XBB.1.9", "XBB.2.3", "EG.5.1", "Recombinant", "non_who"
)
FctLevel <- c(
  "Alpha", "Beta", "Gamma", "Delta", "BA.2", "BA.1", "BA.5", "BA.4",
  "BA.2.12.1", "BA.2.75", "BQ.1", "XBB", "XBB.1.5", "XBB.1.16", "CH.1.1",
  "XBB.1.9", "XBB.2.3", "EG.5.1", "Others"
)

Plot.Stream.Global (Area, CurDat, FctLevel, ReportCountry = F)

# For each country
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
CountryListArea <- foreach(
  df = split(Area, Area$Entity),
  .packages = c("data.table", "tidyverse", "ggstream", "viridis")
) %dopar% {
  Plot.Stream.Global (df, CurDat, FctLevel, ReportCountry = T)
}
names(CountryListArea) <- dput(names(split(Area, Area$Entity)))
stopCluster(cl)


Bar <- Bar[Day >= CurDat - 365, -"Code"]
colnames(Bar) <- c(
  "Entity", "Day", "Alpha", "Beta", "Gamma", "Delta", "BA.2", "BA.1", "BA.5",
  "BA.4", "BA.2.12.1", "BA.2.75", "BQ.1", "XBB", "XBB.1.5", "XBB.1.16",
  "CH.1.1", "XBB.1.9", "XBB.2.3", "EG.5.1", "Recombinant", "non_who"
)
Plot.Stream.Global (Bar, CurDat, FctLevel, ReportCountry = F)


### CoVariant --------------------------------------------------------------------------------
CoVariant <- read.csv("json.csv") %>% as.data.table()
colnames(CoVariant) <- c("total_sequences", "Day", "country", "Label", "cases", "year", 
                         "month", "TotalYear", "PercYear", "percDay")
CoVariant <- CoVariant[, ":="(Day = as.Date(Day))][, check := sum(cases), by = Label] # 找到完全没有病例的亚型
a <- CoVariant[check > 0, ]
unique(a$Label)

# Virus name
VirName <- c("a_20A/S:439K", "Beta", "Alpha", "Gamma", "Delta21A", "Delta21J", "BA.1", 
             "BA.2", "BA.4", "BA.5", "BA.2.12.1", "BA.2.75", "BQ.1", "XBB", "XBB.1.5", 
             "XBB.1.16", "CH.1.1", "XBB.1.9", "XBB.2.3", "EG.5.1", "Recombinant")

CoVariant_Global <- copy(CoVariant)
CoVariant_Global <- CoVariant_Global[, TotlaWeek := sum(total_sequences), by = .(Day)
][, cases := sum(cases), by = .(Day, type)
][, ":="(country = NULL, total_sequences = NULL, TotalYear = NULL, percDay = NULL, PercYear = NULL)]
CoVariant_Global <- unique(CoVariant_Global, by = c("Day", "type"))

CoVariant_Global <- CoVariant_Global[, TotalPerDay := sum(cases), by = Day
][, PercDay := cases / TotalPerDay][, ":="(total_sequences = NULL, TotalYear = NULL )]


dput(colnames(CoVariant))

fill_label <- unique(datPeak$Label)
fill_label <- fill_label[fill_label != "Others"]
fill_label <- c(fill_label, "Others")

fill_color <- viridis(n = length(fill_label) - 1, option = "A")
fill_color <- c(fill_color, "grey50")
names(fill_color) <- unique(fill_label)

axis_break <- seq.Date(min(dat$Day), max(dat$Day), by = "3 months")

datLabel <- dat[Day == max(Day), .SD
][, ":="(PercLabel = paste0(Label, " (", sprintf("%.2f", PercDay * 100), "%)"),
         Label = factor(Label, levels = FctLevel))
][order(Label)][, index := (1:length(Label)) * 0.1]

dat[, Label := factor(Label, levels = FctLevel)]

fig1 <- CoVariant_Global %>% as.data.frame() %>% 
  ggplot() +
  geom_stream(aes(x = week, y = PercDay, fill = type), type = "proportional") +
  geom_label(data = datLabel, mapping = aes(x = Day + 15, y = index, label = PercLabel),
             color = "black", fill = "#FCFDBFFF", label.size = NA, size = 8, hjust = 0) +
  scale_fill_manual(values = fill_color) +
  scale_x_date(expand = expansion(mult = c(0.1, 0.3)), breaks = axis_break, date_labels = "%b %Y") +
  scale_y_continuous(expand = c(0, 0), labels = scales::percent_format(scale = 100), breaks = seq(0, 1, 0.1),
                     sec.axis = sec_axis(trans = ~., labels = scales::percent_format(scale = 100), breaks = seq(0, 1, 0.1))) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.line.x = element_blank(),
    legend.position = "bottom",
    legend.spacing.y = unit(50, "pt"),
    legend.key.height = unit(0.3, "cm"),
    legend.key.width = unit(0.7, "cm"),
    legend.text = element_text(size = 14),
    plot.title = element_text(hjust = 1, face = "bold"),
    plot.subtitle = element_text(hjust = 1, face = "bold"),
    plot.caption = element_text(hjust = 0),
    plot.margin = margin(10, 10, 10, 10))

fig2 <- CoVariant_Global %>% as.data.frame() %>% 
  ggplot() +
  geom_line(aes(x = week, y = PercDay, color = type), linewidth = 1) +
  scale_x_date(expand = c(0.01, 0.5), breaks = axis_break, date_labels = "%b %Y") +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  scale_color_manual(values = fill_color) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.position = "bottom",
    legend.spacing.y = unit(50, "pt"),
    legend.key.height = unit(0.3, "cm"),
    legend.key.width = unit(0.7, "cm"),
    legend.text = element_text(size = 14),
    plot.title = element_text(hjust = 1, face = "bold"),
    plot.subtitle = element_text(hjust = 1, face = "bold"),
    plot.caption = element_text(hjust = 0),
    plot.margin = margin(10, 10, 10, 10))


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

Plot.Stream.JS(JSDat_Unique, FctLevelJS)

PiePlot <- JSDat[, PatientCount := sum(case), by = PatientType
                 ][, .(PatientCount, PatientType)]
PiePlot <- unique(PiePlot)
PiePlot <- PiePlot[, Fraction := PatientCount / sum(PatientCount)
                   ][, Percentage := paste0(round(Fraction * 100, 1), "%")]

ggplot(PiePlot, aes(x = "", y = PatientCount, fill = PatientType)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) +
  geom_text(aes(label = Percentage), position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.spacing.y = unit(50, "pt"),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.7, "cm"),
        legend.text = element_text(size = 14),
        plot.title = element_text(hjust = 1, face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.margin = margin(10, 10, 10, 10))


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
ImportV1 <- ImportCases[, .N, by = .(Lab1)][, percentage := N / sum(N) * 100]
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
ggplot() +
  geom_point(data = a2, aes(x = SampelDate, y = var3), color = "red", alpha = 0.3) +
  geom_point(data = a1, aes(x = SampelDate, y = var3), color = "blue", alpha = 0.9, size = 1.5) +
  theme_bw() +
  labs(x = "Sample Date", y = "Variant", title = "Variant Distribution of Local and Imported Cases") +
  theme(legend.position = "bottom",
        legend.spacing.y = unit(50, "pt"),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.7, "cm"),
        legend.text = element_text(size = 14),
        plot.title = element_text(hjust = 1, face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.margin = margin(10, 10, 10, 10))





HeavyCases <- JSDat[ClinicalType == "Heavy"]
HeavyCount <- HeavyCases[, sum(case)]
table(HeavyCases$var2)

c("
  1. 新冠病毒序列概况
2023年1月1日至8月4日，全省共获得本士感染者新冠病毒基因组有效序列2959株，均为奥密克戎变异株，其中重组变异株 XBB 型后代谱系1675 株（占56.6%，包括XBB.1.5及其后代谱系 442 株，XBB.1.9及其后代谱系446 株，XBB.1.16及其后代谱系 210株，XBB.1.22 及其后代谱系490 株，其它 XBB 型及其后代谱系87 株），BA.5.2 型及其后代谱系 656株（占22.2%）；BF.7型及其后代谱系 563 株（占19.0%）；其他重点关注变异株65株，（占2.2%）。
.
2. 入境感染者新冠病毒序列概况
2023年1月1日至8月4日，全省共发现入境新冠病毒感染者167例，对其中符合基因测序条件的101例进行测序，结果发现重组变异株 XBB 型后代谱系70株（占69.3%，包括XBB.1.5型及其后代谱系 12 株，XBB.1.9 型及其后代谱系33株，XBB.1.16 型及其后代谱系 16 株，XBB.1.22及其后代谱系2株，其它XBB 型后代谱系 7株）；BA.5.2 型及其后代谱系10株（占10.0%）；BF.7 型及其后代谱系3株（占3.0%）；其他重点关注变异株18株（占18.0%）。
3. 重症及死亡病例测序情况
2023年1月1日-8月4日全省累计重症病例215例，其中 BA.5.2 型及其后代谱系 127例，BF.7 型及其后代谱系 76 例，XBB.1.22 型及其后代谱系3例，XBB.1.5 型及其后代谱系1例，XBB.1.9型及其后代谱系2例，XBB.1.16 型及其后代谱系1例，BN.1型及其后代谱系 2例，其他重点关注变异株3例。死亡病例1例，为BA.5.2.49型。
  ")

