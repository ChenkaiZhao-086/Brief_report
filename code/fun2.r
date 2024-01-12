

Plot.Stream.Global <- function(dat, CurDat, FctLevel, ReportCountry = F) {
  dat <- melt(dat, id.vars = 1:2, measure.vars = 3:length(dat),
              variable.names = "type", value.name = "cases", variable.factor = F)
  dat <- dat[cases > 0
  ][, ":="(year = year(Day), month = month(Day), Day = as.Date(Day))
  ][, TotalYear := sum(cases)
  ][, PercYear := cases / TotalYear # 每日病例占全年病例的比例
  ][, Label := ifelse(variable == "non_who", "Others", ifelse(variable == "Recombinant", "Others", variable))
  ][, LabelCases := sum(cases), by = .(Label, Day)
  ][, TotalPerDay := sum(cases), by = .(Day)
  ][, PercDay := LabelCases / TotalPerDay]
  
  dat <- unique(dat, by = c("Day", "Label"))
  # 这一步非常必要，不去重会出现Others亚型中出现Recombinant和non_who同时存在导致的两条记录，绘图时会出错
  
  datPeak <- dat[, .(Date = max(Day)), by = .(Label)][order(Label)]
  
  
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
  
  fig1 <- dat %>%
    ggplot() +
    geom_stream(aes(x = Day, y = PercDay, fill = Label), type = "proportional") +
    geom_label(data = datLabel, mapping = aes(x = Day + 15, y = index, label = PercLabel),
               color = "black", fill = "#FCFDBFFF", label.size = NA, size = 8, hjust = 0) +
    scale_fill_manual(values = fill_color) +
    scale_x_date(expand = expansion(mult = c(0.1, 0.3)), breaks = axis_break, date_labels = "%b %Y") +
    scale_y_continuous(expand = c(0, 0), labels = scales::percent_format(scale = 100), breaks = seq(0, 1, 0.1),
                       sec.axis = sec_axis(trans = ~., labels = scales::percent_format(scale = 100), breaks = seq(0, 1, 0.1))) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 14),
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
      
  
  fig2 <- dat %>%
    ggplot() +
    geom_line(aes(x = Day, y = PercDay, color = Label), linewidth = 1) +
    scale_x_date(expand = c(0.01, 0.5), breaks = axis_break, date_labels = "%b %Y") +
    scale_y_continuous(expand = c(0,0)) +
    theme_classic() +
    scale_color_manual(values = fill_color) +
    theme(axis.text.x = element_text(size = 14),
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
      
  
  if (ReportCountry) {
    fig1 <- fig1 + labs(title = paste0("Timecourse of SARS-Cov-2 Variants sublineage distribution in ", unique(dat$Entity)),
                        subtitle = paste0(min(dat$Day), " — ", max(dat$Day)))
    fig2 <- fig2 + labs(title = paste0("Timecourse of SARS-Cov-2 Variants sublineage distribution in ", unique(dat$Entity)),
                        subtitle = paste0(min(dat$Day), " — ", max(dat$Day)))
  } else {
    fig1 <- fig1 + labs(title = "Timecourse of SARS-Cov-2 Variants sublineage distribution",
                        subtitle = paste0(min(dat$Day), " — ", max(dat$Day)))
    fig2 <- fig2 + labs(title = "Timecourse of SARS-Cov-2 Variants sublineage distribution",
                        subtitle = paste0(min(dat$Day), " — ", max(dat$Day)))
  }
    
  MaxDay <- dat[, .SD[which.max(PercDay)], by = .(Label)
   ][order(Label)
   ][, -c("Entity", "variable", "cases", "year", "month", 'TotalYear', "PercYear")
   ][, PercDay := PercDay * 100]
  colnames(MaxDay) <- c("Virus type", "Date", "Reported cases", "Total cases", "Max proportion")
  
  print(fig1)
  print(fig2)
  
  list(fig1, fig2, MaxDay)
}


Plot.Stream.JS <- function(dat, FctLevelJS){
  datPeak <- dat[, .(Date = max(SampelDate)), by = .(Label)][order(Label)]
  
  fill_color <- viridis(n = length(FctLevelJS) - 1, option = "A")
  fill_color <- c(fill_color, "grey50")
  names(fill_color) <- unique(FctLevelJS)
  
  axis_break <- seq.Date(min(dat$SampelDate), max(dat$SampelDate), by = "4 week") 
  
  datLabel <- dat[week == max(week), .SD
  ][, ":="(PercLabel = paste0(Label, " (", sprintf("%.1f", PercWeek * 100), "%)"),
           var3 = factor(Label, levels = FctLevelJS), Date = max(dat$SampelDate))
  ][order(-PercWeek)][1:5][, index := rev(1:length(Label)) * 0.1]
  
  dat[, Label := factor(Label, levels = FctLevelJS)]
  
  fig1 <- ggplot(dat) +
    geom_stream(aes(x = SampelDate, y = PercWeek, fill = Label), type = "proportional") +
    geom_label(data = datLabel, mapping = aes(x = Date + 2, y = index, label = PercLabel),
               color = "black", fill = "#FCFDBFFF", label.size = NA, size = 4, hjust = 0) +
    scale_fill_manual(values = fill_color) +
    scale_x_date(expand = expansion(mult = c(0.01, 0.3)), breaks = axis_break, date_labels = "%Y-%m") +
    scale_y_continuous(
      expand = c(0, 0), labels = scales::percent_format(scale = 100), breaks = seq(0, 1, 0.1),
      sec.axis = sec_axis(trans = ~., labels = scales::percent_format(scale = 100), breaks = seq(0, 1, 0.1))
    ) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
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
    labs(title = NULL, # "新冠病毒亚型分布变化",
         x = NULL, y = NULL, fill = "病毒亚型",
         subtitle = paste0("报告时间：", min(dat$SampelDate), " — ", max(dat$SampelDate)))
  
  
  fig2 <- ggplot(dat) +
    geom_line(aes(x = SampelDate, y = PercWeek, color = Label), linewidth = 1) +
    scale_x_date(expand = c(0.01, 0.5), breaks = axis_break, date_labels = "%Y-%m") +
    scale_y_continuous(expand = c(0, 0)) +
    theme_classic() +
    scale_color_manual(values = fill_color) +
    theme(axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.position = "bottom",
          legend.spacing.y = unit(50, "pt"),
          legend.key.height = unit(0.3, "cm"),
          legend.key.width = unit(0.7, "cm"),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14, face = "bold"),
          plot.title = element_text(hjust = 1, face = "bold"),
          plot.subtitle = element_text(hjust = 1, face = "bold"),
          plot.caption = element_text(hjust = 0),
          plot.margin = margin(10, 10, 10, 10)) + 
    labs(title = "Timecourse of SARS-Cov-2 Variants sublineage distribution",
         x = NULL, y = NULL, color = "病毒亚型",
         subtitle = paste0("报告时间：", min(dat$SampelDate), " — ", max(dat$SampelDate)))

  
  MaxDay <- dat[, .SD[which.max(PercWeek)], by = .(Label)
  ][, week := ISOweek2date(paste0(year(SampelDate), "-W", 
                                  sprintf("%02d", week), "-1")) 
    # %d表示目标数据是一个整数，02：这部分指定了数字的最小宽度为2位。如果数字本身不足两位，那么会在数字前面补充0。
  ][, PercWeek := round((PercWeek * 100), 1)
  ][, -c("SampelDate")
  ][order(-PercWeek)
  ][, PercWeek := paste0(PercWeek, "%")
  ][, .(Label, week, PercWeek, TotalVarWeek, TotalWeek)]
  colnames(MaxDay) <- c("病毒亚型", "流行峰值时间", "亚型占比", "亚型报告病例数", "累积病例数")
  print(fig1)
  print(fig2)
  
  list(fig1, fig2, MaxDay)
}

Text.Date <- function(dat){
  MinDate <- min(dat$SampelDate)
  MaxDate <- max(dat$SampelDate)
  paste0(year(MinDate), "年", month(MinDate), "月", day(MinDate), "日至", 
         year(MaxDate), "年", month(MaxDate), "月", day(MaxDate), "日，")
}

Text.Date2 <- function(dat){
  Date <- ymd(dat)
  paste0(year(Date), "年", month(Date), "月", day(Date), "日")
}
