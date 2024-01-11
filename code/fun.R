fig <- function(data, loca, type = c("death", "case"), path, save=F, width, height){
  dat <- data %>%
    filter(location == loca) %>% 
    mutate(date = as.Date(date))
  
  switch (type,
          "case" = {
            p <- dat %>% 
              ggplot(., aes(x = date, y = case, group = location)) +
              geom_line(linewidth = 1.2, colour = "#082243") +
              scale_x_date(date_breaks = "6 week", date_labels = "%Y-W%W") +
              labs(x = "日期", y = "确诊病例数") +
              theme_bw(base_family = "Songti SC") +
              theme(axis.text = element_text(size = 24, colour = "black", family = "serif"),
                    axis.title = element_text(size = 30, face = "bold", colour = "black"),
                    plot.title = element_text(size = 30, face = "bold", colour = "black"),
                    plot.margin = unit(c(0.5,1,0.5,1),"cm"),
                    plot.tag = element_text(size = 18),
                    plot.tag.position = c(0, 1))
          },
          "death" = {
            p <- dat %>% 
              ggplot(., aes(x = date, y = death, group = location)) +
              geom_line(linewidth = 1.2, colour = "#082243") +
              scale_x_date(date_breaks = "6 week", date_labels = "%Y-W%W") +
              labs(x = "日期", y = "死亡病例数") +
              theme_bw(base_family = "Songti SC") +
              theme(axis.text = element_text(size = 24, colour = "black", family = "serif"),
                    axis.title = element_text(size = 30, face = "bold", colour = "black"),
                    plot.title = element_text(size = 30, face = "bold", colour = "black"),
                    plot.margin = unit(c(0.5,1,0.5,1),"cm"),
                    plot.tag = element_text(size = 18),
                    plot.tag.position = c(0, 1))
          }
  )
  if (save == T) {
    ggsave(filename = path, p, width = width, height = height, units = "in")
  }
  plot(p)
}