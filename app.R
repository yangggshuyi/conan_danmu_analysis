library(shiny)
library(shinythemes)
library(ggplot2)
library(showtext)             # 中文字体
showtext_auto(enable = TRUE)  # 中文字体
library(plotly)               # 交互绘图
library(data.table)
library(reshape)

load("data/danmu_data.RData")
load("data/role_episode.RData")


### 1.画图函数
# 函数1.根据选定集数输出热门角色
get_hot_roles <- function(n = 10, start = 1, end = 979){
  plot_episodes <- role_episode[start:end, ]
  role_hot <- sort(colSums(plot_episodes), decreasing = T)[1:n]
  return(role_hot)
}

# 函数2.根据选定集数计算热门角色被讨论的次数
plot_hot_roles <- function(n = 10, start = 1, end = 979){
  role_sum = get_hot_roles(n,start,end)
  plot_episodes <- data.frame(
    role = factor(names(role_sum), levels = names(role_sum)),
    count = role_sum
  )
  ggplot(plot_episodes[1:n,], aes(x = role, y = count, fill = role)) + 
    geom_bar(stat = "identity", width = 0.75, alpha = I(0.9),show.legend = F) +
    xlab("热门角色") + ylab("热议次数")
}

# 函数3.给出某个角色在选定集数内出现的概率密度图
plot_role_distribution <- function(name, start = 1, end = 979){
  role_episode_ = role_episode[start:end, ]
  distribution = cbind(start:end, as.data.frame(role_episode_)[name])
  colnames(distribution) <- c('part','count')
  tmp = unlist(mapply(distribution$part,distribution$count,
                      FUN = function(x,y){rep(x,y)}))
  ggplot(data.frame(tmp), aes(x = tmp))+
    geom_density(bw = 2.5, color = "lightcoral", fill = "lightcoral", alpha = I(0.75))+
    xlab("集数") + ylab("密度") + ggtitle(paste(name,'热议情况'))
  
}

# 输入角色，给出讨论次数最高的几集
print_hot_episode <- function(name, top = 5){
  tmp = data.frame(
    episode = 1:979,
    count = as.data.frame(role_episode)[name]
  )
  top_episode = tmp[order(tmp[,2],decreasing = T), 1][1:top]
  
  hot_titles = info[top_episode,2]
  colnames(hot_titles) <- '该角色热门集数'
  return(hot_titles)
}

# 函数4.画给定集数内热门角色关系
plot_relation <- function(n = 10, start = 1, end = 979){
  role_episode_cut = role_episode[start:end, names(get_hot_roles(n,start,end))]
  relations <- crossprod(role_episode_cut)
  diag(relations) <- 0
  relations <- relations / max(relations)
  TOP10rolePlot <- melt(relations)                # 使用reshape包
  TOP10rolePlot2 <- data.table(TOP10rolePlot, key = "value")   # 使用data.table包
  TOP10rolePlot2 <- TOP10rolePlot2[order(value, decreasing = T), head(.SD, 3), by = value]
  TOP10rolePlot2 <- data.frame(TOP10rolePlot2)
  TOP10rolePlot2[, 1] <- round(TOP10rolePlot2[, 1], 2)      # 将其中元素保留两位小数
  ggplot(TOP10rolePlot2, aes(X1, X2, fill = value)) + 
    geom_tile(colour = "red") +
    geom_text(data = TOP10rolePlot2, aes(x = X2, y = X1, label = value), size = 4) +
    scale_fill_gradient(low = "white", high = "red") +
    theme(legend.position = "none",                           # 放大坐标标签字号
          axis.text.y = element_text(color = "grey20", size = 12),
          axis.text.x = element_text(angle = 90, color = "grey20", size = 12))+
    scale_x_discrete(expand=c(0, 0), limits = c(rownames(relations))) +
    scale_y_discrete(expand=c(0, 0), limits = c(rownames(relations)[n:1]))
}


##### 布局定义 ----
ui <- fluidPage(
  theme = shinytheme("paper"),
  titlePanel("B站柯南弹幕角色分析"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("选择"),
      
      sliderInput(inputId = "range_of_roles",
                  label = "想看前几名热门人物？",
                  value = 5,
                  min = 1,
                  max = 15),
      
      sliderInput("range_of_episodes", 
                  label = "感兴趣的集数范围:",
                  min = 1, max = 979, value = c(1, 979)),
      
      br(),
      
      selectInput(inputId = "interest_role",
                  label = "选择一个你感兴趣的角色:",
                  choices = c("柯南", "毛利兰","灰原哀","小五郎", "怪盗基德"))
      
      ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel('热门角色',
                           plotOutput("roles_barplot"),
                           plotOutput("roles_relation")),
                  tabPanel('感兴趣的角色', 
                           plotOutput("roles_density"),
                           tableOutput("hot_episode"))
      )
                  
    )
  )
)

# server ---- 
server <- function(input, output) {
  output$roles_barplot <- renderPlot({
    plot_hot_roles(n = input$range_of_roles, 
                   start = input$range_of_episodes[1], 
                   end = input$range_of_episodes[2])
  })
  
  output$roles_density <- renderPlot({
    plot_role_distribution(name = input$interest_role,
                           start = input$range_of_episodes[1], 
                           end = input$range_of_episodes[2])
  })
  
  output$roles_relation <- renderPlot({
    plot_relation(n = input$range_of_roles,
                  start = input$range_of_episodes[1], 
                  end = input$range_of_episodes[2])
  })
  
  output$hot_episode <- renderTable({
    print_hot_episode(input$interest_role)
  })
}


# Run the app ---- 
shinyApp(ui = ui, server = server)
