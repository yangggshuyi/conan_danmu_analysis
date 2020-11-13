library(ggplot2)
library(showtext)             # 中文字体
showtext_auto(enable = TRUE)  # 中文字体
library(plotly)               # 交互绘图

load("data/role_episode.RData")


get_hot_roles <- function(n = 10, start = 1, end = 979){
  plot_episodes <- role_episode[start:end, ]
  role_hot <- sort(colSums(plot_episodes), decreasing = T)[1:n]
  return(role_hot)
}

#get_hot_roles()

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
#plot_hot_roles()



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

plot_role_distribution('柯南')




plot_relation <- function(n = 10, start = 1, end = 979){
  role_episode_cut = role_episode[start:end, names(get_hot_roles(n,start,end))]
  relations <- crossprod(role_episode_cut)
  diag(relations) <- 0
  
  relations <- relations / max(relations)
  TOP10rolePlot <- melt(relations)                # 使用reshape包
  TOP10rolePlot2 <- data.table(TOP10rolePlot, key = "Var1")   # 使用data.table包
  TOP10rolePlot2 <- TOP10rolePlot2[order(value, decreasing = T), head(.SD, 3), by=Var1]
  TOP10rolePlot2 <- data.frame(TOP10rolePlot2)
  TOP10rolePlot2[, 3] <- round(TOP10rolePlot2[, 3], 2)      # 将其中元素保留两位小数
  
  ggplot(TOP10majorPlot, aes(Var2, Var1, fill = value)) + 
    geom_tile(colour = "red") +
    geom_text(data = TOP10majorPlot2, aes(x = Var2, y = Var1, label = value), size = 4) +
    scale_fill_gradient(low = "white", high = "red") +
    theme(legend.position = "none",                           # 放大坐标标签字号
          axis.text.y = element_text(color = "grey20", size = 13),
          axis.text.x = element_text(angle = 90, color = "grey20", size = 13))+
    scale_x_discrete(expand=c(0, 0), limits = c(rownames(relations))) +
    scale_y_discrete(expand=c(0, 0), limits = c(rownames(relations)[n:1]))
}

plot_relation()





