load("data/danmu_data.RData")

roles <- readLines("data/主角名单.txt", encoding = "UTF-8")
roles1 <- paste0("(", gsub(" ", ")|(", roles), ")")  
roles_l <- strsplit(roles, " ") # 总结每个人的不同称呼
role_episode <- vector()

# 统计每个人在每一集出现的次数
for(i in 1:length(danmu_data)){
  role_episode1 <- sapply(roles1, grepl, danmu_data[[i]]$danmu)
  colnames(role_episode1) <- sapply(roles_l, function(x) x[1])
  role_count1 = colSums(role_episode1)
  role_episode = rbind(role_episode,colSums(role_episode1))
}

# 测试输出结果
#head(role_episode)
#colSums(role_episode)

save(role_episode, file = "data/role_episode.RData")