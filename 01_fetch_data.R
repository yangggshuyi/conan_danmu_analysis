library(jsonlite)
library(tibble)
library(curl)

episodes = read_json("data/ep_list.json")   # 预存的爬取数据
length(episodes)  # 一共979集

info = lapply(episodes, function(ep) {
    tibble(   # 提取这几列
        episode = as.integer(ep$title),
        title   = paste(ep$titleFormat, ep$longTitle, sep = "-"),
        bv      = ep$bvid,
        ev      = ep$id,
        cid     = ep$cid
    )
})
info = do.call(rbind, info)
#head(info)   # 整理完的结果

api = "http://api.bilibili.com/x/v1/dm/list.so?oid=%s"
requests = sprintf(api, info$cid)
i = 1
returns = lapply(requests, function(url) {
    print(i)
    i <<- i + 1
    ret = curl_fetch_memory(url)
    Sys.sleep(0.1)  # IP may be banned if requests are too intense
    ret
})

save(info, returns, file = "data/raw_data.RData")
