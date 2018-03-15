library("ggplot2")
ggplot(data=mpg,mapping = aes(x=cty,y=hwy)) + # 数据层
  geom_point(size = I(4)) + # 几何层
  aes(colour = factor(mpg$year)) + # 美学层
  stat_smooth() # 拟合曲线和置信区间

# 只显示一条拟合曲线和一个置信区间
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + # 数据层
  geom_point(aes(colour = factor(mpg$year))) + # 几何层 & 美学层
  stat_smooth() # 拟合

# 将displ变量映射到散点大小
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point(aes(colour = factor(year),size = displ))+
  stat_smooth()

# 设置透明度
ggplot(data = mpg, aes(x = cty, y = hwy))+
  geom_point(aes(colour = factor(mpg$year), size = displ, alpha = 0.25))+
  stat_smooth()

# 细节设置
ggplot(data = mpg, aes(x = cty, y = hwy)) + 
  geom_point(aes(colour = class, size = displ), alpha = 0.5,
             position = "jitter") + 
  stat_smooth() +
  scale_size_continuous(range = c(4,10)) + 
  facet_wrap(~ year, ncol = 1) +
  ggtitle("汽车油耗与型号") +
  labs(y = "每加仑高速公路行驶距离",
       x = "每加仑城市公路行驶距离") + 
  guides(size = guide_legend(title = "排量"),
         colour = guide_legend(titile = "车型"), 
         override.aes = list(size =5))
  
# 不同图层的叠加
ggplot() +
  geom_point(aes(x = mpg$cty, y = mpg$hwy),colour = "red") +
  geom_point(aes(x = mpg$cty, y = mpg$displ),colour = "green")

# 扇形图
ggplot(mpg) + 
  geom_bar(width = 1,aes(x = factor(1), fill = mpg$class)) + 
  coord_polar(theta = "y")

# 玫瑰图
ggplot(mpg, aes(x = factor(mpg$class))) + 
  geom_bar(width = 0.7, aes(color = factor(mpg$class), 
                            fill = mpg$class)) + 
  coord_polar()
  