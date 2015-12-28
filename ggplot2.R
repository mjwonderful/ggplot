require(ggplot2)
qplot(x, y = NULL, ..., data, facets = NULL, margins = FALSE, geom = "auto", stat = list(NULL), position = list(NULL), xlim = c(NA, NA), ylim = c(NA, NA), log = "", main = NULL, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), asp = NA)


data(diamonds)
dsmall <- diamonds[sample(nrow(diamonds), 1000), ]
head(dsmall)

qplot(carat,price,data=dsmall,color=I("red"),shape=cut) # I()表示手动设定图形属性
qplot(log(carat),log(price),data=dsmall,alpha=I(1/10)) #alpha表示透明度
qplot(carat,x*y*x,data=dsmall)

#二维变量
geom = "point" #指定x、y时 默认输出散点图 "jitter"是另一种散点图（扰动点图）
geom = "smooth" #平滑曲线
geom = "boxplot" #箱线图
geom = "path" 和 geom = "line" #在数据点之间绘制连线

#对于连续变量
geom = "histogram" #直方图
geom = "freqploy" #绘制频率多边形
geom = "density" #绘制密度曲线
只指定x，默认输出直方图

#对于离散变量
geom = "bar"  #绘制柱状图

qplot(carat,price,data=dsmall,geom=c("point","smooth"),span=0.5) #span控制平滑度
qplot(carat,price,data=dsmall,geom=c("point","smooth"),method="lm") #线性模型拟合
qplot(carat,price,data=dsmall,geom=c("point","smooth"),method="lm",formula=y~ns(x,5)) #多项式拟合，第二个参数表示自由度，越大曲线波动越大

qplot(color,price/carat,data=dsmall,geom="jitter") #扰动点图

qplot(carat, data=dsmall,geom="histogram",binwidth=0.1) #binwidth设定组距，控制平滑度

qplot(carat, data=dsmall,geom="density",color=color) #颜色
qplot(carat, data=dsmall,geom="histogram",fill=color) #填充


qplot(date,unemploy/pop, data=economics,geom="line")
qplot(date,unemploy, data=economics,geom="line")


year<- function(x) as.POSIXlt(x)$year+1900  #日期时间转换功能,从1900年起，>as.POSIXlt("1899-01-02")$year  >1
qplot( unemploy/pop,uempmed, data=economics,geom=c("point","line"))
qplot( unemploy/pop,uempmed, data=economics,geom="path",color=year(date))

#分面
qplot(carat, data=dsmall,facets= color ~ ., geom="histogram",binwidth=0.1,xlim=c(0,3)) #分组计数   .表示占位符，指定一行或一列
qplot(carat,..density.., data=dsmall,facets= color ~ ., geom="histogram",binwidth=0.1,xlim=c(0,3))  #密度   


----------------------------------------------------------------------------------------------------
  head(mpg)
qplot(displ,hwy,data=mpg,color=cyl) #同种颜色深浅
p<- qplot(displ,hwy,data=mpg,color=factor(cyl)) #不同颜色

#查看结构
summary(p)
#保存图片
save(p,file="plot.rdata")
load("plot.rdata")
ggsave("plot.png",width=5,height=5)

----------------------------------------------------------------------------------------------------
  require(ggplot2)
p<- ggplot(dsmall,aes(carat,price,color=cut))

p<- p+layer(geom="point") #添加一个图层
# layer(geom,geom_params,stat,stat_params,data,mapping,position) layer的参数


geom_histogram(binwidth=2,fill="stellblue")

#快捷函数
geom_XXX(mapping,data,...,stat,position)
stat_XXX(mapping,data,...,geom,position)
mapping:一组图形属性映射，通过aes()控制


ggplot(msleep,aes(sleep_rem/sleep_total,awake))+geom_point() #等价于
qplot(sleep_rem/sleep_total,awake,data=msleep)

qplot(sleep_rem/sleep_total,awake,data=msleep)+geom_smooth() #等价于
qplot(sleep_rem/sleep_total,awake,data=msleep,geom=c("point","smooth")) #也等价于
p<- ggplot(msleep,aes(sleep_rem/sleep_total,awake))+geom_point()+geom_smooth()

summary(p) #不画出图形，查看图形对象结构

#将图层保存为变量，多次使用
bestfit<-geom_smooth(method="lm",se=F,color=alpha("steelblue",0.5),size=2)
qplot(sleep_rem,sleep_total,data=msleep)+bestfit
qplot(awake,brainwt,data=msleep,log="y")+bestfit
qplot(bodywt,brainwt,data=msleep,log="xy")+bestfit

p<-ggplot(mtcars,aes(mpg,wt,color=cyl))+geom_point()
mtcarss<-transform(mtcars,mpg=mpg^2)
p %+% mtcarss #更换数据集 用 %+%



p<-ggplot(mtcars,aes(mpg,wt))
p+geom_point()
p+geom_point(aes(color=factor(cyl))) #添加 相当于aes(mpg,wt,color=cyl)
p+geom_point(aes(y=disp)) #修改 相当于aes(mpg,disp)

#设定和映射的区别
p+geom_point(color="darkblue") #将参数设定成一个特定值
p+geom_point(aes(color="darkblue")) #只是创建了一个"darkblue"的变量，然后将color映射到这个变量上,这个变量是离散型的，用色轮上等间距的颜色，又因为只有一个值，所以是桃红色
qplot(mpg,wt,data=mtcars,color=I("darkblue")) #qplot可以用I()来实现映射

require(nlme)
head(Oxboys)
#分组
p<-ggplot(Oxboys,aes(age,height,group=Subject))+geom_line() #指定了分组
ggplot(Oxboys,aes(age,height))+geom_line() #未指定分组 相当于group=1

#不同图层上的不同分组
p+geom_smooth(aes(group=Subject),method="lm",se=F) #给每个都加了一条光滑线条
p+geom_smooth(aes(group=1),method="lm",se=F) #基于整体的一条光滑线条


boysbox<- ggplot(Oxboys,aes(Occasion,height))+geom_boxplot()
boysbox+geom_line(aes(group=Subject),color="#3366FF")



ggplot(dsmall,aes(carat))+geom_histogram(aes(y=..density..),binwidth=0.1)
#生成变量的名字要用.. ..围起来，防止生成的变量和原始变量混淆
#等同于
qplot(carat,..density..,data=dsmall,geom="histogram",binwidth=0.1)
#density 表示每个观测值的密度（占整体的百分比/组宽）

#位置调整
p <- ggplot(data=mpg,aes(x=class,fill=factor(year)))
p + geom_bar(position='dodge')
p + geom_bar(position='stack')
p + geom_bar(position='fill')
p + geom_bar(position='identity',alpha=0.3)


#结合几何对象和统计变换
d<- ggplot(diamonds,aes(carat))+xlim(0,3)
d + stat_bin(aes(ymax=..count..),binwidth=0.1,geom="area")
d + stat_bin(aes(size=..density..),binwidth=0.1,geom="point",positin="identity")

---------------------------------------------------------------------------------------
  df<- data.frame(x=c(3,1,5),y=c(2,4,6),label=c("a","b","c"))

require(gridExtra)
p<- ggplot(df,aes(x,y))+xlab(NULL)+ylab(NULL)
p1<- p + geom_point() +labs(title="geom_point")  #散点图#
p2<- p + geom_bar(stat="identity")+labs(title="geom_bar(stat=\"identity\")") #条形图
p3<- p + geom_line() + labs(title="geom_line") #线条图
p4<- p + geom_area() + labs(title="geom_area") #面积图
p5<- p + geom_path() + labs(title="geom_path") #路径图
p6<- p + geom_text(aes(label=label)) + labs(title="geom_text") # 含标签的散点图
p7<- p + geom_tile() + labs(title="geom_tile") #色深图/水平图
p8<- p + geom_polygon() + labs(title="geom_polygon") #多边形图
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,ncol=2) #在两列展示

head(diamonds)
depth_dist <- ggplot(diamonds,aes(depth))+xlim(58,68)
depth_dist + geom_histogram(aes(y=..density..),binwidth=0.1)+facet_grid(cut ~ .) #按cut分组绘制多个直方图
depth_dist + geom_histogram(aes(fill=cut),binwidth=0.1,position="fill") #频率多边形图
depth_dist + geom_freqpoly(aes(y=..density..,color=cut),binwidth=0.1) #条件密度图


qplot(cut,depth,data=diamonds,geom="boxplot")
qplot(carat,depth,data=diamonds,geom="boxplot"),group=round_any(carat,0.1,floor),xlim=c(0.3))

qplot(class,cty,data=mpg,geom="jitter")
qplot(class,drv,data=mpg,geom="jitter")

qplot(depth,data=diamonds,geom="density",xlim=c(54,70))
qplot(depth,data=diamonds,geom="density",xlim=c(54,70),fill=cut,alpha=I(0.2))


#处理遮盖绘制问题
df<- data.frame(x=rnorm(2000),y=rnorm(2000))
norm<-ggplot(df,aes(x,y))
norm + geom_point()
norm + geom_point(shape =1 ) #中空的点
norm + geom_point(shape = ".") #点的大小为像素级

norm + geom_point(color="black",alpha=1/30)

td<-ggplot(diamonds,aes(table,depth))+xlim(50,70)+ylim(50,70)
td + geom_point() #不做处理
td + geom_jitter() #使用默认扰动参数
jit <- position_jitter(width=0.5)
td+ geom_jitter(position= jit) #横向扰动参数为0.5
td+ geom_jitter(position= jit,color="black",alpha=1/10) #横向扰动参数为0.5且透明度为1/10
td+ geom_jitter(position= jit,color="black",alpha=1/50)
td+ geom_jitter(position= jit,color="black",alpha=1/200)

----------------------------------------------------------------------------------------
  #添加图形注解
  unemp<- qplot(date,unemploy,data=economics,geom="line",xxlab="",ylab="No. unemployed(1000s)")

head(presidential)

presidential<- presidential[-(1:3),]   
yrng <- range(economics$unemploy)
xrng <- range(economics$date)
unemp+ geom_vline(aes(xintercept= as.numeric(start)),data=presidential) #vline 添加垂直线 xintercept表示取x的值,取了presidential数据集的年份，用来获取各个总统的任期年份

library(scales)
unemp + geom_rect(aes(NULL,NULL,xmin=start,xmax=end,fill=party),ymin=yrng[1],ymax=yrng[2],data=presidential,alpha=0.2)+scale_fill_manual(values=c("blue","red")) #强调图形中感兴趣的矩形区域

last_plot()+geom_text(aes(x=start,y=yrng[2],label=name),data=presidential,size=5,hjust=0,vjust=0)#last()是调用最近的一副图; geom_text添加文字叙述或点的标签;  vjust和hjust是对图中文字label位置的设置 ; 水平(Horizontal)垂直(vertical); size控制文本的大小

caption <- paste(strwrap("Unemploymen rates in the US have varied alot over the years",40),collapse="\n") #strwrap()把一个字符串当成一个段落的文字 40控制了每行的字符数
unemp+geom_text(aes(x,y,label=caption),data=data.frame(x=xrng[2],y=yrng[2]),hjust=1,vjust=1,,size=5)

highest<- subset(economics,unemploy==max(unemploy))
lowest<- subset(economics,unemploy==min(unemploy))
unemp+geom_point(data=highest,size=3,color="red",alpha=0.5)+geom_point(data=lowest,size=3,color="blue",alpha=0.8) #标注最大值、最小值


# geom_text 可添加文字叙述或为点添加标签
# geom_vline,geom_hline:向图片添加垂直线或水平线
# geom_abline:向图形添加任意斜率和截距的直线
# geom_rect:强调图形中感兴趣的矩形区域，有xmin,xmax,ymin,ymax几种图形属性
# geom_line,geom_path,geom_segemt:添加直线 都有一个arrow参数，可以用来在线上放置一个箭头，arrow()函数有angle,length,ends及type几个参数


---------------------------------------------------------------------------------------------------------
  ###标度 位置标度 颜色标度 手动标度 同一类型标度
  p <- qplot(sleep_total,sleep_cycle,data=msleep,color=vore)
p + scale_color_hue() #显示添加默认标度
p + scale_color_hue("What does\nit eat?",  
                    breaks = c("herbi","carni","omni",NA),
                    labels = c("plants","meat","both","don't know")) #修改默认标度的参数，这里改变了图例的外观
p + scale_color_brewer(palette = "Set1") #使用一种不同的标度

##通用参数
# name:设置坐标轴或图例上的标签，用\n换行
p <- qplot(cty,hwy,data=mpg,color=displ)
p + scale_x_continuous("City mpg")
p + xlab("City mpg")
p + ylab("Highway mpg")
p + labs(x="City mpg", y="Highway",color="Displacement")
p + xlab(expression(frac(miles,gallon))) #frac()表示分数线

#limits:固定标度的定义域，影响显示在图形上的元素
#breaks:控制显示在坐标轴或图例上的值
#labels:指定了应在断点处显示标签，若设置了labels，必须同时指定breaks

p <- qplot(cyl,wt,data=mtcars)
p + scale_x_continuous(breaks=c(5.5,6.5)) #坐标轴标注变化，断的位置变了
p + scale_x_continuous(limits=c(5.5,6.5)) #选取的数据变化，数据被限制了
p <- qplot(wt,cyl,data=mtcars,color=cyl)
p + scale_color_gradient(breaks=c(5.5,6.5)) #图例的标注变化，段的位置变了
p + scale_color_gradient(limits=c(5.5,6.5)) #选取的数据变化，数据被限制了

#formatter:如果未指定任何标签，则在每个断电出自动调用格式刷来格式化生成标签。连续性标度，comma,percent,dollar,scientific;离散型标度， abbreviate

##标度位置
qplot(log10(carat),log10(price),data=diamonds) #对数据进行对数变换
qplot(carat,price,data=diamonds)+scale_x_log10()+scale_y_log10() #对标度进行对数变换
#两幅图图形相同，坐标轴标签不同

plot <- qplot(date,psavert,data=economics,geom="line")+
  ylab("Personal savings rate")+
  geom_hline(yintercept=0, color="grey50") #hline表示横线 yintercept取y的值
plot + scale_x_date(breaks=date_breaks("10 years")) #按10年分段
plot + scale_x_date(limits= as.Date(c("2004-01-01","2005-01-01")),labels=date_format("%Y-%m-%d"))


##颜色标度
point <- qplot(brainwt,bodywt,data=msleep,log="xy",color=vore)
area <- qplot(log10(brainwt),data=msleep,fill=vore,binwidth=1)
point + scale_color_brewer(palette="Set1")
point + scale_color_brewer(palette="Set2")
point + scale_color_brewer(palette="Pastel1")

area + scale_fill_brewer(palette="Set1")
area + scale_fill_brewer(palette="Set2")
area + scale_fill_brewer(palette="Pastel1")


#手动离散标度
plot <- qplot(brainwt,bodywt,data=msleep,log="xy")
plot + aes(color=vore)+scale_color_manual(values=c("red","orange","yellow","green","blue")) #为变量vore的每个值赋一个颜色
colors <- c(carni="red","NA"="orange",insecti="yellow",herbi="green",omni="blue") #一一对应颜色
plot + aes(color=vore)+scale_color_manual(values=colors)
plot + aes(shape=vore)+scale_shape_manual(values=c(1,2,6,0,23)) #设置形状

huron <- data.frame(year= 1875:1972,level=LakeHuron)
ggplot(huron,aes(year))+
  geom_line(aes(y=level-5),color="blue")+
  geom_line(aes(y=level+5),color="red") #并不知道这两条线代表什么

ggplot(huron,aes(year))+
  geom_line(aes(y=level-5,color="below"))+
  geom_line(aes(y=level+5,color="above"))+
  scale_color_manual("Direction",values=c("below"="blue","above"="red")) #添加了一个图例

-----------------------------------------------------------------------------------------------------------------------
  ###定位
  ##分面
  #网格型 facet_grid 封装型 facet_wrap
  #在qplot()中，2维分面使用facet_grid，一维分面使用facet_wrap
  
  #网格分面
  mpg2 <- subset(mpg,cyl!=5 & drv %in% c("4","f"))
qplot(cty,hwy,data=mpg2)+facet_null()
qplot(cty,hwy,data=mpg2)+facet_grid(.~cyl) #一列多行" . ~ a "
qplot(cty,data=mpg2,geom="histogram",binwidth=2)+facet_grid( cyl~. ) #一列多行 "a ~ ."
qplot(cty,hwy,data=mpg2)+facet_grid(drv~cyl) #多行多列 "a ~ b"

#边际图
p <- qplot(displ,hwy,data=mpg2)+geom_smooth(method="lm",se=F)
p + facet_grid(drv~cyl)
p + facet_grid(drv~cyl,margin=T) #margin=T表示所有边际图，margins=c("sex","age")表示指定的变量名称，grand_row或grand_col表示所有的行或列的边际图

qplot(displ,hwy,data=mpg2)+geom_smooth(aes(color=drv),method="lm",se=F)+facet_grid(drv~cyl,margin=T)

#封装分面
movies$decade <- round_any(movies$year,10,floor)
qplot(rating,..density..,data=subset(movies,decade >1980),geom="histogram",binwidth=0.5)+facet_wrap(~deacde,ncol=6)

#标度控制
#对应两种分面，可以通过调整参数scales来控制面板的位置标度是相同（固定）还是允许变化（自由）
sacles="fixed":x和y的标度在所有面板中都相同scakes="free":x和y的标度在每个面板都可以变化
scakes="free_x":x的标度可以变，y固定
scakes="free_y":y的标度可变，x固定

p <- qplot(cty,hwy,data=mpg)
p + facet_wrap( ~ cyl)
p + facet_wrap( ~cyl,scales="free") #x、y均可变

library(reshape2)
em<-melt(economics,id="date")
qplot(date,value,data=em,geom="line",group =variable)+facet_grid(variable ~.,scale="free_y")


mpg3 <- within(mpg2,{
  model <- reorder(model,cty)
  manufacturer <- reorder(manufacturer,-cty)
})
models <- qplot(cty,model,data=mpg3)
models
models + facet_grid(manufacturer ~ . ,scales="free",space="free")+theme(strip.text.y = element_text()) #主题设置用来旋转分面标签

#并列与分面
qplot(color,data=diamonds,geom="bar",fill=cut,position="dodge") #位置调整（Position adjustments）是针对同一图层内元素的位置进行微调的方法。它包括五种设置，分别是stack、dodge、fill、identity、jitter。
qplot(cut,data=diamonds,geom="bar",fill=cut)+facet_grid(.~color)+theme(axis.text.x=element_text(angle=90,hjust=1,size=8,color="grey50"))

#连续型变量
#对连续型变量进行分面，需先将其变为离散型
#将数据分为n个长度相同的部分，用cut_interval(x,n=10),控制划分数目，或用cut_interval(x,length=1)控制每个部分的长度
#将数据划分为n个有相同数据点的部分，cut_number(x,n=10)

mpg2 <- subset(mpg,cyl!=5 & drv %in% c("4","f"))
mpg2$disp_ww <- cut_interval(mpg2$displ,length=1) #面板长度为1
plot <- qplot(cty,hwy,data=mpg2)+labs(x=NULL,y=NULL)
plot + facet_wrap( ~ disp_ww,nrow=1)
mpg2$disp_ww <- cut_interval(mpg2$displ,n=6) #每个面板等长
plot + facet_wrap( ~ disp_ww,nrow=1)
mpg2$disp_ww <- cut_number(mpg2$displ,n=6)  #每个面板包含数目的点相同
plot + facet_wrap( ~ disp_ww,nrow=1)


#笛卡尔坐标系设置范围
p <- qplot(disp,wt,data=mtcars)+geom_smooth() #完整数据集
p + scale_x_continuous(limits=c(325,500)) #x的标度范文设置为（325,500）,超出范围的数据会删除
p + coord_cartesian(xlim=c(325,500))#坐标系x轴范围为（325,500），使用的是所有数据

d <- ggplot(diamonds,aes(carat,price))+stat_bin2d(bins=25,color="grey70")+theme(legend.position="none")
d + scale_x_continuous(limits=c(0,2))
d + coord_cartesian(xlim=c(0,2))


#坐标轴翻转
qplot(displ,cty,data=mpg)+geom_smooth()
qplot(cty,displ,data=mpg)+geom_smooth()
qplot(cty,displ,data=mpg)+geom_smooth()+coord_flip()

#坐标轴变换
qplot(carat,price,data=diamonds,log="xy")+geom_smooth(method="lm") #做了对数变换
library(scales)
last_plot()+coord_trans(x=exp_trans(10),y=exp_trans(10)) #标度还原


#非笛卡尔坐标系
#堆叠条状图
pie <- ggplot(mtcars,aes(x=factor(1),fill=factor(cyl)))+geom_bar(width=1)
#饼图
pie + coord_polar(theta="y")
#牛眼图
pie + coord_polar()

----------------------------------------------------------------------------------------------------------
  ##主题
  #内置主题
  theme_gray()使用淡灰色背景和白色网格线
theme_bw()使用白色背景和深灰色网格线
两者都由base_size来控制基础字体的大小

#全局性设置
theme_set(theme_grey())或theme_set(theme_bw())
#局部性设置 只改变单个图形的主题
qplot()+ theme_grey()

hgram <- qplot(rating,data=movies,binwidth=1)
hgram
previous_theme <- theme_set(theme_bw())
hgram
hgram + previous_theme
theme_set(previous_theme) #永久性存储初始主体

#主题元素和元素函数
# element_text()绘制标签和标题，可控制字体的family,face,color,size,hjust,vjust,angle,hineheight
hgramt <- hgram+labs(title="This is a histogram")
hgramt
hgramt + theme(plot.title=element_text(size=20))
hgramt + theme(plot.title=element_text(size=20,color="red"))
hgramt + theme(plot.title=element_text(size=20,hjust=0))
hgramt + theme(plot.title=element_text(size=20,face="bold"))
hgramt + theme(plot.title=element_text(size=20,angle=180))

# element_line()绘制线条或线段，控制color,size,linetype
hgram + theme(panel.grid.major=element_line(color="red"))
hgram + theme(panel.grid.major=element_line(size=2))
hgram + theme(panel.grid.major=element_line(linetype="dotted"))
hgram + theme(axis.line=element_line())
hgram + theme(axis.line=element_line(color="red"))
hgram + theme(axis.line=element_line(size=0.5,linetype="dashed"))


# element_rect()绘制主要供背景使用的矩形，可以控制填充颜色（fill)和边界的color,size,linetype
hgram + theme(plot.background=element_rect(fill="grey80",color=NA))
hgram + theme(plot.background=element_rect(size=2))
hgram + theme(plot.background=element_rect(color="red"))
hgram + theme(plot.background=element_rect())
hgram + theme(plot.background=element_rect(color=NA))
hgram + theme(plot.background=element_rect(linetype="dotted"))

# element_blank()表示空主题，即对元素不分配相应的绘图控件。该函数可以删除我们不感兴趣的绘图元素

last_plot()+theme(panel.grid.minor=element_blank())
last_plot()+theme(panel.grid.major=element_blank())
last_plot()+theme(panel.background=element_blank())
last_plot()+theme(axis.title.x=element_blank(),axis.title.y=element_blank())
last_plot()+theme(axis.line=element_line())

#几何对象和统计变换
# update_geom_defaults() update_stat_feaults
update_geom_defaults("point",aes(color="darkblue"))
qplot(mpg,wt,data=mtcars)
update_stat_defaults("bin",aes(y=..density..))
qplot(rating,data=movies,geom="histogram",binwidth=1)


#存储输出
qplot(mpg,wt,data=mtcars)
ggsave(file="output.png",width=5,height=5)


###数据操作
ddply(.data,.(variables),.fun,...)
#取子集 subset()
require("plyr") require(ggplot2)
ddply(diamonds,.(color),subset,carat==min(carat))#选取各颜色里最小的
ddply(diamonds,.(color),subset,order(carat)<=2)#选最小的两颗
ddply(diamonds,.(color),subset,carat>quantile(carat,0.99))#选取每组里面大小为前1%的钻石
ddply(diamonds,.(color),subset,price>mean(price))#选出所有比组平均值大的钻石

#进行数据变换 transform()
ddply(diamonds,.(color),transform,price=scale(price))#把每个颜色组里钻石的价格标准化，使其均值为0，方差为1
ddply(diamonds,.(color),transform,price=price-mean(price))# 减去组均值

#绘图模板
gradient_rb <- scale_color_gradient(low="red",high="blue")
qplot(cty,hwy,data=mpg,color=displ)+gradient_rb
qplot(bodywt,brainwt,data=msleep,color=awake,log="xy")+gradient_rb





























































