require(ggplot2)
qplot(x, y = NULL, ..., data, facets = NULL, margins = FALSE, geom = "auto", stat = list(NULL), position = list(NULL), xlim = c(NA, NA), ylim = c(NA, NA), log = "", main = NULL, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), asp = NA)


data(diamonds)
dsmall <- diamonds[sample(nrow(diamonds), 1000), ]
head(dsmall)

qplot(carat,price,data=dsmall,color=I("red"),shape=cut) # I()��ʾ�ֶ��趨ͼ������
qplot(log(carat),log(price),data=dsmall,alpha=I(1/10)) #alpha��ʾ͸����
qplot(carat,x*y*x,data=dsmall)

#��ά����
geom = "point" #ָ��x��yʱ Ĭ�����ɢ��ͼ "jitter"����һ��ɢ��ͼ���Ŷ���ͼ��
geom = "smooth" #ƽ������
geom = "boxplot" #����ͼ
geom = "path" �� geom = "line" #�����ݵ�֮���������

#������������
geom = "histogram" #ֱ��ͼ
geom = "freqploy" #����Ƶ�ʶ����
geom = "density" #�����ܶ�����
ָֻ��x��Ĭ�����ֱ��ͼ

#������ɢ����
geom = "bar"  #������״ͼ

qplot(carat,price,data=dsmall,geom=c("point","smooth"),span=0.5) #span����ƽ����
qplot(carat,price,data=dsmall,geom=c("point","smooth"),method="lm") #����ģ�����
qplot(carat,price,data=dsmall,geom=c("point","smooth"),method="lm",formula=y~ns(x,5)) #����ʽ��ϣ��ڶ���������ʾ���ɶȣ�Խ�����߲���Խ��

qplot(color,price/carat,data=dsmall,geom="jitter") #�Ŷ���ͼ

qplot(carat, data=dsmall,geom="histogram",binwidth=0.1) #binwidth�趨��࣬����ƽ����

qplot(carat, data=dsmall,geom="density",color=color) #��ɫ
qplot(carat, data=dsmall,geom="histogram",fill=color) #���


qplot(date,unemploy/pop, data=economics,geom="line")
qplot(date,unemploy, data=economics,geom="line")


year<- function(x) as.POSIXlt(x)$year+1900  #����ʱ��ת������,��1900����>as.POSIXlt("1899-01-02")$year  >1
qplot( unemploy/pop,uempmed, data=economics,geom=c("point","line"))
qplot( unemploy/pop,uempmed, data=economics,geom="path",color=year(date))

#����
qplot(carat, data=dsmall,facets= color ~ ., geom="histogram",binwidth=0.1,xlim=c(0,3)) #�������   .��ʾռλ����ָ��һ�л�һ��
qplot(carat,..density.., data=dsmall,facets= color ~ ., geom="histogram",binwidth=0.1,xlim=c(0,3))  #�ܶ�   


----------------------------------------------------------------------------------------------------
  head(mpg)
qplot(displ,hwy,data=mpg,color=cyl) #ͬ����ɫ��ǳ
p<- qplot(displ,hwy,data=mpg,color=factor(cyl)) #��ͬ��ɫ

#�鿴�ṹ
summary(p)
#����ͼƬ
save(p,file="plot.rdata")
load("plot.rdata")
ggsave("plot.png",width=5,height=5)

----------------------------------------------------------------------------------------------------
  require(ggplot2)
p<- ggplot(dsmall,aes(carat,price,color=cut))

p<- p+layer(geom="point") #����һ��ͼ��
# layer(geom,geom_params,stat,stat_params,data,mapping,position) layer�Ĳ���


geom_histogram(binwidth=2,fill="stellblue")

#��ݺ���
geom_XXX(mapping,data,...,stat,position)
stat_XXX(mapping,data,...,geom,position)
mapping:һ��ͼ������ӳ�䣬ͨ��aes()����


ggplot(msleep,aes(sleep_rem/sleep_total,awake))+geom_point() #�ȼ���
qplot(sleep_rem/sleep_total,awake,data=msleep)

qplot(sleep_rem/sleep_total,awake,data=msleep)+geom_smooth() #�ȼ���
qplot(sleep_rem/sleep_total,awake,data=msleep,geom=c("point","smooth")) #Ҳ�ȼ���
p<- ggplot(msleep,aes(sleep_rem/sleep_total,awake))+geom_point()+geom_smooth()

summary(p) #������ͼ�Σ��鿴ͼ�ζ���ṹ

#��ͼ�㱣��Ϊ���������ʹ��
bestfit<-geom_smooth(method="lm",se=F,color=alpha("steelblue",0.5),size=2)
qplot(sleep_rem,sleep_total,data=msleep)+bestfit
qplot(awake,brainwt,data=msleep,log="y")+bestfit
qplot(bodywt,brainwt,data=msleep,log="xy")+bestfit

p<-ggplot(mtcars,aes(mpg,wt,color=cyl))+geom_point()
mtcarss<-transform(mtcars,mpg=mpg^2)
p %+% mtcarss #�������ݼ� �� %+%



p<-ggplot(mtcars,aes(mpg,wt))
p+geom_point()
p+geom_point(aes(color=factor(cyl))) #���� �൱��aes(mpg,wt,color=cyl)
p+geom_point(aes(y=disp)) #�޸� �൱��aes(mpg,disp)

#�趨��ӳ�������
p+geom_point(color="darkblue") #�������趨��һ���ض�ֵ
p+geom_point(aes(color="darkblue")) #ֻ�Ǵ�����һ��"darkblue"�ı�����Ȼ��colorӳ�䵽���������,�����������ɢ�͵ģ���ɫ���ϵȼ�����ɫ������Ϊֻ��һ��ֵ���������Һ�ɫ
qplot(mpg,wt,data=mtcars,color=I("darkblue")) #qplot������I()��ʵ��ӳ��

require(nlme)
head(Oxboys)
#����
p<-ggplot(Oxboys,aes(age,height,group=Subject))+geom_line() #ָ���˷���
ggplot(Oxboys,aes(age,height))+geom_line() #δָ������ �൱��group=1

#��ͬͼ���ϵĲ�ͬ����
p+geom_smooth(aes(group=Subject),method="lm",se=F) #��ÿ��������һ���⻬����
p+geom_smooth(aes(group=1),method="lm",se=F) #���������һ���⻬����


boysbox<- ggplot(Oxboys,aes(Occasion,height))+geom_boxplot()
boysbox+geom_line(aes(group=Subject),color="#3366FF")



ggplot(dsmall,aes(carat))+geom_histogram(aes(y=..density..),binwidth=0.1)
#���ɱ���������Ҫ��.. ..Χ��������ֹ���ɵı�����ԭʼ��������
#��ͬ��
qplot(carat,..density..,data=dsmall,geom="histogram",binwidth=0.1)
#density ��ʾÿ���۲�ֵ���ܶȣ�ռ����İٷֱ�/�����

#λ�õ���
p <- ggplot(data=mpg,aes(x=class,fill=factor(year)))
p + geom_bar(position='dodge')
p + geom_bar(position='stack')
p + geom_bar(position='fill')
p + geom_bar(position='identity',alpha=0.3)


#��ϼ��ζ����ͳ�Ʊ任
d<- ggplot(diamonds,aes(carat))+xlim(0,3)
d + stat_bin(aes(ymax=..count..),binwidth=0.1,geom="area")
d + stat_bin(aes(size=..density..),binwidth=0.1,geom="point",positin="identity")

---------------------------------------------------------------------------------------
  df<- data.frame(x=c(3,1,5),y=c(2,4,6),label=c("a","b","c"))

require(gridExtra)
p<- ggplot(df,aes(x,y))+xlab(NULL)+ylab(NULL)
p1<- p + geom_point() +labs(title="geom_point")  #ɢ��ͼ#
p2<- p + geom_bar(stat="identity")+labs(title="geom_bar(stat=\"identity\")") #����ͼ
p3<- p + geom_line() + labs(title="geom_line") #����ͼ
p4<- p + geom_area() + labs(title="geom_area") #���ͼ
p5<- p + geom_path() + labs(title="geom_path") #·��ͼ
p6<- p + geom_text(aes(label=label)) + labs(title="geom_text") # ����ǩ��ɢ��ͼ
p7<- p + geom_tile() + labs(title="geom_tile") #ɫ��ͼ/ˮƽͼ
p8<- p + geom_polygon() + labs(title="geom_polygon") #�����ͼ
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,ncol=2) #������չʾ

head(diamonds)
depth_dist <- ggplot(diamonds,aes(depth))+xlim(58,68)
depth_dist + geom_histogram(aes(y=..density..),binwidth=0.1)+facet_grid(cut ~ .) #��cut������ƶ��ֱ��ͼ
depth_dist + geom_histogram(aes(fill=cut),binwidth=0.1,position="fill") #Ƶ�ʶ����ͼ
depth_dist + geom_freqpoly(aes(y=..density..,color=cut),binwidth=0.1) #�����ܶ�ͼ


qplot(cut,depth,data=diamonds,geom="boxplot")
qplot(carat,depth,data=diamonds,geom="boxplot"),group=round_any(carat,0.1,floor),xlim=c(0.3))

qplot(class,cty,data=mpg,geom="jitter")
qplot(class,drv,data=mpg,geom="jitter")

qplot(depth,data=diamonds,geom="density",xlim=c(54,70))
qplot(depth,data=diamonds,geom="density",xlim=c(54,70),fill=cut,alpha=I(0.2))


#�����ڸǻ�������
df<- data.frame(x=rnorm(2000),y=rnorm(2000))
norm<-ggplot(df,aes(x,y))
norm + geom_point()
norm + geom_point(shape =1 ) #�пյĵ�
norm + geom_point(shape = ".") #��Ĵ�СΪ���ؼ�

norm + geom_point(color="black",alpha=1/30)

td<-ggplot(diamonds,aes(table,depth))+xlim(50,70)+ylim(50,70)
td + geom_point() #��������
td + geom_jitter() #ʹ��Ĭ���Ŷ�����
jit <- position_jitter(width=0.5)
td+ geom_jitter(position= jit) #�����Ŷ�����Ϊ0.5
td+ geom_jitter(position= jit,color="black",alpha=1/10) #�����Ŷ�����Ϊ0.5��͸����Ϊ1/10
td+ geom_jitter(position= jit,color="black",alpha=1/50)
td+ geom_jitter(position= jit,color="black",alpha=1/200)

----------------------------------------------------------------------------------------
  #����ͼ��ע��
  unemp<- qplot(date,unemploy,data=economics,geom="line",xxlab="",ylab="No. unemployed(1000s)")

head(presidential)

presidential<- presidential[-(1:3),]   
yrng <- range(economics$unemploy)
xrng <- range(economics$date)
unemp+ geom_vline(aes(xintercept= as.numeric(start)),data=presidential) #vline ���Ӵ�ֱ�� xintercept��ʾȡx��ֵ,ȡ��presidential���ݼ�����ݣ�������ȡ������ͳ���������

library(scales)
unemp + geom_rect(aes(NULL,NULL,xmin=start,xmax=end,fill=party),ymin=yrng[1],ymax=yrng[2],data=presidential,alpha=0.2)+scale_fill_manual(values=c("blue","red")) #ǿ��ͼ���и���Ȥ�ľ�������

last_plot()+geom_text(aes(x=start,y=yrng[2],label=name),data=presidential,size=5,hjust=0,vjust=0)#last()�ǵ��������һ��ͼ; geom_text���������������ı�ǩ;  vjust��hjust�Ƕ�ͼ������labelλ�õ����� ; ˮƽ(Horizontal)��ֱ(vertical); size�����ı��Ĵ�С

caption <- paste(strwrap("Unemploymen rates in the US have varied alot over the years",40),collapse="\n") #strwrap()��һ���ַ�������һ����������� 40������ÿ�е��ַ���
unemp+geom_text(aes(x,y,label=caption),data=data.frame(x=xrng[2],y=yrng[2]),hjust=1,vjust=1,,size=5)

highest<- subset(economics,unemploy==max(unemploy))
lowest<- subset(economics,unemploy==min(unemploy))
unemp+geom_point(data=highest,size=3,color="red",alpha=0.5)+geom_point(data=lowest,size=3,color="blue",alpha=0.8) #��ע���ֵ����Сֵ


# geom_text ����������������Ϊ�����ӱ�ǩ
# geom_vline,geom_hline:��ͼƬ���Ӵ�ֱ�߻�ˮƽ��
# geom_abline:��ͼ����������б�ʺͽؾ��ֱ��
# geom_rect:ǿ��ͼ���и���Ȥ�ľ���������xmin,xmax,ymin,ymax����ͼ������
# geom_line,geom_path,geom_segemt:����ֱ�� ����һ��arrow�������������������Ϸ���һ����ͷ��arrow()������angle,length,ends��type��������


---------------------------------------------------------------------------------------------------------
  ###��� λ�ñ�� ��ɫ��� �ֶ���� ͬһ���ͱ��
  p <- qplot(sleep_total,sleep_cycle,data=msleep,color=vore)
p + scale_color_hue() #��ʾ����Ĭ�ϱ��
p + scale_color_hue("What does\nit eat?",  
                    breaks = c("herbi","carni","omni",NA),
                    labels = c("plants","meat","both","don't know")) #�޸�Ĭ�ϱ�ȵĲ���������ı���ͼ�������
p + scale_color_brewer(palette = "Set1") #ʹ��һ�ֲ�ͬ�ı��

##ͨ�ò���
# name:�����������ͼ���ϵı�ǩ����\n����
p <- qplot(cty,hwy,data=mpg,color=displ)
p + scale_x_continuous("City mpg")
p + xlab("City mpg")
p + ylab("Highway mpg")
p + labs(x="City mpg", y="Highway",color="Displacement")
p + xlab(expression(frac(miles,gallon))) #frac()��ʾ������

#limits:�̶���ȵĶ�����Ӱ����ʾ��ͼ���ϵ�Ԫ��
#breaks:������ʾ���������ͼ���ϵ�ֵ
#labels:ָ����Ӧ�ڶϵ㴦��ʾ��ǩ����������labels������ͬʱָ��breaks

p <- qplot(cyl,wt,data=mtcars)
p + scale_x_continuous(breaks=c(5.5,6.5)) #�������ע�仯���ϵ�λ�ñ���
p + scale_x_continuous(limits=c(5.5,6.5)) #ѡȡ�����ݱ仯�����ݱ�������
p <- qplot(wt,cyl,data=mtcars,color=cyl)
p + scale_color_gradient(breaks=c(5.5,6.5)) #ͼ���ı�ע�仯���ε�λ�ñ���
p + scale_color_gradient(limits=c(5.5,6.5)) #ѡȡ�����ݱ仯�����ݱ�������

#formatter:���δָ���κα�ǩ������ÿ���ϵ���Զ����ø�ʽˢ����ʽ�����ɱ�ǩ�������Ա�ȣ�comma,percent,dollar,scientific;��ɢ�ͱ�ȣ� abbreviate

##���λ��
qplot(log10(carat),log10(price),data=diamonds) #�����ݽ��ж����任
qplot(carat,price,data=diamonds)+scale_x_log10()+scale_y_log10() #�Ա�Ƚ��ж����任
#����ͼͼ����ͬ���������ǩ��ͬ

plot <- qplot(date,psavert,data=economics,geom="line")+
  ylab("Personal savings rate")+
  geom_hline(yintercept=0, color="grey50") #hline��ʾ���� yinterceptȡy��ֵ
plot + scale_x_date(breaks=date_breaks("10 years")) #��10��ֶ�
plot + scale_x_date(limits= as.Date(c("2004-01-01","2005-01-01")),labels=date_format("%Y-%m-%d"))


##��ɫ���
point <- qplot(brainwt,bodywt,data=msleep,log="xy",color=vore)
area <- qplot(log10(brainwt),data=msleep,fill=vore,binwidth=1)
point + scale_color_brewer(palette="Set1")
point + scale_color_brewer(palette="Set2")
point + scale_color_brewer(palette="Pastel1")

area + scale_fill_brewer(palette="Set1")
area + scale_fill_brewer(palette="Set2")
area + scale_fill_brewer(palette="Pastel1")


#�ֶ���ɢ���
plot <- qplot(brainwt,bodywt,data=msleep,log="xy")
plot + aes(color=vore)+scale_color_manual(values=c("red","orange","yellow","green","blue")) #Ϊ����vore��ÿ��ֵ��һ����ɫ
colors <- c(carni="red","NA"="orange",insecti="yellow",herbi="green",omni="blue") #һһ��Ӧ��ɫ
plot + aes(color=vore)+scale_color_manual(values=colors)
plot + aes(shape=vore)+scale_shape_manual(values=c(1,2,6,0,23)) #������״

huron <- data.frame(year= 1875:1972,level=LakeHuron)
ggplot(huron,aes(year))+
  geom_line(aes(y=level-5),color="blue")+
  geom_line(aes(y=level+5),color="red") #����֪���������ߴ���ʲô

ggplot(huron,aes(year))+
  geom_line(aes(y=level-5,color="below"))+
  geom_line(aes(y=level+5,color="above"))+
  scale_color_manual("Direction",values=c("below"="blue","above"="red")) #������һ��ͼ��

-----------------------------------------------------------------------------------------------------------------------
  ###��λ
  ##����
  #������ facet_grid ��װ�� facet_wrap
  #��qplot()�У�2ά����ʹ��facet_grid��һά����ʹ��facet_wrap
  
  #�������
  mpg2 <- subset(mpg,cyl!=5 & drv %in% c("4","f"))
qplot(cty,hwy,data=mpg2)+facet_null()
qplot(cty,hwy,data=mpg2)+facet_grid(.~cyl) #һ�ж���" . ~ a "
qplot(cty,data=mpg2,geom="histogram",binwidth=2)+facet_grid( cyl~. ) #һ�ж��� "a ~ ."
qplot(cty,hwy,data=mpg2)+facet_grid(drv~cyl) #���ж��� "a ~ b"

#�߼�ͼ
p <- qplot(displ,hwy,data=mpg2)+geom_smooth(method="lm",se=F)
p + facet_grid(drv~cyl)
p + facet_grid(drv~cyl,margin=T) #margin=T��ʾ���б߼�ͼ��margins=c("sex","age")��ʾָ���ı������ƣ�grand_row��grand_col��ʾ���е��л��еı߼�ͼ

qplot(displ,hwy,data=mpg2)+geom_smooth(aes(color=drv),method="lm",se=F)+facet_grid(drv~cyl,margin=T)

#��װ����
movies$decade <- round_any(movies$year,10,floor)
qplot(rating,..density..,data=subset(movies,decade >1980),geom="histogram",binwidth=0.5)+facet_wrap(~deacde,ncol=6)

#��ȿ���
#��Ӧ���ַ��棬����ͨ����������scales����������λ�ñ������ͬ���̶������������仯�����ɣ�
sacles="fixed":x��y�ı������������ж���ͬscakes="free":x��y�ı����ÿ����嶼���Ա仯
scakes="free_x":x�ı�ȿ��Ա䣬y�̶�
scakes="free_y":y�ı�ȿɱ䣬x�̶�

p <- qplot(cty,hwy,data=mpg)
p + facet_wrap( ~ cyl)
p + facet_wrap( ~cyl,scales="free") #x��y���ɱ�

library(reshape2)
em<-melt(economics,id="date")
qplot(date,value,data=em,geom="line",group =variable)+facet_grid(variable ~.,scale="free_y")


mpg3 <- within(mpg2,{
  model <- reorder(model,cty)
  manufacturer <- reorder(manufacturer,-cty)
})
models <- qplot(cty,model,data=mpg3)
models
models + facet_grid(manufacturer ~ . ,scales="free",space="free")+theme(strip.text.y = element_text()) #��������������ת�����ǩ

#���������
qplot(color,data=diamonds,geom="bar",fill=cut,position="dodge") #λ�õ�����Position adjustments�������ͬһͼ����Ԫ�ص�λ�ý���΢���ķ������������������ã��ֱ���stack��dodge��fill��identity��jitter��
qplot(cut,data=diamonds,geom="bar",fill=cut)+facet_grid(.~color)+theme(axis.text.x=element_text(angle=90,hjust=1,size=8,color="grey50"))

#�����ͱ���
#�������ͱ������з��棬���Ƚ����Ϊ��ɢ��
#�����ݷ�Ϊn��������ͬ�Ĳ��֣���cut_interval(x,n=10),���ƻ�����Ŀ������cut_interval(x,length=1)����ÿ�����ֵĳ���
#�����ݻ���Ϊn������ͬ���ݵ�Ĳ��֣�cut_number(x,n=10)

mpg2 <- subset(mpg,cyl!=5 & drv %in% c("4","f"))
mpg2$disp_ww <- cut_interval(mpg2$displ,length=1) #��峤��Ϊ1
plot <- qplot(cty,hwy,data=mpg2)+labs(x=NULL,y=NULL)
plot + facet_wrap( ~ disp_ww,nrow=1)
mpg2$disp_ww <- cut_interval(mpg2$displ,n=6) #ÿ�����ȳ�
plot + facet_wrap( ~ disp_ww,nrow=1)
mpg2$disp_ww <- cut_number(mpg2$displ,n=6)  #ÿ����������Ŀ�ĵ���ͬ
plot + facet_wrap( ~ disp_ww,nrow=1)


#�ѿ�������ϵ���÷�Χ
p <- qplot(disp,wt,data=mtcars)+geom_smooth() #�������ݼ�
p + scale_x_continuous(limits=c(325,500)) #x�ı�ȷ�������Ϊ��325,500��,������Χ�����ݻ�ɾ��
p + coord_cartesian(xlim=c(325,500))#����ϵx�᷶ΧΪ��325,500����ʹ�õ�����������

d <- ggplot(diamonds,aes(carat,price))+stat_bin2d(bins=25,color="grey70")+theme(legend.position="none")
d + scale_x_continuous(limits=c(0,2))
d + coord_cartesian(xlim=c(0,2))


#�����ᷭת
qplot(displ,cty,data=mpg)+geom_smooth()
qplot(cty,displ,data=mpg)+geom_smooth()
qplot(cty,displ,data=mpg)+geom_smooth()+coord_flip()

#������任
qplot(carat,price,data=diamonds,log="xy")+geom_smooth(method="lm") #���˶����任
library(scales)
last_plot()+coord_trans(x=exp_trans(10),y=exp_trans(10)) #��Ȼ�ԭ


#�ǵѿ�������ϵ
#�ѵ���״ͼ
pie <- ggplot(mtcars,aes(x=factor(1),fill=factor(cyl)))+geom_bar(width=1)
#��ͼ
pie + coord_polar(theta="y")
#ţ��ͼ
pie + coord_polar()

----------------------------------------------------------------------------------------------------------
  ##����
  #��������
  theme_gray()ʹ�õ���ɫ�����Ͱ�ɫ������
theme_bw()ʹ�ð�ɫ���������ɫ������
���߶���base_size�����ƻ�������Ĵ�С

#ȫ��������
theme_set(theme_grey())��theme_set(theme_bw())
#�ֲ������� ֻ�ı䵥��ͼ�ε�����
qplot()+ theme_grey()

hgram <- qplot(rating,data=movies,binwidth=1)
hgram
previous_theme <- theme_set(theme_bw())
hgram
hgram + previous_theme
theme_set(previous_theme) #�����Դ洢��ʼ����

#����Ԫ�غ�Ԫ�غ���
# element_text()���Ʊ�ǩ�ͱ��⣬�ɿ��������family,face,color,size,hjust,vjust,angle,hineheight
hgramt <- hgram+labs(title="This is a histogram")
hgramt
hgramt + theme(plot.title=element_text(size=20))
hgramt + theme(plot.title=element_text(size=20,color="red"))
hgramt + theme(plot.title=element_text(size=20,hjust=0))
hgramt + theme(plot.title=element_text(size=20,face="bold"))
hgramt + theme(plot.title=element_text(size=20,angle=180))

# element_line()�����������߶Σ�����color,size,linetype
hgram + theme(panel.grid.major=element_line(color="red"))
hgram + theme(panel.grid.major=element_line(size=2))
hgram + theme(panel.grid.major=element_line(linetype="dotted"))
hgram + theme(axis.line=element_line())
hgram + theme(axis.line=element_line(color="red"))
hgram + theme(axis.line=element_line(size=0.5,linetype="dashed"))


# element_rect()������Ҫ������ʹ�õľ��Σ����Կ��������ɫ��fill)�ͱ߽��color,size,linetype
hgram + theme(plot.background=element_rect(fill="grey80",color=NA))
hgram + theme(plot.background=element_rect(size=2))
hgram + theme(plot.background=element_rect(color="red"))
hgram + theme(plot.background=element_rect())
hgram + theme(plot.background=element_rect(color=NA))
hgram + theme(plot.background=element_rect(linetype="dotted"))

# element_blank()��ʾ�����⣬����Ԫ�ز�������Ӧ�Ļ�ͼ�ؼ����ú�������ɾ�����ǲ�����Ȥ�Ļ�ͼԪ��

last_plot()+theme(panel.grid.minor=element_blank())
last_plot()+theme(panel.grid.major=element_blank())
last_plot()+theme(panel.background=element_blank())
last_plot()+theme(axis.title.x=element_blank(),axis.title.y=element_blank())
last_plot()+theme(axis.line=element_line())

#���ζ����ͳ�Ʊ任
# update_geom_defaults() update_stat_feaults
update_geom_defaults("point",aes(color="darkblue"))
qplot(mpg,wt,data=mtcars)
update_stat_defaults("bin",aes(y=..density..))
qplot(rating,data=movies,geom="histogram",binwidth=1)


#�洢���
qplot(mpg,wt,data=mtcars)
ggsave(file="output.png",width=5,height=5)


###���ݲ���
ddply(.data,.(variables),.fun,...)
#ȡ�Ӽ� subset()
require("plyr") require(ggplot2)
ddply(diamonds,.(color),subset,carat==min(carat))#ѡȡ����ɫ����С��
ddply(diamonds,.(color),subset,order(carat)<=2)#ѡ��С������
ddply(diamonds,.(color),subset,carat>quantile(carat,0.99))#ѡȡÿ�������СΪǰ1%����ʯ
ddply(diamonds,.(color),subset,price>mean(price))#ѡ�����б���ƽ��ֵ�����ʯ

#�������ݱ任 transform()
ddply(diamonds,.(color),transform,price=scale(price))#��ÿ����ɫ������ʯ�ļ۸��׼����ʹ���ֵΪ0������Ϊ1
ddply(diamonds,.(color),transform,price=price-mean(price))# ��ȥ���ֵ

#��ͼģ��
gradient_rb <- scale_color_gradient(low="red",high="blue")
qplot(cty,hwy,data=mpg,color=displ)+gradient_rb
qplot(bodywt,brainwt,data=msleep,color=awake,log="xy")+gradient_rb




























































