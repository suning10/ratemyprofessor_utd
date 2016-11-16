
library(RMySQL)
mydb<-dbConnect(MySQL(),user='root',password='bryant718',dbname='lang_qin',host='127.0.0.1')
query<-dbSendQuery(mydb,'select substring(name,1,instr(name,",")-1) as lastname,reverse(substring(reverse(dept_name),1,instr(reverse(dept_name),",")-1)) as department from dept;')
dat<-fetch(query,n=-1)
dept<-as.data.frame(dat)
query_1<-dbSendQuery(mydb,'select name as name, score as score,hard as hardness, hotness as hot from pro_rate')
dat_1<-fetch(query_1,n=-1)
pro_rate<-as.data.frame(dat_1)
query_2<-dbSendQuery(mydb,'select substring(name,1,instr(name,",")-1) as lastname,dept_name from dept_1')
dat_2<-fetch(query_2,n=-1)
df<-merge(dat_2,dat_1,by.x = "lastname",by.y = 'name',all.x = T)

# plot the data frame to take a look at the insight of the data by using ggplot

library(ggplot2)
library(plotly)
install.packages('fastmatch')
library(fastmatch)
head(df)
df$D_department<-as.factor(df$department)
df$hardness<-as.numeric(df$hardness)
vec<-df$hot
dvec<-as.factor(vec)
hotness<-fmatch(dvec,c('/assets/chilis/cold-chili.png','/assets/chilis/new-hot-chili.png'))
df$hotness<-hotness
colnames(df)
# histogram of score and hardness 
p1<-ggplot(df,aes(x=score))
p1+geom_histogram(color='white',fill='blue')

p3<-ggplot(df,aes(x=hardness))
p3+geom_histogram(color='white',fill='blue')

#histogram of score and hardness for different department
p4<-ggplot(df,aes(x=score,fill=dept_name))
p4+geom_histogram(position = 'dodge')

# barchart showing the score and hardness of different department
p2<-ggplot(df,aes(x=dept_name,y=score))
p2+geom_bar(stat = 'identity')
plot_ly(df,x= ~dept_name, y = ~score,text= ~paste("dept_name: ", dept_name))

p5<-ggplot(df,aes(x=dept_name,y=hardness))
p5+geom_bar(stat = 'identity')
plot_ly(df,x= ~dept_name, y = ~hardness,text= ~paste("dept_name: ", dept_name))

# scatter plot to see the relation ship between rate and hardness
p6<-ggplot(df,aes(x=hardness,y=score))
p6+geom_point(color='skyblue')+geom_smooth()+
  geom_smooth(method='lm',color='red')

#bubble chart
p7<-ggplot(df,aes(x=hardness,y=score,size=hotness,color=dept_name))
p7+geom_point()


#hotness affect?
cold<-subset(df,hotness==1)
hot<-subset(df,hotness==2)
p7<-ggplot(cold,aes(x=as.numeric(hardness),y=score))
p7+geom_point(color='red')+geom_smooth(method='lm',se=F)+
  geom_point(data=hot,aes(x=as.numeric(hardness),y=score))+
  geom_smooth(data=hot,aes(x=as.numeric(hardness),y=score),se=F,method = 'lm',color='yellow')
#for professor with less hotness, score decrease more significantly as hardness goes up



#dig into the data
#cluster:
for(i in 1:ncol(sdf)){
  sdf[is.na(sdf[,i]), i] <- mean(sdf[,i], na.rm = TRUE)
}
#sub the na to mean

sdf<-data.frame(df$score,as.numeric(df$hardness))
names(sdf)<-c('score','hardness')
sdf$score<-scale(sdf$score)
sdf$hardness<-scale(sdf$hardness)
#standardize

#number of clusters best fit
wss <- (nrow(sdf)-1)*sum(apply(sdf,2,var))
for (i in 1:10) wss[i] <- sum(kmeans(sdf, centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
#plot the wss for best #of clusters


cluster_1 <-kmeans(sdf[,c(1,2)],4) 
cluster_1
#with a bss/tss =71%
#pamk

#plot the first cluster

plot(sdf[clusters[['cluster']]==1,c(1,2)])

#regression 
#convert hotness to dummy variable
dummy_df<-within(df,hotness<-relevel(as.factor(hotness),"1"))


#simply look at hardness and score
l1<-lm(as.numeric(df$hardness)~df$score)
summary(l1)
#p<2e-16, significant y = -0.46 x, however only 23% x are releated

#run lm by using hotness of 1(cold) as reference
l2<-lm(as.numeric(dummy_df$score)~dummy_df$hotness,dummy_df)
summary(l2)
#estimate is 0.86 and p value is less than 0.01
#hotness will have a positive impact on score, and only 12% can be explained

#department
l4<-lm(as.numeric(dummy_df$score)~dummy_df$dept_name,dummy_df)
summary(l4)


#rule out all the possible difference between department, hotness
dummy_df$dept_name<-relevel(as.factor(dummy_df$dept_name),"Women's Studies")
l3<-lm(dummy_df$score~dummy_df$dept_name+dummy_df$hotness)
summary(l3)

# we use hotness as 1 which is cold and women's studies as ref, it shows that, for
#department, changing from business to women's studies have the most effect on the
#score, whereas not specified group has the least effect, which have a difference 
#from only consider department
#for hotness, changing from hot to cold will have a 0.83 increase on socre, it is
#slightly different from only consider hotness.

#Add hardness into account
l5<-lm(dummy_df$score~dummy_df$dept_name+dummy_df$hotness+log(dummy_df$hardness),dummy_df)
summary(l5)

#as we can see from the result log(hardness) have a negative effect on score
#note we take log(hardness) here

#add interaction 
l6<-lm(dummy_df$score~dummy_df$hotness+dummy_df$dept_name+dummy_df$hotness * dummy_df$dept_name)
summary(l6)

#to interpret
#women's study and hotness of 1 as reference
#take for example, dummy_df$hotness2:dummy_df$dept_nameMath
#the estimate is 0.58 meaning the it will have a postive effect of hotness on score
#when changing from women's study to math
