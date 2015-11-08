
install.packages('rvest')
install.packages('KoNLP')
install.packages('stringi')
install.packages('dplyr')
install.packages('reshape2')
install.packages('ggplot2')
install.packages('wordcloud')

library(rvest)
library(KoNLP)
#Error: package ??rJava?? could not be loaded
#Try re-installing Java and make sure R and Java have matching architectures.
library(stringi)
library(dplyr)
library(reshape2)
library(ggplot2)
library(wordcloud)

# 영화, 마션
# 데이터 URL) http://movie.daum.net/moviedetail/moviedetailNetizenPoint.do?movieId=94626&searchType=all&type=after&page=1

urls<-paste0("http://movie.daum.net/moviedetail/moviedetailNetizenPoint.do?movieId=94626&searchType=all&type=after&page=",c(1:60))
urls <- lapply(urls,read_html)

martian<-NA
martian.star<-NA
martian.date<-NA

for (i in 1:length(urls) ) {
  martian[i]<-melt(urls[[i]] %>% html_nodes(".article") %>% html_text())
  martian.star[i]<-melt(urls[[i]] %>% html_nodes("#movieNetizenPointList em") %>% html_text())
  martian.date[i]<-melt(urls[[i]] %>% html_nodes(".datetxt") %>% html_text())
}

martian<-melt(martian)
martian.star<-melt(martian.star)
martian.date<-melt(martian.date)
martian$value<-iconv(martian$value, "UTF-8", "CP949")

martian_df<-cbind(martian, martian.star, martian.date)

martian_df$id<-1
martian_df$id<-cumsum(martian_df$id)

martian_df<-martian_df[c(-2,-4)]
names(martian_df)<-c('text','star','date','page','id')
martian_df$star<-as.character(martian_df$star)
martian_df$star<-as.numeric(martian_df$star)
martian_df$date<-as.character(martian_df$date)
martian_df$date<-gsub("\\.", "-", martian_df$date)
martian_df$date<-as.POSIXct(martian_df$date)

hist(martian_df$star)
mean(martian_df$star)

# 시간 데이터 처리
# 참고URL) http://www.stat.berkeley.edu/~s133/dates.html
plot(table(format(martian_df$date,'%j')))
table(format(martian_df$date,'%A'))

martian_df<-martian_df[complete.cases(martian_df$text),]

# 문자열 데이터 처리
# 참고URL) http://gastonsanchez.com/Handling_and_Processing_Strings_in_R.pdf
martian_df$text.clean<- gsub("\t", " ", martian_df$text)
martian_df$text.clean<- gsub("\n", " ", martian_df$text.clean)
martian_df$text.clean<- gsub("\r", " ", martian_df$text.clean)
martian_df$text.clean<- gsub("ㅋ", " ", martian_df$text.clean)
martian_df$text.clean<- gsub("ㅎ", " ", martian_df$text.clean)

# KoNLP pacakges에서 띄어쓰기는 중요한 정보
martian_df$text.clean<- gsub("[[:punct:]]", " ", martian_df$text.clean)
martian_df$text.clean<- gsub("\\W", " ", martian_df$text.clean)
martian_df$text.clean<- gsub("\\s+", " ", martian_df$text.clean)

# 평점과 영화평 길이
martian_df$nchar<- nchar(martian_df$text.clean)
hist(martian_df$nchar)
ggplot(martian_df, aes(factor(star), nchar))+geom_boxplot()

# 명사추출, 그러나 비교적 부정확
text.noun<- NA
for (i in 1:nrow(martian_df) ) {
  text.noun[i]<-melt(extractNoun(martian_df[i,6]))
}

# Pos22는 단순 명사추출 명령어 보다는 좋은 성능. 그러나 정확하지 않음. 전처리에 추가적 노력이 필요. R 패키지 자체 한계 존재.
# 한글 전처리의 정확성 향상을 위해서는 R의 KoNLP보다 다른 프로그램 사용이 좋은 성능을 보여줄 수 도 있음
text.noun<- NA
for (i in 1:nrow(martian_df) ) {
  text.noun[i]<-melt(SimplePos22(martian_df[i,6]))
}
text.noun<-melt(text.noun)

text.noun$num<-stri_count_regex(text.noun$value, "/NC")
text.noun<- text.noun %>% filter(num>0)
text.noun$value<-gsub("(/NC)+[[:print:]]+$", "", text.noun$value)
text.noun$value<- gsub("/NC", "", text.noun$value)

# 빈도와 함께 명사 목록 확인
text.noun.table<-table(text.noun$value)

# 워드클라우드 만들기 예제
pal <- brewer.pal(12,"Paired")
# windowsFonts(malgun=windowsFont(""))
wordcloud(names(text.noun.table),freq=text.noun.table, scale=c(5,.5),rot.per=0.25,min.freq=10, random.order=F,random.color=T,colors=pal)

# 추출된 명사와 별정 정보
names(text.noun)[1:2]<-c('noun','id')
text.noun.star<-merge(text.noun,martian_df, c('id'),all.x=T)
text.noun.star$date<-as.character(text.noun.star$date)

# 특정 별정 구간별로 추출된 명사 확인
text.noun.star %>% filter(star<=4) %>% count(noun) %>% arrange(-n) %>% mutate(n/sum(n)*100) %>% head(20)
text.noun.star %>% filter(star>4&star<=9) %>% count(noun) %>% arrange(-n) %>% mutate(n/sum(n)*100) %>% head(20)
text.noun.star %>% filter(star==10) %>% count(noun) %>% arrange(-n) %>% mutate(n/sum(n)*100) %>% head(20)

# 다른 영화와 비교 언급 (ex, 인터스텔라, 그래비티)
martian_df$movie.name<-stri_count_regex(martian_df$text.clean, "인터스텔라|그래비티|그레비티|그라비티|인터스탤라")
table(martian_df$movie.name)/nrow(martian_df)

# 배우, 감독 언급 (ex, 맷데이먼, 리들리 스콧)
martian_df$act.drt.name<-stri_count_regex(martian_df$text.clean, "리들리|스콧|맷|데이먼|멧|대이먼")
table(martian_df$act.drt.name)/nrow(martian_df)

# 영화 내용에 대한 언급 (ex, 이야기, 스토리, 내용)
martian_df$contents<-stri_count_regex(martian_df$text.clean, "이야기|스토리|내용")
table(martian_df$contents)/nrow(martian_df)

# 위의 주제를 평점별로 비교
martian_df %>% filter(movie.name>0) %>% summarise(mean(star), sd(star))
martian_df %>% filter(act.drt.name>0) %>% summarise(mean(star), sd(star))
martian_df %>% filter(contents>0) %>% summarise(mean(star), sd(star))

# Ddensity로 보기 (별점)
ggplot(martian_df, aes(star)) + 
  geom_density(data = subset(martian_df,movie.name > 0), fill = "red", alpha = 0.2) + 
  geom_density(data = subset(martian_df,act.drt.name > 0), fill = "blue", alpha = 0.2) +
  geom_density(data = subset(martian_df,contents > 0), fill = "green", alpha = 0.2)

# 위의 주제를 날짜별로 비교
martian_df %>% filter(movie.name>0) %>% summarise(mean(date))
martian_df %>% filter(act.drt.name>0) %>% summarise(mean(date))
martian_df %>% filter(contents>0) %>% summarise(mean(date))

# Ddensity로 보기 (날짜)
ggplot(martian_df, aes(date)) + 
  geom_density(data = subset(martian_df,movie.name > 0), fill = "red", alpha = 0.2) + 
  geom_density(data = subset(martian_df,act.drt.name > 0), fill = "blue", alpha = 0.2) +
  geom_density(data = subset(martian_df,contents > 0), fill = "green", alpha = 0.2)
