#Global 변수
# DATA PROCESSING
library(tidyverse)
library(dplyr)

# Random Forest
library(tidyverse)
library(tidymodels)
library(randomForest)
library(yardstick)
library(showtext)
library(extrafont)

#Visual 3
library(caret)
library(janitor)
library(ggridges)
library(ggthemes)
library(cowplot)
library(corrplot)
library(corrr)
library(plotly)
library(crosstalk)
library(waffle)
library(formattable)
library(ggcorrplot)
library(shinydashboard)
library(shinythemes)

# SHINY
pacotes = c("shiny", "shinydashboard", "shinythemes", "plotly", "shinycssloaders","tidyverse",
            "ggplot2", "DT", "extrafont", "corrplot","GGally","ggpubr", "dplyr"
            ,"rpart",  "RColorBrewer","caret", "ggthemes", "corrr", "waffle", "formattable"
            ,"gridExtra", "echarts4r", "echarts4r.assets", "hrbrthemes", "magrittr", "ggcorrplot", "reshape2")



#font_add_google("Nanum Gothic", "nanumgothic")
# install.packages('randomForest')

theme_set(theme_grey(base_family='NanumGothic'))

theme.ti <- element_text(family="NanumGothic", face="bold", size=12) #그래프 제목 스타일 변경
theme.ax <- element_text(family="NanumGothic", face="bold", size=10, angle=00, hjust=0.54, vjust=0.5) #그래프 축 이름 스타일 변경
theme.leti<-element_text(family="NanumGothic", face="bold") #범례 제목 (legend) 스타일 변경, 범례가 있을 경우에만 쓰세요
theme.lete<-element_text(family="NanumGothic") #범례 항목 (legend) 스타일 변경, 범례가 있을 경우에만 쓰세요

# 
# package.check <- lapply(pacotes, FUN = function(x) {
#   if (!require(x, character.only = TRUE)) {
#     install.packages(x, dependencies = TRUE)
#   }
# })

#wp <- paste0(getwd(), "/hrda")
#setwd(wp)

source('data/data_processing.R',local=TRUE, encoding="utf-8")
source('eda.R',local=TRUE, encoding="utf-8")
source('Modeling.R',local=TRUE, encoding="utf-8")
# 작업 디렉터리
#nraw <- read_csv("data/dataset.csv")
#source('eda.R',local=TRUE, encoding="utf-8")

