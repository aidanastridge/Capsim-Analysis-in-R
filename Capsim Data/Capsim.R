library(tidyverse)
library(readxl)
require(scales)

balance<-read_excel("/Capsim Balance.xlsx")
income<-read_excel("/Capsim Income Statement.xlsx")
andrews<-read_excel("/Capsim Round Andrews.xlsx")
cashflow<-read_excel("/Capsim Cash Flow.xlsx")
compare<-read_excel("/Capsim D.xlsx")
forecast<-read_excel("/Capsim Forecasting Production.xlsx")
eps<-read_excel("/Capsim eps.xlsx")

ggplot(compare, aes(x=Year))  + geom_line(aes(y=`ChesterMC`), color="#85f748", size=1) + geom_line(aes(y=`BaldwinMC`),color="#3e407a", size=1) + geom_line(aes(y=`AndrewMC`), color="#427eff", size=1) + geom_line(aes(y=`ErieMC`), color="#61c2ff", size=1) + geom_line(aes(y=`FerrisMC`), color="#76428f", size=1) + geom_vline(xintercept = 2025,linetype="dashed") + geom_vline(xintercept = 2027,linetype="dashed")+theme_minimal()+ylab("")+ scale_y_continuous(labels = comma)+scale_color_manual(name = "Y series", values = c("Y1" = "darkblue", "Y2" = "red"))
ggplot(compare, aes(x=Year))  + geom_line(aes(y=`ChesterMS`), color="#85f748", size=1) + geom_line(aes(y=`BaldwinMS`),color="#3e407a", size=1) + geom_line(aes(y=`AndrewMS`), color="#427eff", size=1) + geom_line(aes(y=`ErieMS`), color="#61c2ff", size=1) + geom_line(aes(y=`FerrisMS`), color="#76428f", size=1) + geom_vline(xintercept = 2025,linetype="dashed") + geom_vline(xintercept = 2027,linetype="dashed")+theme_minimal()+ylab("")+ scale_y_continuous(labels = percent)
ggplot(compare, aes(x=Year))  + geom_line(aes(y=`ChesterS`), color="#85f748", size=1) + geom_line(aes(y=`BaldwinCP`),color="#3e407a", size=1) + geom_line(aes(y=`AndrewCP`), color="#427eff", size=1) + geom_line(aes(y=`ErieCP`), color="#61c2ff", size=1) + geom_line(aes(y=`FerrisCP`), color="#76428f", size=1) + geom_vline(xintercept = 2025,linetype="dashed") + geom_vline(xintercept = 2027,linetype="dashed")+theme_minimal()+ylab("")+ scale_y_continuous(labels = comma)

#Andrews Product Segments

ggplot(andrews, aes(x=Year))+ 
  geom_line(aes(y=`Traditional Segment Share`), color="#ff7e0d", size=1, alpha=.6)+ 
  geom_line(aes(y=`Low End Segment Share`),color="#994903", size=1, alpha=.6)+ 
  geom_line(aes(y=`High End Segment Share`), color="#0b2d9e", size=1, alpha=.6)+ 
  geom_line(aes(y=`Performance Segment Share`), color="#1c9174", size=1, alpha=.6)+
  geom_line(aes(y=`Size Segment Share`), color="#76428f", size=1, alpha=.6)+
  geom_vline(xintercept = 2025,linetype="dashed") + geom_vline(xintercept = 2027,linetype="dashed")+
  theme_minimal()+
  ylab("")+
  scale_y_continuous(labels = percent)+geom_line(data=forecast, aes(y=`TF`), color="#ff7e0d", size=1,linetype="dashed")+
  geom_line(data=forecast, aes(y=`LF`),color="#994903", size=1,linetype="dashed")+
  geom_line(data=forecast, aes(y=`HF`), color="#0b2d9e", size=1,linetype="dashed")+
  geom_line(data=forecast, aes(y=`PF`), color="#1c9174", size=1,linetype="dashed")+
  geom_line(data=forecast, aes(y=`SF`), color="#76428f", size=1,linetype="dashed")+
  geom_segment(aes(x = 2030, y = .196, xend = 2031, yend = .18), color="#ff7e0d")+
  geom_segment(aes(x = 2030, y = .214, xend = 2031, yend = .192),color="#994903")+
  geom_segment(aes(x = 2030, y = .243, xend = 2031, yend = .21), color="#0b2d9e")+
  geom_segment(aes(x = 2030, y = .21, xend = 2031, yend = .17), color="#1c9174")+
  geom_segment(aes(x = 2030, y = .22, xend = 2031, yend = .19), color="#76428f")

#Andrews Income and Balance Sheet 

ggplot(income, aes(x=Year))+ 
  geom_line(aes(y=`Total Variable Cost`), color="Red", size=1)+ 
  geom_line(aes(y=`Sales`),color="#427eff", size=1)+
  geom_vline(xintercept = 2025,linetype="dashed") + geom_vline(xintercept = 2027,linetype="dashed")+
  theme_minimal()+
  ylab("")+
  geom_area(aes(y=`Sales`),alpha=.2,fill="#427eff")+
  geom_area(aes(y=`Total Variable Cost`), alpha=.4, fill="Red")+ scale_y_continuous(labels = comma)

ggplot(compare, aes(x=Year))  + geom_line(aes(y=`Chester`), color="#85f748", size=1) + geom_line(aes(y=`Baldwin`),color="#3e407a", size=1) + geom_line(aes(y=`Andrews`), color="#427eff", size=1) + geom_line(aes(y=`Erie`), color="#61c2ff", size=1) + geom_line(aes(y=`Ferris`), color="#76428f", size=1) + geom_vline(xintercept = 2025,linetype="dashed") + geom_vline(xintercept = 2027,linetype="dashed")+theme_minimal()+ylab("")+scale_y_discrete(limits=c("C","CC","CCC","B","BB","BBB","A"))
ggplot(andrews, aes(x=Year))+ geom_line(aes(y=`Working Capital`), color="#427eff", size=1)+theme_minimal()+ylab("")+ scale_y_continuous(labels = comma)+ geom_vline(xintercept = 2025,linetype="dashed") + geom_vline(xintercept = 2027,linetype="dashed")
ggplot(andrews, aes(x=Year))+ geom_point(aes(y=`Quick Ratio`), color="#427eff", size=3)+geom_point(aes(y=`Current Ratio`), color="Red", size=3)+theme_minimal()+ylab("")+ scale_y_continuous(labels = comma)+ geom_vline(xintercept = 2025,linetype="dashed") + geom_vline(xintercept = 2027,linetype="dashed")
ggplot(andrews)+theme_minimal()+ylab("")+ geom_vline(xintercept = 2025,linetype="dashed") + geom_vline(xintercept = 2027,linetype="dashed")+geom_line(aes(Year,`Days of Working Capital`),color="#427eff", size=1)
ggplot(balance, aes(x=Year))+ geom_line(aes(y=`TD/TE`), color="#427eff", size=1)+ geom_point(aes(y=`Percent`), color="Red", size=2)+theme_minimal()+ylab("")+ scale_y_continuous(labels = percent)+ geom_vline(xintercept = 2025,linetype="dashed") + geom_vline(xintercept = 2027,linetype="dashed")
ggplot(income,aes(x=Year))+ geom_line(aes(y=`Net Margin`/`Sales`), color="#427eff", size=1)+ geom_line(aes(y=`Net Profit`/`Sales`), color="Red", size=1)+theme_minimal()+ylab("")+ scale_y_continuous(labels = comma)+ geom_vline(xintercept = 2025,linetype="dashed") + geom_vline(xintercept = 2027,linetype="dashed")+ scale_y_continuous(labels = percent)
ggplot(andrews,aes(x=Year))+ geom_line(aes(y=`ROE`), color="#427eff", size=1)+theme_minimal()+ylab("")+ scale_y_continuous(labels = percent)+ geom_vline(xintercept = 2025,linetype="dashed") + geom_vline(xintercept = 2027,linetype="dashed")

#Compare Market Cap, EPS, ROE, ROS

ggplot(compare, aes(x=Year))  + geom_line(aes(y=`ChesterMC`), color="#85f748", size=1) + geom_line(aes(y=`BaldwinMC`),color="#3e407a", size=1) + geom_line(aes(y=`AndrewMC`), color="#427eff", size=1) + geom_line(aes(y=`ErieMC`), color="#61c2ff", size=1) + geom_line(aes(y=`FerrisMC`), color="#76428f", size=1) + geom_vline(xintercept = 2025,linetype="dashed") + geom_vline(xintercept = 2027,linetype="dashed")+theme_minimal()+ylab("")+ scale_y_continuous(labels = comma)+scale_color_manual(name = "Y series", values = c("#85f748" = "darkblue", "Y2" = "red"))
ggplot(compare, aes(x=Year)) + geom_vline(xintercept = 2025,linetype="dashed") + geom_vline(xintercept = 2027,linetype="dashed")+theme_minimal()+ylab("")+ scale_y_continuous(labels = comma)
ggplot(compare, aes(x=Year))  + geom_line(aes(y=`ChesterEPS`), color="#85f748", size=1) + geom_line(aes(y=`BaldwinEPS2`),color="#3e407a", size=1) + geom_line(aes(y=`AndrewS`), color="#427eff", size=1) + geom_line(aes(y=`ErieS`), color="#61c2ff", size=1) + geom_line(aes(y=`FerrisS`), color="#76428f", size=1) + geom_vline(xintercept = 2025,linetype="dashed") + geom_vline(xintercept = 2027,linetype="dashed")+theme_minimal()+ylab("")+ scale_y_continuous(labels = comma)
ggplot(compare, aes(x=Year))  + geom_line(aes(y=`ChesterROE`), color="#85f748", size=1) + geom_line(aes(y=`BaldwinROE`),color="#3e407a", size=1) + geom_line(aes(y=`AndrewROE`), color="#427eff", size=1) + geom_line(aes(y=`ErieROE`), color="#61c2ff", size=1) + geom_line(aes(y=`FerrisROE`), color="#76428f", size=1) + geom_vline(xintercept = 2025,linetype="dashed") + geom_vline(xintercept = 2027,linetype="dashed")+theme_minimal()+ylab("")+ scale_y_continuous(labels = percent)
ggplot(compare, aes(x=Year))  + geom_line(aes(y=`ChesterROS`), color="#85f748", size=1) + geom_line(aes(y=`BaldwinROS`),color="#3e407a", size=1) + geom_line(aes(y=`AndrewROS`), color="#427eff", size=1) + geom_line(aes(y=`ErieROS`), color="#61c2ff", size=1) + geom_line(aes(y=`FerrisROS`), color="#76428f", size=1) + geom_vline(xintercept = 2025,linetype="dashed") + geom_vline(xintercept = 2027,linetype="dashed")+theme_minimal()+ylab("")+ scale_y_continuous(labels = percent)