
library(ggplot2)
library("gridExtra")
library(extrafont) #do czcionek
font_import() #importuje czcionki
loadfonts(device="win")



zanieczyszczenie2<- read.csv("C:/Users/Michal/Desktop/stezenieWro.csv", sep=";")
zanieczyszczenie<- read.csv("C:/Users/Michal/Desktop/pm25.csv", sep=";")
zanieczyszczenie$Wro<-as.factor(zanieczyszczenie$Wro)
czcionka="Corbel"
kolory<-c("#C0D5F2","#F99D4A")

przekrojowy<-ggplot(zanieczyszczenie, aes(x=reorder(Miasto, Stezenie),y=Stezenie,fill=Wro))+
  geom_bar(stat="identity")+
  coord_flip()+
  scale_fill_manual(values=kolory)+
  labs(title="\nW porównaniu z innymi miastami",y="Œrednia roczna µg/m3\n",x="")+
  theme_bw() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background=element_rect(fill="#f6f6f6"), #t³o najbardziej na zwen¹trz
        panel.grid.major = element_blank(),#element_line(linetype="solid",color="#e0e0e0"), #wiêksza siatka
        panel.grid.minor = element_blank(), #mniejsza siatka
        #panel.border = element_blank(), #osie
        panel.border=element_rect(color="black"),
        axis.title=element_text(face='bold', size=13 ,family=czcionka,color="black"),
        axis.text=element_text(face='bold',size=15,color="black",family=czcionka),
        plot.title=element_text(face='bold',size=18,family=czcionka,color="black"),
        axis.ticks.y=element_blank(),
        plot.margin = unit(c(0,0,0,0), "lines")) +
  scale_y_continuous(expand = c(0,0),limits = c(0, 40)) +
  guides(fill=FALSE) +
  geom_hline(yintercept = 25, color="red",size=1.3,linetype="dashed") +
  geom_text(label="Norma 2015", x=5,y=26.5, col="red",angle=90,cex=6,family=czcionka)+
  geom_hline(yintercept = 20, color="purple",size=1.3,linetype="dashed") +
  geom_text(label="Norma 2020", x=5,y=21.5, col="purple",angle=90,cex=6,family=czcionka)


czasowy<-ggplot(zanieczyszczenie2,aes(x=Rok,y=Stezenie))+
  geom_line(color="#F99D4A",size=2)+
  labs(title="\nNa przestrzeni lat",y="\nŒrednia roczna µg/m3\n",x="")+
  theme_bw() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background=element_rect(fill="#f6f6f6"), #t³o najbardziej na zwen¹trz
        panel.grid.major = element_line(linetype="solid",color="#e0e0e0"), #wiêksza siatka
        panel.grid.minor = element_blank(), #mniejsza siatka
        #panel.border = element_blank(), #osie
        panel.border=element_rect(color="black"),
        axis.title=element_text(face='bold', size=15 ,family=czcionka,color="black"),
        axis.text=element_text(face='bold',size=15,color="black",family=czcionka),
        plot.title=element_text(face='bold',size=18,family=czcionka,color="black"),
        axis.ticks.y=element_blank(),
        plot.margin = unit(c(0,0,0,0), "lines")) +
scale_y_continuous(expand = c(0,0),limits = c(20, 30)) 
grid.arrange(przekrojowy,czasowy, ncol=2)

p<-ggplot()+
  theme(panel.background = element_rect(fill = "#f6f6f6"),
        plot.background=element_rect(fill="#f6f6f6"), #t³o najbardziej na zwen¹trz
        panel.grid.major = element_blank(), #wiêksza siatka
        panel.grid.minor = element_blank(), #mniejsza siatka
        #panel.border = element_blank(), #osie
        panel.border=element_blank(),
        axis.title=element_text(face='bold', size=13 ,family=czcionka,color="black"),
        axis.text=element_text(face='bold',size=15,color="black",family=czcionka),
        plot.title=element_text(face='bold',size=18,family=czcionka,color="black"),
        axis.ticks.y=element_blank(),
        plot.margin = unit(c(0,0,0,0), "lines"))

macierz<-rbind(c(1,2),c(1,3))
library(grid)
png("aaaa3.png",bg="#f6f6f6",width=12, height=8,units = 'in',res=300)
grid.arrange(przekrojowy,czasowy,p, layout_matrix = macierz)
dev.off()