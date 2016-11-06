library(ggplot2)
library(gridExtra)
library(grid)
library(extrafont)
#font_import()
loadfonts(device="win")

#dane
zanieczyszczenie<- read.csv("pm25.csv", sep=";")
zanieczyszczenie$Wro<-as.factor(zanieczyszczenie$Wro)
zanieczyszczenie2<- read.csv("stezenieWro.csv", sep=";")

#parametry
czcionka="Corbel"
kolory<-c("#C0D5F2","#F99D4A")
kolorTla<-c("#f6f6f6")
kolorTekstu<-c("black")

przekrojowy<-ggplot(zanieczyszczenie, aes(x=reorder(Miasto, Stezenie),y=Stezenie,fill=Wro))+
  geom_bar(stat="identity")+
  coord_flip()+
  scale_fill_manual(values=kolory)+
  labs(title="\nW porównaniu z innymi miastami",y="Œrednia roczna µg/m3\n",x="")+
  theme_bw() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background=element_rect(fill=kolorTla),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border=element_rect(color="black"),
        axis.title=element_text(face='bold', size=13 ,family=czcionka,color=kolorTekstu),
        axis.text=element_text(face='bold',size=15,color=kolorTekstu,family=czcionka),
        plot.title=element_text(face='bold',size=18,family=czcionka,color=kolorTekstu),
        axis.ticks.y=element_blank(),
        plot.margin = unit(c(0,1,0,0), "lines")) +
  scale_y_continuous(expand = c(0,0),limits = c(0, 41)) +
  guides(fill=FALSE) +
  #linie pionowe
  geom_hline(yintercept = 25, color="red",size=1.3,linetype="dashed") +
  geom_hline(yintercept = 20, color="purple",size=1.3,linetype="dashed") +
  geom_text(label="Norma 2015", x=5,y=26.5, col="red",angle=90,cex=6,family=czcionka)+
  geom_text(label="Norma 2020", x=5,y=21.5, col="purple",angle=90,cex=6,family=czcionka)


czasowy<-ggplot(zanieczyszczenie2,aes(x=Rok,y=Stezenie))+
  geom_line(color=kolory[2],size=2)+
  labs(title="\nNa przestrzeni lat",y="\nŒrednia roczna µg/m3\n",x="")+
  theme_bw() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background=element_rect(fill=kolorTla),
        panel.grid.major = element_line(linetype="solid",color="#e0e0e0"),
        panel.grid.minor = element_blank(), 
        panel.border=element_rect(color="black"),
        axis.title=element_text(face='bold', size=15 ,family=czcionka,color=kolorTekstu),
        axis.text=element_text(face='bold',size=15,color=kolorTekstu,family=czcionka),
        plot.title=element_text(face='bold',size=18,family=czcionka,color=kolorTekstu),
        axis.ticks.y=element_blank(),
        plot.margin = unit(c(0,1,0,0), "lines")) +
  scale_y_continuous(expand = c(0,0),limits = c(20, 30)) 


png("zanieczyszczenie.png",bg="kolorTla",width=12, height=8,units = 'in',res=300)
grid.arrange(przekrojowy,czasowy,grid.rect(gp=gpar(col=kolorTla,fill=kolorTla)),layout_matrix=matrix(c(1,2,1,3), 2, 2, byrow = TRUE))
dev.off()