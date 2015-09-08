
require("rgdal") 
require("maptools")
require("ggplot2")
require("ggmap")
require("plyr")
require("RColorBrewer") 
require("classInt") 
require("scales")
library(rgeos)       # loads polygon clipping library
library(maptools)     # loads sp library too
library(rgdal)
library(sp)




cria_faixa<- function (x){
  # x <- seq(0,4000,50)
  faixas <- c(seq(0,250,50),500,1000,2000,Inf)
  ref <-data.frame(matrix(ncol = 2,nrow = length(faixas)))
  names(ref) <-  c("brks","brks2")
  for(i in 1:(length(faixas)-1)){
    if(i + 1 <10  ){
      ref[i,2] <- paste(0,0,i,"-","[",faixas[i],"-",faixas[i+1],"]",sep="") 
    }
    else{
      ref[i,2] <- paste(0,i,"-","[",faixas[i],"-",faixas[i+1],"]",sep="")
    }
    ref[i,1]<-paste(faixas[i],"-",faixas[i+1])  
  }
  
  faixa_nova <- data.frame(matrix(ncol = 1,nrow = length(x)))
  names(faixa_nova) <-  c("brks")
  for(j in 1:length(x)){
    for(i in 1:(length(faixas)-1)){
      if (faixas[i] <= x[j] && x[j] <= faixas[i+1]) faixa_nova[j,1] <- paste(faixas[i],"-",faixas[i+1])  
    }
  } 
  require(dplyr)
  z<- left_join(faixa_nova,ref,by="brks",match=all,type="full")
  z[1:length(x),2]
  
}


meses <- c("janeiro","fevereiro","marco","abril","maio","junho",
           "julho","agosto","setembro","outubro","novembro","dezembro")

library(geoR)
library(lattice)
salvar_em <- "I:/LAFEE/Projeto_krigagem_SP/"

serie_temp<- total
unique(total$latitude)
setwd("I:\\LAFEE\\projeto cristina\\dados_criados")
serie_temp <- read.csv2("serie_temporal_mensal.csv")

plot(serie_temp$longitude,serie_temp$latitude)

class(serie_temp$longitude)
ggmap(map) +
  geom_polygon(data=sp.df, aes(long,lat,group=group ),fill = "white" , colour="white",alpha=0) +
geom_point(data =serie_temp,aes(x=longitude,y=latitude),colour ="red") +
  gtitle("Pontos Cemaden")


ano <- 2014
for(mes in 1:12 ){
  ##
  titulo <- paste(meses[mes],"de",ano,sep=" ")
  serie <-serie_temp[serie_temp$ano == ano & serie_temp$mes == mes , ]
  
  serie <-serie[!is.na(serie$mm.sum),]
  variaveis1 <- c("latitude","longitude","mm.sum")
  serie <- serie[,variaveis1]
  serie$latitude <- as.numeric(as.character(serie$latitude))
  serie$longitude <- as.numeric(as.character(serie$longitude))
  serie$mm        <- as.numeric(as.character(serie$mm.sum))
  ##
  geodata <- as.geodata(serie,coords.col = c(1,2),data.col = 3)
  ##
  serie_nova <- serie[order(serie$mm.sum),]
  serie_nova <- serie_nova[!is.na(serie_nova$mm.sum),]
  xrange <- c(min(geodata[[1]][,1]),max(geodata[[1]][,1]))
  yrange <- c(min(geodata[[1]][,2]),max(geodata[[1]][,2]))
  ##
  grid <- expand.grid(seq(xrange[1],xrange[2],by=.05),seq(yrange[1],yrange[2],by=.05))
  
  variogram1 <- variog(geodata, trend="1st", uvec=seq(0, 1.5, by=0.05))
  
  #valores inicias para encontrar a melhor aproximação
  initial.values <- expand.grid(seq(min(variogram1$v), max(variogram1$v), by=round(max(variogram1$v) -min(variogram1$v))),
                                seq(min(variogram1$u), max(variogram1$u),by=0.05))
  
  
  #Krigagem
  models<-c("matern", "exponential", "gaussian",
            "spherical", "circular", "cubic", "wave")
  #models<-"cubic"
  #models <- "circular"
  for(model in models){
    
    fit <- variofit(variogram1, cov.model= model , ini.cov.pars=initial.values,
                    fix.nugget=FALSE, nugget=min(variogram1$v))
    jpeg(paste(salvar_em,"Variogram_",meses[mes],"_",ano,"_","modelo_", model,".jpg",sep=""))
    plot(variogram1)
    #title(paste(ano,meses[mes],dim(geodata)[1]," estacoes",model))
    title(paste(meses[mes],"de",ano,"-", "modelo", model ))
    lines(fit)
    dev.off()
    #}
    #for(model in models){
    #fit <- variofit(variogram1, cov.model= model , ini.cov.pars=initial.values,
    #                fix.nugget=FALSE, nugget=min(variogram1$v))
    
    krigagem <- krige.control(type.krige = "ok", cov.model = model,
                              cov.pars = fit$cov.pars,nugget = min(variogram1$v))
    krigagem2 <- krige.conv(geodata, locations = grid, krige = krigagem)
    jpeg(paste(salvar_em,"SP_krigagem","_",model,"_com_geodata",".jpg",sep = ""))
    contour(krigagem2,col =terrain.colors(25),filled = TRUE,coords.data = geodata$coords,zlim = c(min(krigagem2$predict),max(krigagem2$predict)))
    title(paste(meses[mes],"de",ano,"-","modelo", model ))
    dev.off()
    jpeg(paste(salvar_em,"SP_krigagem","_",model,"_sem_geodata",".jpg",sep = ""))
    contour(krigagem2,col =terrain.colors(25),filled = TRUE,zlim = c(min(krigagem2$predict),max(krigagem2$predict)))
    title(paste(meses[mes],"de",ano,"-", "modelo", model ))
    dev.off()
    #line <- readline()
    
    malha <- data.frame(grid[,1],grid[,2],krigagem2$predict)
    names("")
    write.csv2(malha,paste("malha","_",model,".csv",sep = ""),row.names = FALSE)
  }
  
  
  
  #######################
  #plota malha krigagem#
  #####################
  setwd("I:\\LAFEE\\projeto cristina\\dados_criados")
  arquivos <-dir()[grep("malha",dir())]
  
  
  for(arquivo in arquivos){
    gg<- read.csv2(file = arquivo )
    names(gg)<- c("y","x","z")
    #if(length(unique(gg$z)) < 2*tam){
    # print("jump")
    # print(arquivo)
    # break
    # }
    
    
    gg$brks <- cria_faixa(gg$z)
    modelo <- gsub(x=arquivo,pattern = ".csv","")
    modelo <- gsub(x=modelo,pattern = "malha_krig_","")
    z <-c(seq(0,250,50),500,1000,2000,Inf)
    x <- seq(1,length(z))
    y <- seq(1,length(z))
    test <- data.frame(y=y ,x= x ,z= z, brks = cria_faixa(z))
    gg<-rbind(gg,test)
    
    color <- c("#007336","#00814A","#009160","#00A57D",
               "#73C6AB","#ACB4E3","#7D8AD3","#5D70CB","#3E5BC7","#0C49C8")
    #2
    color  <-c("#a50026","#d73027",  "#f46d43",  "#fdae61",  "#fee08b",
               "#d9ef8b",  "#a6d96a",  "#66bd63",  "#1a9850",  "#006837")
    color <- color[order(1:10,decreasing = TRUE)]
    mapa <- ggplot() %+% gg+aes(x,y)+
      geom_polygon(data=sp.df, aes(long,lat,group=group,fill=""),fill="white",colour="black") +
      geom_tile(aes(fill=brks,alpha=0.6 ))+
      scale_fill_manual("mm",values= color)+
      ggtitle(paste("SP_ -",modelo,ano,meses[mes],sep=" ")) +
      coord_equal()
    
    jpeg(paste(salvar_em,"SP_","_",modelo,ano,meses[mes],".jpg",sep = ""),width=1920,height=1080)
    print(mapa)
    dev.off()
    
    gg_map<- ggmap(map)  %+% gg+aes(x,y)+
      geom_polygon(data=sp.df, aes(long,lat,group=group ),fill = "white" , colour="white",alpha=0) +
      geom_tile(aes(fill=brks,alpha=0.6 ))+
      scale_fill_manual("mm",values= color)+
      ggtitle(paste("SP_ -",modelo,ano,meses[mes],sep=" ")) +
      coord_equal()
    jpeg(paste(salvar_em,"SP_GGMapa","_",modelo,ano,meses[mes],".jpg",sep = ""),width=1920,height=1080)
    print(gg_map)
    print(meses[mes])
    dev.off()
    
    
  }
  file.remove(arquivos)
}   