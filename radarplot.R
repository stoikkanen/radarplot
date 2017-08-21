## Create a radar plot for % data (e.g. Data completeness from various sites ("centres" for different variables ("organs"))

## This code is modified from Riddhiman's 
## R-blogger post, se https://www.r-bloggers.com/radar-charts-in-r-using-plotly/

rm(list=ls())
library(plotly)
library(dplyr)
colorpalette<-c("#7CD0D8FF", "#84B266FF", "#D888A9FF", "#7699D6FF",
                "#CC9966FF", "#CC7ACCFF", "#CCCC72FF",
                "#9F7FCCFF", "#71CC96FF", "#25A5A2FF", "#5FAF2CFF",
                "#BF4073FF", "#2F62ADFF", "#CC7219FF",
                "#993499FF", "#A59C2BFF", "#6938AFFF", "#16994AFF",
                "#11414CFF", "#244911FF", "#7A2242FF",
                "#0E1E47FF", "#663D0CFF", "#571259FF", "#595616FF",
                "#3B007FFF", "#06602BFF" ,"#00E5CFFF",
                "#5AE500FF", "#E50068FF",
                "#0073E5FF", "#E57700FF" ,"#D900E5FF" ,"#E5D400FF",
                "#5E00E5FF","#00E55CFF")

##################
## Fake 3 different data

organ<-c("N",  "S", "Le",  "Lu", "P")
centers<-LETTERS[1:5]
means1<-seq(15,85, length.out=c(length(centers)))
means2<-seq(10,97.5, length.out=c(length(centers)))
means3<-seq(50,100, length.out=c(length(centers)))

fakedata<-function(means=means1, sd=13, seed=123){
set.seed(seed)
  dct<-list()
  for (i in 1:c(length(centers))){
  dct[[i]]<-rnorm(n=length(organ), mean =means[i] , sd=sd)
  dct[[i]]<-ifelse(dct[[i]]<0, 0, ifelse(dct[[i]]>100, 100, dct[[i]]))
  }
dcdata<-do.call("rbind", dct)
rownames(dcdata)<-centers
colnames(dcdata)<-organ
t(dcdata)
}
gendata<-fakedata()
labdata<-fakedata(means2, sd=8, seed=345)
pocdata<-fakedata(means3, sd=20, seed=678)


###################
## FUNCTION BLOCK
###################

getPolarCoord <- function(r, matrix = F, na = F){
  # Get starting angle and angle increments
  theta <- 0
  dtheta <- 360 / length(r)
  dtheta <- (pi / 180) * dtheta  # in radians
  
  # Get polar coordinates
  x <- c()
  y <- c()
  
  for(i in 1:length(r)){
    
    x <- c(x, r[i] * cos(theta))
    y <- c(y, r[i] * sin(theta))
    
    theta <- theta + dtheta
  }
  
  x[length(x) + 1] <- x[1]
  y[length(y) + 1] <- y[1]
  
  if(na == T){
    x[length(x) + 1] <- NA
    y[length(y) + 1] <- NA
  }
  
  
  if(matrix == T){
    return(cbind(x, y))
  }else{
    return(list(x = x, 
                y = y))
  }
  
}

preparedata<-function(data=dcdata, scale=FALSE){
  
  if(scale){data<-100*(data/max(data, na.rm=T))}
  
  keskukset<-colnames(data)
    koordin<-apply(data, 2, getPolarCoord)
    koordinaatit<-list()
  for (i in 1:length(koordin)){
    koordinaatit[[i]]<-data.frame(do.call("cbind", koordin[[i]]))
    koordinaatit[[i]]$centre<-keskukset[i]
    }
  finaldf<-do.call("rbind", koordinaatit)
  finaldf
  }

getcolors<-function(cols=colorpalette[1:ncol(dcdata)], data=dcdata, alpha=1){
  if(length(cols)!=ncol(data)){
    if(length(cols)<ncol(data)){
      cols<-rep(cols, length.out=ncol(data))
    }
    else(cols<-cols[1:ncol(data)])
  }
  rrgb<-col2rgb(cols)
  rrgbs<-sapply(1:ncol(rrgb), function(a){
    paste0("rgba(", paste0(rrgb[,a], collapse=","), ",", alpha,")")})
  rrgbs
##  finaldf$col<-cols[match( finaldf$centre,names(cols))]
  }

# Plotting defaults 
bgcolor <- "white"

# CALCULATE GRIDS
grid <- rbind(getPolarCoord(rep(20, 50), matrix = T, na = T),
              getPolarCoord(rep(40, 80), matrix = T, na = T),
              getPolarCoord(rep(60, 150), matrix = T, na = T),
              getPolarCoord(rep(80, 170), matrix = T, na = T),
              getPolarCoord(rep(100, 200), matrix = T, na = T))

grid <- as.data.frame(grid)
inner <- getPolarCoord(rep(0.1, 5))
outer <- getPolarCoord(rep(100, 5))
x = t(cbind(inner$x, outer$x))
y = t(cbind(inner$y, outer$y))

x <- as.numeric(apply(x, 2, function(vec){
  return(c(vec, NA))
}))
y <- as.numeric(apply(y, 2, function(vec){
  return(c(vec, NA))
}))
linegrid <- data.frame(x = x, y = y)

##### TEXT
organs<-rownames(labdata)
labels<-c(organs, NA)

markers<-data.frame(rbind(do.call("cbind", getPolarCoord(rep(5, 10)))[2,],
                          do.call("cbind",getPolarCoord(rep(20+5, 10)))[2,],
do.call("cbind",getPolarCoord(rep(40+5, 10)))[2,],
do.call("cbind",getPolarCoord(rep(60+5, 10)))[2,],
do.call("cbind",getPolarCoord(rep(80+5, 10)))[2,],
do.call("cbind",getPolarCoord(rep(100+5, 10)))[2,]))

markers$lab<-paste0(seq(0, 100, by=20), "%")
  
##### MERGE
mergedatas<-function(data=dcdata, alpha1=1, alpha2=0.3, scale=FALSE){
  finaldf<-preparedata(data=data, scale=scale)
  linecols<-getcolors(cols=colorpalette, data = data, alpha=alpha1)
  fillcols<-getcolors(cols=colorpalette, data = data, alpha=alpha2)
  names(fillcols)<-names(linecols)<-unique(finaldf$centre)
  finaldf$fillcol<-paste0("\"", 
                          fillcols[match(finaldf$centre,names(fillcols))], "\"")
  finaldf$linecol<-linecols[match(finaldf$centre,names(linecols))]
  centers<-unique(finaldf$centre)
  list(finaldf=finaldf, centers=centers)
}

###### plotly querys
genpolyquery<-function(centreq="HD",smooth=0.6){
  (paste0( "\n add_trace(data = subset(finaldf, centre==\"", centreq, "\"), 
           x = ~x, y = ~y, mode = \"lines\", 
           showlegend = T, name=", paste0("\"", centreq, "\""), 
           ", fill = \"toself\",
           fillcolor=subset(finaldf, centre==\"", centreq, "\")$fillcol,
           line = list(smoothing = smooth, shape = \"spline\", 
           color=subset(finaldf, centre==\"", centreq, "\")$linecol))%>%"))
}
genmarkquery<-function(centreq="HD"){
  (paste0("add_trace(data = subset(finaldf,centre==\"", centreq, "\"),  
          x = ~x, y = ~y, mode = \"markers\", 
          marker = list(color =  subset(finaldf,centre==\"", centreq,"\")$linecol, 
          size = 5, 
          line = list(width = 4)),
          hoverinfo = \"none\",
          showlegend = F)%>% "))
}

###### plotting function
plotfun<-function(data=labdata,centres=NULL, scale=FALSE, smooth=0.6){
  foo<-mergedatas(data, scale=scale)
  finaldf<-foo$finaldf
  if(is.null(centres)){  centres<-foo$centers}

    eval(parse(text=paste0("p <- plot_ly() %>%",
                         paste0(sapply(centres, genpolyquery, smooth=smooth), collapse=""),
                         paste0(sapply(centres, genmarkquery), collapse="") ,
                         "\n add_trace(p, data = grid, 
                         x = ~x, y = ~y, mode = \"lines\",
                         line = list(color = \"57788e\", dash = \"4px\", width = 1),
                         fillcolor=bgcolor,
                         showlegend = F,
                         hoverinfo = \"none\") %>%
                         \n add_trace(p, data = getPolarCoord(rep(110, 5)),
                         x = ~x, y = ~y, mode = \"text\", text = ~labels,
                         showlegend = F,
                         hoverinfo = \"none\",
                         textfont = list(family = \"serif\", color = \"#808080\")) %>% 
                         
                         \n add_trace(p, data = markers,
                         x = ~x, y = ~y, mode = \"text\", text = ~lab,
                         showlegend = F,
                         hoverinfo = \"none\",
                         textfont = list(family = \"serif\", color = \"#808080\")) %>% 

                        add_trace(p, data = getPolarCoord(rep(1, 200)),
                        x = ~x, y = ~y,
                         fill = \"toself\",
                         fillcolor = \"57788e\",
                         line = list(color = \"transparent\"),
                         mode = \"lines\",
                         hoverinfo = \"none\",
                         showlegend = F) %>%

                         \n layout(xaxis = list(title = \"\", showgrid = F, 
                         zeroline = F, showticklabels = F,
                         domain = c(0.0, 1)), width=600, height=650,
                         yaxis = list(title = \"\", 
                         showgrid = F, zeroline = F, showticklabels = F,
                         domain = c(0, 1)),
                         font = list(family = \"serif\", size = 15),
                         legend = list(orientation=\"h\", bgcolor = \"transparent\"),
                         plot_bgcolor = bgcolor,
                         paper_bgcolor = bgcolor)
                         print(p)"
                         )))
  
}

