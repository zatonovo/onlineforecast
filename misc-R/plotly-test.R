## ----------------------------------------------------------------
## Load the current version directly from the folder
library(devtools)
load_all(as.package("../../onlineforecast"))


D <- Dbuildingheatload



library(plotly)



plotly_ts(D, c("heatload","Ta","I|heatload"), kseq=1:3)
oi

fig <- plot_ly(x=D$t, y=D$heatload)
fig <- fig %>% add_lines()
fig



nlines <- 3
colormap <- colorRampPalette(c("black","cyan","purple","blue","red","green"))(nlines)

Df <- as.data.frame(D)


L <- list()
for(ii in 1:2){
    fig <- plot_ly(x=Df$t)
    for(i in 1:nlines){
        ip <- (ii-1)*nlines + i + 1
        fig <- fig %>% add_lines(y = Df[ ,ip], name = names(Df)[ip], color=colormap[i], legendgroup = paste0('group',ii), zeroline=FALSE)#, yaxis=paste0('group',ii))
    }
    # Add empty to make legend gap
    fig <- fig %>% add_lines(y = rep(NA,nrow(Df)), name = "", color=colormap[i], legendgroup = paste0('group',ii), zeroline=FALSE)#, yaxis=paste0('group',ii))
    # Keep it
    L[[ii]] <- fig
}
subplot(L, shareX=TRUE, nrows=2)



fig1 <- fig
fig <- plot_ly(Df, x = ~t) 
fig <- fig %>% add_lines(y = ~heatload, name = 'trace 0')#, legendgroup = 'group2') 
fig <- fig %>% add_lines(y = ~Ta.k1, name = 'trace 1')#, legendgroup = 'group2')
fig2 <- fig
subplot(fig1, fig2, shareX=TRUE, nrows=2)

fig2 <- plot_ly(Df, x = ~t) 
fig2 <- fig2 %>% add_trace(y = ~heatload, name = 'trace 0',mode = 'lines', legendgroup="2") 
fig2 <- fig2 %>% add_trace(y = ~Ta.k1, name = 'trace 1', mode = 'lines+markers', legendgroup="2") 

subplot(fig, fig2
