library(scales)
library(kableExtra)
library(webshot)
library(magick)
library(xtable)
library(gridExtra)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggpubr)
library(apcluster)
library(reshape2)



getData <- function(item , group)
{
    library(dplyr)
    link = paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_" ,
                 item , "_global.csv" , sep ="" )
    data = read.csv(url(link) , sep =",")
    #dropping State , Lat , Long columns 
    drops = c(1 ,3 , 4)
    data = select (data,-drops)
    for(i in 2:ncol(data))
    {
        names(data)[i] = sub('X', '',names(data)[i])
        names(data)[i] = as.Date(names(data)[i] , "%m.%d.%y")
    }
    # Grouping By Country instead of States
    names(data)[1] = "Country"
    data = as.data.frame(data %>%  group_by(Country) %>%  summarise_all(sum) )
    
    data 
    
}

getCountryValues = function(data , Name)
{
    data  = data %>% filter(Country == Name)
    values = as.numeric(data[1,2:ncol(data)])
    values
}


plot_Data = function(DATA , CountryName)
{
    
    xValues = names(DATA)
    xValues = xValues[xValues!= "Country"]
    yValues = getCountryValues(DATA , CountryName )
    xValues = as.Date(as.numeric(xValues), origin = '1970-1-1')
    data= data.frame(Date = xValues , Cases = yValues)
    
        p = ggplot(data ,  aes(x= Date, y=Cases , group = 1)) +
        geom_line(color ='#ffbc2d' ) +
        geom_point(color = ft_cols$yellow , size = 2) +
        labs(x="Date", y="Number of Cases",
             title=paste("Evolution of COVID19 in " , CountryName),
             subtitle="A plot that is only useful for demonstration purposes",
             caption="Brought to you by :Tarek") + 
        theme_ft_rc()  + 
        theme(axis.text.x = element_text(angle = 90, hjust = 2 , size = 10) )   
    
        p 
}

bar = function(data)
{
    data = data %>% filter(x %in% c('Algeria' , 'Tunisia' , 'Morocco' , 'Libya' , 'Egypt') )
    ggplot(data, aes(x= x , y = y )) +
        geom_bar(color= ft_cols$yellow, fill=ft_cols$yellow , width = 0.2 , stat = "identity" )  +
        theme_ft_rc() + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1 , size = 10) )
    
}


multiLine = function(data , names )
{
    #data = data %>% filter(Country %in% names)
    
    df = data.frame( 'Date' = as.Date(as.numeric(names(data)[2:ncol(data)] )  , 
                                       origin = '1970-1-1') 
                      ) 
    for(i in names )
    {
        df[ , i] = as.numeric(as.vector(data[data$Country == i ,2:ncol(data) ]))
    }
    point <- format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)
    df = melt(df , id.vars = 'Date' , variable.name = 'series')
    #view(df) 
    ggplot(df , aes(x = Date , y = value , group = series , color = series) )  +
        geom_line( )+
            ggtitle("Compare Evolution of COVID19 in Egypt, China, Italy, Iran ans USA") +
            theme_ft_rc()   +
            ylab("Number of Cases") +
            scale_y_sqrt (labels = point)+
            scale_x_date(date_labels = "%b %d") 
}


plotBox = function (data)
{
    
    df = data.frame(
        name = c('Total Cases' , 'Total Deaths' , 'Total Recovered'  ,'Active Cases'),
        value = c(data$Total.Cases , data$Total.Deaths , data$Total.Recovered , data$Active.Casses)
    )
    
    df %>%
        ggplot( aes(x=name, y=value, color =name)) +
        geom_boxplot() +
        #coord_cartesian(ylim = quantile(df$value, c(0.1, 0.8)))+
        theme_ft_rc() +
        theme(
            legend.position="none",
            plot.title = element_text(size=11)
        ) +
        ggtitle("Boxblots of Total Confirmed, Recovered, Deaths & Active Cases World Wide") +
        ylab("Number of Cases")+
        xlab("")+
        ylim(1000 , 100000)
}


getSimilarity = function(data)
{
    data = data %>% filter(Country %in% c('Algeria' , 'Tunisia' , 'Morocco' , 'Libya' , 'Egypt') )
    p = corSimMat(data, sel=c(1:5), r=1, signed=TRUE, method="pearson")
    df = data.frame(Countrey = c('Algeria' , 'Egypt' , 'Libya' , 'Morocco' , 'Tunisia') ,
                    "Algeria" = as.numeric(p[ ,1 ]),
                    "Egypt" = as.numeric(p[,2 ]),
                    "Libya" = as.numeric(p[ ,3 ]),
                    "Morocco" = as.numeric(p[ ,4]),
                    "Tunisia" = as.numeric(p[ ,5]) 
                    )
    
    p = negDistMat(data, r = 1)
    #p = linSimMat(data, sel=c(1:5), w=1, method="euclidean", p=1)
    df = data.frame(Countrey = c('Algeria' , 'Egypt' , 'Libya' , 'Morocco' , 'Tunisia') ,
                    "Algeria" = as.numeric(p[ ,1 ]) /10000000,
                    "Egypt" = as.numeric(p[,2 ]) /10000000,
                    "Libya" = as.numeric(p[ ,3 ]) /10000000,
                    "Morocco" = as.numeric(p[ ,4]) /10000000,
                    "Tunisia" = as.numeric(p[ ,5])/10000000 
    )
    view(df)
    p = kable(df, "html") %>%
        kable_styling("striped")
    p
    
}



getSkewness = function(data , case )
{
    if(case==1)
    {
        p = ggplot(data  = data   )  +
            stat_density(aes(x=Total.Deaths, y=..scaled..), position="dodge", geom="area" , fill = "#ff3427", color = "#ff3427") +
            ggtitle("Density of COVID19 Deaths World Wide ") +
            theme_ft_rc()  +
            scale_x_sqrt() +
            scale_y_continuous(labels = scales::percent) +
            xlab("Number of Deaths") 
        
    }
    
    else if(case ==2 )
    {
        p = ggplot(data  = data   )  +
            stat_density(aes(x=Total.Cases, y=..scaled..), position="dodge", geom="area" , fill = "#ffd427", color = "#ffd427") +
            ggtitle("Density of COVID19 Confirmed Cases World Wide ") +
            theme_ft_rc()  +
            scale_x_sqrt()+
            scale_y_continuous(labels = scales::percent) +
            xlab("Number of Confirmed Cases") 
        
    }
    else
    {
        p = ggplot(data  = data   )  +
            stat_density(aes(x=Total.Recovered , y=..scaled..), position="dodge", geom="area" , fill = "#3ad717", color = "#3ad717") +
            ggtitle("Density of COVID19 Recovered Cases World Wide ") +
            theme_ft_rc()  +
            scale_x_sqrt()+
            scale_y_continuous(labels = scales::percent) +
            xlab("Number of Recovered Cases") 
        
    }
    
    p
}


getCorrelation = function(data , case )
{
    drops <- c("Country")
    df = data[ , !(names(data) %in% drops)]
    cor = round(cor(df) , 2)
    p = kable(cor, "html") %>%
        kable_styling("striped")
    p
}




TotalCases = getData("confirmed")
TotalDeaths= getData("deaths")
Totalrecoverd = getData("recovered")

#Making a table contains all the three categories 
data = data.frame('Country' = TotalCases$Country , 'Total Cases' = TotalCases[,ncol(TotalCases)], 
                  'Total Deaths' = TotalDeaths[,ncol(TotalDeaths)] , 'Total Recovered' = Totalrecoverd[,ncol(Totalrecoverd)] ,
                  'Active Casses' = TotalCases[,ncol(TotalCases)] - (TotalDeaths[,ncol(TotalDeaths)] +
                                                                         Totalrecoverd[,ncol(Totalrecoverd)])
)
#TotalCases = read.csv('data/TotalCases.csv')
#TotalDeaths = read.csv('data/TotalDeaths.csv')
#Totalrecoverd = read.csv('data/TotalRecovered.csv')


#write.csv(TotalCases, 'data/TotalCases.csv',row.names = FALSE)
#write.csv(TotalDeaths, 'data/TotalDeaths.csv',row.names = FALSE)
#write.csv(Totalrecoverd, 'data/TotalRecovered.csv',row.names = FALSE)
#write.csv(data, 'data/All.csv',row.names = FALSE)


# ---------------------------plot Egypt progress ---------------------------------
plot_Data(TotalCases , "Egypt")





# ----------------------------------------------------------------------

plotData = data.frame(x = data$Country , y = data$Total.Cases)  
bar(plotData)



# -------------------------- Drawing  BocPlot  ---------------------------

plotBox(data)





# -------------------------- Finding Similarity ---------------------------

getSimilarity(data)




#-------------------------- Finding Density ---------------------------
getSkewness(data , 3)





#--------------------------Findding Correlation -------------------------------
getCorrelation(data , 1)




# -------------------------- PLot Egypt, Italy, Us, Iran and China progress  ---------------------------

multiLine(TotalCases , c('Egypt' , 'Italy', 'US' , 'Iran' , 'China' ))
