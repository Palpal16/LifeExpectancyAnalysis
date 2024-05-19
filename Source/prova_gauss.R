
#importate il dataset chiamando lo dati oppure rinominatelo:
dati<-  # qui nome file
  

# tenderei a tenere un dataset con al massimo 5000 valori se no, non funziona lo shapito.test
#la riga successiva prende le prime 5000 righe
dataset<-dati[1:5000,]

dim(dati)
dim(dataset)

summary(dataset)


sum(is.na(dataset))  
# numero di valori vuoit. Anche se maggiore di 0 non è un problema, rstudio quando fa la regressione li togli in automatico





#For visualizing the data, we can plot the pairs. It is useful also for making an idea about the relationship between the variables.

pairs(dataset, pch=16)
#se sono troppi dati o alcuni sono non numerici, modifico lo script successivo
#pairs(dataset[ , c('popularity', 'danceability', 'energy', 'loudness', 'speechiness', 'duration_ms')], pch = 16)


#For a nicer visualization of these scatterplots, we can use the package 'GGally'. We can easily vizualize the relationship of couple of variables, 
# their sample correlation and their approximated density function.

library (GGally) #prima di runnare questa riga c'è da importare la libreria da "tools"

ggpairs(data = dataset, title ="Relationships between predictors & response",
        lower = list(continuous=wrap("points", alpha = 0.5, size=0.1)))
# se voglio farlo solo su alcune variabili, uso il seguente:
ggpairs(data = dataset[ , c('popularity', 'danceability', 'energy', 'loudness', 'speechiness', 'duration_ms')], title ="Relationships between predictors & response",
        lower = list(continuous=wrap("points", alpha = 0.5, size=0.1)))



library(rgl) #prima di runnare questa riga c'è da importare la libreria da "tools"

# riga successiva da modificare 
g = lm( danceability ~ popularity + energy + loudness + duration_ms, data = dataset )

summary(g)
# qua mi interessa vedere le variabili non significative (meno di un *)
# e R-squared che dovrebbe essere sopra a 0.5

# se covariate poco significativa la tolgo e se R2 piccolo cambio dataset o semplicente uso un'altra y



## 3. Hypotheses of the model

### Homoscedasticity 

#__3.a__ Plot residuals ( $\hat{\varepsilon}$ ) vs fitted  values ( $\hat{y}$ ).

#A sequence of random variables is homoscedastic if all its random variables have the same finite variance. 
#Homoscedasticity is a fundamental assumptions in OLS.

plot( g$fit, g$res, xlab = "Fitted", ylab = "Residuals", 
      main = "Residuals vs Fitted Values", pch = 16 )
abline( h = 0, lwd = 2, lty = 2, col = 'red' )  # variance seems uniform across the fitted values

#The residual vs fitted value plot does not show any particular features that challenge the normality and 
#constant variance assumptions of the residuals.

#Alternatively, we can plot
plot(g, which=1 ) 


#per l'omoschedasticità voglio che questi due grafici siano saprsi e non abbiano relazione con la linea rossa




### Normality

#__3.b__ Plot QQ plot and test the normality of the residuals with Shapiro-Wilks test.

#Normality of data is another strong assumption of the OLS model. 

# QQ plot
qqnorm( g$res, ylab = "Raw Residuals", pch = 16 )
qqline( g$res )
# linear trend, the hypothesis is verified 

# Shapiro-Wilk normality test



shapiro.test( gx$res )




b=boxcox(popularity ~ danceability + energy + loudness + duration_ms, data = dataset)  #la risposta deve essere positiva ( >0 ) per il box-cox
ind=which.max(b$y)
lam=b$x[ind]
# se lambda è diverso da zero uso il segunete:   (altrimenti faccio logaritmo)
gx = lm( (popularity^lam-1)/lam ~ danceability + energy + loudness + duration_ms, data = dataset )
shapiro.test( gx$res )


# per la gaussianità mi serve un p-value alto




# sarebbe furbo verificare anche i seguenti:

#Histogram of residuals and boxplot are useful tools to look at the shape of the residual distribution and 
#to see if the tails of the distributions are souspiciously heavy (i.e., there are many outliers) respectively.

# other useful tools...
hist( g$res, 10, probability = TRUE, col = 'lavender', main = 'residuals'  )
boxplot( g$res, main = "Boxplot of savings residuals", pch = 16, col = 'lavender' )





