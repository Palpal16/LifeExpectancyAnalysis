library( car )
library( ellipse )
library( faraway )
library( leaps )
library(MASS)
library( GGally)
library(BAS)
library(rgl)
library(corrplot)
library(Matrix)

# DA MODIFICARE
setwd("/Users/simop/Documents/Statistica/R lab/Dataset")




# DATI E TRATTAMENTO DEI DATI
lifexp = read.csv('../Data/life_expectancy.csv')
lifexp=na.omit(lifexp)                       #tolgo le righe con i dati mancanti
lifexp=lifexp[which(lifexp$Year==2013),]       #seleziono l'anno di interesse
rownames(lifexp)=lifexp[,'Country']
lifexp=lifexp [ , c(-1, -2, -3)]       #tolgo le non numeriche


print(sapply(lifexp, typeof))
summary(lifexp)


#--------------------------------------------


# VISUALIZZAZIONE DATI
ggpairs(lifexp[ , c('Population', 'Hepatitis.B','Diphtheria','HIV.AIDS','infant.deaths','Total.expenditure','GDP','BMI','thinness..1.19.years','Life.expectancy')], title ="Relationships between predictors & response", lower = list(continuous=wrap("points", alpha = 0.5, size=0.1)))


#--------------------------------------------


# SCELTA COVARIATE - MODELLO LINEARE E BONTA'

#visualizzazione correlazione più in generale tutte le covariate (numeriche) di lifexp 
A=lifexp [ , c(-15)] 
corrplot(cor(A), method='number')
corrplot(cor(A), method='color')

g=lm( Life.expectancy ~ Population + Hepatitis.B + Diphtheria + HIV.AIDS + infant.deaths + Total.expenditure + GDP + BMI + thinness..1.19.years + Schooling , data = lifexp)
summary(g)


#--------------------------------------------


# OMOSCHEDASTICITA' E NORMALITA' RESIDUI (validità modello)
plot( g$fit, g$res, xlab = "Fitted", ylab = "Residuals", main = "Residuals vs Fitted Values", pch = 20 )
abline( h = 0, lwd = 2, lty = 2, col = 'red' )  
plot(g,which=1,pch=20)


plot(g,which=2,pch=20) #ci sta qui commentare gli outliers (non tutti, fatti bene dopo con criterio residui standardizzati) 
#lifexp[c('243','51','2211'), 'Country']
shapiro.test( g$res )



#--------------------------------------------


# EVENTUALE CORRELAZIONE TRA COVARIATE (check)
C=vcov(g) #non sembrano essercene, le covarianze sono tutte molto piccole #CAPIRE BENE CHE é STA TABELLA
vif(g) #(Rule of thumb: VIF > 5 or 10)


#--------------------------------------------


# DIAGNOSTICA : LEVERAGES
X = model.matrix( g )
lev = hat( X )
r = g$rank 
p = r-1
n = dim(lifexp)[1] 
sum(lev)==p+1 #controllino al volo che sia giusto

# plot pt leva
plot( g$fitted.values, lev, ylab = "Leverages", main = "Plot of Leverages", pch = 16, col = 'black' )
abline( h = 2 * r/n, lty = 2, col = 'red' )  #soglia convenzionale per identificare i punti di leva
# leverages che superano soglia
watchout_points_lev = lev[ which( lev > 2 * r/n  ) ]
watchout_points_lev
# a chi corrispondono tali leverages
watchout_ids_lev = seq_along( lev )[ which( lev > 2 * r/n ) ] ## identify the rows relative to leverage points
lifexp[watchout_ids_lev,]  #!!!!! !!!! !!! SISTEMARE SE PU0' FA
# grafico più figo
points( g$fitted.values[ watchout_ids_lev ], watchout_points_lev, col = 'red', pch = 16 )

# modello in cui tolgo i punti leva
gl = lm( Life.expectancy ~ Population + Hepatitis.B + Diphtheria + HIV.AIDS + infant.deaths + Total.expenditure + GDP + BMI + thinness..1.19.years + Schooling , data = lifexp, subset = ( lev < 2 * (p+1) / n  ) )
summary( gl )
# variazione percentuale dei beta   MHHHH DISCUTERE UN ATT
abs( ( g$coefficients - gl$coefficients ) / g$coefficients  )*100 -100


#--------------------------------------------


# DIAGNOSTICA: OUTLIERS
# andiamo molto intuitivamente a cercare i valori con un residuo molto alto
# residui non standardizzati ne studentizzati
plot( g$res, ylab = "Residuals", main = "Plot of residuals" )
sort( g$res ) [ c( 1, n ) ]  ## per vedere il più grande e il più piccolo


#residui standardizzati
gs = summary(g)
res_std = g$res/gs$sigma
watchout_ids_rstd = which( abs( res_std ) > 2 ) #rule of thumb visto che e ~ N(0,1) e il 95% dei valori di una gauss std sta circa tra -2*sigma e 2*sigma
watchout_rstd = res_std[ watchout_ids_rstd ]
watchout_rstd 
#It is easy to see that influential points according to standardized residuals and to leverages are different... confronta con watchout_ids_lev... ci sta perchè se fanno leva vuol dire che la retta di regressione si sta avvcinando molto a loro


#residui studentizzato (in alternativa ai standardizzati)   stud = g$residuals / ( gs$sigma * sqrt( 1 - lev ) ) #considera tutte e due le cose: maggiore è la leva maggiore è stud
stud = rstandard( g )

watchout_ids_stud = which( abs( stud ) > 2 )
watchout_stud = stud[ watchout_ids_stud ]
watchout_stud #in termini di residui non sembra che abbiamo andamenti diversi delle osservazioni tra stud e std


# Distanza di cook Ci = stud/p+1 * hii/1-hii    mette assieme i concetti: aumento di residuo stud e/o di leva la fanno aumentare --> avremo un warning di un pt leva se la distanza di cook è alta
Cdist = cooks.distance( g )
watchout_ids_Cdist = which( Cdist > 4/(n-p-1) ) 
watchout_Cdist = Cdist[ watchout_ids_Cdist ]
watchout_Cdist #eh quì ne detecta di più

#Confronto
par( mfrow = c( 1, 3 ) )
plot( g$fitted.values, Cdist, pch = 16, xlab = 'Fitted values', ylab = 'Cooks Distance', main = 'Cooks Distance' )
points( g$fitted.values[ watchout_ids_Cdist ], Cdist[ watchout_ids_Cdist ], col = 'green', pch = 16 )
plot( g$fitted.values, stud, pch = 16, xlab = 'Fitted values', ylab = 'Studentized Residuals', main = 'Studentized Residuals' )
points( g$fitted.values[ watchout_ids_stud ], stud[ watchout_ids_stud ], col = 'red', pch = 16 )
plot( g$fitted.values, lev, pch = 16, xlab = 'Fitted values', ylab = 'Leverages', main = 'Leverages' )
points( g$fitted.values[ watchout_ids_lev ], lev[ watchout_ids_lev ],col = 'orange', pch = 16 )

par( mfrow = c( 1, 1 ) )

#volendo si può provare a togliere qualcosa ma mi pare aver capito che a lei non piace
id_to_keep = !( 1:n %in% watchout_ids_Cdist )

gc = lm(Life.expectancy ~ Population + Hepatitis.B + Diphtheria + HIV.AIDS + infant.deaths + Total.expenditure + GDP + BMI + thinness..1.19.years + Schooling, lifexp[ id_to_keep, ] ) 

summary( gc )

abs( ( gc$coef - g$coef )/g$coef )*100 - 100

#--------------------------------------------


# COVARIATE ??????????

# remove Hepatitis.B
g1 = update( g, . ~ . - Hepatitis.B )
summary( g1 )

# remove Illiteracy
g2 = update( g1, . ~ . - Illiteracy )
summary( g2 )

# Remove Income
g3 = update( g2, . ~ . - Income )
summary( g3 )

# remove Population
g4 = update( g3, . ~ . - Population )
summary( g4 )


#--------------------------------------------


# PREVISIONE
