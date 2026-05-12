install.packages("ROCR")
library(ROCR)

# zmienne niezależne:
#   Male - 0,1(0 - kobieta, 1 - mężczyzna)
#   age(wiek) -  age(wiek)
#   hypertension(nadciśnienie) - 0,1(0 - nie posiada, 1 - posiada)
#   heart_disease(problemy serca) - 0,1(0 - nie posiada, 1 - posiada)
#   ever_married(czy kiedykolwiek w związku małzeńskim) - 0,1(0 - nie, 1 - tak)
#   Urban - 0,1(0 - wieś, 1 - miasto)
#   avg_glucose_level - average glucose level in blood(średni poziom glukozy we krwi)
#   BMI - body mass index(wskaźnik masy ciała)

# zmienna zależna:
#   stroke(udar mózgu) - 0,1(0 - nie posiadał, 1 - posiadał)
#
# ceteris paribus (cp) - przy pozostałych zmiennych bez zmian 

dane = read.csv("C:\\Users\\kubap\\Desktop\\III rok\\zastosowanie R\\projekt\\brain_stroke.csv", header = T, sep = ',')
dane$ever_married = ifelse(dane$ever_married == 'Yes',1,0)
dane$Male = ifelse(dane$gender == 'Male', 1, 0)
dane$Urban = ifelse(dane$Residence_type == 'Urban',1, 0)

#struktura danych:
summary(dane)


#jak wygląda funkcja logitowa:
x = seq(-4, 4, 0.005)
plot(x, 1/(1+exp(-x)), type = "l", ylab = "P(y = 1)", main = "Funkcja logistyczna")
p = seq(0,1,0.005)
plot(p, log(p/(1-p)), type = "l", ylab = "logit", main = "Funkcja logitowa")

#logit
logit = glm(stroke~age+Male+hypertension+heart_disease+ever_married+Urban+avg_glucose_level+bmi, data = dane, family = 'binomial')
summary(logit)
dopasowany = step(logit, direction = "backward")
summary(dopasowany)
#Jako, ze tylko age, hypertension, avg_glucose_level sa istotne statystycznie, sprawdzamy model po usunieciu reszty:
logit_istotne = glm(stroke~age + hypertension + avg_glucose_level, data = dane, family = 'binomial')
summary(logit_istotne)


#postać modelu: ln(P/(1-P)) = -7,5404 + 0,07*age + 0,4074*hypertension + 0,0043*avg_glucose_level

#parametry modelu:
parametry = logit_istotne$coefficients
#age (0.0701): Wartość dodatnia. Wraz ze wzrostem wieku, prawdopodobieństwo wystąpienia udaru rośnie, przy cp.
#hypertension (0.4074): Wartość dodatnia. Nadciśnienie zwiększa szansę na wystąpienie udaru, przy cp.
#avg_glucose_level (0.0043): Wartość dodatnia. Wyższy poziom glukozy wiąże się z wyższym ryzykiem, choć wpływ jednostkowy wydaje się mały.
#(Intercept) (-7.5404): To wartość logarytmu szansy w sytuacji, gdy wszystkie zmienne niezależne wynosiłyby 0

tapply(logit_istotne$fitted.values, logit_istotne$y, mean)

# macierz klasyfikacji - W analizowanym zbiorze danych zmienna zależna „Stroke” przyjmuje wartość 1 
# stosunkowo rzadko. W konsekwencji oszacowane prawdopodobieństwa wystąpienia zdarzenia są niskie 
# i nie przekraczają poziomu 0.5. Zastosowanie standardowego progu klasyfikacyjnego 0.5 prowadziłoby 
# do zaklasyfikowania wszystkich obserwacji do klasy 0. Dlatego w macierzy klasyfikacji zadajemy obniżony próg decyzyjny
# równy 0.1.
macierz_klasyfikacji = table(logit_istotne$y, logit_istotne$fitted.values > 0.1)

#obliczamy zliczeniowy R^2
zliczeniowy_R_kwadrat = (macierz_klasyfikacji[1,1] + macierz_klasyfikacji[2,2]) / sum(macierz_klasyfikacji)
#uzyskany wynik 84% mówi nam, że nasz model sklasyfikował poprawnie 84% obserwacji.

#wyliczamy iloraz szans
iloraz_szans = exp(coef(logit_istotne))
#odczytując wartości, jesteśmy w stanie stwierdzić, że: wzrost wieku o rok, zwiększa szansę udaru średnio o 7,3% przy ceteris paribus,
#jeśli posiada nadciśnienie, to szansa udaru wzrasta średnio o 50,3% przy cp Natomiast wzrost średniego poziomu glukozy we krwi
#podnosi szansę udaru średnio o 0.4% przy cp

ROCPred = prediction(logit_istotne$fitted.values,logit$y)
ROCPerf = performance(ROCPred,'tpr','fpr')
plot(ROCPerf)
abline(a = 0, b = 1, lty = 2, col = 'blue')
auc_perf = performance(ROCPred, measure = 'auc')
auc_value = auc_perf@y.values[[1]]
#Nasza krzywa ROC wygina się mocno w stronę lewego rogu. Oznacza to, że nasz model ma wysoką czułość,
#zachowując przy tym niski poziom fałszywych alarmów. Ponadto, obszar pod wykresem wynosi 0,84 co tylko nas
#w tym upewnia.

#wykres prawdopodobienstwa udaru w zaleznosci od wieku
plot(dane$age, logit_istotne$fitted.values,
     pch = 16, cex = 0.5,
     xlab = "Wiek",
     ylab = "P(stroke = 1)",
     main = "Prawdopodobieństwo udaru a wiek")
lines(lowess(dane$age, logit_istotne$fitted.values), col = "red", lwd = 2)
#wykres prawdopodobienstwa udaru w zaleznosci od poziomu glukozy
plot(dane$avg_glucose_level, logit_istotne$fitted.values,
     pch = 16, cex = 0.5,
     xlab = "glukoza",
     ylab = "P(stroke = 1)",
     main = "Prawdopodobieństwo udaru a glukoza")
lines(lowess(dane$avg_glucose_level, logit_istotne$fitted.values), col = "red", lwd = 2)

