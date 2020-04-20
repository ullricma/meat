#================================================#
# Preparing the R Session ####
#================================================#

#req_packages <- c("Hmisc", "ggplot2", "car", "psych", "GPArotation","hornpa")
#req_packages <- req_packages[!req_packages %in% installed.packages()]
#lapply(req_packages, install.packages)

#================================================#
# Loading Packages ####
#================================================#

library(haven)
library(dplyr)

options(digits = 2)
options(max.print = 100)

#================================================#
# Download necessary files ####
#================================================#

# 2015 data
q15 <-  read_sav("./spss/ZA5665_a1_ca-cf_v32-0-0.sav")

# 2017 data
q17 <-  read_sav("./spss/ZA5665_a1_ea-ef_v32-0-0.sav")

# 2016 data
q16 <-  read_sav("./spss/ZA5665_a1_da-df_v32-0-0.sav")

# SozDemo Data
demo <- read_sav("./spss/ZA5665_a1_a11-a12_v32-0-0.sav")

#================================================#
# Grab necessary demographic variables ####
#================================================#

id <- "z000001a"


# demographics
vars <- 
c(
"a11d054a", #Geschlecht
"a11d056b", #Geburtsjahr
"a11d082b", #Höchster Schulabschluss, inkl. o. A
"a11d086b", #Beruflicher Ausbildungsabschluss, inkl. o. A
"a11d081a", #Joint Household
"a11d097c", #Haushaltseinkommen, 14 Kategorien standard edition
"a11d096b") #Persönliches Einkommen, 15 Kategorien standard edition

demo <- demo[,c(id, vars)]

q15demo <- c(
"cfzh071a", #Geschlecht
"cfzh072c", #Geburtsjahr
"cfzh078a", #Höchster Schulabschluss, inkl. o. A
"cfzh079a", #Berufsabschluss
#"cfzh080a", #Anderen Abschluss
"cfzh081a", #Universitätsabschluss
"cfzh084a", #Aktuell in beruflicher Ausbildung
"cfzh077a", #gemeinsamer Haushalt
"cfzh090c", #Haushaltseinkommen, 14 Kategorien standard edition
"cfzh089b") #Persönliches Einkommen, 15 Kategorien standard edition



#================================================#
# Questionnaires from 2015 ####
#================================================#

#Wave CB 06/2015
### Fleisch
q15a <- c(
"cbas062a", #Häufigkeit Fleischkonsum 
"cbas063b", #Einschätzung Fleischkonsum: An jedem Tag mehrmals 
"cbas064b", #Einschätzung Fleischkonsum: An jedem Tag einmal 
"cbas065b", #Einschätzung Fleischkonsum: An 5-6 Tagen pro Woche 
"cbas066b", #Einschätzung Fleischkonsum: An 3-4 Tagen pro Woche 
"cbas067b", #Einschätzung Fleischkonsum: An 1-2 Tagen pro Woche 
"cbas068b", #Einschätzung Fleischkonsum: Seltener 
"cbas069b", #Einschätzung Fleischkonsum: Personen, die nie Fleisch essen 
"cbas070a", #Schwierigkeit Einschätzung Fleischkonsum anderer 
"cbas071a", #Schwierigkeit eigenen Fleischkonsum zu reduzieren 
"cbas072a", #Angemessenheit Fleischkonsum 
"cbas073a") #Fleischkonsum zukünftig
  
### green house gases
q15b <- c(
"cbas074a", #Einschätzung Treibhausgase: Gemüse und Obst 
"cbas075a", #Einschätzung Treibhausgase: Fleisch und Fleischerzeugnisse 
"cbas076a", #Einschätzung Treibhausgase: Milch und Milcherzeugnisse 
"cbas077a", #Einschätzung Treibhausgase: Öl und Eier 
"cbas078a", #Einschätzung Treibhausgase: Zucker, Honig und Kakao
"cbas079a", #Einschätzung Treibhausgase: Reis, Kartoffeln und Hülsenfrüchte 
"cbas080a") #Einschätzung Treibhausgase: Getreide und Getreideerzeugnisse

### Umwelt
q15c <- c(
"cbaq081a", # Mensch und Umwelt: Umweltverhältnissen für Nachfahren 
"cbaq082a", # Mensch und Umwelt: Umweltkatastrophe 
"cbaq083a", # Mensch und Umwelt: Durch Zeitungsberichte empört und wütend 
"cbaq084a", # Mensch und Umwelt: Grenzen des Wachstums überschritten 
"cbaq085a", # Mensch und Umwelt: Bevölkerung wenig umweltbewusst 
"cbaq086a", # Mensch und Umwelt: Umweltproblem übertrieben 
"cbaq087a", # Mensch und Umwelt: Politiker tun viel zu wenig für den Umweltschutz 
"cbaq088a", # Mensch und Umwelt: Lebensstandard einschränken 
"cbaq089a" # Mensch und Umwelt: Umweltschutzmaßnahmen trotz Arbeitsplatzverlusten
)
# #CE 12/2015
# Fleisch
q15aa <- c(
"ceas097a", # Häufigkeit Fleischkonsum 
"ceas105a", # Angemessenheit Fleischkonsum 
"ceas106a" # Fleischkonsum zukünftig 
)

q15bb <- c(
"ceas107a", # Einschätzung Treibhausgase: Gemüse und Obst 
"ceas108a", # Einschätzung Treibhausgase: Fleisch und Fleischerzeugnisse 
"ceas109a", # Einschätzung Treibhausgase: Milch und Milcherzeugnisse 
"ceas110a", # Einschätzung Treibhausgase: Öl und Eier 
"ceas111a", # Einschätzung Treibhausgase: Zucker, Honig und Kakao 
"ceas112a", # Einschätzung Treibhausgase: Reis, Kartoffeln und Hülsenfrüchte 
"ceas113a", # Einschätzung Treibhausgase: Getreide und Getreideerzeugnisse
"ceas114a" #Diskussionen Treibhausgase und Klimaschutz
)

q15z <- c(
"ceas098a", # Einschätzung Fleischkonsum: An jedem Tag mehrmals 4
"ceas099a", # Einschätzung Fleischkonsum: An jedem Tag einmal 4

"ceas100a", # Einschätzung Fleischkonsum: An 5-6 Tagen pro Woche 3

"ceas101a", # Einschätzung Fleischkonsum: An 3-4 Tagen pro Woche 3
"ceas102a", # Einschätzung Fleischkonsum: An 1-2 Tagen pro Woche 2

"ceas103a", # Einschätzung Fleischkonsum: Seltener 1
"ceas104a" # Einschätzung Fleischkonsum: Personen, die nie Fleisch essen  1
)

#CC 08/2015
q15d <- c(
#"cczd001a", # Großstadtnähe Wohngegend
"cczd002a", # NEP-Skala: Nähern uns Höchstzahl an Menschen 
"cczd003a", # NEP-Skala: Recht Umwelt an Bedürfnisse anzupassen 
"cczd004a", # NEP-Skala: Folgen von menschlichem Eingriff 
"cczd005a", # NEP-Skala: Menschlicher Einfallsreichtum 
"cczd006a", # NEP-Skala: Missbrauch der Umwelt durch Menschen 
"cczd007a", # NEP-Skala: Genügend natürliche Rohstoffe 
"cczd008a", # NEP-Skala: Pflanzen und Tiere gleiches Recht 
"cczd009a", # NEP-Skala: Gleichgewicht der Natur stabil genug 
"cczd010a", # NEP-Skala: Menschen Naturgesetzen unterworfen 
"cczd011a", # NEP-Skala: Umweltkrise stark übertrieben. 
"cczd012a", # NEP-Skala: Erde ist wie Raumschiff 
"cczd013a", # NEP-Skala: Menschen zur Herrschaft über Natur bestimmt 
"cczd014a", # NEP-Skala: Gleichgewicht der Natur ist sehr empfindlich 
"cczd015a", # NEP-Skala: Natur kontrollieren 
"cczd016a") # NEP-Skala: Umweltkatastrophe

#Nutzung und andere Meinungen
q15e <- c(
"cczd030a", # Meinung Atomausstieg 
"cczd031a", # Klimaschutzpolitik - Tempo 
"cczd032a", # Ernsthaftigkeit Problem Klimawandel 
"cczd033a", # Besitz ÖPNV-Karte 
"cczd034a", # Verfügbarkeit Auto 
"cczd035a", # Nutzungshäufigkeit: Auto 
"cczd036a", # Nutzungshäufigkeit: Fahrrad 
"cczd037a", # Nutzungshäufigkeit: Bus oder Bahn in der Region 
"cczd038a", # Nutzungshäufigkeit: Bahn auf längeren Strecken 
"cczd039a", # Einkauf Bio-Lebensmittel 
"cczd040a", # Einkauf Regionale Lebensmittel 
"cczd041a" # Bezug Ökostrom
)

#Partei
q15f <- c("cczc043a") # Sonntagsfrage Wahlentscheidung standard edition

### Now let´s combine this into our dataframe with all questions from 2015

# first we combine all variables in one vector

q15_vars <- c(q15demo, q15a,q15aa,q15b,q15bb,q15c,q15d,q15e,q15f,q15z)

q15 <- q15[,c(id,q15_vars)]


#================================================#
# Questionnaires from 2016 ####
#================================================#
q16a <- c(
#"dczd001a", # Großstadtnähe Wohngegend 
"dczd002a", # NEP-Skala: Nähern uns Höchstzahl an Menschen 
"dczd003a", # NEP-Skala: Recht Umwelt an Bedürfnisse anzupassen 
"dczd004a", # NEP-Skala: Folgen von menschlichem Eingriff 
"dczd005a", # NEP-Skala: Menschlicher Einfallsreichtum 
"dczd006a", # NEP-Skala: Missbrauch der Umwelt durch Menschen 
"dczd007a", # NEP-Skala: Genügend natürliche Rohstoffe 
"dczd008a", # NEP-Skala: Pflanzen und Tiere gleiches Recht 
"dczd009a", # NEP-Skala: Gleichgewicht der Natur stabil genug 
"dczd010a", # NEP-Skala: Menschen Naturgesetzen unterworfen 
"dczd011a", # NEP-Skala: Umweltkrise stark übertrieben. 
"dczd012a", # NEP-Skala: Erde ist wie Raumschiff 
"dczd013a", # NEP-Skala: Menschen zur Herrschaft über Natur bestimmt 
"dczd014a", # NEP-Skala: Gleichgewicht der Natur ist sehr empfindlich 
"dczd015a", # NEP-Skala: Natur kontrollieren 
"dczd016a" # NEP-Skala: Umweltkatastrophe
)

#Nutzung und andere Meinungen
q16b <- c(
"dczd030a", # Meinung Atomausstieg 
"dczd031a", # Klimaschutzpolitik - Tempo 
"dczd032a", # Ernsthaftigkeit Problem Klimawandel 
"dczd033a", # Besitz ÖPNV-Karte 
"dczd034a", # Verfügbarkeit Auto 
"dczd035a", # Nutzungshäufigkeit: Auto 
"dczd036a", # Nutzungshäufigkeit: Fahrrad 
"dczd037a", # Nutzungshäufigkeit: Bus oder Bahn in der Region 
"dczd038a", # Nutzungshäufigkeit: Bahn auf längeren Strecken 
"dczd039a", # Einkauf Bio-Lebensmittel 
"dczd040a", # Einkauf Regionale Lebensmittel 
"dczd041a" # Bezug Ökostrom
)

### Now let´s combine this into our dataframe with all questions from 2016

# first we combine all variables in one vector

q16_vars <- c(q16a,q16b)
q16 <- q16[,c(id,q16_vars)]

#================================================#
# Questionnaires from 2017 ####
#================================================#

q17ident <- c(
"eabk081a", # Selbsteinschätzung: Im Einklang mit Natur 
"eabk082a", # Selbsteinschätzung: Von Umwelt entfremdet 
"eabk083a", # Selbsteinschätzung: Um Umwelt besorgt 
"eabk084a", # Selbsteinschätzung: Umwelt beschützend 
"eabk085a", # Selbsteinschätzung: Der Umwelt überlegen #
"eabk086a", # Selbsteinschätzung: Leidenschaftlicher Naturfreund 
"eabk087a", # Selbsteinschätzung: Respektlos gegenüber Umwelt #
"eabk088a", # Selbsteinschätzung: Unabhängig von Umwelt #
"eabk089a", # Selbsteinschätzung: Fürsprecher von Umweltbelangen 
"eabk090a", # Selbsteinschätzung: Bewahrer der Umwelt 
"eabk091a") # Selbsteinschätzung: Wehmütig bezüglich Umwelt #

q17pro <- c(
"eabk092a", # Zustimmung: Kauf Öko-Produkte Teil des 
"eabk093a", # Zustimmung: Lege keinen Wert auf Öko-Produkte 
"eabk094a", # Zustimmung: Konsument hat soziale Verantwortung 
"eabk095a" # Zustimmung: Kauf Öko-Produkte zeigt soziale Verantwortung
)

q17sal <- c(
"eabk096a", # Andere überzeugen: Klimawandel schwächen 
"eabk097a", # Andere überzeugen: Energiewende voranbringen 
"eabk098a", # Andere überzeugen: Bio-Lebensmittel besser 
"eabk099a", # Andere überzeugen: Elektroautos gehört Zukunft 
"eabk100a" # Andere überzeugen: Massentierhaltung verbieten
)

q17com <- c(
"eabk101a", # Mitgliedschaft Umweltschutzorganisation 
"eabk102a", # Bedeutung Wahrnehmung umweltbewusst Freunde 
"eabk103a" # Bedeutung Wahrnehmung umweltbewusst Familie
)

q17b <- c(
"eabk079a", # Lebensmitteleinkauf Wochenmärkten letzten zwei Wochen 
"eabk080a") # Letzter Einkauf Bio-Lebensmittel 

### Now let´s combine this into our dataframe with all questions from 2016

# first we combine all variables in one vector

q17 <- q17[,c(id,q17ident,q17pro,q17sal,q17com,q17b)]


#================================================#
# Data Combining and NA Handling ####
#================================================#

# Now we are going to create our raw data set with all necessary variables,
# we take the data from 2015 as a starting point, because this is where we have meat consumption in

raw <- q15 %>% left_join(demo, by = c("z000001a"))
raw <- raw %>% left_join(q16, by = c("z000001a"))
raw <- raw %>% left_join(q17, by = c("z000001a"))

# First let´s drop the variables we probably won´t need (just included them to not look for them again)
raw <- raw %>% select(-c(q15a,q15b,q15c))

# Now convert every missing value to NA
cols <- names(raw)
missing_values <- c(-11 ,-22 ,-33 ,-44 ,-55 ,-66 ,-77 ,-88 ,-99 ,-111)
raw[cols] <- lapply(raw[cols], function(x) replace(x,x %in% missing_values, NA) )


# rename id variable, because we will use this quite often
raw <- raw %>% rename("id" = "z000001a")


#================================================#
# Data Analysis ####
#================================================#

# let´s create a new data set here so we can return to raw data any time we want
dat <- raw

# delete all columns we don´t have an answer for meat consumption
dat <- dat %>% filter(!is.na(ceas097a))

# now delete all cases where we don´t have any information about the environmental identity

#let´s count the number of NA´s for our environmental identity item battery
dat$nas <- dat %>% 
  select(id, q17ident) %>% 
  is.na %>% 
  rowSums 

# now let´s delete the cases where we only have NAs for our environmental identity question
dat <- dat %>% filter(nas < length(q17ident))

#================================================#
# COMPLEX VARIABLE CONSTRUCTION ####
#================================================#

#================================================#
# IV: Environmental Identity ####
#================================================#

# Recoding the variable so that high score means environmental friendly
q17ident_rec <- q17ident[c(1,3,4,6,9,10,11)]


dat[, q17ident_rec] <- lapply(dat[, q17ident_rec], function(i) 
  ifelse(i == 1, 5, 
  ifelse(i == 2, 4,
  ifelse(i == 3, 3,
  ifelse(i == 4, 2,
  ifelse(i == 5, 1, NA))))))

#--------------------------------------------------#
# PCA (Environmental Identity)                             
#--------------------------------------------------#

### STEP 1

# Inspect correlation matrix

raq_matrix <- cor(dat[,q17ident], use="complete.obs") #create matrix
round(raq_matrix,3)

# Low correlations by variable

correlations <- as.data.frame(raq_matrix)
# Correlation plot

library(psych)
corPlot(correlations,numbers=TRUE,upper=FALSE,diag=FALSE,main="Correlations between variables")
# Check number of low correlations adn mean correlaiton per variable

diag(correlations) <- NA #set diagonal elements to missing
apply(abs(correlations) < 0.3, 1, sum, na.rm = TRUE) #count number of low correlations for each variable
apply(abs(correlations),1,mean,na.rm=TRUE) #mean correlation per variable
# Conduct Bartlett's test (p should be < 0.05)

# I see that item eabk085 has very low correlations with all the other variables, so I decide to delete them
# for the further analysis and repeat the steps aboce
q17ident <- q17ident[!(q17ident %in% c("eabk085a","eabk084a"))]

raq_matrix <- cor(dat[,q17ident], use="complete.obs") #create matrix
round(raq_matrix,3)

# Low correlations by variable

correlations <- as.data.frame(raq_matrix)
# Correlation plot

library(psych)
corPlot(correlations,numbers=TRUE,upper=FALSE,diag=FALSE,main="Correlations between variables")
# Check number of low correlations adn mean correlaiton per variable

diag(correlations) <- NA #set diagonal elements to missing
apply(abs(correlations) < 0.3, 1, sum, na.rm = TRUE) #count number of low correlations for each variable
apply(abs(correlations),1,mean,na.rm=TRUE) #mean correlation per variable
# Conduct Bartlett's test (p should be < 0.05)

cortest.bartlett(raq_matrix, n = nrow(dat))
# Count number of high correlations for each variable

apply(abs(correlations) > 0.8, 1, sum, na.rm = TRUE)

# Compute determinant (should be > 0.00001)
det(raq_matrix)
det(raq_matrix) > 0.00001

# Compute MSA statstic (should be > 0.5)
KMO(dat[,q17ident])

# we delete the fifth item due to very low correlation with all the other variables


### STEP 2

# Deriving factors

# Find the number of factors to extract
pc1 <- principal(dat[,q17ident], nfactors = length(q17ident), rotate = "none")
pc1

plot(pc1$values, type="b")
abline(h=1, lty=2)

# Run model with appropriate number of factors
pc2 <- principal(dat[,q17ident], nfactors = 2, rotate = "none")
pc2

### Inspect residuals
# Create residuals matrix
residuals <- factor.residuals(raq_matrix, pc2$loadings)
round(residuals,3)

# Create reproduced matrix
reproduced_matrix <- factor.model(pc2$loadings)
round(reproduced_matrix,3)

# Compute model fit manually (optional - also included in output)
ssr <- (sum(residuals[upper.tri((residuals))]^2)) #sum of squared residuals 
ssc <- (sum(raq_matrix[upper.tri((raq_matrix))]^2)) #sum of squared correlations
ssr/ssc #ratio of ssr and ssc
1-(ssr/ssc) #model fit
# Share of residuals > 0.05 (should be < 50%)

residuals <- as.matrix(residuals[upper.tri((residuals))])
large_res <- abs(residuals) > 0.05
sum(large_res)
sum(large_res)/nrow(residuals)

# Test if residuals are approximately normally distributed
hist(residuals)
qqnorm(residuals) 
qqline(residuals)
shapiro.test(residuals)

### STEP 3

# Oblique factor rotation (oblimin), because there is high correlation assumed
pc3 <- principal(dat[,q17ident], nfactors = 2, rotate = "oblimin", scores = TRUE)
print.psych(pc3, cut = 0.3, sort = TRUE)

# see: http://hosted.jalt.org/test/PDF/Brown31.pdf
# Interpretation of Factor Analysis tutorial: https://data.library.virginia.edu/getting-started-with-factor-analysis/



#--------------------------------------------------#
# headline2                             
#--------------------------------------------------#
# I will create two Factors now, first the factor with the two separate levels, second the factor with one combined level
q17ident_pca_1 <- q17ident[c(3:9)]
q17ident_pca_2 <- q17ident[c(1:2)]

dat <- cbind(dat, pc3$scores)

dat <- dat %>% rename("ei_fac1" = "TC1",
                      "ei_fac2" = "TC2")




# here I take the factors into one single variable
pc4 <- principal(dat[,q17ident], nfactors = 1, rotate = "none", scores = TRUE)
print.psych(pc4, cut = 0.3, sort = TRUE)

# Add factor scores to dataframe 
dat <- cbind(dat, pc4$scores)

dat <- dat %>% rename("ei_single" = "PC1")

### Creating an index as second approach next to using scores

# Calculate the sum of value of the identity (without the weighting of the scores)
dat$ei_sum <- dat %>% select(all_of(q17ident)) %>% 
  mutate(ei_sum = rowSums(., na.rm = TRUE)) %>% pull(ei_sum)
  
# calculate the NAs for all the variables
dat$ei_nas <- apply(dat[,q17ident], MARGIN = 1, function(x) sum(is.na(x)))


dat[,c("ei_sum","ei_nas")]
table(dat$ei_nas)


# if there are more than 50% Nas don´t calculate the mean, otherwise take the average
dat <- dat %>% mutate(ei_ind = ifelse(ei_nas > round(length(q17ident)/2), NA, 
                                  ifelse(ei_nas == 0, ei_sum/length(q17ident),
                                  ifelse(ei_nas !=0, ei_sum/(length(q17ident)-ei_nas),NA))))


# Additive Indexing, problem: Missings can not be handled
# dat$ei_ind_add <- dat$ei_sum/max(dat$ei_sum)*10

# With this approach I take into account that some values have missings and I´m simply
# normalizing the mean to a 1-10 Scale
# dat$ei_ind  <- dat$ei_ind/max(dat$ei_ind, na.rm = TRUE)*10



hist(dat$ei_index)
mean(dat$ei_index, na.rm = TRUE)
sd(dat$ei_index, na.rm = TRUE)

min(dat$ei_sum)
max(dat$ei_sum)


# make a plausibility check

dat %>% select(all_of(q17ident), ei_sum, ei_nas, ei_ind)


#--------------------------------------------------#
# Reliability Analysis                            
#--------------------------------------------------#
identity1 <- dat[,q17ident_pca_1]
identity2 <- dat[,q17ident_pca_2]
psych::alpha(identity1)
psych::alpha(identity2)


identity_single <- dat[,q17ident]
psych::alpha(identity_single)


dat %>% group_by(meat_con) %>% summarise_at(.vars = q17ident,
                                            .funs = mean, na.rm = TRUE)




# #--------------------------------------------------#
# # Another Factor Analysis approach                             
# #--------------------------------------------------#
# q17ident_pca <- q17ident[c(1,2,3,4,6,7,10,11)]
# 
# pc5 <- principal(dat[,q17ident_pca], nfactors = 1, rotate = "varimax", scores = TRUE)
# print.psych(pc5, cut = 0.3, sort = TRUE)
# 
# # Add factor scores to dataframe 
# dat <- cbind(dat, pc5$scores)
# 
# # for now just let´s name the PC1 as ei for (environmental identity)
# dat <- dat %>% rename("ei_scores_3" = "PC1")
# 
# dat %>% group_by(meat_con) %>% summarise_at(.vars = c("ei_scores_3", "ei_scores_2", "ei_scores"),
#                                             .funs = mean, na.rm = TRUE)
# 
# 
# ### Creating an index as second approach next to using scores
# 
# # Calculate the sum of value of the identity (without the weighting of the scores)
# dat$ei_sum <- dat %>% select(all_of(q17ident_pca)) %>% 
#   mutate(ei_sum = rowSums(., na.rm = TRUE)) %>% pull(ei_sum)
# 
# # calculate the NAs for all the variables
# dat$ei_nas <- apply(dat[,q17ident_pca], MARGIN = 1, function(x) sum(is.na(x)))
# 
# 
# dat[,c("ei_sum","ei_nas")]
# table(dat$ei_nas)
# 
# 
# # if there are more than 50% Nas don´t calculate the mean, otherwise take the average
# dat <- dat %>% mutate(ei_ind_2 = ifelse(ei_nas > round(length(q17ident_pca)/2), NA, 
#                                       ifelse(ei_nas == 0, ei_sum/length(q17ident_pca),
#                                              ifelse(ei_nas !=0, ei_sum/(length(q17ident_pca)-ei_nas),NA))))


#================================================#
# IV2: Salience, Prominence und Commitment  ####
#================================================#


#--------------------------------------------------#
# Reliability Analysis                             
#--------------------------------------------------#
# Specify subscales according to results of PCA
salience <- dat[,q17sal]
prominence <- dat[,q17pro]
commitment <- dat[,q17com]
# Test reliability of subscales
psych::alpha(salience)
psych::alpha(prominence, keys = c("eabk093a"))
psych::alpha(commitment, keys = c("eabk101a")) # Int: drop item eabk101a


# Now based on these Alphas let´s create our variables
#--------------------------------------------------#
# Identity Salience                            
#--------------------------------------------------#

# calculate the rowsums
dat$sal_sum <- dat %>% select(q17sal) %>% 
mutate(sum = rowSums(., na.rm = TRUE)) %>% pull(sum)

# calculate the NAs
dat$sal_nas <- apply(dat[,q17sal], MARGIN = 1, function(x) sum(is.na(x)))

# create new variable if NAs < 50%
dat <- dat %>% mutate(ident_sal = ifelse(sal_nas > 3, NA,
                     ifelse(sal_nas == 0 | sal_nas !=0, sal_sum/(5-sal_nas),NA)))


dat %>% select(q17sal, sal_sum, sal_nas, ident_sal)

#--------------------------------------------------#
# Identity Prominence                            
#--------------------------------------------------#

# first let´s recode the one item that is reverse coded
dat[,"eabk093a"] <-  
ifelse(dat$eabk093a == 1, 5, 
ifelse(dat$eabk093a == 2, 4,
ifelse(dat$eabk093a == 3, 3,
ifelse(dat$eabk093a == 4, 2,
ifelse(dat$eabk093a == 5, 1, NA)))))

# calculate the rowsums
dat$pro_sum <- dat %>% select(q17pro) %>% 
  mutate(sum = rowSums(., na.rm = TRUE)) %>% pull(sum)

# calculate the NAs
dat$pro_nas <- apply(dat[,q17pro], MARGIN = 1, function(x) sum(is.na(x)))

# create new variable if NAs < 50%
dat <- dat %>% mutate(ident_pro = ifelse(pro_nas > 2, NA,
                                  ifelse(pro_nas == 0 | pro_nas !=0, pro_sum/(4-pro_nas),NA)))

dat %>% select(q17pro, pro_sum, pro_nas, ident_pro)

#--------------------------------------------------#
# Identity commitment                            
#--------------------------------------------------#

# calculate the rowsums
dat$com_sum <- dat %>% select(q17com[2:3]) %>% 
  mutate(sum = rowSums(., na.rm = TRUE)) %>% pull(sum)

# calculate the NAs
dat$com_nas <- apply(dat[,q17com[2:3]], MARGIN = 1, function(x) sum(is.na(x)))

# create new variable if NAs < 50%
dat <- dat %>% mutate(ident_com = ifelse(com_nas > 1, NA,
                                   ifelse(com_nas == 0 | com_nas !=0, com_sum/(2-com_nas),NA)))


dat %>% select(q17com[2:3], com_sum, com_nas, ident_com)

# rename the eabk101a variable for further use 

dat <- dat %>% rename("envorga" = "eabk101a")

#--------------------------------------------------#
# DV: Meat consumption ####                           
#--------------------------------------------------#
dat <- dat %>% rename("meat_con" = "ceas097a")

# Recoding meat consumption so a low number means low meat consumption

dat[,"meat_con"] <-  
ifelse(dat$meat_con == 1, 7, 
ifelse(dat$meat_con == 2, 6,
ifelse(dat$meat_con == 3, 5,
ifelse(dat$meat_con == 4, 4,
ifelse(dat$meat_con == 5, 3,
ifelse(dat$meat_con == 6, 2,
ifelse(dat$meat_con == 7, 1,NA)))))))

# overview of cases
prop.table(table(dat$meat_con))

tabyl(dat$meat_con)

taby

# Creating a second meat_consumption variable that is easier to interpret

dat %>% group_by(meat_con) %>% 
  summarise(mean = mean(ei_single, na.rm = TRUE), n = n()) %>% 
  mutate(per = n / sum(n)*100) %>% 
  mutate(cum = cumsum(per))

dat <- dat %>% mutate(meat_concat = ifelse(meat_con %in% c(1,2), 1,
                                     ifelse(meat_con %in% c(3,4), 2,
                                     ifelse(meat_con %in% c(5), 3,
                                     ifelse(meat_con %in% c(6,7), 4, NA)))))

dat %>% group_by(meat_concat) %>% 
  summarise(mean = mean(ei_single, na.rm = TRUE), n = n()) %>% 
  mutate(per = n / sum(n)*100) %>% 
  mutate(cum = cumsum(per))

dat <- dat %>% mutate(meat_concat2 = ifelse(meat_con %in% c(1,2), 1,
                                     ifelse(meat_con %in% c(3,4), 2,
                                     ifelse(meat_con %in% c(5,6), 3,
                                     ifelse(meat_con %in% c(7), 4, NA)))))

dat %>% group_by(meat_concat2) %>% 
  summarise(mean = mean(ei_single, na.rm = TRUE), n = n()) %>% 
  mutate(per = n / sum(n)*100) %>% 
  mutate(cum = cumsum(per))



#--------------------------------------------------#
# CV: NEP Scale (Ecological Worldview) 2016 ####                          
#--------------------------------------------------#
q16a <- c(
#  "dczd001a", # Großstadtnähe Wohngegend 
  "dczd002a", # NEP-Skala: Nähern uns Höchstzahl an Menschen C
  "dczd003a", # NEP-Skala: Recht Umwelt an Bedürfnisse anzupassen A
  "dczd004a", # NEP-Skala: Folgen von menschlichem Eingriff B
  "dczd005a", # NEP-Skala: Menschlicher Einfallsreichtum A
  "dczd006a", # NEP-Skala: Missbrauch der Umwelt durch Menschen B
  "dczd007a", # NEP-Skala: Genügend natürliche Rohstoffe A
  "dczd008a", # NEP-Skala: Pflanzen und Tiere gleiches Recht B
  "dczd009a", # NEP-Skala: Gleichgewicht der Natur stabil genug A
  "dczd010a", # NEP-Skala: Menschen Naturgesetzen unterworfen B
  "dczd011a", # NEP-Skala: Umweltkrise stark übertrieben. A
  "dczd012a", # NEP-Skala: Erde ist wie Raumschiff C
  "dczd013a", # NEP-Skala: Menschen zur Herrschaft über Natur bestimmt A
  "dczd014a", # NEP-Skala: Gleichgewicht der Natur ist sehr empfindlich B
  "dczd015a", # NEP-Skala: Natur kontrollieren A
  "dczd016a" # NEP-Skala: Umweltkatastrophe B
)

#A: human domination of nature (RC2)
#B: balance of nature (RC1)
#C: limits to growth (RC3)
# Recoding the variable so that high score means "high ecological worldview"
q16a_nep_rec <- q16a[c(1,3,5,7,9,11,13,15)]

dat[, q16a_nep_rec] <- lapply(dat[, q16a_nep_rec], function(i) 
ifelse(i == 1, 5, 
ifelse(i == 2, 4,
ifelse(i == 3, 3,
ifelse(i == 4, 2,
ifelse(i == 5, 1, NA))))))

#--------------------------------------------------#
# PCA (NEP)                             
#--------------------------------------------------#

# STEP 1

# Inspect correlation matrix

raq_matrix <- cor(dat[,q16a], use="complete.obs") #create matrix
round(raq_matrix,3)

# Low correlations by variable

correlations <- as.data.frame(raq_matrix)
# Correlation plot

library(psych)
corPlot(correlations,numbers=TRUE,upper=FALSE,diag=FALSE,main="Correlations between variables")
# Check number of low correlations adn mean correlaiton per variable

diag(correlations) <- NA #set diagonal elements to missing
apply(abs(correlations) < 0.3, 1, sum, na.rm = TRUE) #count number of low correlations for each variable
apply(abs(correlations),1,mean,na.rm=TRUE) #mean correlation per variable
# Conduct Bartlett's test (p should be < 0.05)

cortest.bartlett(raq_matrix, n = nrow(dat))
# Count number of high correlations for each variable

apply(abs(correlations) > 0.8, 1, sum, na.rm = TRUE)
# Compute determinant (should be > 0.00001)

det(raq_matrix)
det(raq_matrix) > 0.00001

# Compute MSA statstic (should be > 0.5)

KMO(dat[,q16a])

# STEP 2

# Deriving factors
# Find the number of factors to extract
pc1 <- principal(dat[,q16a], nfactors = 15, rotate = "none")
pc1

plot(pc1$values, type="b")
abline(h=1, lty=2)

# Run model with appropriate number of factors

pc2 <- principal(dat[,q16a], nfactors = 3, rotate = "none")
pc2
# Inspect residuals

# Create residuals matrix
residuals <- factor.residuals(raq_matrix, pc2$loadings)
round(residuals,3)

# Create reproduced matrix
reproduced_matrix <- factor.model(pc2$loadings)
round(reproduced_matrix,3)
# Compute model fit manually (optional - also included in output)

ssr <- (sum(residuals[upper.tri((residuals))]^2)) #sum of squared residuals 
ssc <- (sum(raq_matrix[upper.tri((raq_matrix))]^2)) #sum of squared correlations
ssr/ssc #ratio of ssr and ssc
1-(ssr/ssc) #model fit

# Share of residuals > 0.05 (should be < 50%)
residuals <- as.matrix(residuals[upper.tri((residuals))])
large_res <- abs(residuals) > 0.05
sum(large_res)
sum(large_res)/nrow(residuals)

# Test if residuals are approximately normally distributed

hist(residuals)
qqnorm(residuals) 
qqline(residuals)
shapiro.test(residuals)

# STEP 3
# oblique factor rotation 
pc3 <- principal(dat[,q16a], nfactors = 3, rotate = "oblimin")
pc3
print.psych(pc3, cut = 0.3)


# Orthogonal factor rotation without the 07 item, which loads to high on other factors
pc3 <- principal(dat[,q16a[-6]], nfactors = 3, rotate = "varimax")
pc3
print.psych(pc3)

#--------------------------------------------------#
# Reliability Analysis                            
#--------------------------------------------------#

### Three Dimensional NEP Scale as suggested by PCA

# Specify subscales according to results of PCA
#A: human domination of nature (RC2)
#B: balance of nature (RC1)
#C: limits to growth (RC3)
nep_dom <- dat[,q16a[c(2,4,8,10,12,14)]]
nep_bal <- dat[,q16a[c(3,5,7,9,13,15)]]
nep_gro <- dat[,q16a[c(1,11)]]

# Test reliability of subscales
psych::alpha(nep_dom)
psych::alpha(nep_bal)
psych::alpha(nep_gro)

### One Dimensional NEP Scale
nep <- dat[,q16a]

# Test reliability of subscales
psych::alpha(nep)

#--------------------------------------------------#
# Constructing different NEP Variables                            
#--------------------------------------------------#

### Three Dimensional

#get the scores from the PCA in our original data frame
dat <- cbind(dat, pc3$scores)

# give factors meaningful names

dat <- dat %>% rename("nep_bal" = "RC1",
                      "nep_dom" = "RC2",
                      "nep_gro" = "RC3")



#--------------------------------------------------#
# One Dimensional Summing Index                             
#--------------------------------------------------#

# we will use all variables as in Dunlap(2000) stated

# Test reliability of subscales
q16a_ind <- q16a

nep <- dat[,q16a_ind]
psych::alpha(nep)

# Interesting comment on what to take https://www.theanalysisfactor.com/index-score-factor-analysis/

# Factor Scores
# Orthogonal factor rotation without the 07 item, which loads to high on other factors
pc3 <- principal(dat[,q16a], nfactors = 1, rotate = "none")
pc3
print.psych(pc3, cut = 0.3)

# now bind this only One Dimensional NEP factor to our original data frame
dat <- cbind(dat, pc3$scores)

dat <- dat %>% rename("nep_scores" = "PC1")


# Factor-Based Scores (Summed up and standardized to 1-10)
# excluding item 6 to keep things consistent (Might need to reverse this, depending on how I proceed)


# creating a sum over all the selected items
dat$nep_sum <- dat %>% select(q16a_ind) %>%
  mutate(sum = rowSums(., na.rm = TRUE)) %>% pull(sum)

# counting the NAs
dat$nep_nas <- apply(dat[,q16a_ind], MARGIN = 1, function(x) sum(is.na(x)))

# quick check
dat[,c(q16a_ind, "nep_sum", "nep_nas")]

# if there are more than 50% Nas don´t calculate the mean, otherwise take the average
dat <- dat %>% mutate(nep_ind = ifelse(nep_nas > round(length(q16a_ind)/2)  , NA, 
                                 ifelse(nep_nas == 0, nep_sum/length(q16a_ind),
                                 ifelse(nep_nas !=0, nep_sum/(length(q16a_ind)-nep_nas),NA))))


dat %>% select(q16a_ind,"nep_sum","nep_nas","nep_ind")



#--------------------------------------------------#
# CV: NEP Scale (Ecological Worldview) 2015 ####                          
#--------------------------------------------------#

q15d <- c(
  #"cczd001a", # Großstadtnähe Wohngegend
  "cczd002a", # NEP-Skala: Nähern uns Höchstzahl an Menschen 
  "cczd003a", # NEP-Skala: Recht Umwelt an Bedürfnisse anzupassen 
  "cczd004a", # NEP-Skala: Folgen von menschlichem Eingriff 
  "cczd005a", # NEP-Skala: Menschlicher Einfallsreichtum 
  "cczd006a", # NEP-Skala: Missbrauch der Umwelt durch Menschen 
  "cczd007a", # NEP-Skala: Genügend natürliche Rohstoffe 
  "cczd008a", # NEP-Skala: Pflanzen und Tiere gleiches Recht 
  "cczd009a", # NEP-Skala: Gleichgewicht der Natur stabil genug 
  "cczd010a", # NEP-Skala: Menschen Naturgesetzen unterworfen 
  "cczd011a", # NEP-Skala: Umweltkrise stark übertrieben. 
  "cczd012a", # NEP-Skala: Erde ist wie Raumschiff 
  "cczd013a", # NEP-Skala: Menschen zur Herrschaft über Natur bestimmt 
  "cczd014a", # NEP-Skala: Gleichgewicht der Natur ist sehr empfindlich 
  "cczd015a", # NEP-Skala: Natur kontrollieren 
  "cczd016a") # NEP-Skala: Umweltkatastrophe

#A: human domination of nature (RC2)
#B: balance of nature (RC1)
#C: limits to growth (RC3)
# Recoding the variable so that high score means "high ecological worldview"
q15d_nep_rec <- q15d[c(1,3,5,7,9,11,13,15)]

dat[,q15d_nep_rec] <- lapply(dat[, q15d_nep_rec], function(i) 
ifelse(i == 1, 5, 
ifelse(i == 2, 4,
ifelse(i == 3, 3,
ifelse(i == 4, 2,
ifelse(i == 5, 1, NA))))))

#--------------------------------------------------#
# PCA (Environmental Identity)                             
#--------------------------------------------------#

# STEP 1

# Inspect correlation matrix

raq_matrix <- cor(dat[,q15d], use="complete.obs") #create matrix
round(raq_matrix,3)

# Low correlations by variable

correlations <- as.data.frame(raq_matrix)
# Correlation plot

library(psych)
corPlot(correlations,numbers=TRUE,upper=FALSE,diag=FALSE,main="Correlations between variables")
# Check number of low correlations adn mean correlaiton per variable

diag(correlations) <- NA #set diagonal elements to missing
apply(abs(correlations) < 0.3, 1, sum, na.rm = TRUE) #count number of low correlations for each variable
apply(abs(correlations),1,mean,na.rm=TRUE) #mean correlation per variable


# let´s kick variables that don´t correlate with any others (< 0.3) and repeat the step above
q15d <- q15d[!(q15d %in% c("cczd007a","cczd002a","cczd010a"))]

# STEP 1

# Inspect correlation matrix

raq_matrix <- cor(dat[,q15d], use="complete.obs") #create matrix
round(raq_matrix,3)

# Low correlations by variable

correlations <- as.data.frame(raq_matrix)
# Correlation plot

library(psych)
corPlot(correlations,numbers=TRUE,upper=FALSE,diag=FALSE,main="Correlations between variables")
# Check number of low correlations adn mean correlaiton per variable

diag(correlations) <- NA #set diagonal elements to missing
apply(abs(correlations) < 0.3, 1, sum, na.rm = TRUE) #count number of low correlations for each variable
apply(abs(correlations),1,mean,na.rm=TRUE) #mean correlation per variable

# Conduct Bartlett's test (p should be < 0.05)

cortest.bartlett(raq_matrix, n = nrow(dat))
# Count number of high correlations for each variable

apply(abs(correlations) > 0.8, 1, sum, na.rm = TRUE)
# Compute determinant (should be > 0.00001)

det(raq_matrix)
det(raq_matrix) > 0.00001

# Compute MSA statstic (should be > 0.5)

KMO(dat[,q15d])

# STEP 2

# Deriving factors



# Find the number of factors to extract
pc1b <- principal(dat[,q15d], nfactors = length(q15d), rotate = "none")
pc1b

plot(pc1b$values, type="b")
abline(h=1, lty=2)

# Run model with appropriate number of factors

pc2b <- principal(dat[,q15d], nfactors = 2, rotate = "none")
pc2b
# Inspect residuals

# Create residuals matrix
residuals <- factor.residuals(raq_matrix, pc2b$loadings)
round(residuals,3)

# Create reproduced matrix
reproduced_matrix <- factor.model(pc2b$loadings)
round(reproduced_matrix,3)
# Compute model fit manually (optional - also included in output)

ssr <- (sum(residuals[upper.tri((residuals))]^2)) #sum of squared residuals 
ssc <- (sum(raq_matrix[upper.tri((raq_matrix))]^2)) #sum of squared correlations
ssr/ssc #ratio of ssr and ssc
1-(ssr/ssc) #model fit

# Share of residuals > 0.05 (should be < 50%)
residuals <- as.matrix(residuals[upper.tri((residuals))])
large_res <- abs(residuals) > 0.05
sum(large_res)
sum(large_res)/nrow(residuals)

# Test if residuals are approximately normally distributed

hist(residuals)
qqnorm(residuals) 
qqline(residuals)
shapiro.test(residuals)

# STEP 3
# Orthogonal factor rotation without the 07 item, which loads to high on other factors
pc3c <- principal(dat[,q15d], nfactors = 2, rotate = "varimax")
pc3c

print.psych(pc3c, cut = 0.3)

# Orthogonal factor rotation 
pc3b <- principal(dat[,q15d], nfactors = 2, rotate = "oblimin")
pc3b
print.psych(pc3b, cut = 0.3)

#--------------------------------------------------#
# Reliability Analysis                            
#--------------------------------------------------#

### Three Dimensional NEP Scale as suggested by PCA

# Specify subscales according to results of PCA
#A: human domination of nature (RC2)
#B: balance of nature (RC1)
#C: limits to growth (RC3)

#"cczd001a", # Großstadtnähe Wohngegend
#"cczd002a", # NEP-Skala: Nähern uns Höchstzahl an Menschen # deleted
#"cczd003a", # NEP-Skala: Recht Umwelt an Bedürfnisse anzupassen # TC1
#"cczd004a", # NEP-Skala: Folgen von menschlichem Eingriff #TC2
#"cczd005a", # NEP-Skala: Menschlicher Einfallsreichtum # TC1
#"cczd006a", # NEP-Skala: Missbrauch der Umwelt durch Menschen #TC2 
#"cczd007a", # NEP-Skala: Genügend natürliche Rohstoffe deleted
#"cczd008a", # NEP-Skala: Pflanzen und Tiere gleiches Recht #TC2 
#"cczd009a", # NEP-Skala: Gleichgewicht der Natur stabil genug # TC1 
#"cczd010a", # NEP-Skala: Menschen Naturgesetzen unterworfen deleted
#"cczd011a", # NEP-Skala: Umweltkrise stark übertrieben. # TC1
#"cczd012a", # NEP-Skala: Erde ist wie Raumschiff #TC2
#"cczd013a", # NEP-Skala: Menschen zur Herrschaft über Natur bestimmt # TC1
#"cczd014a", # NEP-Skala: Gleichgewicht der Natur ist sehr empfindlich #TC2
#"cczd015a", # NEP-Skala: Natur kontrollieren # TC1
#"cczd016a") # NEP-Skala: Umweltkatastrophe #TC2

#TC 1: Mensch Herrschaft über Natur
#TC 2: balance of nature



#--------------------------------------------------#
# Constructing different NEP Variables                            
#--------------------------------------------------#

### Three Dimensional

#get the scores from the PCA in our original data frame
dat <- cbind(dat, pc3b$scores)

# give factors meaningful names

dat <- dat %>% rename("nep_bal15" = "TC1",
                      "nep_dom15" = "TC2")



#--------------------------------------------------#
# Reliability Analysis                             
#--------------------------------------------------#


# Test reliability of subscales
nep_dom15 <- dat[,q15d[c(1,3,6,7,9,11)]]
nep_bal15 <- dat[,q15d[c(2,4,5,8,10,12)]]

# Test reliability of subscales
psych::alpha(nep_dom15)
psych::alpha(nep_bal15)



#--------------------------------------------------#
# creating a single NEP Scale                             
#--------------------------------------------------#

q15d_ind <- q15d

# Interesting comment on what to take https://www.theanalysisfactor.com/index-score-factor-analysis/

# Factor Scores
# Orthogonal factor rotation without the 07 item, which loads to high on other factors
pc4b <- principal(dat[,q15d], nfactors = 1, rotate = "none")
pc4b
print.psych(pc4b, cut = 0.3)

# now bind this only One Dimensional NEP factor to our original data frame
dat <- cbind(dat, pc4b$scores)

dat <- dat %>% rename("nep_single15" = "PC1")


# Factor-Based Scores (Summed up and standardized to 1-10)
# excluding item 6 to keep things consistent (Might need to reverse this, depending on how I proceed)


# creating a sum over all the selected items
dat$nep_sum <- dat %>% select(q15d_ind) %>%
  mutate(sum = rowSums(., na.rm = TRUE)) %>% pull(sum)

# counting the NAs
dat$nep_nas <- apply(dat[,q15d_ind], MARGIN = 1, function(x) sum(is.na(x)))

# quick check
dat[,c(q15d_ind, "nep_sum", "nep_nas")]

# if there are more than 50% Nas don´t calculate the mean, otherwise take the average
dat <- dat %>% mutate(nep_ind15 = ifelse(nep_nas > round(length(q15d_ind)/2)  , NA, 
                                       ifelse(nep_nas == 0, nep_sum/length(q15d_ind),
                                              ifelse(nep_nas !=0, nep_sum/(length(q15d_ind)-nep_nas),NA))))


dat %>% select(q15d_ind,"nep_sum","nep_nas","nep_ind15")

dat %>% select(nep_ind15, nep_ind, nep_scores, nep_single15)



#================================================#
# ADDITIONAL VARIABLE CONSTRUCTION ####
#================================================#


#--------------------------------------------------#
# Schätzung Fleischkonsum allgemeine Bevölkerung                               
#--------------------------------------------------#

dat <- dat %>% rename("m7" = "ceas098a", 
                      "m6" = "ceas099a", 
                      "m5" = "ceas100a", 
                      "m4" = "ceas101a", 
                      "m3" = "ceas102a", 
                      "m2" = "ceas103a", 
                      "m1" = "ceas104a")

# create a vector for the variables to handle them easier
vars = 
c("m7",
"m6",
"m5",
"m4",
"m3",
"m2",
"m1")

# reverse order of vector
vars <- rev(vars)
dat[,vars] <- lapply(dat[,vars], function(x) as.integer(x))




dat <- dat %>% mutate(meat_pop = ifelse(meat_con == 1, rowSums(.[vars[2:7]], na.rm = TRUE), 
                                 ifelse(meat_con == 2, rowSums(.[vars[3:7]], na.rm = TRUE),
                                 ifelse(meat_con == 3, rowSums(.[vars[4:7]], na.rm = TRUE),
                                 ifelse(meat_con == 4, rowSums(.[vars[5:7]], na.rm = TRUE),
                                 ifelse(meat_con == 5, rowSums(.[vars[6:7]], na.rm = TRUE),
                                 ifelse(meat_con == 6, rowSums(.[vars[7:7]], na.rm = TRUE),
                                 ifelse(meat_con == 7, 0, NA))))))))

dat$meat_pop_nas <- apply(dat[,vars], MARGIN = 1, function(x) sum(is.na(x)))

dat <- dat %>% mutate(meat_pop = ifelse(meat_pop_nas %in% c(6:7), NA, meat_pop))

# mehr als 50% der Gesellschaft isst mehr Fleisch als ich
dat <- dat %>% mutate(meat_pop_more = ifelse(meat_pop > 50, 1,0))

#--------------------------------------------------#
# Angemessenheit (Injunctive norm of eating meat)
#--------------------------------------------------#
dat <- dat %>% rename("meat_norm" = "ceas105a")

# First I recode the meat_norm variable so the coding scheme is the same as for meat consumption
dat[,"meat_norm"] <-  
ifelse(dat$meat_norm == 1, 7,
ifelse(dat$meat_norm == 2, 6,
ifelse(dat$meat_norm == 3, 5,
ifelse(dat$meat_norm == 4, 4,
ifelse(dat$meat_norm == 5, 3,
ifelse(dat$meat_norm == 6, 2,
ifelse(dat$meat_norm == 7, 1,NA)))))))

# + means that people eat more meat than what they think is appropiate, 
# - means that people eat less meat than what they think is appropiate
dat <- dat %>% mutate(meat_norm_diff = meat_con - meat_norm)


dat %>% select(meat_con, meat_norm, meat_norm_diff)

table(dat$meat_norm, useNA = "ifany")

# Recoding the variable
#4:  taeglich
#3:  3-6 Tage/Woche
#2:  1-2 Tage/ Woche
#1:  seltener/ gar nicht

#dat[,"meat_norm"] <-  
#ifelse(dat$meat_norm %in% c(1:2), 1, 
#ifelse(dat$meat_norm %in% c(3:4), 2,
#ifelse(dat$meat_norm == 5, 3,
#ifelse(dat$meat_norm %in% c(6:7), 4, NA))))

#--------------------------------------------------#
# Einschätzung Treibhausgase                            
#--------------------------------------------------#

q15bb <- c(
  "ceas107a", # Einschätzung Treibhausgase: Gemüse und Obst
  "ceas108a", # Einschätzung Treibhausgase: Fleisch und Fleischerzeugnisse
  "ceas109a", # Einschätzung Treibhausgase: Milch und Milcherzeugnisse
  "ceas110a", # Einschätzung Treibhausgase: Öl und Eier
  "ceas111a", # Einschätzung Treibhausgase: Zucker, Honig und Kakao
  "ceas112a", # Einschätzung Treibhausgase: Reis, Kartoffeln und Hülsenfrüchte
  "ceas113a" # Einschätzung Treibhausgase: Getreide und Getreideerzeugnisse
)

# transform to integers
dat[,q15bb] <- lapply(dat[,q15bb], function(x) as.integer(x))

# somehow there are odd values given as an answer, so convert all values >7 (max rank) to NA
dat[,q15bb] <- lapply(dat[,q15bb], function(x) ifelse(x > 7, NA, x))


# Create a new variable. If meat is ranked as 1st OR 2nd place and milk is place 1, give it 1 
# (meaning the person knows about the harm meat does to the environment)
dat <- dat %>% mutate(meat_know = ifelse(ceas108a == 1, 1,
                                  ifelse(ceas108a == 2 & ceas109a == 1,1,0)))

dat <- dat %>% mutate(meat_know2 = ifelse(ceas108a == 7, NA, ceas108a))

# Other Code that was tested (can be deleted if happy with results)
#dat <- dat %>% mutate(meat_know3 = ifelse(ceas108a %in% c(1,2), 1,0))
#dat <- dat %>% mutate(meat_know2 = ifelse(ceas108a %in% c(1), 1,0))

table(dat$meat_know, useNA = "ifany")

# quick check whether it worked as intended
#dat %>% select(ceas108a, ceas109a, meat_know3) %>% filter(ceas109a == 1, meat_know3 == 1)

#--------------------------------------------------#
# Mensch und Umwelt Scale  (evtl. noch hinzufügbar)                        
#--------------------------------------------------#
# q15c <- c(
#   "cbaq081a", # Mensch und Umwelt: Umweltverhältnissen für Nachfahren 
#   "cbaq082a", # Mensch und Umwelt: Umweltkatastrophe 
#   "cbaq083a", # Mensch und Umwelt: Durch Zeitungsberichte empört und wütend 
#   "cbaq084a", # Mensch und Umwelt: Grenzen des Wachstums überschritten 
#   "cbaq085a", # Mensch und Umwelt: Bevölkerung wenig umweltbewusst 
#   "cbaq086a", # Mensch und Umwelt: Umweltproblem übertrieben 
#   "cbaq087a", # Mensch und Umwelt: Politiker tun viel zu wenig für den Umweltschutz 
#   "cbaq088a", # Mensch und Umwelt: Lebensstandard einschränken 
#   "cbaq089a" # Mensch und Umwelt: Umweltschutzmaßnahmen trotz Arbeitsplatzverlusten
# )


#--------------------------------------------------#
# Ernsthaftigkeit Klimawandel (1 (not at all serious) - 11 (extremly serious))
#--------------------------------------------------#
dat <- dat %>% rename("ekw" = "cczd032a")

table(dat$ekw)

#--------------------------------------------------#
# Einkauf Bio Lebensmittel/ Regionale Lebensmittel                            
#--------------------------------------------------#
# 1 Nein, keines
# 2 Ja, teilweise
# 3 Ja, (fast) ausschließlich
# 98 Weiß nicht

dat <- dat %>% rename("lm_bio" = "cczd039a",
                      "lm_reg" = "cczd040a")

dat[, c("lm_bio", "lm_reg")] <- lapply(dat[, c("lm_bio", "lm_reg")], function(i) 
ifelse(i == 1, 0,
ifelse(i %in% c(2,3), 1,
ifelse(i == 98, NA, NA))))

table(dat$lm_bio, useNA = )

#--------------------------------------------------#
# Öko Strom                             
#--------------------------------------------------#
dat <- dat %>% rename("strom_öko" = "cczd041a")

dat[,"strom_öko"] <-  
ifelse(dat$strom_öko == 1, 3, 
ifelse(dat$strom_öko %in% c(2,3), 2,
ifelse(dat$strom_öko == 4, 1, NA)))

table(dat$strom_öko)
# 1 Beziehe ich bereits
# 2 Habe ich fest vor
# 3 Vielleicht zukünftig
# 4 Nein
# 98 Weiß nicht

#--------------------------------------------------#
# #Schwierigkeit eigenen Fleischkonsum zu reduzieren                            
#--------------------------------------------------#

### Comment: Lasse ich erstmal aus, könnte vielleicht später noch relevant werden

#dat <- dat %>% rename("meat_redu" = "cbas071a")


#================================================#
# SOCIODEMOGRAPHIC VARIABLE CONSTRUCTION ####
#================================================#

### Initial Inverview Sociodemographics

# "a11d054a", #Geschlecht
# "a11d056b", #Geburtsjahr
# "a11d082b", #Höchster Schulabschluss, inkl. o. A
# "a11d086b", Beruflicher Ausbildungsabschluss, inkl. o. A
# "a11d097c", #Haushaltseinkommen, 14 Kategorien standard edition
# "a11d096b") #Persönliches Einkommen, 15 Kategorien standard edition

### 2015 Interview Demographics

#q15demo <- 
#  c(
#    "cfzh071a", #Geschlecht
#    "cfzh072c", #Geburtsjahr
#    "cfzh078a", #Höchster Schulabschluss, inkl. o. A
#    "cfzh079a", #Berufsabschluss
#    "cfzh081a", #Universitätsabschluss
#    "cfzh084a", #Aktuell in beruflicher Ausbildung
#    "cfzh090c", #Haushaltseinkommen, 14 Kategorien standard edition
#    "cfzh089b") #Persönliches Einkommen, 15 Kategorien standard edition

#--------------------------------------------------#
# Sex                             
#--------------------------------------------------#

# due to changes in the sex throughout the survey, I decide to take the most recent 
# sex interpretation, if there is none available, I take the one from the initial 
# interview

dat$sex <-   
with(dat, 
ifelse(is.na(cfzh071a), a11d054a,
ifelse(cfzh071a != a11d054a, cfzh071a, cfzh071a)))

dat %>% select(cfzh071a, a11d054a, sex) %>% filter(is.na(cfzh071a))

# recode so male == 0, female == 1
dat$sex <- ifelse(dat$sex == 2,1,
           ifelse(dat$sex == 1,0,NA))

#--------------------------------------------------#
# create a new variable for age                             
#--------------------------------------------------#
dat$age <- 2020-dat$a11d056b


#--------------------------------------------------#
# Income (Household and Personal)                             
#--------------------------------------------------#

### Step 1: harmonize the income categories 

# househould income

# recoding the initial household income, so that the categories match with the most recent version
dat <- dat %>% 
  mutate(a11d097c = ifelse(a11d097c %in% c(1:2), 1,
                    ifelse(a11d097c %in% c(3:4), 2,
                    ifelse(a11d097c %in% c(5:6), 3,
                    ifelse(a11d097c %in% c(7:8), 4,
                    ifelse(a11d097c %in% c(9:10), 5,
                    ifelse(a11d097c %in% c(11), 6,
                    ifelse(a11d097c %in% c(12), 7,
                    ifelse(a11d097c %in% c(13), 8,
                    ifelse(a11d097c %in% c(14), 9, a11d097c))))))))))

#dat$a11d097c
#dat$cfzh090c
table(dat$a11d097c, useNA = "ifany")
table(dat$cfzh090c, useNA = "ifany")


# personal income
#dat$a11d096b
#dat$cfzh089b

table(dat$a11d096b, useNA = "ifany")
table(dat$cfzh089b, useNA = "ifany")
# Joint household
#dat$a11d081a
#dat$cfzh077a

# setting 98 to NA, because we can´t get any information from it
vars <- c("a11d097c", "cfzh090c", "a11d096b", "cfzh089b")
dat[,vars] <- lapply(dat[,vars], function(x) ifelse(x == 98, NA, x))

#dat %>% select(a11d097c, a11d096b, cfzh090c, cfzh089b, income_hh, income_p)

### step2: combine the income data (if there is none for 2015, use the one from the initial interview)

dat$income_hh <- with(dat, ifelse(is.na(cfzh090c), a11d097c, cfzh090c))
dat$income_p <- with(dat, ifelse(is.na(cfzh089b), a11d096b, cfzh089b))

### step3: deal with cases that don´t have a own personal income

# first let´s filter out people who have no information provided for either income
# convert househould categories to private income categories as close as possible

dat <- dat %>% mutate(income_p = ifelse(income_p == 97 & income_hh == 1, 2,
                                 ifelse(income_p == 97 & income_hh == 2, 3,
                                 ifelse(income_p == 97 & income_hh == 3, 4,
                                 ifelse(income_p == 97 & income_hh == 4, 5,
                                 ifelse(income_p == 97 & income_hh == 5, 7,
                                 ifelse(income_p == 97 & income_hh == 6, 9,
                                 ifelse(income_p == 97 & income_hh == 7, 10,
                                 ifelse(income_p == 97 & income_hh == 8, 12,
                                 ifelse(income_p == 97 & income_hh == 9, 13,
                                 ifelse(income_p == 97 & is.na(income_hh), NA, income_p)))))))))))

table(dat$income_p, useNA = "ifany")
table(dat$income_hh, useNA = "ifany")

dat %>% select(income_p, income_hh)


#--------------------------------------------------#
# Highest educational degree (ISCED 1997)                             
#--------------------------------------------------#

table(dat$a11d082b, useNA = "ifany")
table(dat$cfzh078a, useNA = "ifany")


dat <- dat %>% rename("edu" = "cfzh078a",
                      "job" = "cfzh079a",
                      "uni" = "cfzh081a")

#####
### edu 

#1 Schüler/-in
#2 Von der Schule abgegangen ohne Abschluss
#3 Abschluss nach höchstens 7 Jahren Schulbesuch (im Ausland)
#4 Polytechnische Oberschule DDR, Abschluss 8. oder 9. Klasse
#5 Polytechnische Oberschule DDR, Abschluss 10. Klasse
#6 Hauptschulabschluss, Volksschulabschluss
#7 Realschulabschluss, Mittlere Reife
#8 Fachhochschulreife
#9 Abitur, allgemeine oder fachgebundene Hochschulreife

### job

#1 Keinen beruflichen Ausbildungsabschluss
#2 Anlernausbildung oder ein berufliches Praktikum
#3 Berufsvorbereitungsjahr oder Berufsgrundbildungsjahr
#4 Abgeschlossene Lehre, Berufsausbildung im dualen System
#5 Abschluss einer Berufsfachschule, Kollegschule
#6 Laufbahnprüfung für den mittleren Dienst
#7 Abschluss einer einjährigen Schule des Gesundheitswesens
#8 Abschluss einer zwei- oder dreijährigen Schule des Gesundheitswesens
#9 Abschluss einer Ausbildungsstätte/ Schule für Erzieher/-innen
#10 Meister/-in, Techniker/-in oder gleichwertigen Fachschulabschluss
#11 Abschluss einer Fachschule DDR
#12 Abschluss einer Fachakademie (nur in Bayern)
#13 Anderer beruflicher Abschluss

### uni

#1 Keinen Abschluss einer (Fach-)Hochschule oder Universität
#2 Abschluss einer Berufsakademie
#3 Abschluss einer Verwaltungsfachhochschule
#4 Abschluss einer Fachhochschule
#5 Abschluss einer Universität

#####

addmargins(table(dat$edu, dat$job))

table(dat$edu)

# If respondents used 13 for job, we can´t be sure what ISCED to give, so we treat it as NA

dat$job[dat$job %in% 13] <- NA

dat$isced_cf <-
with(dat,
ifelse(job %in% c(8:12) | uni %in% c(2:5), 5,
ifelse(job %in% c(4:7) & edu %in% c(8:9), 4,
ifelse(is.na(job) & edu %in% c(8:9) | job %in% c(1:3) & edu %in% c(8:9) | job %in% c(4:7) & edu %in% c(1:7) | job %in% c(4:7) & is.na(edu), 3,
ifelse(is.na(job) & edu %in% c(3:7) | job == 1 & edu %in% c(3:7) | job %in% c(2:3) & edu %in% c(1:7), 2, 
ifelse(is.na(job) & edu %in% c(1:2) | job == 1 & edu %in% c(1,2), 1,
ifelse(is.na(job) & is.na(edu), NA, NA))))))
)


#--------------------------------------------------#
# Construction of ISCED from initial dataset                             
#--------------------------------------------------#

#####

# now get the isced for the initial interview to eventually fill up the missings
# unfortunately the items were different at the beginning


#1 Schüler/-in
#Student
#2 Von der Schule abgegangen ohne Hauptschulabschluss
#Left school without degree of a lower secondary school

#3 Hauptschulabschluss
#Lower secondary school
#4 Realschulabschluss
#Secondary school
#5 Polytechnische Oberschule DDR, Abschluss 8.oder 9. Klasse
#Polytechnic secondary school GDR, Degree 8th or 9th grade
#6 Polytechnische Oberschule DDR, Abschluss 10. Klasse
#Polytechnic secondary school GDR, Degree 10th grade

#7 Fachhochschulreife, Fachoberschule
#Advanced technical college certificate
#8 Abitur, allgemeine oder fachgebundene Hochschulreife
#General qualification for university entrance
#9 Anderer Schulabschluss
#Other degree

a11d086b

#1 Noch in beruflicher Ausbildung
#In vocational training
#2 Student/-in
#Student
#3 Schüler/-in an berufsorientiertre Aufbau-, Fachschule o. Ä.
#Student in a additional training course or vocational school

#4 Kein beruflicher Abschluss, nicht beruflicher Ausbildung
#Not in vocational training/study

#5 Beruflich-betriebliche Berufsausbildung
#Professional-occupational vocational training
#6 Beruflich-schulische Ausbildung
#Professional-educational vocational training

#7 Ausbildung an Fachschule der DDR
#Degree of a college GDR
#8 Ausbildung an Fach-, Meister-, Technikerschule,Berufs- oder
#Fachakademie
#Trainingat a college,master,technical school, vocational- or professional school
#9 Fachhochschulabschluss
#Technical College degree
#10 Universitätsabschluss
#University degree

#11 Anderer beruflicher Abschluss
#Other vocational education

#####

dat <- dat %>% rename("edu2" = "a11d082b",
                      "job2" = "a11d086b")

dat$isced_in <- 
with(dat,
ifelse(job2 %in% c(7:10), 5,      
ifelse(job2 %in% c(5:6,11) & edu2 %in% c(7:8), 4,    
ifelse(job2 %in% c(5:6,11) & edu2 %in% c(2:6,9) | job2 %in% c(1,3,4) & edu2 %in% c(7:8) | job2 == 2 & edu2 %in% c(7:8), 3,  
ifelse(job2 %in% c(1,3,4) & edu2 %in% c(3:6,9), 2,   
ifelse(job2 %in% c(1,3,4) & edu2 %in% c(1:2) | is.na(job2) & edu2 == 1, 1, NA))))))


dat %>% select(job2, edu2, isced_in) %>% filter(is.na(isced_in))

# comarison of ISCED, now let´s take a isced value from the most recent demographic data,
# if there is an NA, take the initial one
dat$isced <-
ifelse(is.na(dat$isced_cf), dat$isced_in, dat$isced_cf)

# check
dat %>% select(job, job2, edu, edu2, uni, isced, isced_in) 


#================================================#
# Setting up a summary descriptive table ####
#================================================#

# relevant variable list

vars <- c("ei_single","ei_fac1","ei_fac2","ident_sal","ident_pro","ident_com",
          "nep_bal15","nep_dom15","nep_single15",  "meat_con","meat_know","meat_pop", 
          "meat_pop_more", "meat_norm", "meat_norm_diff","lm_reg", 
          "lm_bio","strom_öko","ekw","envorga","age","sex","income_p", "income_hh", "isced", "edu", "job")

vars <- c("ei_single","ident_sal","ident_pro","ident_com",
          "nep_single15", "meat_con","meat_concat2", "meat_concat", "meat_know","meat_know2",
          "meat_pop_more", "meat_norm", "meat_norm_diff","lm_reg", 
          "lm_bio","strom_öko","age","sex","income_p","income_hh", "isced")


library(knitr)
library(kableExtra)

#dt <- mtcars[1:5, 1:6]
#kable(dt, "latex", booktabs = T) 
#df <- dat[1:10,vars]
#%>% as_image(file = "C:/Users/Ulli/Desktop/Thesis/thesis_small/graphics/descriptives/table1.png")
#%>%  save_kable("x", self_contained = TRUE, keep_tex = TRUE, latex_header_includes = c("\\usepackage[ngerman]{babel}"))



# create a data frame that has the information
tmp <- do.call(data.frame, 
               list(mean = apply(dat[,vars], 2, mean, na.rm= TRUE),
                    sd = apply(dat[,vars], 2, sd, na.rm= TRUE),
                    median = apply(dat[,vars], 2, median, na.rm= TRUE),
                    min = apply(dat[,vars], 2, min, na.rm= TRUE),
                    max = apply(dat[,vars], 2, max, na.rm= TRUE),
                    n = apply(dat[,vars], 2, length),
                    na = apply(dat[,vars], 2, function(x) sum(is.na(x)))) )

# changing names of columns/ rows
names(tmp)
rownames(tmp) <- c("Environmental Identity (Index)",
                   "Environmental Identity (Scores)", 
                   "Environmental Identity (Salience)")


# check whether we like the style
# library(magick)
# kable(tmp, "latex", booktabs = T, caption = "Überblick der Variablen") %>% as_image(file = "./test1.png")
# plot(magick::image_read("./test1.png"))


#================================================#
# Initial exploration ####
#================================================#


  

#--------------------------------------------------#
# Crosstabs                             
#--------------------------------------------------#

library(janitor)

dat %>% tabyl(ceas108a, ei_single, show_missing_levels = FALSE) 


dat %>% group_by(meat_con,ceas108a) %>% 
  summarise(n = n()) %>% 
  spread(ceas108a, n) %>%
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row", na.rm = TRUE) %>%
  adorn_pct_formatting() 
#   kable("latex", booktabs = T, caption = "Überblick der Variablen") %>% 
#   as_image(file = "./test1.png")
# plot(magick::image_read("./test1.png"))




dat %>% group_by(meat_con) %>% 
  filter(ceas108a == 1 & ident_pro > 3 & ei_single > 1.5) %>% 
  summarise(n = n()) %>% 
  adorn_percentages("col")


# environmental identity X meat consumption

dat %>% group_by(meat_con) %>% summarise(mean = mean(ei_single, na.rm = TRUE), n = n())


# environmental identity x prominence of identity

dat %>% 

# environmental identity X meat consumption for cases with high prominence

dat %>% group_by(meat_con) %>% filter(ident_pro > 3) %>%  summarise(mean = mean(ei_single, na.rm = TRUE), n = n())

# Angemessenheit Fleischkonsum (vor Hintergrund Treibhausgas) X meat_consumption
dat %>% group_by(meat_con) %>% summarise_at(.vars = c("meat_norm","meat_norm_diff","ident_pro", "ident_sal", "ei_single"),
                                            .funs = mean, na.rm = TRUE)


# amount of people who have a strong identity + high prominence and salience
x <- dat %>% 
  group_by(meat_con) %>% 
  filter(ident_pro > 3 & ident_sal > 3 & ei_single > 1) %>% 
  summarise(n = n())

y <- dat %>% group_by(meat_con) %>% summarise(n = n())

x %>% bind_cols(select(y, n)) %>% mutate(amount = n/n1*100)

# Einschätzung Treibhausgasausstoß X Norm wie viel Fleisch wäre okay zu essen?
dat %>% group_by(meat_norm) %>% summarise(mean = mean(ceas108a, na.rm = TRUE), n = n())

# only for people who understood question ceas 108a correct?
dat %>% group_by(meat_norm) %>% 
  filter(ceas108a %in% c(1:6)) %>%  
  summarise_at(.vars = c("ceas108a", "meat_know"),
               .funs = mean, na.rm = TRUE)


dat %>% tabyl(meat_con, ceas108a)

#%>% kable("latex", booktabs = T)



#--------------------------------------------------#
# Correlation Matrix                             
#--------------------------------------------------#

dat[,vars]

mcor <- round(cor(dat[,vars], use = "complete.obs"),2)

upper<-mcor
upper[upper.tri(mcor)]<-""
upper<-as.data.frame(upper)
upper


kable(upper, "latex", booktabs = T, caption = "Überblick der Variablen") %>% as_image(file = "./test1.png")
plot(magick::image_read("./test1.png"))



corstarsl <- function(x){
  x <- as.matrix(x)
  R <- Hmisc::rcorr(x)$r
  p <- Hmisc::rcorr(x)$P
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew)
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew)
}

corstarsl(dat[,vars])

test <- xtable::xtable(corstarsl(dat[,vars]))




table(dat$meat_know, dat$meat)


round(prop.table(xtabs(~meat_con + meat_norm_diff, data = dat),1),2)

prop.table(xtabs(~meat_con + meat_pop_more, data = dat),1)
prop.table(xtabs(~meat_know + meat_norm, data = dat),1)


dat %>% group_by(meat_con) %>% summarise(mean(meat_norm_diff, na.rm = TRUE))

ftable(xtabs(~ meat_con + meat_pop_more, data = dat))
ftable(xtabs(~ meat_con + mean(ei_ind) + meat_know, data = dat))
table(dat$meat_con, dat$meat_pop_more)





table(dat$meat_con)
hist(dat$ei_ind)
hist(dat$ei_scores)
hist(dat$ei_sum)

dat %>% group_by(meat_con) %>% summarise_at(.vars = c("ei_ind", "ei_scores", "ident_pro"),
                                            .funs = mean, na.rm = TRUE)

dat %>% group_by(meat_con) %>% summarise_at(.vars = ei_ind,
                                            .funs = mean, na.rm = TRUE)

dat %>% group_by(meat_con) %>% summarize(index = mean(ei_ind, na.rm = TRUE),
                                         scores = mean(ei_scores, na.rm = TRUE),
                                         sum = mean(ei_sum, na.rm = TRUE),
                                         com = mean(ident_com, na.rm = TRUE),
                                         sal = mean(ident_sal, na.rm = TRUE),
                                         pro = mean(ident_pro, na.rm = TRUE),
                                         env = mean(envorga, na.rm = TRUE),
                                         know = mean(meat_know, na.rm = TRUE),
                                         inc = mean(income_p, na.rm = TRUE),
                                         age = mean(age, na.rm = TRUE),
                                         isced = mean(isced, na.rm = TRUE))

dat %>% group_by(meat_con) %>% summarize(index = mean(nep_ind, na.rm = TRUE),
                                         scores = mean(nep_scores, na.rm = TRUE),
                                         sum = mean(nep_sum, na.rm = TRUE),
                                         nep1 = mean(nep_gro, na.rm = TRUE),
                                         nep2 = mean(nep_dom, na.rm = TRUE),
                                         nep3 = mean(nep_bal, na.rm= TRUE))

lm(meat_con ~ ei_scores, data = dat)

plot(jitter(dat$meat_con, amount = 0.2), jitter(dat$ei_ind, amount = 0.2))



### Check out whether the meat consumption is reduced when there is a high
# identity and it is prominent

dat %>% 
  group_by(meat_con) %>% 
  filter(ident_pro > 3 & ident_sal > 3 & ident_com > 3) %>% 
  summarise(ident = mean(ei_single, na.rm = TRUE),
            nep = mean(nep_single15, na.rm = TRUE),
            count = length(ei_single))

dat %>% 
  group_by(meat_con) %>% 
  filter(ident_pro < 3 & ident_sal < 3) %>% 
  summarise(mean = mean(ei_single, na.rm = TRUE),
            count = length(ei_single))


#================================================#
# INFERENCIAL STATISTICS ####
#================================================#




#--------------------------------------------------#
# Korrelationsmatrix                             
#--------------------------------------------------#
  



  

#--------------------------------------------------#
# Regression                             
#--------------------------------------------------#




reg1 <- lm(meat_con ~ ei_ind , data = dat)
summary(reg1)

vars

paste(vars, collapse = ' + ')



dat %>% group_by(ceas097a) %>% summarize(mean(new2, na.rm = TRUE))


library(ggplot2)
library(ggstatsplot)

ggplot(dat, mapping = aes(ei_ind, meat_con)) + 
  geom_point(shape = 1) +
  geom_smooth(method = "lm", fill = "blue", alpha = 0.1) + 
  geom_hline(yintercept = mean(dat$meat_con), linetype="dotted") + #mean of sales
  geom_vline(xintercept = mean(dat$ei_scores), linetype="dotted") + #mean of advertising
  labs(x = "Advertising expenditures (EUR)", y = "Number of sales") + 
  theme_bw()

scatterplot <- ggscatterstats(
  data = dat,
  x = ei_ind,
  y = meat_con,
  xlab = "Advertising expenditure (EUR)", # label for x axis
  ylab = "Sales", # label for y axis
  line.color = "black", # changing regression line color line
  title = "Advertising expenditure and Sales", # title text for the plot
  marginal.type = "histogram", # type of marginal distribution to be displayed
  xfill = "steelblue", # color fill for x-axis marginal distribution
  yfill = "darkgrey", # color fill for y-axis marginal distribution
  xalpha = 0.6, # transparency for x-axis marginal distribution
  yalpha = 0.6, # transparency for y-axis marginal distribution
  bf.message = FALSE,
  messages = FALSE # turn off messages and notes
)


ggbetweenstats(
  data = dat,
  x = ei_scores,
  y = meat_con,
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.annotation = "p.value",
  p.adjust.method = "bonferroni",
  effsize.type = "partial_eta",
  var.equal = FALSE,
  mean.plotting = TRUE, # whether mean for each group is to be displayed
  mean.ci = TRUE, # whether to display confidence interval for means
  mean.label.size = 2.5, # size of the label for mean
  type = "np", # which type of test is to be run
  k = 3, # number of decimal places for statistical results
  outlier.label.color = "darkgreen", # changing the color for the text label
  title = "Comparison of listening times between groups",
  xlab = "Experimental group", # label for the x-axis variable
  ylab = "Listening time", # label for the y-axis variable
  messages = FALSE,
  bf.message = FALSE
)
