# !diagnostics suppress=<job>
# !diagnostics off
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
library(tidyr)
library(janitor)
library(kableExtra)
library(knitr)
library(ggplot2)
library(ggstatsplot)

library(extrafont)
library(tikzDevice)
library(psych)
library(corrr)

options(digits = 2)
options(max.print = 100)
fontsize <- 11


#--------------------------------------------------#
# Defining Latex working path for graphics
#--------------------------------------------------#
setwd("C:/Users/Ulli/Desktop/Thesis/Data/meat")
wd <- "C:/Users/Ulli/Desktop/Thesis/Data/meat"
latexpath <- "C:/Users/Ulli/Desktop/Thesis/thesis_small/graphics/"

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
"eabk092a", # Zustimmung: Kauf Öko-Produkte Teil des Lebensstils
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
# creating descriptives                             
#--------------------------------------------------#


# create the labels
labels <-
  c(
    "Einklang (1)",
    "Verbunden (2)",
    "Besorgt (3)",
    "Beschützend (4)",
    "Unterlegen (5)",
    "Leidenschaftlich (6)",
    "Respektvoll (7)",
    "Abhängig (8)",
    "Fürsprecher (9)",
    "Bewahrer (10)",
    "Wehmütig (11)"
  )

#--------------------------------------------------#
# Creating a graph for the means of the environmental identity                             
#--------------------------------------------------#

# now calculate the means and the labels
# function to add standard errors if desired std <- function(x) sd(x)/sqrt(length(x))

eichart <- dat %>%
  select(q17ident) %>%
  drop_na(q17ident) %>%
  summarise_at(.vars = q17ident,
               .funs = mean,
               na.rm = TRUE) %>%
  gather(q17ident) %>%
  ggplot(aes(x = q17ident, y = as.numeric(value))) +
  geom_point() +
  geom_line(group = 1) +
  coord_flip() +
  geom_text(aes(label = round(value, 1)), hjust = -0.7, vjust = 1) +
  scale_x_discrete(name = "Umweltidentität\n", labels = eval(labels))  +
  scale_y_continuous(name = "Mittelwert", limits = c(1, 5)) +
  theme(text = element_text(size = 11)) +
  theme_light()
  #theme(text = element_text(size=16, family="CM Roman"))

#loadfonts()
pdf(paste0(latexpath,"descriptives/eichart.pdf"), height=6, width=6, family = "CM Roman")
print(eichart)
dev.off()

# working with different fonts in R Plots see
#https://github.com/wch/fontcm

# tikz(file = paste0(latexpath,"descriptives/eichart.tex"),width=3.5, height=3)
# eichart
# dev.off()


#--------------------------------------------------#
# PCA (Environmental Identity) -  2 factors                            
#--------------------------------------------------#


#--------------------------------------------------#
# Step 1                             
#--------------------------------------------------#


# Inspect correlation matrix

raq_matrix <- cor(dat[,q17ident], use="complete.obs") #create matrix
round(raq_matrix,3)

# Low correlations by variable

correlations <- as.data.frame(raq_matrix)
# Correlation plot

library(psych)
pdf(paste0(latexpath,"descriptives/eicors.pdf"), family = "CM Roman")
corPlot(correlations, numbers=TRUE, upper=FALSE, diag=TRUE, colors = FALSE, xlas = 2, zlim = c(0:1), labels = eval(labels))
dev.off()

# command to open pdf from R
# system2('open', args = paste0(latexpath,"descriptives/eicors.pdf"))
# Check number of low correlations adn mean correlaiton per variable

diag(correlations) <- NA #set diagonal elements to missing
apply(abs(correlations) < 0.3, 1, sum, na.rm = TRUE) #count number of low correlations for each variable
apply(abs(correlations),1,mean,na.rm=TRUE) #mean correlation per variable
# Conduct Bartlett's test (p should be < 0.05)
 
# I see that item eabk085 has very low correlations with all the other variables, so I decide to delete them
# for the further analysis and repeat the steps aboce
#q17ident <- q17ident[!(q17ident %in% c("eabk085a","eabk084a"))]
q17ident <- q17ident[!(q17ident %in% c("eabk085a"))]

raq_matrix <- cor(dat[,q17ident], use="complete.obs") #create matrix
round(raq_matrix,3)

# Low correlations by variable

correlations <- as.data.frame(raq_matrix)
# Correlation plot

library(psych)
corPlot(correlations,numbers=TRUE,upper=FALSE,diag=TRUE, colors = FALSE, main="Correlations between variables")
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


#--------------------------------------------------#
# Step 2                            
#--------------------------------------------------#

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

#--------------------------------------------------#
# Step 3                             
#--------------------------------------------------#

# Oblique factor rotation (oblimin), because there is high correlation assumed
pc3 <- principal(dat[,q17ident], nfactors = 2, rotate = "oblimin", scores = TRUE)
print.psych(pc3, cut = 0.3, sort = TRUE)

# see: http://hosted.jalt.org/test/PDF/Brown31.pdf
# Interpretation of Factor Analysis tutorial: https://data.library.virginia.edu/getting-started-with-factor-analysis/


# I will create two Factors now, first the factor with the two separate levels, second the factor with one combined level
q17ident_pca_1 <- q17ident[c(3:9)]
q17ident_pca_2 <- q17ident[c(1:2)]

dat <- cbind(dat, pc3$scores)

dat <- dat %>% rename("ei_fac1" = "TC1",
                      "ei_fac2" = "TC2")

#--------------------------------------------------#
# Reliability Analysis                            
#--------------------------------------------------#
identity1 <- dat[,q17ident_pca_1]
identity2 <- dat[,q17ident_pca_2]
identity1_rel <- psych::alpha(identity1)
identity2_rel <- psych::alpha(identity2)


#--------------------------------------------------#
# Creating the Latex Table                             
#--------------------------------------------------#

# creating a summary table in kable
# step 1: create the data frame with the loadings
eipca <- as.data.frame(unclass(pc3$loadings))
# cut out everything below 0.3
# eipca[ eipca < 0.3 ] <- ""


# step 2: extract the relevant other informations
# first: variance explained
var <- pc3$Vaccounted[4,]*100

#second: Eigenvalues
eig <- pc3$Vaccounted[1,]

# third: reliability analysis
rel1 <- identity1_rel$total[2]
rel2 <- identity2_rel$total[2]

# step 3: combine all in a data frame
eipca[nrow(eipca) + 1,] <- var
eipca[nrow(eipca) + 1,] <- eig
eipca[nrow(eipca) + 1,] <- c(rel1,rel2)

# assign row and column labels
rownames(eipca) <- c(labels[-5],"Erklärte Varianz in %","Eigenwert","Alpha")
colnames(eipca) <- c("Faktor 1","Faktor 2")

# create the table and save it
kable(eipca, format = "latex", booktabs = TRUE, digits = 2) %>%
  row_spec(10, hline_after = T) %>%
  kable_styling(font_size = fontsize) %>%
  save_kable(paste0(latexpath, "appendix/eipca"), keep_tex = TRUE)

setwd(wd)


#================================================#
# IV: Environmental Identity (Single Item) ####
#================================================#


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


# if there are more than 50% Nas don´t calculate the mean, otherwise take the average
dat <- dat %>% mutate(ei_ind = ifelse(ei_nas > round(length(q17ident)/2), NA, 
                               ifelse(ei_nas == 0, ei_sum/length(q17ident),
                               ifelse(ei_nas !=0, ei_sum/(length(q17ident)-ei_nas),NA))))


# Additive Indexing, problem: Missings can not be handled
# dat$ei_ind_add <- dat$ei_sum/max(dat$ei_sum)*10

# With this approach I take into account that some values have missings and I´m simply
# normalizing the mean to a 1-10 Scale
# dat$ei_ind  <- dat$ei_ind/max(dat$ei_ind, na.rm = TRUE)*10

# make a plausibility check

dat %>% select(all_of(q17ident), ei_sum, ei_nas, ei_ind)


identity_single <- dat[,q17ident]
identitysingle_rel <- psych::alpha(identity_single)

#--------------------------------------------------#
# Creating the Latex Table                             
#--------------------------------------------------#

# creating a summary table in kable
# step 1: create the data frame with the loadings
eipca_single <- as.data.frame(unclass(pc4$loadings))
# cut out everything below 0.3
# eipca[ eipca < 0.3 ] <- ""


# step 2: extract the relevant other informations
# first: variance explained
var <- pc4$Vaccounted[2,]*100

#second: Eigenvalues
eig <- pc4$Vaccounted[1,]

# third: reliability analysis
rel1 <- identitysingle_rel$total[2]


# step 3: combine all in a data frame
eipca_single[nrow(eipca_single) + 1,] <- var
eipca_single[nrow(eipca_single) + 1,] <- eig
eipca_single[nrow(eipca_single) + 1,] <- rel1

# create row and column labels
rownames(eipca_single) <- c(labels[-5],"Erklärte Varianz in %","Eigenwert","Alpha")
colnames(eipca_single) <- c("Umweltidentität")

# create the table and save it
kable(eipca_single, format = "latex", booktabs = TRUE, digits = 2) %>% 
  row_spec(10, hline_after = T) %>% 
  kable_styling(font_size = fontsize) %>% 
  save_kable(paste0(latexpath, "appendix/eipca_single"), keep_tex = TRUE)
setwd(wd)



#--------------------------------------------------#
# creating the histogram                             
#--------------------------------------------------#
pdf(paste0(latexpath,"descriptives/eihist.pdf"), family = "CM Roman", width = 8, height = 5)
#tikz(file = paste0(latexpath,"descriptives/eihist.tex"))
ggplot(dat, aes(x=ei_ind)) + 
  geom_histogram(bins=20, na.rm = TRUE) +
  theme_minimal() +
  labs(x = "Umweltidentität (Index)", y = "Häufigkeit\n") +
  geom_vline(xintercept = mean(dat$ei_ind, na.rm = TRUE), size = 1.5) +
  annotate("text", x=3.2, y=550, label= "Mittelwert (3.8)") +
  theme_set(theme_minimal(base_size = 11))

dev.off()


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

# Creating a second meat_consumption variable that is easier to interpret 
#(optional if later in analysis we need to rearrange our meat consumption variable)

# dat %>% group_by(meat_con) %>% 
#   summarise(mean = mean(ei_single, na.rm = TRUE), n = n()) %>% 
#   mutate(per = n / sum(n)*100) %>% 
#   mutate(cum = cumsum(per))
# 
# dat <- dat %>% mutate(meat_concat = ifelse(meat_con %in% c(1,2), 1,
#                                     ifelse(meat_con %in% c(3,4), 2,
#                                     ifelse(meat_con %in% c(5,6), 3,
#                                     ifelse(meat_con %in% c(7), 4, NA)))))
# 
# 
# dat %>% group_by(meat_concat) %>% 
#   summarise(mean = mean(ei_single, na.rm = TRUE), n = n()) %>% 
#   mutate(per = n / sum(n)*100) %>% 
#   mutate(cum = cumsum(per))
# 
# dat <- dat %>% mutate(meat_concat2 = ifelse(meat_con %in% c(1,2), 1,
#                                      ifelse(meat_con %in% c(3,4), 2,
#                                      ifelse(meat_con %in% c(5), 3,
#                                      ifelse(meat_con %in% c(6,7), 4, NA)))))
# 
# dat %>% group_by(meat_concat2) %>% 
#   summarise(mean = mean(ei_single, na.rm = TRUE), n = n()) %>% 
#   mutate(per = n / sum(n)*100) %>% 
#   mutate(cum = cumsum(per))



#--------------------------------------------------#
# creating a Latex Table
#--------------------------------------------------#
# create labels for the plot
labels <- c(
"An jedem Tag\n mehrmals",
"An jedem Tag\n einmal",
"An 5-6 Tagen\n pro Woche",
"An 3-4 Tagen\n pro Woche",
"An 1-2 Tagen\n pro Woche",
"Seltener",
"Gar nicht")

# reverse the labels to match the calculation
labels <- rev(labels)

# create a data frame with the relevant data
df <- dat %>% group_by(meat_con) %>% summarise(n = n())

# add the labels column
df$labs <- labels

# get percentages
df$total <- nrow(dat)
df$per <- round(df$n/df$total*100,0)

df$per <- paste0("(",df$per," %)")

# plot it
pdf(paste0(latexpath,"descriptives/meatconbar.pdf"), family = "CM Roman", width = 8.5 , height= 5)

ggplot(df, aes(x = labels, y=n, label = n)) + 
  geom_bar(stat = "identity", width = 0.2) + 
  scale_x_discrete(limits = labels) +
  geom_text(size = 3.5, position=position_dodge(width=0.9), vjust=-1.5) +
  geom_text(aes(label=per), size = 3.5, position=position_dodge(width=0.9), vjust=-0.25) +
  labs(x = "\n Eingeschätzter Fleischkonsum", y = "Häufigkeit\n") + 
  theme_minimal(base_size = 12) +
  scale_y_continuous(limits = c(0,800), n.breaks = 3)

dev.off()

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

# Recoding the variable so that high score means "high ecological worldview"
q15d_nep_rec <- q15d[c(1,3,5,7,9,11,13,15)]

dat[,q15d_nep_rec] <- lapply(dat[, q15d_nep_rec], function(i) 
ifelse(i == 1, 5, 
ifelse(i == 2, 4,
ifelse(i == 3, 3,
ifelse(i == 4, 2,
ifelse(i == 5, 1, NA))))))

# creating labels

nep_labels <- c("Höchstzahl (1)",
                "Bedürfnisse (2)",
                "Eingriff (3)",
                "Einfallsreichtum (4)",
                "Missbrauch (5)",
                "Rohstoffe (6)",
                "Recht (7)",
                "Gleichgewicht (8)",
                "Naturgesetze (9)",
                "Umweltkrise (10)",
                "Raumschiff (11)",
                "Herrschaft (12)",
                "Gleichgewicht2 (13)",
                "Kontrollieren (14)",
                "Katastrophe (15)")


#--------------------------------------------------#
# PCA for NEP - Scale                        
#--------------------------------------------------#

# STEP 1

# Inspect correlation matrix

raq_matrix <- cor(dat[,q15d], use="complete.obs") #create matrix
round(raq_matrix,3)

# Low correlations by variable

correlations <- as.data.frame(raq_matrix)
# Correlation plot
library(psych)
pdf(paste0(latexpath,"descriptives/nepcors.pdf"), family = "CM Roman")
corPlot(correlations, numbers=TRUE, upper=FALSE, diag=TRUE, colors = FALSE, xlas = 2, zlim = c(0:1), labels = nep_labels)
dev.off()
#knitr::plot_crop(paste0(latexpath,"descriptives/nepcors.pdf"))
# Check number of low correlations adn mean correlaiton per variable
diag(correlations) <- NA #set diagonal elements to missing
apply(abs(correlations) < 0.3, 1, sum, na.rm = TRUE) #count number of low correlations for each variable
apply(abs(correlations),1,mean,na.rm=TRUE) #mean correlation per variable

# let´s kick variables that don´t correlate with any others (< 0.3) and repeat the step above
# q15d <- q15d[!(q15d %in% c("cczd007a","cczd002a","cczd010a"))]


#--------------------------------------------------#
# STEP 1                             
#--------------------------------------------------#


# Inspect correlation matrix
raq_matrix <- cor(dat[,q15d], use="complete.obs") #create matrix
round(raq_matrix,3)

# Low correlations by variable
correlations <- as.data.frame(raq_matrix)
# Correlation plot 
corPlot(correlations,numbers=TRUE,upper=FALSE,diag=FALSE,main="Correlations between variables")
# Check number of low correlations adn mean correlaiton per variable

diag(correlations) <- NA #set diagonal elements to missing
apply(abs(correlations) < 0.3, 1, sum, na.rm = TRUE) #count number of low correlations for each variable
apply(abs(correlations),1,mean,na.rm=TRUE) #mean correlation per variable

# inter item total correlation

cors <- dat[,q15d]
cors$score <- rowMeans(cors)
item_total <- cors %>% correlate() %>% focus(score)
mean(item_total$score)
max(item_total$score)

# Conduct Bartlett's test (p should be < 0.05)
cortest.bartlett(raq_matrix, n = nrow(dat))

# Count number of high correlations for each variable
apply(abs(correlations) > 0.8, 1, sum, na.rm = TRUE)

# Compute determinant (should be > 0.00001)
det(raq_matrix)
det(raq_matrix) > 0.00001

# Compute MSA statstic (should be > 0.5)
KMO(dat[,q15d])


#--------------------------------------------------#
# STEP 2                             
#--------------------------------------------------#

# Find the number of factors to extract
pc1b <- principal(dat[,q15d], nfactors = 1, rotate = "none")
pc1b

plot(pc1b$values, type="b")
abline(h=1, lty=2)

# Run model with appropriate number of factors

pc2b <- principal(dat[,q15d], nfactors = 4, rotate = "none")
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
shapiro.test(residuals)


#--------------------------------------------------#
# STEP 3                             
#--------------------------------------------------#

# Orthogonal factor rotation without the 07 item, which loads to high on other factors
pc3c <- principal(dat[,q15d], nfactors = 4, rotate = "varimax")
pc3c
print.psych(pc3c, cut = 0.3)


#--------------------------------------------------#
# Creating the Latex Table                             
#--------------------------------------------------#

# creating a summary table in kable
# step 1: create the data frame with the loadings
neppca_single <- as.data.frame(round(unclass(pc3c$loadings),2))

# cut out everything below 0.3
# eipca[ eipca < 0.3 ] <- ""


# step 2: extract the relevant other informations
# first: variance explained
var <- pc3c$Vaccounted[2,]*100

#second: Eigenvalues
eig <- pc3c$Vaccounted[1,]

# third: reliability analysis
rel1 <- identity1_rel$total[2]
rel2 <- identity2_rel$total[2]

# step 3: combine all in a data frame
neppca_single[nrow(neppca_single) + 1,] <- var
neppca_single[nrow(neppca_single) + 1,] <- eig

nep_labels_fac <- 
  c("Limits",
    "Anti-Anthro",
    "Balance",
    "Anti-Exempt",
    "Eco-Crisis",
    "Limits",
    "Anti-Anthro",
    "Balance",
    "Anti-Exempt",
    "Eco-Crisis",
    "Limits",
    "Anti-Anthro",
    "Balance",
    "Anti-Exempt",
    "Eco-Crisis")


# create a new columns
neppca_single$Theorie <- NA

# fill the new column with labels
neppca_single[1:15,"Theorie"] <- nep_labels_fac
rownames(neppca_single) <- c(nep_labels,"Erklärte Varianz in %","Eigenwert")
colnames(neppca_single) <- c("Faktor 1","Faktor 2","Faktor 3","Faktor 4","Theorie")

neppca_single <- neppca_single %>% select(5,1:4)

neppca_single <- neppca_single[order(neppca_single[,1]),]

kable(neppca_single, format = "latex", booktabs = TRUE, digits = 2, linesep = c("", "", "\\addlinespace")) %>% 
  row_spec(15, hline_after = T) %>% 
  kable_styling(font_size = fontsize) %>% 
  save_kable(paste0(latexpath, "appendix/neppca"), keep_tex = TRUE)

setwd(wd)


#--------------------------------------------------#
# creating a single NEP Scale                             
#--------------------------------------------------#

# Interesting comment on what to take https://www.theanalysisfactor.com/index-score-factor-analysis/

# Factor Scores
# Orthogonal factor rotation without the 07 item, which loads to high on other factors
pc4b <- principal(dat[,q15d], nfactors = 1, rotate = "none", scores = TRUE, missing = TRUE)
pc4b
print.psych(pc4b, cut = 0.3)

# now bind this only One Dimensional NEP factor to our original data frame
dat <- cbind(dat, pc4b$scores)

dat <- dat %>% rename("nep_single15" = "PC1")

dat %>% select(q15d) %>% summarise_all(funs(sum(is.na(.))))

# Factor-Based Scores (Summed up and standardized to 1-10)
# excluding item 6 to keep things consistent (Might need to reverse this, depending on how I proceed)


# creating a sum over all the selected items
dat$nep_sum <- dat %>% 
  select(q15d) %>%
  mutate(sum = rowSums(., na.rm = TRUE)) %>% pull(sum)

# counting the NAs
dat$nep_nas <- apply(dat[,q15d], MARGIN = 1, function(x) sum(is.na(x)))

# if there are more than 50% NAs, set the scores to NA
dat <- dat %>% mutate(nep_single15 = ifelse(nep_nas > round(length(q15d)/2), NA, nep_single15))

# quick check
dat[,c(q15d, "nep_sum", "nep_nas")]

# if there are more than 50% Nas don´t calculate the mean, otherwise take the average
dat <- dat %>% mutate(nep_ind = ifelse(nep_nas > round(length(q15d)/2)  , NA, 
                                ifelse(nep_nas == 0, nep_sum/length(q15d),
                                ifelse(nep_nas !=0, nep_sum/(length(q15d)-nep_nas),NA))))

dat %>% select(q15d,"nep_sum","nep_nas","nep_ind")

#--------------------------------------------------#
# reliability                             
#--------------------------------------------------#
rel_nep <- psych::alpha(dat[,q15d])


#--------------------------------------------------#
# creating the latex table                             
#--------------------------------------------------#
# step 1: create the data frame
neppca_single <- as.data.frame(round(unclass(pc4b$loadings),2))

# step 2: extract the relevant other informations
# first: variance explained
var <- pc4b$Vaccounted[2,]*100

#second: Eigenvalues
eig <- pc4b$Vaccounted[1,]

# third: reliability analysis
rel1 <- rel_nep$total[2]

# get item-total correlations
corrs <- item_total$score

# create a new columns
neppca_single$corrs <- corrs

# step 3: combine all in a data frame
neppca_single[nrow(neppca_single) + 1,1] <- var
neppca_single[nrow(neppca_single) + 1,1] <- eig
neppca_single[nrow(neppca_single) + 1,1] <- rel1


# fill the new column with labels
rownames(neppca_single) <- c(nep_labels,"Erklärte Varianz in Prozent","Eigenwert","Cronbach's Alpha")


kable(neppca_single, format = "latex", booktabs = TRUE, digits = 2, 
      col.names = c("Faktor", linebreak("Item-Total\n Korrelation")), escape = FALSE) %>% 
  row_spec(15, hline_after = T) %>% 
  kable_styling(font_size = fontsize) %>% 
  save_kable(paste0(latexpath, "appendix/neppca_single"), keep_tex = TRUE)
setwd(wd)


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


dat %>% group_by(ceas108a, meat_con) %>% summarise(n = n()) %>% spread(ceas108a, n) %>% janitor::adorn_percentages()*100


#--------------------------------------------------#
# Creating Latex table
#--------------------------------------------------#

# create data frame
meat_know <- dat %>% drop_na(ceas108a) %>% 
  group_by(meat_con, ceas108a) %>% 
  summarise(n = n()) %>% 
  spread(ceas108a, n) %>%
  select(-'0') %>% 
  adorn_totals(where = "col") %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 0, affix_sign = FALSE)

# assign column labels
labels <- c(
  "An jedem Tag\n mehrmals",
  "An jedem Tag\n einmal",
  "An 5-6 Tagen\n pro Woche",
  "An 3-4 Tagen\n pro Woche",
  "An 1-2 Tagen\n pro Woche",
  "Seltener",
  "Gar nicht")

# turn vector around
labels <- rev(labels)

colnames(meat_know) <- c("Fleischkonsum",as.character(seq(1,7,1)),"Total")
meat_know[,1] <- labels


# creat the table and save it
meat_know %>% 
  kable(format = "latex", booktabs = TRUE, escape = FALSE) %>% 
  add_header_above(c(" " = 1,"Treibhausgasausstoß Fleisch (Rang)" = 7)) %>% 
  column_spec(2:8, width = "0.5cm") %>% 
  kable_styling(font_size = fontsize) %>% 
  save_kable(paste0(latexpath, "appendix/meatknow"), keep_tex = TRUE)

# set wd again, because somehow the function changes it
setwd(wd)


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
dat$age <- as.numeric(substr(Sys.time(),1,4))-dat$a11d056b

#--------------------------------------------------#
# Income (Household and Personal)                             
#--------------------------------------------------#

### Step 1: harmonize the income categories 

# household income
#dat$a11d097c
#dat$cfzh090c

# personal income
#dat$a11d096b
#dat$cfzh089b

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



# setting 98 to NA, because we can´t get any information from it
vars <- c("a11d097c", "cfzh090c", "a11d096b", "cfzh089b")
dat[,vars] <- lapply(dat[,vars], function(x) ifelse(x == 98, NA, x))

#dat %>% select(a11d097c, a11d096b, cfzh090c, cfzh089b, income_hh, income_p)

### step2: combine the income data (if there is none for 2015, use the one from the initial interview)
# how many cases are going to be affected by this? 

table(dat$cfzh090c, useNA = "ifany") # household income = 480 NAs
table(dat$cfzh089b, useNA = "ifany") # personal income = 242 NAs

dat$income_hh <- with(dat, ifelse(is.na(cfzh090c), a11d097c, cfzh090c))
dat$income_p <- with(dat, ifelse(is.na(cfzh089b), a11d096b, cfzh089b))

# after filling in the NAs from the initial interview
table(dat$income_hh, useNA = "ifany") # household income = 227 NAs
table(dat$income_p, useNA = "ifany") # personal income = 53 NAs


### step3: deal with cases that don´t have a own personal income or not information for household income

# first let´s filter out people who have no information provided for either income
# convert househould categories to private income categories as close as possible
# logic behind this: household income divided by 2, assuming that most adults share it with a partner

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

table(dat$income_p, useNA = "ifany") # 96 NAs
table(dat$income_hh, useNA = "ifany") # 227 NAs

# now let´s create a household income variable for cases we have no information
# on household income, but on personal income. We can use the conversion from above

dat %>% select(income_hh, income_p) %>%  filter(is.na(income_hh) & !is.na(income_p)) 

dat <- dat %>% mutate(income_hh = ifelse(is.na(income_hh) & income_p %in% c(1,2), 1,
                                 ifelse(is.na(income_hh) & income_p == 3, 2,
                                 ifelse(is.na(income_hh) & income_p == 4, 3,
                                 ifelse(is.na(income_hh) & income_p %in% c(5,6), 4,
                                 ifelse(is.na(income_hh) & income_p %in% c(7,8), 5,
                                 ifelse(is.na(income_hh) & income_p == 9, 6,
                                 ifelse(is.na(income_hh) & income_p %in% c(10,11), 7,
                                 ifelse(is.na(income_hh) & income_p == 12, 8,
                                 ifelse(is.na(income_hh) & income_p == 13, 9, income_hh))))))))))

# now let´s check the Nas again
table(dat$income_hh, useNA = "ifany") # household income_NAs: 96

#--------------------------------------------------#
# Highest educational degree (ISCED 1997)                             
#--------------------------------------------------#

table(dat$a11d082b, useNA = "ifany")
table(dat$cfzh078a, useNA = "ifany")


dat <- dat %>% rename("edu" = "cfzh078a",
                      "job" = "cfzh079a",
                      "uni" = "cfzh081a")

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
# INITIAL EXPLORATION ####
#================================================#
# renaming for commitment
dat <- dat %>% rename("ident_com_qual" = "ident_com",
                      "ident_com_quant" = "envorga")

dat$ident_com_quant <- ifelse(dat$ident_com_quant == 2, 0,
                       ifelse(dat$ident_com_quant == 1, 1,NA))
#================================================#
# Descriptive Summary Statistics ####
#================================================#

# relevant variable list
vars <- c("ei_ind","ident_pro","ident_sal","ident_com_qual","ident_com_quant",
          "nep_ind", "meat_con", "meat_know", "meat_norm","age","sex","income_hh","isced")

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
                    na = apply(dat[,vars], 2, function(x) sum(is.na(x)))))

# changing names of columns/ rows
varnames <- c("Umweltidentität",
              "Umweltidentität Wichtigkeit",
              "Umweltidentität Auslebungsgrad",
              "Umweltidentität Bekenntnis (qual.)", 
              "Umweltidentität Bekenntnis (quant.)",
              "Ökologisches Weltbild (NEP)",
              "Fleischkonsum",
              "Wissen Treibhausgasausstoß",
              "Angemessenheit Fleischkonsum",
              "Alter",
              "Geschlecht",
              "Haushaltseinkommen",
              "ISCED-1997")

# assign new rownames
rownames(tmp) <- varnames

# assign new colnames
colnames(tmp) <- c("MW","SA","MD","Min","Max","NA")

# save as a Latex table
tmp %>% 
  kable(format = "latex", booktabs = TRUE, escape = FALSE, align = "c") %>% 
  add_footnote(c("MW = Mittelwert, SA = Standardabweichung, MD = Median",
                 "Min = Minimum, Max = Maximum, NA = Fehlend"), notation="none", escape = TRUE) %>% 
  kable_styling(font_size = fontsize) %>% 
  save_kable(paste0(latexpath, "descriptives/summary"), keep_tex = TRUE)

# set wd again
setwd(wd)



# other descriptives

#================================================#
# Correlation Matrix####
#================================================#

# converting envorga to factor before transformation so 0 = "not in environmental organisation" and 1 = yes
# changing the ident_com_quant to be more logical (0 = no member of environmental organisation, 1 = yes, member)


# Correlation plot
cp <- corr.test(dat[,vars])
r <- cp$r
p <- cp$p


#gr <- colorRampPalette(c("#B52127", "white", "#2171B5")) other color palette
plotvars <- varnames
plotvars[c(4,5,8,9,11)] <- c("Umweltidentität\n Bekenntnis (qual.)",
                             "Umweltidentität\n Bekenntnis (quant.) - Mitglied",
                             "Wissen Treibhausgasausstoß - Ja",
                             "Angemessenheit\n Fleischkonsum",
                             "Geschlecht - weiblich")

# now save the graph as pdf
pdf(paste0(latexpath,"descriptives/varcors.pdf"), family = "CM Roman", paper= "USr", width = 11, height = 9)
corPlot(r, numbers=TRUE, 
        upper=FALSE, 
        diag=TRUE, 
        colors = TRUE, 
        zlim = c(-1,1),
        pval = p,
        stars = TRUE, 
        xlas = 2, 
        adjust = TRUE,
        cex = 0.8,
        n = 20,
        labels = plotvars,
        cex.axis = 0.8)
dev.off()

# command to check the file created
#system2("open", args = paste0(latexpath,"descriptives/varcors.pdf"))


### create a latex summary statistics table for the main variables
stat <- as.data.frame(unlist(apply(dat[,vars[c(11, 12, 13, 8, 9)]],MARGIN=2,table, useNA= "ifany")))

stat$total <- nrow(dat)
names(stat) <- c("var","total")

stat$per <- round(stat$var/stat$total,2)*100

labels <- 
c(
"männlich",
"weiblich",
"Unter 900",
"900 bis unter 1300",
"1300 bis unter 1700",
"1700 bis unter 2300",
"2300 bis unter 3200",
"3200 bis unter 4000",
"4000 bis unter 5000",
"5000 bis unter 6000",
"6000 und mehr",
"NA",
"Level 1",
"Level 2",
"Level 3",
"Level 4",
"Level 5",
"Weiß Bescheid",
"Weiß nicht Bescheid",
"NA",
"An jedem Tag mehrmals",
"An jedem Tag einmal",
"An 5-6 Tagen pro Woche",
"An 3-4 Tagen pro Woche",
"An 1-2 Tagen pro Woche",
"Seltener",
"Gar nicht",
"NA")

stat_matrix <- as.matrix(stat)
rownames(stat_matrix) <- labels
stat_matrix <- stat_matrix[,c("var","per")]

kable(stat_matrix, format = "latex", booktabs = TRUE, linesep = "", col.names = c("Häufigkeit","Prozent")) %>% 
  pack_rows(index=c("Geschlecht" = 2, 
                    "Haushaltseinkommen in Euro" = 10, 
                    "ISCED-1997" = 5, 
                    "Wissen Treibhausgasausstoß" = 3, 
                    "Angemessenheit Fleischkonsum" = 8)) %>% 
  kable_styling(font_size = fontsize) %>% 
  save_kable(paste0(latexpath, "descriptives/controlvarssummary"), keep_tex = TRUE)
setwd(wd)

#system2("open", args = "test.png")

#================================================#
# Crosstabs ####
#================================================#
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


# high prominence and knowledge
dat %>% group_by(meat_con) %>% summarise(mean = mean(ident_pro, na.rm = TRUE), 
                                         know = mean(meat_know2, na.rm = TRUE)) 



#================================================#
# INFERENCIAL STATISTICS ####
#================================================#

# creating factors
dat$meat_know <- factor(dat$meat_know, levels = c(0:1), labels = c("don´t know", "know"))
dat$sex <- factor(dat$sex, levels = c(0:1), labels = c("männlich", "weiblich"))
dat$ident_com_quant <- factor(dat$ident_com_quant, levels = c(0,1), labels = c("Kein Mitglied", "Mitglied"))

#dat$meat_pop_more <- as.factor(dat$meat_pop_more, levels = c(0:1), labels = c("Bev_weniger", "Bev_mehr"))


#--------------------------------------------------#
# Save the dataset for the regression                             
#--------------------------------------------------#
# save my complete data file
save(dat, file = "dat.Rda")

# load my vector for the regression vars
saveRDS(vars, "vars.rds")
saveRDS(varnames, "varnames.rds")
saveRDS(latexpath, "latexpath.rds")
# load my vector for the variable names

#================================================#
# REGRESSION ####
#================================================#


#--------------------------------------------------#
# load Regression specific libraries
#--------------------------------------------------#
library(car)
library(lm.beta)
library(stargazer)

# load all of them in my new environment
load("dat.Rda")
vars <- readRDS("vars.rds")
varnames <- readRDS("varnames.rds")
latexpath <- readRDS("latexpath.rds")

# erase all cases where we don´t have complete observations
# Note: We do this to make all the regressions comparable
dat <- dat[complete.cases(dat[,vars]), vars]


#--------------------------------------------------#
# Model 1: Identity 
#--------------------------------------------------#
reg1 <- lm(meat_con ~ ei_ind , data = dat)
reg1_beta <- lm.beta::lm.beta(reg1)

#--------------------------------------------------#
# Model 2: Identity + Identity Prominence
#--------------------------------------------------#
reg2 <- update(reg1, .~. + ident_pro + ident_sal + ident_com_qual + ident_com_quant)

#--------------------------------------------------#
# Model 2: Socials
#--------------------------------------------------#
reg3 <- update(reg2, .~.  + nep_ind + meat_know + meat_norm)

#--------------------------------------------------#
# Model 4 Control Variables                             
#--------------------------------------------------#
reg4 <- update(reg3, .~. + age + sex + income_hh + isced)
summary(reg4)
reg4_beta <- lm.beta::lm.beta(reg4)
df <- as.data.frame(reg4_beta$standardized.coefficients)


#--------------------------------------------------#
# Model 5 Interaction                             
#--------------------------------------------------#
reg5 <- update(reg4, .~. + ident_pro:meat_know)
summary(reg5)


# check the interaction
emmeans::emtrends(reg5, ~ meat_know, var="ident_pro")
emmeans::emtrends(reg5, pairwise ~ meat_know, var="ident_pro")

# create relevant information for the graph
mylist <- list(ident_pro=seq(1,5,by=0.25))

# raw data of the plot
plot.df <- emmeans::emmip(reg5, meat_know ~ ident_pro, at=mylist, plotit = FALSE)

pdf(paste0(latexpath,"descriptives/interaction.pdf"), width = 8, height = 5, family = "CM Roman")
plot.df %>% 
  ggplot(aes(x = xvar, y = yvar, color = tvar)) +
  geom_line(size = 1) +
  geom_point() +
  geom_errorbar(aes(ymin=yvar-SE, ymax=yvar+SE), width=.1, size = 1,
                position=position_dodge(0.05)) +
  scale_y_continuous(name = "Fleischkonsum", limits = c(4,6)) +
  scale_x_continuous(name = "Wichtigkeit Umweltidentität") + 
  scale_color_discrete(name = "Wissen\nTreibhausgasausstoß\nFleisch", labels = c("Kein Wissen", "Wissen"))
dev.off()




#--------------------------------------------------#
# Creating a stargazer Result Table
#--------------------------------------------------#

varnames[c(8,11)] <- c("Wissen Treibhausgasausstoß - Ja","Geschlecht - weiblich")

# grab the content of the stargazer output
table <- capture.output({ stargazer(reg1, reg2, reg3, reg4, reg5,
                    single.row = FALSE,
                    label = "regression",
                    title= "Ergebnisse der Regressionsmodelle",
                    ci = TRUE,
                    ci.separator = " ; ",
                    digits = 2,
                    initial.zero = TRUE,
                    omit.stat=c("LL","ser","f"),
                    star.cutoffs = c(0.05, 0.01, 0.001),
                    covariate.labels = c(varnames[-7],"Interaktion (Wichtigkeit:Wissen)"),
                    dep.var.caption  = "Abhängige Variable",
                    dep.var.labels   = "Eingesch{\"a}tzter Fleischkonsum",
                    type = "latex",
                    align = TRUE,
                    out = paste0(latexpath,"descriptives/regression.tex"),
                    table.placement = "H")
})

# add resizebox at the beginning and end
table <- gsub("\\begin{tabular}","\\resizebox{\\linewidth}{!}{\\begin{tabular}", table,fixed=T)
table <- gsub("\\end{tabular}","\\end{tabular}}", table,fixed=T)

# add german special characters
table <- gsub("ä","{\"a}", table,fixed=T)
table <- gsub("ö","{\"o}", table,fixed=T)
table <- gsub("Ö","{\"O}", table,fixed=T)
table <- gsub("ü","{\"u}", table,fixed=T)
table <- gsub("ß","{\\ss}", table,fixed=T)


# save the modified file as .tex file
writeLines(table, paste0(latexpath,"descriptives/regression.tex"))

#================================================#
# Regression Diagnostics ####
#================================================#

#--------------------------------------------------#
# Outliers                             
#--------------------------------------------------#
dat$stud_resid <- rstudent(reg4)

pdf(paste0(latexpath,"appendix/residuals.pdf"), family = "CM Roman")
plot(1:nrow(dat),dat$stud_resid, ylim=c(-3.3,3.3), ylab = "Standardisierte Residuen", xlab = "Fälle"  ) #create scatterplot 
abline(h=c(-2,2),col="red",lty=2) #add reference lines
abline(h=c(-2.5,2.5),col="red",lty=2) #add reference lines
dev.off()


outliers <- subset(dat,abs(stud_resid)>2)
nrow(outliers)/nrow(dat)

outliers <- subset(dat,abs(stud_resid)>2.5)
nrow(outliers)/nrow(dat)



#--------------------------------------------------#
# Influencial cases
#--------------------------------------------------#
pdf(paste0(latexpath,"appendix/cooksdistance.pdf"), family = "CM Roman")
plot(reg4,4)
dev.off()

dat$cooks.distance<-cooks.distance(reg4)



#--------------------------------------------------#
# Independence of Errors                             
#--------------------------------------------------#
#The assumption of independent errors implies that for any two observations the residual terms should be uncorrelated. This is also known as a lack of autocorrelation. 
dwt(reg4)

#Der Durbin Watson Test zum Testen für unabhängige Fehler liegt bei 2.02, was weit über den Grenzwerten von 1 und weit unter 3 liegt,
# Außerdem ist der Test mit p = 0.55 weit weg davon signifikant zu sein. (292)


#--------------------------------------------------#
# Multicollinearity                             
#--------------------------------------------------#
vif(reg4)

# die vif Toleranz
1/vif(reg4)

# Durchschnittlicher vif
mean(vif(reg4))

#--------------------------------------------------#
# heteroscedasticity
#--------------------------------------------------#
# get fittet values in the original data set
dat$fitted <- reg4$fitted.values

# heteroscedasticity test
pdf(paste0(latexpath,"appendix/hetero.pdf"), family = "CM Roman")
ggplot(dat, aes(fitted, stud_resid)) + 
  geom_point() + 
  geom_smooth(method = "lm", colour = "Blue") + 
  labs(x = "Geschätzte Werte", y = "Standardisierte Residuen") +
  theme_minimal()
dev.off()
  
# Breusch-Pagan Test
library(lmtest)
bptest(reg4)

# If test is significant, transform the data, or use robust SE's:
library(sandwich)
coeftest(reg4, vcov = vcovHC(reg4))


#--------------------------------------------------#
# Normal Distribution of Residuals
#--------------------------------------------------#
pdf(paste0(latexpath,"appendix/normalverteilung.pdf"), family = "CM Roman")
ggplot(dat, aes(stud_resid)) + 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  labs(x = "Standardisierte Residuen", y = "Dichte") + 
  stat_function(fun = dnorm, 
  args = list(mean = mean(dat$stud_resid, na.rm = TRUE), 
                     sd = sd(dat$stud_resid, na.rm = TRUE)), colour = "blue", size = 1)+
  theme_minimal()
dev.off()
# shapiro
shapiro.test(resid(reg4))

# If the residuals do not follow a normal distribution, transform the data or use bootstrapping
library(boot)
# function to obtain dat coefficients
bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(coef(fit))
}
# bootstrapping with 1000 replications
boot_out <- boot(data=dat, statistic=bs, R=6000, formula = meat_con ~ ei_ind + ident_pro + ident_sal + ident_com_qual + 
                   ident_com_quant + nep_ind + meat_know + meat_norm + age + 
                   sex + income_hh + isced)
# view results
summary(boot_out)
plot(boot_out, index=1)
plot(boot_out, index=2) 
plot(boot_out, index=3) 
plot(boot_out, index=4) 
# get 95% confidence intervals
boot.ci(boot_out, type="bca", index=1)  
boot.ci(boot_out, type="bca", index=1)  
boot.ci(boot_out, type="bca", index=2)  # ei_ind
boot.ci(boot_out, type="bca", index=3)  # ident_pro
boot.ci(boot_out, type="bca", index=4)  # ident_sal
boot.ci(boot_out, type="bca", index=5)  # ident_com_qual
boot.ci(boot_out, type="bca", index=6)  # ident_com_quant
boot.ci(boot_out, type="bca", index=7)  # nep_ind
boot.ci(boot_out, type="bca", index=8)  # meat_know
boot.ci(boot_out, type="bca", index=9)  # meat_norm
boot.ci(boot_out, type="bca", index=10) # age
boot.ci(boot_out, type="bca", index=11) # sex
boot.ci(boot_out, type="bca", index=12) # income_hh
boot.ci(boot_out, type="bca", index=13) # isced

#-------------------------------------------------------------------#
#-------------------------Variable selection------------------------#####
#-------------------------------------------------------------------#

set.seed(123)
# Add another random variable
dat$var_test <- rnorm(nrow(dat),0,1)

# Model comparison with anova
anova(reg4,reg5)
