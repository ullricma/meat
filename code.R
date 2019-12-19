library(haven)
library(dplyr)

#================================================#
# Preparing the R Session ####
#================================================#

req_packages <- c("Hmisc", "ggplot2", "car", "psych", "GPArotation","hornpa")
req_packages <- req_packages[!req_packages %in% installed.packages()]
lapply(req_packages, install.packages)

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
# Grab necessary columns ####
#================================================#

id <- "z000001a"


# demographics
vars <- 
c(
"a11d054a", #Geschlecht
"a11d056b", #Geburtsjahr
"a11d082b", #Höchster Schulabschluss, inkl. o. A
"a11d097c", #Haushaltseinkommen, 14 Kategorien standard edition
"a11d096b") #Persönliches Einkommen, 15 Kategorien standard edition

demo <- demo[,c(id, vars)]

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
"ceas098a", # Einschätzung Fleischkonsum: An jedem Tag mehrmals 
"ceas099a", # Einschätzung Fleischkonsum: An jedem Tag einmal 
"ceas100a", # Einschätzung Fleischkonsum: An 5-6 Tagen pro Woche 
"ceas101a", # Einschätzung Fleischkonsum: An 3-4 Tagen pro Woche 
"ceas102a", # Einschätzung Fleischkonsum: An 1-2 Tagen pro Woche 
"ceas103a", # Einschätzung Fleischkonsum: Seltener 
"ceas104a", # Einschätzung Fleischkonsum: Personen, die nie Fleisch essen 
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

#CC 08/2015
q15d <- c(
"cczd001a", # Großstadtnähe Wohngegend
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
q15d <- c(
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
q15e <- c("cczc043a") # Sonntagsfrage Wahlentscheidung standard edition

### Now let´s combine this into our dataframe with all questions from 2015

# first we combine all variables in one vector

q15_vars <- c(q15a,q15aa,q15b,q15bb,q15c,q15d,q15e)

q15 <- q15[,c(id,q15_vars)]


#================================================#
# Questionnaires from 2016 ####
#================================================#
q16a <- c(
"dczd001a", # Großstadtnähe Wohngegend 
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
"eabk085a", # Selbsteinschätzung: Der Umwelt überlegen 
"eabk086a", # Selbsteinschätzung: Leidenschaftlicher Naturfreund 
"eabk087a", # Selbsteinschätzung: Respektlos gegenüber Umwelt 
"eabk088a", # Selbsteinschätzung: Unabhängig von Umwelt 
"eabk089a", # Selbsteinschätzung: Fürsprecher von Umweltbelangen 
"eabk090a", # Selbsteinschätzung: Bewahrer der Umwelt 
"eabk091a") # Selbsteinschätzung: Wehmütig bezüglich Umwelt

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
# Data Combining and Cleaning ####
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

# create a new variable for age
raw$age <- 2020-raw$a11d056b

# rename some variables, because we will use them quite often
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
# IV: Environmental Identity####
#================================================#

#================================================#
# PCA ####
#================================================#

# Load data

raq_data <- read.table("https://raw.githubusercontent.com/IMSMWU/Teaching/master/MRDA2017/raq.dat", 
                       sep = "\t", 
                       header = TRUE) #read in data
head(raq_data)

# STEP 1

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

cortest.bartlett(raq_matrix, n = nrow(raq_data))
# Count number of high correlations for each variable

apply(abs(correlations) > 0.8, 1, sum, na.rm = TRUE)
# Compute determinant (should be > 0.00001)

det(raq_matrix)
det(raq_matrix) > 0.00001

# Compute MSA statstic (should be > 0.5)

KMO(dat[,q17ident])

# STEP 2

# Deriving factors

# Find the number of factors to extract
pc1 <- principal(dat, nfactors = length(q17ident), rotate = "none")
pc1

plot(pc1$values, type="b")
abline(h=1, lty=2)

# Run model with appropriate number of factors

pc2 <- principal(dat[,q17ident], nfactors = 4, rotate = "none")
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

# Orthogonal factor rotation 

pc3 <- principal(dat[,q17ident], nfactors = 4, rotate = "varimax")
pc3
print.psych(pc3, cut = 0.3, sort = TRUE)

# Oblique factor rotation 

pc4 <- principal(dat[,q17ident], nfactors = 4, rotate = "oblimin", scores = TRUE)
print.psych(pc4, cut = 0.3, sort = TRUE) # QUESTION: What to do with the other factors?

# STEP 4

# Compute factor scores 

head(pc4$scores)
# Add factor scores to dataframe 

dat <- cbind(dat, pc4$scores)

# for now just let´s name the TC1 as ei for (environmental identity)

dat <- dat %>% rename("ei" = "TC1")

#================================================#
# Reliability Analysis ####
#================================================#

# Specify subscales according to results of PCA

identity <- dat[,q17a[c(9,3,11,10,4,6)]]
salience <- dat[,q17sal]
prominence <- dat[,q17pro]
commitment <- dat[,q17com]
# Test reliability of subscales

psych::alpha(identity)
psych::alpha(salience)
psych::alpha(prominence, keys = c("eabk093a"))
psych::alpha(commitment, keys = c("eabk101a"))



#================================================#
# DESCRIPTIVE STATISTICS ####
#================================================# 
dat$ceas097a

# meat consumption
prop.table(table(dat$ceas097a))

# age
table(dat$age)

# gender
table(dat$a11d054a)

# dat %>% group_by(a11d054a) %>% summarize(x = mean(ceas097a))

dat %>% group_by(ceas097a) %>% summarise_at(.vars = q17ident,
                                            .funs = mean, na.rm = TRUE)

#================================================#
# INFERENCIAL STATISTICS ####
#================================================#


dat %>% group_by(ceas097a) %>% summarize(mean(new2, na.rm = TRUE))

dat$new2 <- dat %>% select(q17a[c(9,3,11,10,4,6)], new2) %>% rowSums

dat$new2 <- dat$new2/6


dat$new <- with(dat,)

library(ggplot2)
ggplot(dat, mapping = aes(ei, ceas097a)) + 
  geom_point(shape = 1) +
  geom_smooth(method = "lm", fill = "blue", alpha = 0.1) + 
  geom_hline(yintercept = mean(dat$ceas097a), linetype="dotted") + #mean of sales
  geom_vline(xintercept = mean(dat$ei), linetype="dotted") + #mean of advertising
  labs(x = "Advertising expenditures (EUR)", y = "Number of sales") + 
  theme_bw()

scatterplot <- ggscatterstats(
  data = dat,
  x = new2,
  y = ceas097a,
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
  x = ceas097a,
  y = new2,
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.annotation = "p.value",
  p.adjust.method = "bonferroni",
  effsize.type = "partial_eta",
  var.equal = FALSE,
  mean.plotting = TRUE, # whether mean for each group is to be displayed
  mean.ci = TRUE, # whether to display confidence interval for means
  mean.label.size = 2.5, # size of the label for mean
  type = "parametric", # which type of test is to be run
  k = 3, # number of decimal places for statistical results
  outlier.label.color = "darkgreen", # changing the color for the text label
  title = "Comparison of listening times between groups",
  xlab = "Experimental group", # label for the x-axis variable
  ylab = "Listening time", # label for the y-axis variable
  messages = FALSE,
  bf.message = FALSE
)
