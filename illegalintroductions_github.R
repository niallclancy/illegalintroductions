# For context, please read Clancy and Bourret (2020) 
#Using social and physical variables to assess vulnerability of northwestern Montana lakes to illegal fish introductions
#Hydrobiologia 847:3055–3065
#DOI: https://doi.org/10.1007/s10750-020-04312-z


install.packages("readxl")
install.packages("DescTools")
install.packages("ResourceSelection")
p_pack = c("readxl", "DescTools", "ResourceSelection")
lapply(p_pack, library, character.only = TRUE)
#READ IN DATA FROM EXCEL FILE CALLED CLANCY_BOURRET_NAJFM.xlsx
ill = read_excel("R1_analysis.xlsx")
View(ill)
ill2 = subset(ill, ill$RESP != 0)
ill3 = subset(ill, ill$RESP == 0)
ill2$DIST_INTRO = ill2$OTH_INTR
ill = rbind(ill2, ill3)
ill$OTH_INTR = NULL
ill2 = NULL
ill3 = NULL
ill$ELEV = ill$ELEV*0.3048
ill$binary = ifelse(ill$RESP == 0, 0, 1)
ill$FID = NULL
ill$OBJECTID = NULL
ill$FWB_ID = NULL
ill$POP5MI = NULL
ill$POP50MI = NULL
#ill$NAME = NULL
ill = na.omit(ill)
#DEFINE VARIABLES
acres = ill$ACRES
elev = ill$ELEV
intdist = ill$DIST_INTRO
hidist = ill$DIST_HWY
pop = ill$POP10MI
press = ill$PRESS
resp = ill$RESP
binary = ill$binary

#Pair-wise comparison of covariates
subill = ill[,1:6]
pairs(subill)

#Models by individual variables
acres_mod = glm(binary~acres, binomial)
summary(acres_mod)
elev_mod = glm(binary~elev, binomial())
summary(elev_mod)
intdist_mod = glm(binary~intdist, binomial())
summary(intdist_mod)
hidist_mod = glm(binary~hidist, binomial())
summary(hidist_mod)
pop_mod = glm(binary~pop, binomial())
summary(pop_mod)
press_mod = glm(binary~press, binomial())
summary(press_mod)

#MULTIVARIATE MODELS 
MOD1 = glm(binary~acres*intdist*pop*press, binomial())
MOD2 = glm(binary~intdist*pop*press, binomial())
MOD3 = glm(binary~elev*pop*press, binomial())
MOD4 = glm(binary~elev*press, binomial())
MOD5 = glm(binary~acres*hidist*press, binomial())
MOD6 = glm(binary~intdist*pop*press,binomial())
MOD7 = glm(binary~intdist*press, binomial())
MOD8= glm(binary~intdist*acres, binomial())
MOD9 = glm(binary~intdist+press, binomial())
MOD10 = glm(binary~elev+press, binomial())
MOD11 = glm(binary~elev+press+pop, binomial())
MOD12 = glm(binary~intdist+press+pop, binomial())
MOD13 = glm(binary~elev*press, binomial())
MOD14 = glm(binary~acres*intdist*pop*press, binomial())
MOD15 = glm(binary~acres*elev*pop*press, binomial())
MOD16 = glm(binary~elev*pop, binomial())
MOD17 = glm(binary~elev+pop+press+elev:pop, binomial())

AIC(MOD1, MOD2,MOD3,MOD4,MOD5,MOD6,MOD7,MOD8,MOD9,MOD10,
    MOD11,MOD12,MOD13,MOD14,MOD15,MOD16,MOD17)
summary(MOD17)

#GOODNESS OF FIT TESTS
options(na.action = na.omit)
PseudoR2(MOD17, which = "McFadden")
PseudoR2(MOD17, which = "Nagelkerke")
hoslem.test(ill$binary, fitted(MOD20))

#Variance Inflation Factors
global.mod = glm(binary~acres+intdist+pop+press, binomial(), data = ill)
VIF(global.mod)
