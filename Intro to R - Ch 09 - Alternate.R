############################################################
# Alternate R script to accompany Intro to R for Business, #
# Chapter 09, written by Troy Adair                        #
############################################################

# Load the previously referenced data frame in "YT_Sample_Validated.RData"
Scorecard <- read.csv("most-recent-cohorts-all-data-elements-1.csv")

str(Scorecard)

attach(Scorecard)

# Statistics on 1 numerical variable

summary(HIGHDEG)

mean(HIGHDEG)

median(HIGHDEG)

max(HIGHDEG)

min(HIGHDEG)

sum(HIGHDEG)

sd(HIGHDEG)

var(HIGHDEG)

# Statistics on 1 categorical variable

table(HIGHDEG)

n <- length(Scorecard$HIGHDEG)

for(i in 1:n) {
  if(Scorecard$HIGHDEG[i]==0L) {
    Scorecard$HDEGREE[i] <- "0 - Non-Degree"
  } else if(Scorecard$HIGHDEG[i]==1L) {
    Scorecard$HDEGREE[i] <- "1 - Certificate"
  } else if(Scorecard$HIGHDEG[i]==2L) {
    Scorecard$HDEGREE[i] <- "2 - Associate's"
  } else if(Scorecard$HIGHDEG[i]==3L) {
    Scorecard$HDEGREE[i] <- "3 - Bachelor's"
  } else if(Scorecard$HIGHDEG[i]==4L) {
    Scorecard$HDEGREE[i] <- "4 - Graduate"
  } else{}
}

head(HDEGREE,10)

attach(Scorecard)

head(HDEGREE,10)

table(HDEGREE)

# Statistics on 2 categorical variables

table(HDEGREE, CONTROL)

for(i in 1:n) {
  if(CONTROL[i]==1L) {
    Scorecard$ITYPE[i] <- "Public"
  } else if(CONTROL[i]==2L) {
    Scorecard$ITYPE[i] <- "Private Non-Profit"
  } else if(CONTROL[i]==3L) {
    Scorecard$ITYPE[i] <- "Private For-Profit"
  } else{}
}

attach(Scorecard)

table(HDEGREE,ITYPE)

# Statistics on 1 categorical and 1 numerical variable

by(ADM_RATE,ITYPE,summary)

str(Scorecard$ADM_RATE)

Scorecard$ADM_RATE <- as.numeric(Scorecard$ADM_RATE)

str(Scorecard$ADM_RATE)

print(str(Scorecard$ADM_RATE,digits=4))

by(ADM_RATE,ITYPE,summary)

by(Scorecard$ADM_RATE,ITYPE,summary)

attach(Scorecard)

by(ADM_RATE,ITYPE,summary)

by(ADM_RATE,ITYPE,mean)

by(ADM_RATE,ITYPE,mean,na.rm=TRUE)

by(ADM_RATE,ITYPE,sd,na.rm=TRUE)


# Statistics for 2 numerical values (i.e., simple linear regression, AKA "OLS")

cor(PCIP27,SATMTMID)

str(PCIP27)
str(SATMTMID)

Scorecard$PCIP27 <- as.numeric(Scorecard$PCIP27)
Scorecard$SATMTMID <- as.numeric(Scorecard$SATMTMID)

attach(Scorecard)

cor(PCIP27,SATMTMID)

cor(PCIP27,SATMTMID,use="complete.obs")

cov(PCIP27,SATMTMID,use="complete.obs")

OLS <- lm(SATMTMID~PCIP27)

OLS

summary(OLS)

# Could we have kept ourselves from having to convert so many character objects?

str(Scorecard)

cls <- c(LATITUDE="numeric",LONGITUDE="numeric", ADM_RATE="numeric",
         ADM_RATE_ALL="numeric",SATVR25="numeric",SATVR75="numeric",SATMT25="numeric",
         SATMT75="numeric",SATWR25="numeric",SATWR75="numeric",SATVRMID="numeric",
         SATMTMID="numeric",SATWRMID="numeric",ACTCM25="numeric",ACTCM75="numeric",
         ACTEN25="numeric",ACTEN75="numeric",ACTMT25="numeric",ACTMT75="numeric",
         ACTWR25="numeric",ACTWR75="numeric",ACTCMMID="numeric",ACTENMID="numeric",
         ACTMTMID="numeric",ACTWRMID="numeric",SAT_AVG="numeric",SAT_AVG_ALL="numeric",
         PCIP01="numeric",PCIP03="numeric",PCIP04="numeric",PCIP05="numeric",
         PCIP09="numeric",PCIP10="numeric",PCIP11="numeric",PCIP12="numeric",
         PCIP13="numeric",PCIP14="numeric",PCIP15="numeric",PCIP16="numeric",
         PCIP19="numeric",PCIP22="numeric",PCIP23="numeric",PCIP24="numeric",
         PCIP25="numeric",PCIP26="numeric",PCIP27="numeric",PCIP29="numeric",
         PCIP30="numeric",PCIP31="numeric",PCIP38="numeric",PCIP39="numeric",
         PCIP40="numeric",PCIP41="numeric",PCIP42="numeric",PCIP43="numeric",
         PCIP44="numeric",PCIP45="numeric",PCIP46="numeric",PCIP47="numeric",
         PCIP48="numeric",PCIP49="numeric",PCIP50="numeric",PCIP51="numeric",
         PCIP52="numeric",PCIP54="numeric")
head(cls)
str(cls)

temp <- read.csv("most-recent-cohorts-all-data-elements-1.csv", colClasses=cls,na.strings='NULL')
str(temp)
