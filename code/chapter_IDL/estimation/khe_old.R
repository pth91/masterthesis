source("../../helper/package_check.R")
source("../../helper/data.R")
load_idl_complete()

#-------------------------------------------------------------------------------

countries <- unique(idl_complete$DCOUNTRY)

# has to be done to delete the `.` death country
countries <- countries[nchar(countries) > 1]

# TODO: auf jeden fall muss ich noch irgendwo schreiben, wieso ich gerade diese
# vier rausggenommmen habe...
# es gibt nur frauen und davon zu wenige beobachtungen
# TODO: EW ist das dcountry bei leuten die in UK geboren sind... wofür soll das
# stehen?
countries <- countries[
  countries != "ITA" & countries != "LBN" & countries != "FIN" & countries != "SWE"
]

#-------------------------------------------------------------------------------

data_germany <- idl_complete[idl_complete$DCOUNTRY == "DEU", ]
data_france <- idl_complete[idl_complete$DCOUNTRY == "FRA", ]

data_germany$DDATE <- substr(data_germany$DDATE, 7, 10)
data_france$DDATE <- substr(data_france$DDATE, 7, 10)

ker <- function(x, obs, bw){
    return((1 / sqrt(2 * pi)) * exp(-1/2 * ((x - obs)/bw)^2))
}
khe <- function(bw, nobs, obs, x){
    ret <- 0
    for(it in seq(1, nobs)){
        ret <- ret + 1/(nobs - it + 1) * ker(x, obs[it], bw)
    }
    return(1/bw * ret)
}
opt_bw <- function(bw, ui, obs, nobs){
    first <- 0
    for(it in seq(0, length(ui) - 1)){
        first <- (u[it + 1] - u[it])/2 * (khe(bw, nobs, obs, u[it])^2 + khe(bw, nobs, obs, u[it + 1])^2)
    }
    second <- 0
    for(iti in seq(0, length(obs))){
        for(itj in seq(0, length(obs))){
            if(iti != itj){
                second <- ker(obs[iti], obs[itj], bw) * (1/(nobs - iti + 1)) * (1/(nobs - itj + 1))
            }
        }
    }
    g <- first -2/bw * second
    return(g)
}

fra <- data_france[which(data_france$DDATE == "2017" & data_france$SEX == "F"),]
ad <- sort(fra$AGEDAYS)
x <- khe(1, length(ad), ad, min(ad))
u <- seq(min(ad), 45809, length.out = 100)
f <- optim(1000, opt_bw, ui = u, obs = ad, nobs = length(ad))