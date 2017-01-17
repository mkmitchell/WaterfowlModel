# ***TRUEMET Bioenergetics Model - Version 2.2**

# Date: August 19, 2016

# Authors: 
# Siddharth Penmetcha, University of Missouri
# Frank Feng, University of Missouri
# Mark J. Petrie, Ducks Unlimited
# Joel Sartwell, Missouri Department of Conservation
# Matthew E. Reiter, Point Blue Conservation Science
# Orien M. W. Richmond, U.S. Fish & Wildlife Service; corresponding author email: orien_richmond@fws.gov
# 

# NOTE: to soft-wrap the R code in this window, go to: 
# Tools > Global Options > Code Editing > Soft-wrap R source files

# BACKGROUND: TRUEMET is a daily ration bioenergetics model used to calculate energetic carrying capacity for foraging animals. The underlying model assumes that animals are ideal free foragers such that the distribution of animals between resource sites should match the distribution of resources. Total energy required by foraging guilds is a function of population size and daily energy expenditure (DEE). Total energy available is a function of the area of available forage types, food biomass, metabolizable energy, consumption by foraging guilds, and decomposition or growth of food biomass. Animals often experience temporal variation in availability of food supplies. TRUEMET allows the user to define when forage types become available within the time period being modeled. As a result, the relationship between total energy demand and total energy supply can be examined for any point in time. Model outputs are returned in the form of figures (.jpg) and a data file (.csv). For more information on TRUEMET, see the TRUEMET User Guide (Penmetcha et al. 2016).

#Install required R packages
#If you are running the program for the first time, delete the '#' symbols in the section below and run each line of code (put cursor on a line and type CTRL-R to run). After all packages are installed, replace the '#' symbols and save so you don't re-run the package installation each time you run the program (packages only need to be installed once).
#install.packages("deSolve")
#install.packages("ggplot2")
#install.packages("scales")
#install.packages("sfsmisc")
#install.packages("reshape2")

# Load required R packages. Once the packages are installed, they must be loaded each time the program is run.
library(deSolve)
library(ggplot2)
library(scales)
library(sfsmisc)
library(reshape2)

# Set the working directory. Copy your working directory replacing backward slashes with forward slashes. 
setwd("D:/GIS/projects/Waterfowl model/truemet")

# Read in the data file. See TRUEMET User Guide for formatting the data file.
X <- read.csv("WGCP_input_data_R_Test.csv", header=TRUE) # Read from file
H<-X$N_FORAGE_TYPES[1] # H = number of forage types = the total number of forage types whose data is available. 
# A "forage type" is a food energy source that is defined by a fixed maximum extent (area), a forage type availability vector (representing the area that is accessible to foraging guilds over time), an energy density and a forage type preference value associated with each foraging guild.
forages <-as.character(X$FORAGE_TYPE_NAME[1:H]) # Names of forage types (food energy types)
Tarea<-X$TOTAL_AREA_BY_FORAGE_TYPE[1:H] # Tarea = the total area of each of the forages;
area_unit <- as.character(X$AREA_UNIT[1]) # area_unit = the units for the area values (e.g., "ha" or "acres".
# NOTE: the user must select appropriate units for area and ensure that the units are consistent 
# with the units for other variables (e.g., energy density)
AvtimeColumn <- grep("TIME_VECTOR_FORAGE_TYPE",colnames(X)) # The time steps for which the 
# availability data of the forage types is available. (units: days)
Availtime<- X[,AvtimeColumn][!is.na(as.numeric(X[,AvtimeColumn]))]
Habav_temp <- X[1:length(Availtime),(AvtimeColumn+1):(AvtimeColumn+H)] 
# Read the area availability data for the forage types at each time step.
Habav <- as.matrix(Habav_temp) # Habav = the area availability data for each of the forage types
# at each of the time steps in the AVAIL_TIME column.
Den <- X$METABOLIZABLE_ENERGY[1:H]*X$FOOD_BIOMASS[1:H] # Den = a vector of energy density values for each forage type. Calculated as the product of food biomass (mass/area) * metabolizable energy (energy/mass).
energy_unit <- as.character(X$ENERGY_UNIT[1]) # the units of energy used in the energy density vector (e.g., "kcal"). The area units for the energy density must match area_unit above.
b <- X$RATE_OF_CHANGE_RESERVE[1:H] # Read decomposition rate for the energy in reserve
d <- X$RATE_OF_CHANGE_AVAILABLE[1:H] # Read decomposition rate for the energy available
# NOTE: the user must select appropriate units for energy density and ensure that the units are consistent 
# with the units for other variables (e.g., area of forage types)
K<-X$N_FORAGING_GUILDS[1] # K = number of animal guilds
guilds <-as.character(X$FORAGING_GUILD_NAME[1:K]) # Names of animal guilds
PrefColumn<-grep("FORAGING_GUILD_NAME",colnames(X))+1 # The preference of the animal guilds 
# for the forage types being examined. Preference info must be next to GUILD_NAME column. 
Pref_temp<- X[1:H,PrefColumn:(PrefColumn+K-1)] # Read the forage type preference information.
Pref <- as.matrix(Pref_temp) # Create a matrix of forage type preference by animal guild.
supplies <- Pref # The matrix based on the pref matrix.
supplies[which(supplies>0)] <- 1. # Defines energy supplies by foraging guild.
PoptimeColumn <- grep("TIME_VECTOR_POP",colnames(X)) # The time steps across which the population 
# data of the animal guilds is available (units = days).
Poptime <- X[,PoptimeColumn][!is.na(as.numeric(X[,PoptimeColumn]))] #
Pop_temp<- X[1:length(Poptime),(PoptimeColumn+1):(PoptimeColumn+K)] 
# Read the population data of the various foraging guilds across the time steps in the POP_TIME column.
Pop<- as.matrix(Pop_temp) # Create a matrix of population data of the various foraging guilds across the time steps.
IntaketimeColumn <- grep("TIME_VECTOR_DEE",colnames(X)) # The time steps at which the daily 
# energy expenditure (DEE) of each foraging guild is known (units = days).
Intaketime <- X[,IntaketimeColumn][!is.na(as.numeric(X[,IntaketimeColumn]))]
Intake_temp <- X[1:length(Intaketime),(IntaketimeColumn+1):(IntaketimeColumn+K)] 
# Read the daily energy expenditure (DEE) of each guild at the time steps mentioned in the INTAKE_TIME  column.
DEE <- as.matrix(Intake_temp) # Create a matrix of the daily energy expenditure (DEE) of each guild. 
Ek <-as.numeric(DEE[1,1:K]) # Initial daily energy expenditure (DEE)  in the first time step for all guilds.
Pars <- c(d1<- 0.001,b1 <- 0.001) # Passing parameters. For conformity to the general ODE call format
# Correction for input error when availability exceeds maximum area
maxHabav <- apply(Habav,2,max) # maximum available area of each habitat
Tarea[which(Tarea-maxHabav<0)]<-maxHabav

# Prepare data for Figure 1
ha <- data.frame(Habav) # create a data frame of forage type availability
for (i in 1:length(forages)) { # use a for loop to rename columns
  names(ha)[i] <- forages[i]
}
at <- data.frame(Availtime) # create a data frame of days
ha$day <- at$Availtime # add days to ha data frame
ha <- melt(ha, id.vars = c("day")) # melt data frame to long format
names(ha)[names(ha)=="variable"] <- "Forage_Type" # rename variable
names(ha)[names(ha)=="value"] <- "area" # rename value

# begin plot for Figure 1
plot <- ggplot(data=ha, aes(x=day, y=area, group=Forage_Type, color=Forage_Type)) + 
  geom_line() + xlab("Day") + ylab(paste("Available Area (",area_unit,")",sep="")) + 
  ggtitle("Figure 1. Forage Type Availability")+scale_y_continuous(labels = comma)
print(plot)
ggsave(file="Fig1.jpg", plot=plot, width=6, height=4)
# end plot for Figure 1


# Calculate the curve fit to the population vector and plot
mdays=max(Poptime)
dayz=1:mdays
popplot<-matrix(nrow=length(dayz),ncol=K)
intake_plot<-matrix(nrow=length(dayz),ncol=K)
demand_by_guild<-matrix(nrow=length(dayz),ncol=K)
for(daycount in 1:mdays)
for (k in 1:K) {
  popul<-splinefun(Poptime,Pop[,k],method = "natural") # spline fit to population
  popplot[daycount,k]=max(c(popul(daycount,deriv=0),0)) # population must be non-negative
  intakefun<-splinefun(Intaketime,DEE[,k],method = "natural") # spline fit to population
  intake_plot[daycount,k]=max(c(intakefun(daycount,deriv=0),0))
  demand_by_guild[daycount,k] <- popplot[daycount,k]*intake_plot[daycount,k]
}
population=matrix(t(popplot),nrow=(K*mdays),ncol=1)
da <- data.frame(day=rep(1:mdays,each=K),Guild=rep(guilds),val=population)

# begin plot for Figure 2
plot<- ggplot(da, aes(day,population))+ geom_area(aes(colour=Guild,fill=Guild),position='stack') +
  ggtitle("Figure 2. Abundance by Foraging Guild")+
  labs(x="Day",y="Abundance")+scale_y_continuous(labels = comma)
print(plot)
ggsave(file="Fig2.jpg", plot=plot, width=6, height=4)
# end plot for Figure 2

# Initialization of matrices used in ODE definition 
c <- matrix(0,nrow=H,ncol=K) # consumption matrix
dE <- array(0,dim=H)
dR <- array(0,dim=H)
E <- array(0,dim=H)
R <- array(0,dim=H)
y <- array(0,dim=(2*H))
n_birds<- matrix(0,nrow=K,ncol=1)
### TOP of ODE DEFINITION ###
Ened <- function(Time, y, Pars){
  with(as.list(c(y,Pars)),{
    #Redistribution of initial data
    E <- y[1:H]
    R <- y[(H+1):(2*H)]
    #Determination of energy consumption coefficient
      EPref=E %*% Pref #matrix multiplication E_h and Pref_hk
        for(k in 1:K){
         n_birds[k]= approx(dayz,popplot[,k],xout=Time,method="linear")$y
          intake<-splinefun(Intaketime,DEE[,k])
          Ek[k]<-intake(Time,deriv=0)
          for(h in 1:H){
            if (E[h] > 0.1) 
            c[h,k]<- Pref[h,k]*E[h]/(EPref[k]+0.1)
            else
              c[h,k] <- 0 # set consumption to zero when energy is exhausted
         }
        }
    #Determine energy change between available and reserve energy
    for(h in 1:H){
      if (Time==Availtime[length(Availtime)]) # the last point
      {
        dA_dt <- 0
        A<-Habav[length(Availtime),h]
      } else
      {
      int<-findInterval(Time,Availtime) # find interval for linear interpolation
      dA_dt<- (Habav[int+1,h]-Habav[int,h])/(Availtime[int+1]-Availtime[int])
      A <- Habav[int,h]+dA_dt*(Time-Availtime[int])
      }
      if(A<0.1) dA_dt=max(c(dA_dt,0)) # Area cannot decrease if it is zero
      if(Tarea[h]-A<0.1) dA_dt=min(c(dA_dt,0)) # Area cannot increase if near max
          if(dA_dt>0.01){
              rhi<- R[h]*dA_dt/(Tarea[h]-A+.1)
           }else if(dA_dt> -0.01){
            rhi <- 0 
            }else rhi<- E[h]/(A+0.1)*dA_dt # dA is negative, rhi is negative
            cPk<-sum(c[h,]*Ek*n_birds)
      if(d[h]<=0) # fixed energy type
      {
            dE[h]= d[h] * E[h]  + rhi  -cPk
            dR[h]= b[h] * R[h]- rhi
      } else # none fixed energy type such as invertebrates
          {
            gammah <- X$CARRYING_CAPACITY[h]*X$METABOLIZABLE_ENERGY[h] 
            # carrying capacity energy/area = gram/area * energy/gram
            if (gammah<0.01) # negligible invertebrates
            {dE[h]=0
            dR[h]=0}
            else {
            deltah <- gammah # carrying capacity in Reservoir set equal to that of Available
            if (A>0.1) dE[h]=d[h]*(1-E[h]/(A*gammah+0.1))*E[h]+rhi-cPk
             else dE[h]=rhi-cPk #no breeding if area =0
            if ((Tarea[h]-A)>0.1) dR[h]=b[h]*(1-R[h]/((Tarea[h]-A)*deltah+0.1))*R[h]-rhi
            else dR[h]=-rhi #no breeding if area =0
            }
          }
    }
  list(c(as.vector(dE),as.vector(dR)))
  })
}
# End of ODE definition
#Calculation of initial energy available and in reserve
for(h in 1:H){
  density_h <- Den[h]
  E[h] <-density_h*Habav[1,h]
  R[h] <- density_h*(Tarea[h]-Habav[1,h])
  
}
y = c(as.vector(E),as.vector(R)) #Vectorization of matrices before passing to ODEs
totalsteps= min(c(max(Availtime),max(Poptime),max(Intaketime)))-1 
# simu time determined by available data
tstep <- 1 # time step is set to one day
# Initialization of matrices for output
days=matrix(1,nrow=totalsteps+1,ncol=1)
EnergyAvailable=matrix(0,nrow=totalsteps+1,ncol=H)
supply_by_guild=matrix(0,nrow=totalsteps+1,ncol=K)
supply_by_guild[1,]= E %*% supplies
E_food_type=matrix(0,nrow=totalsteps+1,ncol=H)
E_habitat=matrix(0,nrow=totalsteps+1,ncol=H)
EnergyReserve=matrix(0,nrow=totalsteps+1,ncol=H)
TotalDemand <- matrix(0,nrow=totalsteps+1,ncol=1)
dailyDemandbyGuild <- matrix(0,nrow=totalsteps+1,ncol=K)
dailyDemand <- matrix(0,nrow=totalsteps+1,ncol=1)
energydeficit <- matrix(0,nrow=totalsteps+1,ncol=1)
EnergyAvailable[1,]=y[1:H] # record the first output: given by initial conditions
E_food_type[1,]=E
EnergyReserve[1,]=y[(H+1):(2*H)]
for (istep in 1:totalsteps) {
  day <- seq(istep*tstep,(istep+1)*tstep)
  # check if energy is sufficient: compare available with dailyneed
  # allavailable<-sum(y[1:H])
  for(k in 1:K){
   n_birds[k]= approx(dayz,popplot[,k],xout=(istep+1)*tstep,method="linear")$y
   Ek[k]<- approx(Intaketime,DEE[,k],xout=(istep+1)*tstep,method="linear")$y
  }
  dailyDemandbyGuild[istep+1,]<- t(Ek*n_birds)
  dailyneed<-sum(Ek*n_birds)
# call to the ODE solver
  out <- ode(y, func=Ened, parms=Pars, times = day, method="rk4")
  len<-length(out[,1])
  days[istep+1]<-out[len,1] # save current time
  y <- out[len,2:(2*H+1)] # update initial condition
  if(any(y<0)) print("At least one energy type exhausted")
  y[which(y<0)]=0 # Set exhausted energy to zero
  E<-array(y[1:H],dim=H)
  R<-array(y[(H+1):(2*H)],dim=H)
  EnergyAvailable[istep+1,]=y[1:H] # Remaining energies (I x H)
  E_food_type[istep+1,]=E # sum energy by food type
  EnergyReserve[istep+1,]=y[(H+1):(2*H)] # 
  supply_by_guild[istep+1,]=E %*% supplies
  dailyDemand[istep+1] <- dailyneed
  TotalDemand[istep+1] <-TotalDemand[istep]+dailyneed
  }
TotalAvailEnergy <- rowSums(EnergyAvailable)
TotalResEnergy <- rowSums(EnergyReserve)
TotalEnergy <- TotalAvailEnergy+TotalResEnergy

AvailableE=matrix(t(E_food_type),nrow=(H*(totalsteps+1)),ncol=1)
da1 <- data.frame(day=rep(1:(totalsteps+1),each=H),Forage_Type=rep(forages),val=AvailableE)
# begin plot for Figure 3
plot <- ggplot(da1, aes(day,AvailableE)) + geom_area(aes(colour=Forage_Type,fill=Forage_Type),position='stack') +
  ggtitle("Figure 3. Available Energy by Forage Type") +
  labs(x="Day",y=paste("Available Energy (",energy_unit,")",sep="")) 
# +scale_y_continuous(labels = comma) # optional code to remove scientific notation
print(plot)
ggsave(file="Fig3.jpg", plot=plot, width=6, height=4)
# end plot for Figure 3

# begin plot for Figure AR
AR = da1[substr(da1$Forage_Type,1,2) == "AR",]
plot <- ggplot(AR, aes(day,val)) + geom_area(aes(colour=Forage_Type,fill=Forage_Type),position='stack') +
  ggtitle("Figure AR. Available Energy by Forage Type") +
  labs(x="Day",y=paste("Available Energy (",energy_unit,")",sep="")) 
# +scale_y_continuous(labels = comma) # optional code to remove scientific notation
print(plot)
ggsave(file="FigAR.jpg", plot=plot, width=6, height=4)
# end plot for Figure AR

# begin plot for Figure LA
LA = da1[substr(da1$Forage_Type,1,2) == "LA",]
plot <- ggplot(LA, aes(day,val)) + geom_area(aes(colour=Forage_Type,fill=Forage_Type),position='stack') +
  ggtitle("Figure LA Available Energy by Forage Type") +
  labs(x="Day",y=paste("Available Energy (",energy_unit,")",sep="")) 
# +scale_y_continuous(labels = comma) # optional code to remove scientific notation
print(plot)
ggsave(file="FigLA.jpg", plot=plot, width=6, height=4)
# end plot for Figure LA

# begin plot for Figure MS
MS = da1[substr(da1$Forage_Type,1,2) == "MS",]
plot <- ggplot(MS, aes(day,val)) + geom_area(aes(colour=Forage_Type,fill=Forage_Type),position='stack') +
  ggtitle("Figure MS Available Energy by Forage Type") +
  labs(x="Day",y=paste("Available Energy (",energy_unit,")",sep="")) 
# +scale_y_continuous(labels = comma) # optional code to remove scientific notation
print(plot)
ggsave(file="FigMS.jpg", plot=plot, width=6, height=4)
# end plot for Figure MS


# Prepare data for Figure 4
energy <- data.frame(cbind(days,TotalAvailEnergy,dailyDemand))
names(energy)[1] <- "day" # rename variable
names(energy)[2] <- "Energy Supply" # rename variable
names(energy)[3] <- "Energy Demand" # rename variable
energy <- melt(energy, id.vars = c("day")) # melt data frame to long format
names(energy)[2] <- "Legend" # rename variable
names(energy)[3] <- "energy" # rename variable

# begin plot for Figure 4
plot <- ggplot(data=energy, aes(x=day,y=energy,group=Legend,color=Legend)) + geom_line() + xlab("Day") + ylab(paste("Energy (",energy_unit,")",sep="")) + ggtitle("Figure 4. Daily Food Energy Supply and Demand: All Guilds") + scale_color_manual(values=c("cyan4", "orangered1"))
# + scale_y_continuous(labels = comma) # optional code to remove scientific notation
print(plot)
ggsave(file="Fig4.jpg", plot=plot, width=6, height=4)
# end plot for Figure 4


#Create data frames for guild-specific plots
energy <- data.frame(cbind(dayz,supply_by_guild,demand_by_guild))
names(energy)[1] <- "day" # rename variable
for (i in 1:length(guilds)) {
  names(energy)[i+1] <- "Energy Supply" # rename variable
  names(energy)[i+1+length(guilds)] <- "Energy Demand" # rename variable
}

#Create list of data frames by guild
W <- vector("list", length(guilds))
for (i in 1:length(guilds)) {
  temp <- melt(energy[,c(1, i+1, i+1+length(guilds))], id.vars = c("day")) # melt data frame to long format
  names(temp)[2] <- "Legend" # rename variable
  names(temp)[3] <- "energy" # rename variable
  W[[i]] <- temp
}

#Create guild-specific plots
for (i in 1:length(guilds)) { 
  plot <- ggplot(data=W[[i]], aes(x=day,y=energy,group=Legend,color=Legend)) + geom_line() + xlab("Day") + ylab(paste("Energy (",energy_unit,")",sep="")) + ggtitle(paste("Figure ",i+4,". Daily Food Energy Supply and Demand: ",guilds[i],sep="")) + scale_color_manual(values=c("cyan4", "orangered1"))
  # + scale_y_continuous(labels = comma) # optional code to remove scientific notation
  print(plot)
  ggsave(file=paste("Fig",i+4,".jpg",sep=""), plot=plot, width=7, height=4)
}
#End guild-specific plots

######### Input Uncertainties 
u_biomass<-X$FOOD_BIOMASS_UNCERTAINTY[1:H]
u_meteng <-X$METABOLIZABLE_ENERGY_UNCERTAINTY[1:H]
u_density<- sqrt(u_biomass^2+u_meteng^2)  # uncertainty of energy density
u_Pop <- X$POP_UNCERTAINTY[1:K] # uncertainty in population
u_DEE <- X$DEE_UNCERTAINTY[1:K] # uncertainty of DEE
Hab_U <- Den*u_density # multiply forage energy density by uncertainty in forage energy density
uk <- sqrt(u_DEE^2+u_Pop^2) # combine uncertainties in DEE and population
# Calculate the population for each day 
mdays=max(Poptime) # calculate number of days
guildsum <-matrix(0,nrow=length(days),ncol=K) # create a matrix of zeros with a column for each guild
Area <- matrix(0,nrow=length(days),ncol=H) # create a matrix of zeros with a column for each forage

# Calculating up to date consumption by guild;
for (k in 1:K) { # for loop with index guild (K)
  guildsum[1,k]=dailyDemandbyGuild[1,k]
}
for (daycount in 2:mdays) {
  for (k in 1:K) {
    guildsum[daycount,k]=guildsum[daycount-1,k]+dailyDemandbyGuild[daycount,k]  
  }
  for (h in 1:H) {
    Area[daycount,h] <-approx(Availtime,Habav[,h],xout=daycount,method="linear")$y
  }
}
unp <-rowSums(t(t(guildsum)*as.vector(uk))^2)
unh <- rowSums(t(t(Area)*as.vector(Hab_U))^2)
Uncertainty <-sqrt(unp+unh)
TAE_upper <- TotalAvailEnergy+Uncertainty
TAE_lower <- TotalAvailEnergy-Uncertainty
# U_daily_demand <- sqrt(rowSums((popplot*intakplot*as.vector(t(uk)))^2)) 
U_daily_demand <- sqrt(rowSums((dailyDemandbyGuild*as.vector(t(uk)))^2))
DD_upper <- dailyDemand+U_daily_demand
DD_lower <- dailyDemand-U_daily_demand

# Prepare data for plotting
energy <- data.frame(cbind(days,TotalAvailEnergy,dailyDemand,TAE_upper,TAE_lower,DD_upper,DD_lower))
names(energy)[1] <- "day" # rename variable
names(energy)[2] <- "supply" # rename variable
names(energy)[3] <- "demand" # rename variable
names(energy)[4] <- "TAE_upper" # rename variable
names(energy)[5] <- "TAE_lower" # rename variable
names(energy)[6] <- "DD_upper" # rename variable
names(energy)[7] <- "DD_lower" # rename variable

# begin plot for All Guilds Figure w/ uncertainty
plot <- ggplot(energy,aes(day)) + geom_line(aes(y=supply,colour="Energy Supply")) + 
  geom_line(aes(y=demand,colour="Energy Demand")) + 
  geom_ribbon(aes(ymin=TAE_lower,ymax=TAE_upper),fill="seagreen1",alpha=0.3) + 
  geom_ribbon(aes(ymin=DD_lower,ymax=DD_upper),fill="orangered1",alpha=0.3) + 
  scale_colour_manual("", breaks = c("Energy Supply","Energy Demand"), 
                      values = c("Energy Supply" ="cyan4", "Energy Demand" = "orangered1")) + 
  xlab("Day") + ylab(paste("Energy (",energy_unit,")",sep="")) + 
  ggtitle(paste("Figure ",5+length(guilds),". Daily Food Energy Supply and Demand: All Guilds",sep="")) 
# + scale_y_continuous(labels = comma) # optional code to remove scientific notation
print(plot)
ggsave(file=paste("Fig",5+length(guilds),".jpg",sep=""), plot=plot, width=7, height=4)
# end plot for All Guilds Figure w/uncertainty
# output result into a csv file
output<-data.frame(days,EnergyAvailable,EnergyReserve,popplot*intake_plot,rowSums(popplot*intake_plot),TAE_upper,TAE_lower,DD_upper,DD_lower)
names(output)[2:(H+1)]<-paste("Available Energy",forages)
names(output)[(H+2):(2*H+1)]<- paste("Reserve Energy",forages)
names(output)[(2*H+2):(2*H+1+K)]<- paste("DEE",guilds)
names(output)[(2*H+2+K)]<- "DEE total"
names(output)[(2*H+2+K+1)] <- "TAE_upper"
names(output)[(2*H+2+K+2)] <- "TAE_lower"
names(output)[(2*H+2+K+3)] <- "DD_upper"
names(output)[(2*H+2+K+4)] <- "DD_lower"
write.csv(file="result_WGCP.csv",output) # write data into file
head(output) # show the top of the output file
# Create guild-specific plots w/ uncertainty

# End guild-specific plots


