rm(list = ls())
worldcup <- read.csv(file="WorldCupMatches.csv", sep = ",")    #Import CSV file
#Observed a few last rows are repeating, so check for more repeating data then delete.
UniID_Check = function(x){                #Create Function to Check the ID of the input csv file
  CheckID = length(x)                       #Check the number of matches and matchID
  MatID = unique(x)                         #Check the number of unique matchID
  if ((CheckID == length(MatID)) != TRUE ){
    cat(CheckID == length(MatID),"\n", "Proceed to filtering process.\n")   
    #Check if the statement is true or False
    #Remove all the repeating data bases on matchID
    worldcup <<- worldcup[!duplicated(x),]                                 
    CheckID = length(worldcup$MatchID)
  }
  cat(CheckID == length(worldcup$MatchID),", Number of unique ID is:", CheckID)
}
UniID_Check(worldcup$MatchID)


#Check for unique country and repeating country
namelist = unique(worldcup$Home.Team.Name)                            #Save all the country name into namelist
namelist = namelist[order(namelist)]
namelist                                                              #Check for repeating name for same country

#Combine record with the same name but diff. full name based on namelist
worldcup$Home.Team.Name[which(worldcup$Home.Team.Name == "Germany FR")] = "Germany"
worldcup$Away.Team.Name[which(worldcup$Away.Team.Name == "Germany FR")] = "Germany"
worldcup$Home.Team.Name[which(worldcup$Home.Team.Name == "Korea DPR")] = "Korea"
worldcup$Away.Team.Name[which(worldcup$Away.Team.Name == "Korea DPR")] = "Korea"
worldcup$Home.Team.Name[which(worldcup$Home.Team.Name == "Korea Republic")] = "Korea"
worldcup$Away.Team.Name[which(worldcup$Away.Team.Name == "Korea Republic")] = "Korea"

#Rerun the check unique name process
namelist = unique(worldcup$Home.Team.Name)
namelist = namelist[order(namelist)]
namelist                                                              #Check for repeating name for same country

#---------------------------------Work------------------------------------------
#Actual Process
#The Country with the most goals whole time
i = 1
z = 1

tempsave = data.frame(      #Create data frame for temporary use.
  CountryName = namelist,
  CountryHomeScore = c(0),
  CountryAwayScore = c(0),
  CountryTotalScore = c(0)
)

#Total Home Goal
while (z <= length(worldcup$City)||(i < length(tempsave$CountryName))){   #Condition for the loop
  if (z > length(worldcup$City)){
    z = 1
    i = i + 1
  }
  else if (tempsave$CountryName[i] == worldcup$Home.Team.Name[z]){   #To add on all the country home goal
    tempsave$CountryHomeScore[i] = tempsave$CountryHomeScore[i] + worldcup$Home.Team.Goals[z]
    z = z + 1
  }
  else 
    z = z + 1
}
tempsave

#Total Away Goal
i = 1
z = 1
while (z <= length(worldcup$City)||(i < length(tempsave$CountryName))){
  if (z > length(worldcup$City)){
    z = 1
    i = i + 1
  }
  else if (tempsave$CountryName[i] == worldcup$Away.Team.Name[z]){   #To add on all the country away goal
    tempsave$CountryAwayScore[i] = tempsave$CountryAwayScore[i] + worldcup$Away.Team.Goals[z]
    z = z + 1
  }
  else 
    z = z + 1
}
tempsave

#Total of Country
i = 1
while (i < length(tempsave$CountryName)){        #To add both home and away goals for each country
  tempsave$CountryTotalScore[i] = tempsave$CountryAwayScore[i] + tempsave$CountryHomeScore[i]
  i = i + 1
}
tempsave

maxScore = max(tempsave$CountryTotalScore)    #To find the highest total score
maxCountryIndex = match(maxScore,tempsave$CountryTotalScore)
cat(tempsave$CountryName[maxCountryIndex], ", with a total score of ", maxScore)

TopCountry = tempsave %>%         #Create new data frame with descending total score
  arrange(desc(CountryTotalScore))%>%
  group_by(CountryName)
TopCountry

TopTen = data.frame(               #Create new data frame for top ten country
  TopCountryName = TopCountry$CountryName[1:10],
  TopCountryGoal = TopCountry$CountryTotalScore[1:10]
)
TopTen

colnames(TopTen) = c("Country", "Total_Goals")    #Re-title the data frame

ggplot(data=TopTen, aes(x=Country, y=Total_Goals))+
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=(Total_Goals)),vjust=-0.3, size=3.5)+
  theme_minimal()


#-------------------------------------------------------------------------------
#The year with the most goals whole time
i = 1
z = 1
yearlist = unique(worldcup$Year)
tempsave = data.frame(
  YearTemp = yearlist,
  YearHomeScore = c(0),
  YearAwayScore = c(0),
  YearTotalScore = c(0)
)

#Total Year Home Goal
while (z <= length(worldcup$Year)||(i < length(yearlist))){ #Condition of the loop
  if (z > length(worldcup$Year)){
    z = 1
    i = i + 1
  }
  else if (tempsave$YearTemp[i] == worldcup$Year[z]){ #Add on the home team scores of each year
    tempsave$YearHomeScore[i] = tempsave$YearHomeScore[i] + worldcup$Home.Team.Goals[z]
    z = z + 1
  }
  else 
    z = z + 1
}
tempsave

#Total Away Goal
i = 1
z = 1
while (z <= length(worldcup$Year)||(i < length(yearlist))){
  if (z > length(worldcup$Year)){
    z = 1
    i = i + 1
  }
  else if (tempsave$YearTemp[i] == worldcup$Year[z]){ #Add on the away team scores of each year
    tempsave$YearAwayScore[i] = tempsave$YearAwayScore[i] + worldcup$Away.Team.Goals[z]
    z = z + 1
  }
  else 
    z = z + 1
}
tempsave

#Total of Country
i = 1
while (i <= length(yearlist)){   #Sum of home team and away team scores as a total for each year
  tempsave$YearTotalScore[i] = tempsave$YearAwayScore[i] + tempsave$YearHomeScore[i]
  i = i + 1
}
tempsave

maxScore = max(tempsave$YearTotalScore)
maxYearIndex = match(maxScore,tempsave$YearTotalScore)
cat(tempsave$YearTemp[maxYearIndex], ", Their total score is: ", maxScore)
maxYearIndex
yearlist
tempsave

graphYear= data.frame(
  whichYear = tempsave$YearTemp,
  numberGoal = tempsave$YearTotalScore
)
colnames(graphYear) = c("Year", "Total_Goals")
graphYear

ggplot(data=graphYear, aes(x=Year, y=Total_Goals))+
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=(Year)),vjust=-0.3, size=3.5)+
  theme_minimal()


#-------------------------------------------------------------------------------
#The Most with the most goals difference whole time

MatchHomeMaxScore = max(worldcup$Home.Team.Goals)
MatchHomeMaxIndex = match(MatchHomeMaxScore,worldcup$Home.Team.Goals)
MatchAwayMaxScore = max(worldcup$Away.Team.Goals)
MatchAwayMaxIndex = match(MatchAwayMaxScore,worldcup$Away.Team.Goals)
cat(worldcup$Home.Team.Name[MatchHomeMaxIndex], 
    ",is the first Home Team to achieve the highest score of ", MatchHomeMaxScore,
    " in a single match.\n")
cat(worldcup$Away.Team.Name[MatchAwayMaxIndex], 
    ",is the first Away Team to achieve the highest score of ", MatchAwayMaxScore,
    " in a single match.\n")

i = 1
MatchTotalDiffScore = 0
while ( i <= length(worldcup$MatchID)){ # While loop statement
  MatchHomeScore = worldcup$Home.Team.Goals[i]
  MatchAwayScore = worldcup$Away.Team.Goals[i]
  MatchTotalDiffTemp = MatchHomeScore - MatchAwayScore  #Find the different in home and away score
  MatchTotalDiffTemp = abs(MatchTotalDiffTemp)     #Force positive on the value
  if (MatchTotalDiffTemp > MatchTotalDiffScore){  #Compare current score with previous highest score
    MatchTotalDiffScore = MatchTotalDiffTemp      #If the current score is higher, 
    #replace the previous highest score with current score
    MatchIndex = i                                
    MatchTotalDiffScore = abs(MatchTotalDiffScore)
  }
  else{                                         #Do nothing if it is not higher
    
  }
  if (MatchHomeScore > MatchAwayScore){  #If home score is higher, the home team was the winner.
    CountryTotalWin = worldcup$Home.Team.Name[MatchIndex]  #Save for the winning team
    CountryTotalLose = worldcup$Away.Team.Name[MatchIndex]  #Save for the losing team
    HomeScore = worldcup$Home.Team.Goals[MatchIndex]    #Save for winning score
    AwayScore = worldcup$Away.Team.Goals[MatchIndex]    #Save for losing score
  }
  else{                                  #The away team was the winner.
    CountryTotalWin = worldcup$Away.Team.Name[MatchIndex]
    CountryTotalLose = worldcup$Home.Team.Name[MatchIndex]
    HomeScore = worldcup$Home.Team.Goals[MatchIndex]
    AwayScore = worldcup$Away.Team.Goals[MatchIndex]
  }
  i = i + 1
}

cat(CountryTotalWin, 
    "is the first Country to achieve a", MatchTotalDiffScore, "score difference against", 
    CountryTotalLose, "with a match score of", HomeScore, ":", AwayScore)
worldcup$Home.Team.Name[MatchIndex]
MatchIndex
View(worldcup)

