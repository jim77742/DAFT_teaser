# 0. Introduction ---------------------------------------------------------

# Type Ctrl-Shift-O (the letter) to see the document outline. Click on Sections to jump to them.

# Section 1 and Section 2 are compulsory

# 1. COMPULSORY: select a database or databases to load -------------------------------------------------------

# Move to your database selection of interest and type Ctrl-Enter
# Then jump to Section 2.



tidydir<-"tidy/men/international/test/test_"  # This is the only one available for this demo



# 2. COMPULSORY: run this section to load data --------------------------------------------------
# Put cursor on line above and click Run or type Ctrl-Enter

{
  library(tidyverse)
  options(stringsAsFactors=FALSE) # Very important to run this so csv files load as strings not factors
  
  # Custom scatterplot functions
  scatter1<-function (df, column, labels = "", title = NULL, xlabel = NULL, ylabel = NULL)
  {
    p <- ggplot(data = df, aes(x = {{column}}, y = 1:length({{column}}))) + geom_point(na.rm = TRUE) + labs(title = title, x = xlabel, y = ylabel)
    return(p)
  }
  
  scatter2<-function (df, x, y, labels = "", title = NULL, xlabel = NULL, ylabel = NULL)
  {
    p <- ggplot(data = df, aes(x = {{x}}, y = {{y}})) + geom_point(na.rm = TRUE) + labs(title = title, x = xlabel, y = ylabel)
    return(p)
  }
  
  bat<-read_csv(paste0(tidydir,"batting.csv"),show_col_types = FALSE)
  bowl<-read_csv(paste0(tidydir,"bowling.csv"),show_col_types = FALSE)
  play<-read_csv(paste0(tidydir,"players.csv"),show_col_types = FALSE)
  teams<-read_csv(paste0(tidydir,"teams.csv"),show_col_types = FALSE)
  
  teams<-teams %>% mutate(start_date=as.Date(start_date),end_date=as.Date(end_date))
  
  tmp<-teams %>% select(match_id,team,teams,competition,start_date) %>% unique()
  bat<-left_join(bat,tmp,by=c("match_id","teams"))
  bowl<-left_join(bowl,tmp,by=c("match_id","teams"))

  bat_sum<-bat %>% group_by(batter) %>% summarise(innings=sum(batted),outs=sum(out),runs=sum(score),runs_with_bf=sum(if_else(ballsfaced==0,0,score)),fours=sum(fours),sixes=sum(sixes),ballsfaced=sum(ballsfaced),time=sum(time),median=median(score),HS=max(score)) %>% mutate(average=round(runs/outs,digits=2),strike_rate=round(runs_with_bf*100/ballsfaced,digits=2))
  
  bat_sum10<-bat %>% filter(howout!="did not bat") %>% group_by(batter) %>% arrange(match_id) %>% slice(1:10) %>% summarise(innings=sum(batted),outs=sum(out),runs=sum(score),runs_with_bf=sum(if_else(ballsfaced==0,0,score)),fours=sum(fours),sixes=sum(sixes),ballsfaced=sum(ballsfaced),time=sum(time),median=median(score),HS=max(score)) %>% mutate(average=round(runs/outs,digits=2),strike_rate=round(runs_with_bf*100/ballsfaced,digits=2)) # After 10 innings 
  
  bat_sum10_s<-bat_sum10 %>% select(batter,average10=average)
  
  bat_sum<-left_join(bat_sum,bat_sum10_s,by="batter")
  
  bat_sum<-bat_sum %>% mutate(improve_ratio=average/average10)
  
  bowl_sum<-bowl %>% group_by(bowler) %>% summarise(matches_bowled=n_distinct(match_id),balls=sum(overs*ball_per_over+extraballs),across(c(maidens,runs,wickets,noballs),sum))
  
  field_sum<-bat %>% group_by(fielder,howout) %>% summarise(n=n(),.groups="drop") %>% filter(fielder!="")
  field_sum2<-field_sum %>% pivot_wider(id_cols=fielder,names_from=howout, values_from=n) %>% mutate(caught=c+`c and b`)
  
  teams<-teams %>% mutate(result=if_else(winner %in% c("draw","tie","abandoned"),winner,if_else(winner==team,"win","loss"))) %>% mutate(result=factor(result,levels=c("win","loss","draw","tie","abandoned")))
  
  results<-teams %>% group_by(team,result) %>% summarise(number=n()/2,.groups="drop")
  results<-results %>% pivot_wider(id_cols=team,names_from=result, values_from=number,names_expand=TRUE) %>% replace(is.na(.), 0) # %>% select(team,win,loss,draw,tie,abandoned)
  
  team_sum<-teams %>% group_by(team) %>% summarise(matches=n_distinct(match_id),venues=n_distinct(venue),across(c(wickets:byes),sum))
  
  team_sum<-left_join(team_sum,results,by="team")
  
  overall<-full_join(bat_sum,bowl_sum,by=c("batter"="bowler")) %>% rename(runs_scored=runs.x,runs_conceded=runs.y)

  rm(tmp)
  rm(results)
  rm(bat_sum10)
  rm(bat_sum10_s)
  
}

# 3. What the dataframes contain ------------------------------------------

# The "bat" dataframe contains details of each player's batting innings. If they did not bat in an innings, they are not present here. The column "match_id" is a unique identifier for any match in the database, and "teams" is a 1 if they batted first. The other fields should be obvious.

# Note that players are identified by their name and their birth date in brackets. This is so each player is unique. The birth date can be stripped for a final table using the remove_birthdate() function in the full version. This must be done last, otherwise players with the same name will blend.

# The "bowl" dataframe contains details of each player's bowling in each innings. If they did not bowl in an innings, they are not present here. The column "extraballs" shows the number of balls bowled in a final incomplete over.

# The "play" dataframe contains every player who played in each match, along with their details. For example if the "rh_bat" column has a 1 in it, they batted right handed. We use 1 or 0 instead of TRUE or FALSE because if you want to count the number of right handed batters, you just have to sum the column.

# The "teams" dataframe contains overall results of each innings, and also includes overall match results and information. For a Test much of two innings for each team there will 4 entries in this table. Much of the data is duplicated (e.g. wicketkeeper, umpires, results etc), but it makes queries easier to write.

# The "bat_sum" dataframe has a row for each batter and shows their overall statistics. In earlier matches, balls faced was not recorded. To be able to show a strike rate, "runs_with_bf" shows the number of runs they scored for innings where balls faced were recorded. For players never played in a match where balls faced was recorded, the strike rate will show as "NaN" - Not a Number.

# The "bowl_sum" dataframe shows a summary of each bowler. The columns are obvious.

# The "field_sum" dataframe shows a summary of the overall performances for fielders. This is a "Tidy" dataframe.

# The "field_sum2" dataframe shows a pivot_wider() version of field_sum.

# The "team_sum" dataframe shows a summary of the overall performances of the teams.

# The "overall" dataframe is a full join of bat_sum, bowl_sum, and field_sum to show each players full statistics

# _______________________________ ------------------------------------------------------------

# Who's scored exactly one century and that was a double century ---------------
{
  centuries<-bat %>% filter(score>=100) %>% group_by(batter) %>% summarise(centuries=n())
  dcenturies<-bat %>% filter(score>=200) %>% group_by(batter) %>% summarise(centuries=n())
  
  both_summ<-full_join(centuries,dcenturies,by="batter") %>% rename(centuries=centuries.x,doubles=centuries.y)
  
  single_double_century_equal<-both_summ %>% filter(centuries==doubles) %>% arrange(batter)  # This will find 1 or more
  
}

# Scatter plots -----------------------------------------------------------


scatter1(bowl_sum,wickets,title="Male Bowlers: Wickets taken with Ponting playing",xlabel="Wickets taken",ylabel="Bowler")

scatter2(bat_sum %>% filter(innings>10),ballsfaced/6,average,title="Male: Tests with innings over 10",xlabel="Six ball overs faced",ylabel="Average runs per innings")





# Most left-hand batters in a match ----------------------------------------

lh<-play %>% group_by(match_id) %>% summarise(lhb=sum(lh_bat)) %>% arrange(desc(lhb))





# Breakdown by ground -----------------------------------------------------

ground<-teams %>% group_by(venue) %>% summarise(matches=n_distinct(match_id),runs=sum(runs),wickets=sum(wickets)) %>% mutate(avg=round(runs/wickets,digits=2))



# Season breakdown by batter ----------------------------------------------

# This shows year by year how the player you pick in the filter() went

batter<-bat %>% filter(str_detect(batter,"Ponting")) %>% mutate(year=substr(start_date,1,4)) %>% group_by(year) %>% summarise(runs=sum(score),outs=sum(out),avg=round(runs/outs,digits=2))


# Pick a particular players and year, and create a dataframe with each innings

batter_detail<-bat %>% filter(str_detect(batter,"Ponting")) %>% mutate(year=substr(start_date,1,4)) %>% filter(year=="2003")




