#' country with the highest quality coffee
#'
#' @description `Best_quality` returns the country that produces the coffee that has the highest total score
#'
#' @returns country that produces the highest quality coffee
#'
#' @example Best_quality()
#'
#' @import dplyr
#' @export
Best_quality <- function (data = coffee){
  #first question, What countries produce the best overall quality coffee?

  #find the maximum of the data.score.total

  BestTotal <- max(coffee$Data.Scores.Total)#find the country with the highest total score

  top_country <- coffee$Location.Country[coffee$Data.Scores.Total == BestTotal]
  return(top_country)
}

#' largest coffee producers
#'
#' @description `n_Largest_coffee_producer` returns the country and the region that is the largest coffee producer
#'
#' @details The number for coffee production is product of two variables ; number of bags and bag weight
#' @param number of  countries that contribute the most to coffee production
#' @example n_Largest_coffee_producer(2)
#' @export

n_Largest_coffee_producer <- function(n,data = coffee){
  #Which n-country are the n largest coffee producer which is the product of the number of bags and bag weight
  #find the max of the producing
  coffee$production<- coffee$Data.Production.Number.of.bags*coffee$Data.Production.Bag.weight

  production_countrylocation <- coffee[,c(1,2,24)]

  sorted <- production_countrylocation[order(production_countrylocation$production, decreasing = T),]

  top <- sorted[1:n,c(1,2)]
  return(top)
}


#' top 20 countries form worst to best aroma
#'
#' @description top 20 countries that are the largest coffee producers, sorted based on the score for coffee aroma
#' @example top20_producers_Aroma_sorted()
#' @import dplyr
#' @export
top20_producers_Aroma_sorted <- function(data = coffee){
  #find the top 20 countries that produce largest amount of coffee and sort them based on the coffee aroma
  #from worst to best,

  coffee$production<- coffee$Data.Production.Number.of.bags*coffee$Data.Production.Bag.weight
  # sort our data set based on the highest coffee production
  sorted_production <- coffee[order(coffee$production, decreasing = T),]

  #only keeping the top 20 counties
  top_20_coffee_producer <- sorted_production[1:20,]

  #find the country that produces the coffee with the worst aroma
  worst_aroma_country <- top_20_coffee_producer$Location.Country[top_20_coffee_producer$Data.Scores.Aroma == min(top_20_coffee_producer$Data.Scores.Aroma)]
  library(dplyr)
  #sort the top coffee producer countries based on the Aroma that they produce(dont waste money on them!)
  worst_Aroma <- top_20_coffee_producer %>% select(Location.Country,Data.Scores.Aroma) %>% arrange(Data.Scores.Aroma) %>% select(Location.Country)
  return(worst_Aroma)
}


#' Correlation between altitude and the quality of coffee
#'
#' @description this function produces a model that investigates linear relationship between average altitude of a location and the total score for the coffee quality
#'
#' @details the data for average altitude and total score was not normal. outliers were deleted and boxcox transformation was applied
#' @details The fianl model outputs slope and intercept for predicting square of total score from and logarithm of average altitude
#' @keywords simple linear regression, boxcox transformation
#'
#' @examples  avg_location_altitude_totalScore()
#' @export
avg_location_altitude_totalScore <- function(data = coffee){

  #Is there any relationship between the country’s average location altitude and the total coffee score?
  #since the data was not normal, I used boxcox to figure out what is the best way to make my data normal. for total data score, score less than 70 were removed(8 out of 989)
  # Then the data was normalozid using squared of the average total.
  #the location with zero altitude was removed
  #remove the outlines in data
  work <- coffee %>%  filter(coffee$Data.Scores.Total >70 & Location.Altitude.Average > 400 & Location.Altitude.Average < 5000)
  normal_totalscore <- work$Data.Scores.Total ^ 2

  # alternation suggested by the boxcox to make the data normal was 1/square()
  normal_avgaltitude <- log(work$Location.Altitude.Average)
  model <- lm(normal_totalscore ~ normal_avgaltitude, data = work)
  return(model)
}

#' Difference between method of processing coffee and the coffee aroma
#'
#' @description this function compares the total score for Aroma between different methods of preparation based on Tukey's honestly significant difference test
#'
#' @details In order to normalize the data, outliers and the were removed
#'
#' @details if the processing method was unknown, it was not included in the study.
#'
#' @import dplyr
#'
#' @example Processing_method_Aroma()
#' @keywords simple linear regression model, Tukey HSD
#' @export

Processing_method_Aroma <- function(data = coffee){

  #removing the data where the processing method is nan and total score is less than 70 for making the data normal

  processing_method_totalscore <- coffee %>% filter(Data.Type.Processing.method != "nan" & coffee$Data.Scores.Total > 70)
  #determine if the coffee processing method is associated with the coffee's aroma
  model2 <- lm(processing_method_totalscore$Data.Scores.Aroma^2 ~ processing_method_totalscore$Data.Type.Processing.method)
  final <- anova(model2)
  return(TukeyHSD(aov(model2)))
}


#' Difference between coffee color bean and total quality grade
#'
#' @description this function compares the total score for total coffee quality between different coffee bean color based on Tukey's honestly significant difference test
#'
#' @details If the coffee bean color is unknown, it was removed from the study.
#'
#' @keywords simple linear regression model, Tukey HSD
#'
#' @example CoffeeBeanColor_totalQuality()
#'
#' @import dplyr
#' @export
  CoffeeBeanColor_totalQuality <- function(data = coffee){
  library(dplyr)
  #removing the data where the color is unknown and total score is less than 70 for making the data normal

    color_totalscore <- coffee %>% filter(Data.Color != "None" & coffee$Data.Scores.Total > 70)

    model3 <- lm(color_totalscore$Data.Scores.Total^2 ~ color_totalscore$Data.Color)

    final<- TukeyHSD(aov(model3))

    return(final)
  }

#' A barplot of coffee total quality score grouped by the coffee bean color
#' @example colorBean_totalScore_plot()
#'
#' @import ggplot2
#'@export
  colorBean_totalScore_plot <- function(data = coffee){
    #barplot of coffee color bean and the total score:

    library(ggplot2)

    color_totalscore <- coffee %>% filter(Data.Color != "None" & coffee$Data.Scores.Total > 70)
    plot<- ggplot(data = color_totalscore, aes(x=Data.Color, y=Data.Scores.Total, fill = Data.Color)) +
      geom_bar(stat="identity")+ylim(0, max(coffee$Data.Scores.Total)*1.1) + theme(legend.position="top")
    return(plot)
  }
#' A barplot of coffee total quality score grouped by the coffee color bean and the coffee processing method
#'
#' @example processing_method_totalscore_plot()
#'
#' @import dplyr
#'
#' @import ggplot2
#' @export

processing_method_totalscore_plot <- function(data = coffee){

  #barplot of each coffee color bean, method of processing and the total score!
  library(dplyr)
  library(ggplot2)
  color_totalscore <- coffee %>% filter(Data.Color != "None" & coffee$Data.Scores.Total > 70)
  plot<-  ggplot(data = color_totalscore, aes(x=Data.Color, y=Data.Scores.Total, fill = Data.Type.Processing.method)) +
  geom_bar(stat="identity",position=position_dodge())+
    ylim(0, max(coffee$Data.Scores.Total)*1.1) +
    scale_fill_brewer(palette="Blues")
return(plot)
}

#' A barplot of countries that are the largest coffee producers in descending order
#'
#' @example Country_highestProduction_plot()
#'
#' @import dplyr
#'
#' @import ggplot2
#' @export


Country_highestProduction_plot <- function (data = coffee){
  #recreating old dataseets
  coffee$production<- coffee$Data.Production.Number.of.bags*coffee$Data.Production.Bag.weight
  sorted_production <- coffee[order(coffee$production, decreasing = T),]
  top_20_coffee_producer <- sorted_production[1:20,]
  plot1<-ggplot(top_20_coffee_producer, aes(y=production,x =Location.Country, color = Location.Country)) +
    geom_bar(stat="identity", fill="gray")
return(plot1)
}

#' A scatter plot demonstrating the relationship between average altitude of a country and the total quality score
#'
#' @example Altitude_totalSCore_plot()
#'
#' @import dplyr
#'
#' @import ggplot2
#'
#' @export
#'
Altitude_totalSCore_plot <- function (data = coffee){
  library(dplyr)
  library(ggplot2)
#liniear relationship between altitude and total score
  work <- coffee %>%  filter(coffee$Data.Scores.Total >70 & Location.Altitude.Average > 400 & Location.Altitude.Average < 5000)
  plot<- ggplot(data = work,aes(x =log(Location.Altitude.Average), y = Data.Scores.Total ^ 2)) + geom_smooth() + geom_smooth(method = "lm", color = "peachpuff", se=FALSE)
return(plot)
}

