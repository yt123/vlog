# Predicting Big 5 personality types based on vlog transcripts

The YouTube Personality Dataset
-------------------------------

The YouTube personality dataset consists of  a collection of behavorial features, speech transcriptions, and personality impression scores for a set of 404 YouTube vloggers that explicitly show themselves in front of the a webcam talking about a variety of topics including personal issues, politics, movies, books, etc.  There is no content-related restriction and the language used in the videos is natural and diverse. 
------------------------------------------------------------------------------------------------------------------------------------------

Feature extraction: 
1. counted the total number of words of each vlogger
2. counted the percentage of words that are related to Big 5 personalities
3. sentiment analysis

Modeling: 
1. created a baseline multiple regression model
2. checked multicollinearity of the variables (remove those with VIF > 10)
3. used backward selection to select the features
4. removed outliers
5. modeled non-linear relationships
6. modeled interaction with gender
