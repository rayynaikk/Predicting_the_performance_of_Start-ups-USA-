# Predicting_the_performance_of_Start-ups-USA-
The goal of this project would be to precisely forecast the possibility of success or failure for a  certain startup, depending on pertinent factors and data points. As a result, the model may  give information to investors, business owners, and other stakeholders who are interested in  startup’s success rates and other useful insights.


Abstract:
Startups have become a vital contributor to economic growth by bringing new ideas, spurring 
innovation, and creating employment opportunities. However, predicting the success of a 
startup remains a complex challenge due to the high level of uncertainty and failure rates 
associated with startups. The following dataset provides industry trends, investment insights, 
and individual company information to help investors identify startups with the potential for 
rapid growth and success. With 45 features, including funding rounds, total funding, 
milestones, and industry type, this dataset can offer valuable insights into the factors that 
contribute to a startup's success or failure. The target variable is the startup's “Status”
(acquired or closed), can be used to make informed decisions and gain a competitive edge in 
the market.


Data Source and Description:
Data Source: https://www.kaggle.com/datasets/manishkc06/startup-success-prediction
Following are the variables present in the dataset along with their description.


No. Variables Type Description

1 state_code Chr Shows the state codes

2 latitude Num Shows the latitude for geographical location

3 longitude Num Shows the longitude for geographical location

4 zip_code Chr Shows the zip code

5 id Chr This is the unique id for the startup

6 city Chr Address details: City

7 name Chr Name of the startup

8 labels Int Does the startpup has labels or not

9 founded_at Date Shows the founding date

10 closed_at Date Shows the closing date

11 first_funding_at Date Date of first funding

12 last_funding_at Date Date of last funding

13 age_first_funding_year Num Years of existence at the time of first funding

14 age_last_funding_year Num Years of existence at the time of last funding

15 age_first_milestone_year Num Years of existence at the time of first milestone achieved

16 age_last_milestone_year Num Years of existence at the time of last milestone achieved

17 relationships Int Number of relationships a startup has

18 funding_rounds Int Number of funding rounds did the startup made S. No. Variables Type Description

19 funding_total_usd Num Total amount of funding

20 milestones Int Number of milestones a startup has achieved

21 is_CA Int Is the startup located in California

22 is_NY Int Is the startup located in New York

23 is_MA Int Is the startup located in Massachusetts

24 is_TX Int Is the startup located in Texas

25 is_otherstate Int Is the startup located in States than CA, NY, TX or MA

26 category_code Chr Category of Startup

27 is_software Int Is the startup in software field

28 is_web Int Is the startup in web oriented

29 is_mobile Int Is the startup in mobile oriented

30 is_enterprise Int Is the startup an enterprise operation


31 is_advertising Int Is the startup into advertising domain

32 is_gamesvideo Int Is the startup into gaming domain

33 is_ecommerce Int Is the startup into ecommerce domain

34 is_biotech Int Is the startup into biotech domain

35 is_consulting Int Is the startup into consulting domain

36 is_othercategory Int Is the startup into other domain

37 has_VC Int Does the startup have a VC (Venture Capitalist)

38 has_angel Int Does the startup have an angel investor

39 has_roundA Int Does the startup have secured a round A funding

40 has_roundB Int Does the startup have secured b round B funding

41 has_roundC Int Does the startup have secured c round C funding

42 has_roundD Int Does the startup have secured d round D funding

43 avg_participants Num Number of average participants

44 is_top500 Int Is the startup a top 500 company

45 status Chr Describes if the startup is a success or a failure


Data Exploration – We will look at the following queries using summary statistics and 
visualization tools:

1. What is the overall success/failure rate of startups in various places and categories?
2. The link between various financing categories and qualities, such as the first and last 
funding time differences, location, date of foundation, rounds of funding, milestones, etc.
Prediction Model – To predict the startup’s success or failure, we will use various 
classification models.


1. Data Understanding and Preparation: Here, we may execute data cleaning, merging, 
exploration, removing/fixing missing values, feature engineering, checking for duplicates,
etc.
2. Modelling: Using accessible and significant features discovered via data exploration, we 
will apply classification models to forecast the performance for startups (column: status 
(acquired/closed)).
3. Validation: Create performance measures and validate the model's effectiveness using 
test data.
4. Conclusion: What can we learn from our classification model for performance of startups
and how can we use this model to help investors make informed investment decisions in 
startups?
** End of Document*
