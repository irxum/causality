# causality
Joint Analysis of Hotel Review &amp; Historical Local Economy Metrics for Causal Topics
* Project background - [AlexKwan_ProjectProposal.pdf](https://github.com/irxum/causality/blob/master/AlexKwan_ProjectProposal.pdf)
* Short evaluation of useful packages - [AlexKwan_TechnologyReview.pdf](https://github.com/irxum/causality/blob/master/AlexKwan_TechnologyReview.pdf)
* Presentation with audio - [Team5ProjectPresentation.pptx](https://github.com/irxum/causality/blob/master/Team5ProjectPresentation.pptx)
* R script - [team5proj.R](https://github.com/irxum/causality/blob/master/team5proj.R)  

# instructions
1.  Download contents to chosen directory
2.  Modify team5proj.R (line 55) "wk_dir" to directory chosen in (1)
3.  Use R64bit version 3.3.3 and run team5proj.R
4.  Full code execution can take up to twenty minutes.

# Data Sources:
Hotel Reviews found in "./chicago" directory
- From UCI Machine Learning repository
- Sampling of Chicago-area reviews
- 138 hotels from 2001 to 2009 

Economic Indicator found in "chicago_gdppercapita.csv"
- From The Open Data Network 
- Chicago area Gross Domestic Product per capita
- From 2003 to 2009

# Code walk-through of team5proj.R
* Import text from hotel review files into list of texts [lines 58-85]
* Extract review text and date of entries from list of texts [lines 90-118]
* Use stm package to
  * Remove stopwords and conduct stemming [lines 124-131]
  * Estimate appropriate number of topics [lines 134-157] [this section is commented out to make execution shorter]
  * Determine topics [lines 159-165]
* Take top topic (topic1) and determine percent prevalence in corpus [lines 169-196]
* Import data for Chicago GDP per capita [lines 198-201]
* Perform Granger test [lines 205-213]
* Plot GDP and topic1 ratio [lines 217-231]

