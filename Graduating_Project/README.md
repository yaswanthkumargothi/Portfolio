### Abstract


Mass shootings and their impact personally and economically are currently at the forefront of political debate in the United States. The foundation of this project revolves around Mass shootings impact on restaurant reviews supported by background literature detailing novel approaches in Sentiment Analysis and transfer learning to set up the framework for the research on mass shootings's impact on Public opinion. The accuracy of the Transfer Learning model was compared with a rule-based approach on three different Test sets. Transfer Learning outperforms rule-based approach by 96% to 79% on short-reviews, 94% to 71% on medium reviews, and 94% to 66% on long reviews. The findings in this paper are based on Restaurant Reviews data from Yelp in the year 2017.  Difference in Difference model study has found that there is no effect of 2017 mass shooting event on online restaurant reviews public opinion following a mass shooting event in areas near the shooting event in the city of Las Vegas. 


Keywords: Sentiment Analysis; Causality; Transfer Learning; Rule-based Language Classification; Mass shootings; Online Restaurant Reviews 
 



### Project Flowchart

<img src="images/flowchartlarge.JPG" width="800">

### files

The folder consists of Several files

[review_classifier.py](review_classifier.py): Deep learning model script which produces export.pkl file to infer upon the yelp data

[preprocess.py](preprocess.py): preprocessing script to convert classified data into required cross sectional format for difference in difference modelling 

[yelpdata_explore.ipynb](yelpdata_explore.ipynb): Yelp data variable analysis

[export.pkl](export.pkl): load this pickle file using load_learner method from Fastai and get prediction classifications for data 

[diffndiff.R](diffndiff.R): R file to plot assumptions and build difference in difference models

[data](/data/): This folder consists of processed data from predictions and Yelp data files

[images](/images/): This folder consists of difference in difference assumption trend plots and project flowchart.
