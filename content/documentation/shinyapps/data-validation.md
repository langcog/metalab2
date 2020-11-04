---
title: Data Validator
type: book
weight: 100
---

To ensure that new datasets being added to the MetaLab website are valid, run the Data Validation application. The application tests the new datasets on different criteria, such as the existence of necessary columns (ex. fields in the “long_cite” column are required), the appropriate length of a certain column (ex. fields in the “short_cite” column should be less than length of 60 characters), and the appropriate range of a certain column (ex. fields in the “corr” column should have a value between -1 and 1).

The data validation tool allows you to validate datasets from 3 different sources:  

**MetaLab Data** - these datasets are already part of the MetaLab database 

**External URL** - this allows you to validate a Google sheet by providing the Google sheet URL 

**Upload CSV** - this allows you to validate a csv file that is on your local computer 
