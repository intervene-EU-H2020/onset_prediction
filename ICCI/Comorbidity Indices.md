# Charlson Comorbidity Index 
#### [Manitoba Centre for Health Policy (MCHP) Concept Description](http://mchp-appserv.cpe.umanitoba.ca/viewConcept.php?printer=Y&conceptID=1098 )
#### Yurkovich et al. - 2015 - *A systematic review identifies valid comorbidity indices derived from administrative health data*

- Each comorbidy category of the ICD gets a weight (1-6)
	- Weight is based on adjusted risk of mortality or resource use
	- “adjusted HR for each comorbid condition derived from Cox proportional hazards regression models” (Yurkovich et al. - 2015)
- Sum off all weights gives score
- The original version with 19 categories was reduced to 17 by Deyo et al. 1992 in *Adapting a clinical comorbidity index for use with ICD-9-CM administrative databases* by combining "Leukemia" and "Lymphomas" into one category.
- In  Sundararajan et al. - 2007 - *Cross-national comparative performance of three versions of the ICD-10 Charlson Index* they find the three different ICD-10  codings all perform well
- “Most widely used comorbidity index [...] validated in patient populations with various diagnoses or undergoing various surgical procedures” (Yurkovich et al. - 2015)

### Original publication. 
#### Charlson et al. - 1987 - *A new method of classifying prognostic comorbidity in longitudinal studies: development and validation.*

-  “adjusted relative risks” 
		-  Estimator for the risk of death with a given comorbid condition when controlling for other diseases, illness severity, and reason for admission. 
		-  Based on beta coeffs from proportional hazards (stepwise backward?)
		-  Three weights 1-3 and 6 (rounded 1.2 - 1.5 = 1 etc) for 19 different comorbities.
-  Comorbidies != Diagnoses
		-  Ignores conditions that were treated or "resolved"
		-  Comments from the webpage: Longitudinal studies could benefit from all diagnoses as a development over time. For short period studies should stick to comorbidies to not overestimate disease burden.

### ICD-10 adaption
[Comparison chart for Halfon vs Quan ICD-10 Codes](http://mchp-appserv.cpe.umanitoba.ca/concept/Halfon-Quan-ComparisonofCharlsonICD-10Codes-July2005.pdf#View=Fit)

#### Quan et al. - 2005 - *Coding Algorithms for Defining Comorbidities in ICD-9-CM and ICD-10 Administrative Data*

- Adapted Deyo-CCI to ICD-10 codes, based ony multiple expert opinions
- More systematic approach to creating ICD-10 code translations from the original Deyo-Charlson index grouping (compared to Halfon et al. - 2002)
- List of ICD-9-CM and ICD-10 codes used: [ ICD-9-CM and ICD-10 Coding Algorithms for Charlson Comorbidities](http://mchp-appserv.cpe.umanitoba.ca/concept/Charlson%20Comorbidities%20-%20Coding%20Algorithms%20for%20ICD-9-CM%20and%20ICD-10.pdf)

# Elixhauser Comorbidity Index

- “Elixhauser et al. developed a comorbidity index comprised of a comprehensive set of 30 comorbidities defined using ICD-9-CM codes from administrative data” (Yurkovich et al. - 2015)
- Problem: 30 dichotomous variables without weighing and single score system
- “both the EI and CCI demonstrated poor-to-excellent ability to predict various outcomes” (Yurkovich et al. - 2015)
- Comparison of performance with CCI for predicting mortality
	- 9 studies: All EI versions better than CCI versions
	- 4 studies find no difference
	- 1 study finds Romano CCI better than EI
# Chronic Disease Score
- CDS identify comorbidities based on medication use records instead of diagnostic codes 
- “experts evaluated patterns of use of selected med- ications to create disease categories, and weights were as- signed by consensus” (Yurkovich et al. - 2015)
- Clark et al. changes the weighting of disease categories to be based on regression models
# [Häppölä et al. - 2020 - A data-driven medication score predicts 10-year mortality among aging adults](../../../../undefined)
Short description of how we were planning to calculate the Charlson index within the INTERVENE project.
# [C-index](../../../../undefined)

# R packages with implementation
- https://www.rdocumentation.org/packages/icd/versions/3.3:
	- Package ‘icd’ was removed from the CRAN repository. Archived on 2020-10-06 as check problems were not corrected in time.
- https://www.rdocumentation.org/packages/comorbidity/versions/1.0.2

