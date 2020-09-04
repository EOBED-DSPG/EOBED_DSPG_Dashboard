File(s): 

	2018_Tract_Data_HealthQual.csv



Used Column(s): 

	Geographic Area Name, id_Fix, INDEX_Overall, INDEX_qual, INDEX_readminrate, INDEX_LifeExpect, INDEX_InfantMort


Maps to make:
	Heathcare Outcomes and Quality (index map using INDEX_Overall data)
	Care and Quality Ratings (INDEX_qual)
	30-day Readmission Rate (INDEX_readminrate)
	Life Expectancy (INDEX_LifeExpect)
	Infant Mortality Rate (INDEX_InfantMort)
	
Table columns:
	County
	Census Tract
	Ranking Overall
	Quintile
	30 Day Hospital Wide Readmission Rate
	Cleanliness - star rating
	Nurse communication - star rating
	Doctor communication - star rating
	Staff responsiveness - star rating
	Communication about medicines - star rating
	Discharge information - star rating
	Care transition - star rating
	Overall hospital rating - star rating
	Quietness - star rating
	Recommend hospital - star rating
	Life Expectancy
	Infant Mortality Rate

Hoverbox:
	Heathcare Outcomes and Quality
		County
		Census Tract
		Ranking Overall
	Care and quality ratings
		County
		Census Tract
	30 day readmission rate
		County
		Census Tract
		30 Day Hospital Wide Readmission Rate
	Life expectancy
		County
		Census Tract
		Life Expectancy	
	Infant mortality rate
		County
		Census Tract
		Infant Mortality Rate
Notes:
	Generally speaking, 1 is good, 5 is bad. 




Column Description:

	id_Fix: 
		Usable part of GEO_ID for leaflet maps

	Geographic Area Name: 
		County, Tract, or State name (ie Malheur County, `Census Tract 201, Canyon County, Idaho`, etc.)

	INDEX_Overall:
		The overall index, values range from 1-5 (where 5=the bottom quintile/worst, and 1= top quintile/best)