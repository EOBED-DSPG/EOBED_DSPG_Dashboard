File(s): 

	2018_Tract_Data_StateofEconomicOpp.csv



Used Column(s): 

	Geographic Area Name, id_Fix, INDEX_Overall, INDEX_poverty, INDEX_Employment, INDEX_Commute, INDEX_Hours, INDEX_SNAP

Maps to make:
	State of Economic Opportunity (index map using INDEX_Overall data)
	Poverty (INDEX_poverty)
	Employment status (INDEX_Employment)
	Time to commute to work (INDEX_Commute)	
	Hours worked per week (INDEX_Hours)	
	SNAP recipients living in poverty (INDEX_SNAP)		

Table Columns:
	County
	Tract Name
	Ranking Overall
	Quintile
	Commute Time
	% Employed
	% Below Poverty
	Median Hours Worked
	% Living in Poverty Receiving SNAP

Hoverbox:
	State of Economic Opportunity
		County
		Tract Name
		Ranking Overall
	Poverty 
		County
		Tract Name
		Ranking Overall
	Employment status 
		County
		Tract Name
		% Employed
	Time to commute to work 
		County
		Tract Name
		Commute Time
	Hours worked per week 
		County
		Tract Name
		Median Hours Worked
	SNAP recipients living in poverty
		County
		Tract Name
		% Living in Poverty Receiving SNAP
Notes:
	Generally speaking, 1 is good, 5 is bad. 




Column Description:

	id_Fix: 
		Usable part of GEO_ID for leaflet maps

	Geographic Area Name: 
		County, Tract, or State name (ie Malheur County, `Census Tract 201, Canyon County, Idaho`, etc.)

	INDEX_Overall:
		The overall index, values range from 1-5 (where 5=the bottom quintile/worst, and 1= top quintile/best)