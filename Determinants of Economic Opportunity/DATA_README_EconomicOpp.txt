File(s): 

	2018_Tract_Data_EconomicOpp.csv



Used Column(s): 

	Geographic Area Name, id_Fix, INDEX_Overall, INDEX_carequal, INDEX_housing, INDEX_linguistics, INDEX_broadband


Maps to make:
	Determinants of Economic Opportunity (index map using INDEX_Overall data)
	Associate's degree or higher (INDEX_associates)
`	Affordable housing (INDEX_housing)
	Linguistic isolation (INDEX_linguistics)
	Access to broadband (INDEX_broadband)

Table Columns:
	County
	Tract Name
	Quintile
	Ranking Overall
	% of Income on Housing
	Access to Broadband Subscription
	% Linguistically Isolated
	% Population With Associate's Degree of Higher

Hoverbox:
	Determinants of Economic Opportunity
		County
		Tract Name
		Ranking Overall
	Associate's degree or higher
		County
		Tract Name
		% Population With Associate's Degree of Higher
	Affordable housing 
		County
		Tract Name
		% of Income on Housing
	Linguistic isolation 
		County
		Tract Name
		% Linguistically Isolated
	Access to broadband 
		County
		Tract Name
		Access to Broadband Subscription


Notes:
	Generally speaking, 1 is good, 5 is bad. 




Column Description:

	id_Fix: 
		Usable part of GEO_ID for leaflet maps

	Geographic Area Name: 
		County, Tract, or State name (ie Malheur County, `Census Tract 201, Canyon County, Idaho`, etc.)

	INDEX_Overall:
		The overall index, values range from 1-5 (where 5=the bottom quintile/worst, and 1= top quintile/best)