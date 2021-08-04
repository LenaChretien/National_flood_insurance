# National_flood_insurance

FEMA national flood insurance data, found here: 
https://www.kaggle.com/lynma01/femas-national-flood-insurance-policy-database

with documentation found here:
https://www.fema.gov/media-library/assets/documents/180376

and here: 
https://nfipservices.floodsmart.gov/manuals/jan_2015_consolidated_trrp.pdf

This data contains multiple fields about anonymized flood policy holders in the United States:
    Flood zone rating
    Building elevation
    Zip code, county code, census tract, state code, city code
    Building type
    Business type
    Flood insurance deductibles
    Total insurance coverage
    Total insurance premium of the policy
    How the policy was originally rated

A list of flood zones can be found here: 
https://www.leegov.com/dcd/flood/defined




BEWARE: the file has information of 50,000,000 policies and is 12 GB large. 
For easier processing, the file was split into sub - csv files and then  subset further to states. In additiononly only 20 of the original 45 columns were kept.
The code used for this can be found in 
"loading_data.R" 


Policycost of larger than $5000 were excluded, due to small numbers of them. 




