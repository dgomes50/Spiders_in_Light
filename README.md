[![DOI](https://zenodo.org/badge/235926282.svg)](https://zenodo.org/badge/latestdoi/235926282)

This data is from August 13-23 of 2018 on the Saint-Symphorien suspension bridge over the Loire River in Tours, France (47.399158° N, 0.692519° E). This 350m pedestrian bridge consists of 324 individual metal side railing panels within which orb-weaving spiders (Family Araneidae) constructed webs. Bridge panels had an alternating lighting pattern; every other panel was lit from above by a blue fluorescent tube light, which was absent from the other panels. These two conditions acted as a natural block-on block-off design within I measured spider abundance, web-dimensions, prey capture success, and body condition. In artificially lit conditions, spiders caught more prey with smaller webs, and had higher body condition. However, there were fewer spiders with active webs in those lit areas. 

BC.csv contains data on individual Larinioides sclopetarius body size (femur length in mm) and weight (in g). Body condition can be calculated with the attached R script.
TOURS.csv contains data on spider abundance in bridge panels. This is the number of spiders which had active orb webs in each panel (either lit or unlit).
webs.csv contains data on spider web dimensions and prey capture success. These web dimensions can be used to calculate vertical web asymmetry and total web catch area.

Tours_Spider.R contains the scripts to read all three csv files above, analyze the data, and plot the data.
