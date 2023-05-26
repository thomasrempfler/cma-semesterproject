# cma-semesterproject
Semester project of the course "Computational Movement Analysis"


| Semester:      | FS23                                     |
|:---------------|:---------------------------------------- |
| **Data:**      | GPS locations of red deer  |
| **Title:**     | Migration of red deer                |
| **Student 1:** | Thomas Rempfler                        |
| **Student 2:** | -                      |

## Abstract 
The classification of red deer individuals of 6 study areas in residents, migrants and dispersers has previously been done in a master thesis at the University of Zurich using a QGIS plug-in called "MigrO". Based on these results of seasonal migration I describe patterns in these trajectories.

## Research Questions
The sampling interval in the trajectories varies between the study areas from 1 and 3 hours. Except 1 study area, the interval was regular. Hence, as an overarching research question I test the results for consistency among different sampling intervals. In detail I focus on 2 research questions in the following list, depending on what works with the data.

1) How do the individual migrations proceed?
   a) Timing (start and end),
   b) Duration,
   c) Elevation,
   d) Stopovers,
   e) Distances,
   f) Meets between individuals,
   g) Migration routes,
   h) Repetitive migration routes between years

2) How do hunting ban areas and winter rest zones influence the migrations?

3) Are there differences between the sexes and between the study areas?

## Results / products
I expect a big individual variation in terms of timing, duration, etc. of migration and therefore the variation will exist also between the study areas. I further expect that red deer mostly have persistant migration behaviour over the years. In addition they might prefer hunting ban areas and winter rest zones and probably hop from one to another.

## Data
I use GPS location data of red deer individuals of 6 study areas. Additionally, I use the hunting ban areas and winter rest zones. All data I already have prepared.

## Analytical concepts
Unconstrained continuous locations:
- Calculate Euclidean distances for 1b) - 1f)
- Segmentation into static and moving segments for 1b) - 1f)
- Meet patterns
- Sequence of static segments for 2)
- Statistical testing for 3) and the consistency in the results among different sampling intervals

## R concepts
I will use the R packages tidyverse, data.table, lubridate, sf, mapview, (zoo), plotly, (raster), as well as loops and functions.

## Risk analysis
A big challenge will be the amount of data. I have dozens of individuals and might struggle with costly analyses. In case of problems, for this project I would reduce the dataset or tasks (see "Research Questions"). Another challenge could arise if the classification of the individuals in residents, migrants and dispersers is not compatible to my intended analyses. I then would have to classify the individuals with own methods.

## Questions? 
- Working on an IT network
- Detecting migration routes, intensity of use incl.
- Others may arise.
