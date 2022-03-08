# Biodiversity Graph Database

The coding of information from included papers in the systematic map will result in a graph database.

We will code data directly into the graph database using a custom interface. For this process, certain data inputs are required:

1. List of data sources that have been included according to inclusion criteria (located in *source* subdirectory).
2. Data types for entities in the system.

### Design criteria

* The user interface lists papers that have been included according to the inclusion criteria.

* When the user selects a paper that has yet to be coded, information on the population, exposure and outcome can be input directly into the graph format.
  * A user first links a source to an individual study timeline. The individual study timeline is linked from the temporal index present in the system by inputting dating method readings (e.g. radiocarbon dates).
  * The temporal index can be modified to add in additional time labels to dates.
  * The user can state HAS_PROXY_INFO for a population proxy within the individual study timeline. The proxy is configured specific to the study timeline, using previously entered or newly entered *proxy for P*, *inference method*, and *taxonomy* nodes.
  * A user can add additional required properties to a source paper's timelines (e.g. spatial extent / location).

