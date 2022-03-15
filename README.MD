Palaeo-Biodiversity Indicators - Graph Database
---

A graph database designed to represent biodiversity indicators within the palaeoecological and palaeoenvironmental record.

The following core components exist in the graph:

* Time index: based on nodes that represent individual years, and related in sequence, the time index is used to relate occurrences, time-series and individual dates to points in time. The time index also supports qualitative time periods (e.g. pollen zones).

* Taxonomic tree: the taxonomic tree is based on nodes with relations representing sub-taxa and parent taxa.

* Literature sources: representing bibliographic and grey literature sources.

Based on these components, the following concepts are modelled:

* Biotic proxies and inference to specific taxa: intermediate nodes are used to represent a hyper-edge between a biotic proxy, a method of inferring the 'real' taxon, and the 'real' taxon. The taxon may be of any rank. If there is no given method of inference (e.g., use of a particular morphological key), then the method of inference may be marked as implicit. A single occurrence of the intermediate node will therefore exist for each morphotype or biotic unit identified in a particular study timeline. 

* Study timelines: a study timeline of a particular temporal extent may be continuous or discontinuous (with hiatuses). For dating, it may either be of known continuous time periods (e.g. annual wood increments), or discontinuous based on a number of individual dates. Individual dates are themselves represented as nodes, which contain properties relating to dating method, the material dated, and any calibration used.

* Timeline contexts: the sampling context of a particular timeline is represented in a context node. This contains information on the geographical location and parent material (e.g., peat core, sediment core).