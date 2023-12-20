# Bilirubin chart - shiny application

The graph depends on the gestational age (> or < 35 weeks):

## Bilirubin chart for term infants (> 35 weeks)
Enter
1. Date and hour of birth (yyyy-mm-dd)
2. Date and hour of sampling (hh:mm)
3. Bilirubin value in mg/dL

Values will appear on the graph, with on the latest value the annotated thresholds for low risk, intermediate risk and high risk. 

## Bilirubin chart for preterm infants (< 35 weeks)
Enter
1. Gestational age in the following format: 23+1/7 (= 23 weeks and 1 day) or directly 23.14284.
2. Bilirubin value in mg/dL

Values will/should appear on the graph.
   
## Advanced usage
Manual input allows three timepoints. However, unlimited time points can be entered through the advanced panel and through a HTTP GET request. The input for multiple values is in CSV format. If values are entered in advanced mode (`?advanced=ja`), all values from manual mode are overridden.
For the preterm (`&prematuur=ja`) chart, only the gestational age (`&PML_GET=23.14,24.15`) and the bilirubin level (`&bili_GET=10,9`) is required.
For the term (`&prematuur=nee`) chart, the date/hour or birth (`geboorte_GET=2023-11-22%2010:00:00`) + date/hour of sampling (`afname_GET=2023-11-23%2010:00:00,2023-11-24%2010:00:00`) and bilirubin levels (`&bili_GET=10,9`) are required.

Phototherapy can be annotated on the graph through advanced settings (`&PT_start_GET` and `&PT_stop_GET`). For this, the length of both parameters need to be equal (i.e. 2 start and 2 stop points. If for example phototherapy was started but not stopped, 'NA' can be used instead (`&PT_stop_GET=2023-11-23%2015:00:00,NA`). Phototherapy for the preterm graph is not supported yet.

Examples:
- Term: http://rubenvp.shinyapps.io/bilicurve/?advanced=ja&naam=testbaby&geboorte_GET=2023-11-22%2010:00:00&afname_GET=2023-11-23%2010:00:00,2023-11-24%2010:00:00&bili_GET=10,9
- Preterm: http://rubenvp.shinyapps.io/bilicurve/?advanced=ja&naam=testbaby&prematuur=ja&PML_GET=23%2B1/7,24%2B1/7&bili_GET=10,9
- Term with phototherapy:  http://rubenvp.shinyapps.io/bilicurve/?advanced=ja&geboorte_GET=2023-11-22%2010:00:00&afname_GET=2023-11-23%2010:00:00,2023-11-24%2010:00:00&bili_GET=10,9&PT_start_GET=2023-11-23%2011:00:00,2023-11-24%2012:00:00&PT_stop_GET=2023-11-23%2015:00:00,2023-11-24%2014:00:00


## Installation 

The application can be reached at rubenvp.shinyapps.io/bilicurve or can be self-hosted.

Selfhosting of the shiny application is possible through Docker:

```bash
docker run -dp 0.0.0.0:3838:3838  rmvpaeme/bilicurve:0.2
```

After installation, the shiny server can be accessed at

```
localhost:3838
```

or 

```
server-ip:3838
```

## References
The data is obtained from:
- Maisels MJ, Bhutani VK, Bogen D, Newman TB, Stark AR, Watchko JF. Hyperbilirubinemia in the newborn infant > or =35 weeks' gestation: an update with clarifications. Pediatrics. 2009;124(4):1193-1198. doi:10.1542/peds.2009-032 
- Maisels MJ, Watchko JF, Bhutani VK, Stevenson DK. An approach to the management of hyperbilirubinemia in the preterm infant less than 35 weeks of gestation. Journal of Perinatology 2012;32:660-4.
