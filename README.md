# Bilirubin chart - shiny application

## Bilirubin chart for term infants (> 35 weeks)
Enter
1. Date and hour of birth
2. Date and hour of sampling
3. Bilirubin value in mg/dL

Values will appear on the graph, with on the latest value the annotated thresholds for low risk, intermediate risk and high risk. 

## Bilirubin chart for preterm infants (< 35 weeks)
Enter
1. Gestational age in the following format: 23+1/7 (= 23 weeks and 1 day) or directly 23.14284.
2. Bilirubin value in mg/dL
3. Values will appear on the graph.
   
## Advanced usage
Manual input allows three timepoints. However, unlimited time points can be entered through the advanced panel. If values are entered in advanced mode, all values from manual mode are overridden.

## Installation 

Installation is possible through Docker:

```bash
docker run -dp 0.0.0.0:3838:3838  rmvpaeme/bilicurve:0.1
```

Shiny server can be accessed at

```
localhost:3838
```
