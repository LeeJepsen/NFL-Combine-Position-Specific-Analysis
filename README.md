# NFL-Combine-Metrics-and-Pro-Bowl-Outcomes

## Why
NFL teams invest significant amount of resources in the NFL Combine, but it's still unclear which tests provide the most meaningful signal for position specific demands. 
Rather
than treating "success" as one size fits all, this analysis examines whether combine metrics differentiate Pro Bowl caliber players within position groups.  

The goal is to explore which measurable traits may support scouting, and player development decisions by position.

## Football Question
Within each position group, which combine metrics differentiate Pro Bowl caliber players from their peers?

## Data
-  NFL Combine performance data from **2000-2023**
-  Pro Bowl selection used as a proxy for elite NFL Performance
-  Drafted players only
-  Players grouped into position specific categories (e.g., DB, LB, DE, OL)

## Approach
Rather than building a single league-wide prediction model, this analysis reflects how teams evaluate players **within positional roles**.

The Workflow:
-  Compared Combine performance between Pro Bowl and non-Pro Bowl players by position
-  Tested for meaningful differences using position specific statistical methods
-  Applied simplified logistic regression models to evaluate **associations**, not predictions


## Key Takeaways
-  Combine traits associated with Pro Bowl selection varied meaningfully by position
-  Broad Jump (a measure of lower-body power) showed stronger association than straightline speed (40 yard dash) for several skill positions
-  Size and strength metrics were more informative for lineman and linebackers
-  Overall, Combine metrics explained only a portion of elite outcomes, showing physical ability context but reinforces the importance of contextual evaluation

➡️ Detailed results by position, including metric-level findings, are available in
[results/position_results.md](results/position_results.md).


## Visual Summary
![ANOVA Significance Heatmap](figures/anova_significance_heatmap.png)

*Position specific one-way ANOVA tests were used to compare Combine performance between Pro Bowl and non-Pro Bowl players within each position group, allowing identification
of which metrics meaningfully differed at each position.*




