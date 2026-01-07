# Do NFL Combine Metrics Actually Matter? <img src="https://github.com/user-attachments/assets/a5b581be-844b-4311-9786-42afcfe5dc8d" width="50">
**Position Specific Performance Patterns from Pro Bowl Players (2000-2023)**

## Overview
The NFL Combine plays a central role in player evaluation, and its usefulness varies widely by position.  Rather than asking whether Combine testing predicts successful athletes in general, this project examines a more specific question.
- Do NFL Combine metrics meaningfully differentiate higher level players within their position group?
To explore this, NFL Combine data from 2000-2023 were linked with Pro Bowl selections as a proxy for sustained, high level NFL performance.  By comparing Pro Bowl and non-Pro Bowl players within the same position groups, this analysis evaluates which athletic traits provide actionable signal, and where Combine testing may fall short.  This project is exploratory and supports decision making, not a draft prediction model.  Quarterbacks (for most of this analysis), Kickers, Punters, and Fullbacks were excluded because of the lack of athletes and participation in the NFL Combine.

## Why This Matters
NFL evaluation is inherently contextual.  Athletic traits that matter for one position may be irrelevant for another.  Although, Combine results are often interpreted using generalized benchmarks.

This project reframes the Combine as a position-specific tool, to help answer:
- Which metrics actually separate elite players from their peers?
- Where does Combine testing add value?
- Where does it fail to capture on-field success?

## Data Sources
NFL Combine Data (2000-2023) from Pro Football Reference
- 40 Yard Dash
- Vertical Jump
- Bench Press (225lbs)
- Broad Jump
- Three Cone Drill
- Shuttle Drill
- Body Weights

Pro Bowl Selections (Career Indicators)
-  Binary label indication whether a player earned at least one Pro Bowl selection
Players were grouped by position to account for distinct physical role demands across the NFL

## Analytical Approach
The analysis was done in R, revolving around each position:
1. Exploratory Analysis
   -  Summary statistics, tables, and density plots compared Combine distributions from Pro Bowl vs non-Pro Bowl players
   -  Established baseline differences and overlap within each position group
2. Statistical Inference (ANOVA)
   -  One way ANOVA tests were applied within position group
   -  This approach allowed multiple Combine metrics to be evaluated simultaneously while controlling Type 1 error
   -  Quarterbacks were excluded due to incomplete Combine participation
3. Supplementary Modeling (Logistic Regression)
   -  Position specific logistic regression models explored directional relationships between Combine metrics and Pro Bowl likelihood
   -  Models were interpretive, not predictive
   -  Results served as a consistency check rather than a forecasting tool

## Key Findings

<img width="600" height="800" alt="image" src="https://github.com/user-attachments/assets/0fa18976-0dc5-4ed7-bdd3-a1b3828c0507" />


### Overall Contexts
- Across all Combine participants (2000-2023), the empirical probability of earning a Pro Bowl selection was ~ 8.5%
- Roughly 1 in 12 Athletes that participated in the NFL Combine reached Pro Bowl status

<img width="550" height="350" alt="image" src="https://github.com/user-attachments/assets/92900159-dfab-4a28-a483-6cf87a4e65ff" />


### Position Specific
-	**Defensive Backs (DB)**: Clear separation across speed, agility, jumping, and body weight, suggesting movement efficiency paired with sufficient mass.
-	**Offensive Linemen (OL)**: Separation across speed, power, and movement quality, with performance, not size alone, distinguishing higher-level players.
-	**Defensive Line (DL)**: Limited separation overall; bench press emerged as the primary differentiator, emphasizing upper-body strength.
-	**Linebackers (LB)**: Stronger performances across most movement and jumping metrics, reflecting positional versatility.
-	**Defensive Ends (DE)**: Broad separation across speed and explosiveness metrics, with body weight less influential.
-	**Running Backs (RB)**: Differences primarily in acceleration and lower-body power (40-yard dash, broad jump).
-	**Wide Receivers (WR)**: Minimal separation, with speed and body weight showing limited differentiation.
-	**Tight Ends (TE)**: Separation across speed, jumping, and agility, consistent with hybrid positional demands.

## Interpretation
- Combine metrics provide meaningful signal, only when interpreted within position groups.
- Some roles show consistent separation (OL, DE, LB, DB), while others do not (WR, interior DL).
- Logistic regression reinforced context over raw prediction

## Takeaways
The NFL Combine provides selective signal, and not universal answers, so position specific interpretation is essential.  Athletic traits should be weighted differently depending on role demands, and Combine data works best as a complement to film, usage, and contextual evaluation.  So rather than asking, "Who tests well?" this helps answer, "Which traits matter for this position."

Future extensions of this could include draft position and snap-based usage, along with integrating performance data and career longevity.  

For a detailed breakdown of the statistical methods, full results, and supporting figures, see the complete write-up in the `/Writeup` directory.
