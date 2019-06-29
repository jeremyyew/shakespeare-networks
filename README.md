# Shakespeare's Plays: Network visualization of interactions
In this project, we analyze 6 of Shakespeare’s significant plays. We quantify interactions between characters based on the number of times they appear in the same scene throughout each play.  

We set out with the following broad questions in mind: 
- What is the role of gender and marriage in character interactions?
- How may we account for observed differences in interaction patterns across gender and marriage status? 

# Data visualization
We were initially inspired by this series:  https://flowingdata.com/2015/12/30/shakespeare-tragedies-as-network-graphs/
However, all the data in these visualizations were manually encoded, and does not contain extra information about individual characters. 

Based on an online data source that organizes every single line in each play by character name and scene, we produced visualizations that display a network of character nodes, with **edges representing the number of times each pair of characters appear in the same scene.** 
Source: https://www.kaggle.com/kingburrito666/shakespeare-plays. 

![Screenshot of web app](./romeo-juliet-screenshot.png?raw=true "Screenshot of web app")

Visual mappings included: 
- **Edge Weight: Represents number of scenes two characters appear together in.**
Mapped to edge size and color intensity.

- **Number of Lines: Represents total number of lines in play.** 
Mapped to node color intensity.

- **Number of Edges: Represents number of other characters that they interact with (i.e. appear in the same scene with at least once).** 
Mapped to node size.

- **Total Edge Weight: Sum of weights of all edges. Represents total number of times they interact with other characters (i.e. number of times they appear in the same scene with each character).**

Furthermore, we enhanced the data by **tagging individual characters by gender and marriage status**. This allowed us to identify patterns in the interaction of characters of different genders and marriage status.
Data tagging: https://docs.google.com/spreadsheets/d/1LbGv6n8dV1aQpI8vIoU-1Yn8nfjqmFICoUweHaxmZvY/edit?usp=sharing. 

Lastly, we also included two filters that can a) show interactions between male and female gendered characters,and b) isolate the married couples. These were convenience tools to help us with out analysis. 

# Research questions 
After reviewing the visualizations, we were able to refine our initial research questions to be more specific. 
- Are women more connected to other women or to men?
- Are married people more connected to each other?
- Are married people more significant in the play overall? 
- Are married people’s most significant interaction the one they have with each other? Who does this affect more, men or women?
- Are married women more connected to other characters than unmarried women? 

# Data processing (TODO)

# Findings (TODO)