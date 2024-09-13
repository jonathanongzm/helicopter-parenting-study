# Codebook for Helicopter Parenting Study

## General Information
- **Dataset Name**: hp_dataset_cleaned.csv
- **Description**: This dataset includes survey responses on helicopter parenting, filial piety, social competence, and various demographic and psychosocial variables from single Malaysian adults. This codebook provides detailed descriptions of each item in the dataset, the possible responses for each variable, and how composite variables were created for analysis.

## Variable Descriptions

| Variable Name       | Description                                                                                           | Possible Responses                                                                                      |
|---------------------|-------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------|
| `ID`                | Unique identifier for each participant                                                                | -                                                                                                      |
| `S1_Progress`       | Survey 1 progress in percentage                                                                       | 0-100                                                                                                  |
| `S2_Progress`       | Survey 2 progress in percentage                                                                       | 0-100                                                                                                  |
| `S1_Attention1`     | Survey 1 attention check: Select 'disagree' so we know you are paying attention.                       | "Disagree", "Strongly disagree", "Agree", "Neither agree nor disagree"                                  |
| `S1_Attention2`     | Survey 1 attention check: Please select 'neither agree nor disagree' to show that you are paying attention. | "Neither agree nor disagree", "Agree", "Strongly agree", "Disagree"                                     |
| `S1_Attention3`     | Survey 1 attention check: To show that you are paying attention, please select 'agree.'                | "Agree", "Strongly agree", "Neither agree nor disagree", "Strongly disagree", "Somewhat agree"          |
| `S1_AttentionFailed`| For survey 1, did the participant fail at least 2 attention checks?                                    | "Yes", "No"                                                                                            |
| `S2_Attention1`     | Survey 2 attention check: Select 'quite unimportant' to show that you are paying attention.            | "Quite unimportant", "Quite important", "Slightly important", NA                                        |
| `S2_Attention_2`    | Survey 2 attention check: Select 'often' if you are paying attention.                                  | "Often", "Occasionally", "Almost Always/Always", NA                                                    |
| `S2_Attention_3`    | Survey 2 attention check: To show that you are paying attention, please select 'moderately so.'        | "Moderately so", "Somewhat", "Very much so", NA                                                        |
| `S2_AttentionFailed`| For survey 2, did the participant fail at least 2 attention checks?                                    | "Yes", "No", NA                                                                                        |
| `Age`               | What is your age?                                                                                     | Numerical entry                                                                                        |
| `Rrel_exp4`         | Duration of the longest romantic relationship (definition provided in the question text).              | "1 - 6 months", "7 - 12 months", "more than 1 year but less than 3 years", "more than 3 years but less than 5 years", "more than 5 years but less than 10 years", "10 years or more", NA (have never been in a relationship)
| `Gender`            | Which gender do you identify with? - Selected Choice                                                  | "Male", "Female", "Non-binary/gender diverse", "Prefer not to say", "My gender identity isn’t listed. I identify as:" |
| `Gender_TEXT`       | Which gender do you identify with? - My gender identity isn’t listed. I identify as:  - Text           | Free text                                                                                              |
| `Gender3`           | Gender                                                                                                | "Male", "Female", "Other"                                                                              |
| `Ethnicity`         | What is your ethnicity? - Selected Choice                                                              | "Malay", "Chinese", "Indian", "Other, please specify:"                                                  |
| `Ethnicity_TEXT`    | What is your ethnicity? - Other, please specify:  - Text                                               | Free text                                                                                              |
| `Education`         | What is the highest degree or level of school you have completed?                                      | "Less than high school", "High school graduate", "College/Pre-university graduate", "Bachelor’s degree", "Master’s degree", "Professional degree", "Doctorate" |
| `Religion`          | What is your religion? - Selected Choice                                                               | "Islam", "Sikhism", "Atheism", "Christianity", "Hinduism", "Buddhism", "Agnosticism", "Taoism", "Other, please specify:" |
| `Religion_9_TEXT`   | What is your religion? - Other, please specify: - Text                                                 | Free text                                                                                              |
| `Hel_Pr1`           | My parent(s)/guardian(s) supervised my every move growing up.                                          | "Strongly disagree", "Disagree", "Undecided", "Agree", "Strongly agree"                                 |
| `Hel_Pr2`           | I sometimes felt that my parent(s)/guardian(s) didn’t feel I could make my own decisions.              | "Strongly disagree", "Disagree", "Undecided", "Agree", "Strongly agree"                                 |
| `Hel_Pr3`          | My parent(s)/guardian(s) let me figure things out independently.                                       | "Strongly disagree", "Disagree", "Undecided", "Agree", "Strongly agree"                                 |
| `Hel_Pr4`           | It was very important to my parent(s)/guardian(s) that I never fail in life.                           | "Strongly disagree", "Disagree", "Undecided", "Agree", "Strongly agree"                                 |
| `Hel_Pr5`          | My parent(s)/guardian(s) were not afraid to let me stumble in life.                                    | "Strongly disagree", "Disagree", "Undecided", "Agree", "Strongly agree"                                 |
| `Hel_Pr6`           | My parent(s)/guardian(s) often stepped in to solve life problems for me.                               | "Strongly disagree", "Disagree", "Undecided", "Agree", "Strongly agree"                                 |
| `Hel_Pr7`           | Growing up, I sometimes felt like I was my parent(s)’/guardian(s)' project.                            | "Strongly disagree", "Disagree", "Undecided", "Agree", "Strongly agree"                                 |
| `CPI_1`             | The extent to which parental involvement has changed over time.                                        | "No change", "A little less involved", "Somewhat less involved", "Moderately less involved", "Mostly less involved", "Definitely less involved", "A little more involved", "Somewhat more involved", "Moderately more involved", "Mostly more involved" |
| `ICQ1`              | How good are you at finding and suggesting things to do with new people whom you find interesting and attractive? | "I'm always good at this", "I'm only fair at this", "I'm OK at this", "I'm always poor at this"         |
| `ICQ2`              | How good are you at introducing yourself to someone you might like to get to know/date?                 | "I'm always good at this", "I'm only fair at this", "I'm OK at this", "I'm always poor at this"         |
| `ICQ3`              | How good are you at calling a new date/acquaintance to set up a time to get together and do something?  | "I'm always good at this", "I'm only fair at this", "I'm OK at this", "I'm always poor at this"         |
| `ICQ4`              | How good are you at confronting your close companion when he/she has broken a promise?                  | "I'm always good at this", "I'm only fair at this", "I'm OK at this", "I'm always poor at this"         |
| `ICQ5`              | How good are you at telling a companion that he/she has done something to hurt your feelings?           | "I'm always good at this", "I'm only fair at this", "I'm OK at this", "I'm always poor at this"         |
| `ICQ6`              | How good are you at telling a date/acquaintance that he/she has done something that made you angry?     | "I'm always good at this", "I'm only fair at this", "I'm OK at this", "I'm always poor at this"         |
| `ICQ7`              | How good are you at helping a close companion get to the heart of the problem he/she is experiencing?   | "I'm always good at this", "I'm only fair at this", "I'm OK at this", "I'm always poor at this"         |
| `ICQ8`              | How good are you at saying and doing things to support a close companion when she/he is feeling down?   | "I'm always good at this", "I'm only fair at this", "I'm OK at this", "I'm always poor at this"         |
| `ICQ9`              | How good are you at giving advice in ways that are well received when a close companion needs help and support? | "I'm always good at this", "I'm only fair at this", "I'm OK at this", "I'm always poor at this"         |
| `ICQ10`             | How good are you at confiding in a new friend/date and letting him/her see your softer, more sensitive side? | "I'm always good at this", "I'm only fair at this", "I'm OK at this", "I'm always poor at this"         |
| `ICQ11`             | How good are you at letting a new companion get to know the “real” you?                                 | "I'm always good at this", "I'm only fair at this", "I'm OK at this", "I'm always poor at this"         |
| `ICQ12`             | How good are you at letting down your protective “outer shell” and trusting a close companion?           | "I'm always good at this", "I'm only fair at this", "I'm OK at this", "I'm always poor at this"         |
| `ICQ13`             | How good are you at admitting that you might be wrong when a disagreement with a close companion begins to build into a serious fight? | "I'm always good at this", "I'm only fair at this", "I'm OK at this", "I'm always poor at this"         |
| `ICQ14`             | How good are you at taking a companion’s perspective in a fight and really understanding his or her point of view? | "I'm always good at this", "I'm only fair at this", "I'm OK at this", "I'm always poor at this"         |
| `ICQ15`             | How good are you at not exploding at a close companion (even when it’s justified) to avoid a damaging conflict? | "I'm always good at this", "I'm only fair at this", "I'm OK at this", "I'm always poor at this"         |
| `RecFP_1`           | Be frequently concerned about my parent(s)'/guardian(s)'s health conditions.                           | "Quite important", "Extremely important", "Slightly unimportant", "Slightly important", "Extremely unimportant", "Quite unimportant" |
| `RecFP_2`           | Talk frequently with my parent(s)/guardian(s) to understand their thoughts and feelings.               | "Quite important", "Extremely important", "Slightly unimportant", "Slightly important", "Extremely unimportant", "Quite unimportant" |
| `RecFP_3`           | Be frequently concerned about my parent(s)’/guardian(s)' general well-being.                           | "Quite important", "Extremely important", "Slightly unimportant", "Slightly important", "Extremely unimportant", "Quite unimportant" |
| `RecFP_4`           | Be concerned about my parent(s)/guardian(s), as well as understand them.                               | "Quite important", "Extremely important", "Slightly unimportant", "Slightly important", "Extremely unimportant", "Quite unimportant" |
| `RecFP_5`           | Support my parent(s)’/guardian(s)' livelihood to make their lives more comfortable.                    | "Quite important", "Extremely important", "Slightly unimportant", "Slightly important", "Extremely unimportant", "Quite unimportant" |
| `RecFP_6`           | Be grateful to my parent(s)/guardian(s) for raising me.                                                | "Quite important", "Extremely important", "Slightly unimportant", "Slightly important", "Extremely unimportant", "Quite unimportant" |
| `RecFP_7`           | Hurry home upon the death of my parent(s)/guardian(s), regardless of how far away I am.                | "Quite important", "Extremely important", "Slightly unimportant", "Slightly important", "Extremely unimportant", "Quite unimportant" |
| `RecFP_8`           | Take the initiative to assist my parent(s)/guardian(s) when they are busy.                             | "Quite important", "Extremely important", "Slightly unimportant", "Slightly important", "Extremely unimportant", "Quite unimportant" |
| `AuthFP_1`          | Take my parent(s)’/guardian(s)' suggestions even when I do not agree with them.                        | "Slightly unimportant", "Extremely important", "Quite important", "Slightly important", "Extremely unimportant", "Quite unimportant" |
| `AuthFP_2`          | Live with my parent(s)/guardian(s) (or parent(s)-in-law) when married.                                 | "Slightly unimportant", "Extremely important", "Quite important", "Slightly important", "Extremely unimportant", "Quite unimportant" |
| `AuthFP_3`          | Disregard promises to friends in order to obey my parent(s)/guardian(s).                               | "Slightly unimportant", "Extremely important", "Quite important", "Slightly important", "Extremely unimportant", "Quite unimportant" |
| `AuthFP_4`          | Give up my aspirations to meet my parent(s)’/guardian(s)' expectations.                                | "Slightly unimportant", "Extremely important", "Quite important", "Slightly important", "Extremely unimportant", "Quite unimportant" |
| `AuthFP_5`          | Do whatever my parent(s)/guardian(s) ask right away.                                                   | "Slightly unimportant", "Extremely important", "Quite important", "Slightly important", "Extremely unimportant", "Quite unimportant" |
| `AuthFP_6`          | Avoid getting married to someone my parent(s)/guardian(s) dislike.                                     | "Slightly unimportant", "Extremely important", "Quite important", "Slightly important", "Extremely unimportant", "Quite unimportant" |
| `AuthFP_7`          | Have at least one son for the succession of the family name.                                           | "Slightly unimportant", "Extremely important", "Quite important", "Slightly important", "Extremely unimportant", "Quite unimportant" |
| `AuthFP_8`          | Let my income be handled by my parent(s)/guardian(s) before marriage.                                  | "Slightly unimportant", "Extremely important", "Quite important", "Slightly important", "Extremely unimportant", "Quite unimportant" |


### Composite Score Calculations

#### Helicopter Parenting Average (Hel_Avg)
- **Description**: The average score for Helicopter Parenting, calculated using seven items (`Hel_Pr1` to `Hel_Pr7`). Note that `Hel_Pr3` and `Hel_Pr5` were reverse scored before averaging.
- **Calculation**: 
  - Reverse score `Hel_Pr3` and `Hel_Pr5` by subtracting their values from 6.
  - `Hel_Avg` = Mean of (`Hel_Pr1`, `Hel_Pr2`, reverse-scored `Hel_Pr3`, `Hel_Pr4`, reverse-scored `Hel_Pr5`, `Hel_Pr6`, `Hel_Pr7`), ignoring missing values (`na.rm = TRUE`).

#### Social Competence Average (SCavg)
- **Description**: The average score for Social Competence, computed from 15 items measuring various aspects of social competence (`ICQ1` to `ICQ15`).
- **Calculation**: 
  - `SCavg` = Mean of (`ICQ1`, `ICQ2`, `ICQ3`, `ICQ4`, `ICQ5`, `ICQ6`, `ICQ7`, `ICQ8`, `ICQ9`, `ICQ10`, `ICQ11`, `ICQ12`, `ICQ13`, `ICQ14`, `ICQ15`), ignoring missing values.

#### Subdomain Scores for Social Competence

1. **Initiation (Initiation)**
   - **Description**: Average score for the Initiation subdomain, reflecting how participants initiate social interactions.
   - **Calculation**: 
     - `Initiation` = Mean of (`ICQ1`, `ICQ2`, `ICQ3`), ignoring missing values.

2. **Self-Disclosure (SelfDisc)**
   - **Description**: Average score for the Self-Disclosure subdomain, indicating the extent to which participants disclose personal information.
   - **Calculation**: 
     - `SelfDisc` = Mean of (`ICQ10`, `ICQ11`, `ICQ12`), ignoring missing values.

3. **Negative Assertion (NegAssr)**
   - **Description**: Average score for the Negative Assertion subdomain, capturing how participants assert themselves in potentially negative or confrontational situations.
   - **Calculation**: 
     - `NegAssr` = Mean of (`ICQ4`, `ICQ5`, `ICQ6`), ignoring missing values.

4. **Emotional Support (EmoSup)**
   - **Description**: Average score for the Emotional Support subdomain, reflecting participants' ability to provide emotional support.
   - **Calculation**: 
     - `EmoSup` = Mean of (`ICQ7`, `ICQ8`, `ICQ9`), ignoring missing values.

5. **Conflict Management (ConfMan)**
   - **Description**: Average score for the Conflict Management subdomain, assessing how participants manage conflicts.
   - **Calculation**: 
     - `ConfMan` = Mean of (`ICQ13`, `ICQ14`, `ICQ15`), ignoring missing values.

#### Filial Piety Scores

1. **Reciprocal Filial Piety Total (RecFP)**
   - **Description**: Total score for the Reciprocal Filial Piety dimension, which measures participants' sense of reciprocal obligations to their parents.
   - **Calculation**: 
     - `RecFP` = Sum of (`RecFP_1`, `RecFP_2`, `RecFP_3`, `RecFP_4`, `RecFP_5`, `RecFP_6`, `RecFP_7`, `RecFP_8`), ignoring missing values.

2. **Authoritarian Filial Piety Total (AuthFP)**
   - **Description**: Total score for the Authoritarian Filial Piety dimension, indicating participants' compliance with traditional authority-based expectations of parental respect.
   - **Calculation**: 
     - `AuthFP` = Sum of (`AuthFP_1`, `AuthFP_2`, `AuthFP_3`, `AuthFP_4`, `AuthFP_5`, `AuthFP_6`, `AuthFP_7`, `AuthFP_8`), ignoring missing values.


