# SOOTGraphs
The following script was written by Professor Xingye Qiao of the Math Department, 
and modified for the Cloud by Professor Nancy Um of Art History and the Harpur 
Dean's Office, Binghamton University, to aid in the conversion of SOOT 
(Student Opinion of Teaching) data into an aggregated format.

In order to use this script, you must upload your own data. Navigate to the #
SOOT surveys page in `myBinghamton`, which can be found under the 
`Academic Services` tab. For each relevant course, select `Download.csv`. 
Do not rename the files. Upload all the .csv files at the same time. 

## Graphs produced

Beyond the original stacked rating charts, the app now generates several summary
views. It reports a summary score by question in two weightings: a
student-weighted top-two-box percentage and a course-weighted top-two-box
percentage. It also reports summary trends by term in both weightings, a count of
responses by course, and a question-by-course top-two-box heatmap. The existing
stacked charts now exclude "Not Applicable" from the rating bars and order terms
chronologically.

XLSX import is planned next; the app currently accepts CSV files only.
