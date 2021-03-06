# Covid19 Web Application
This project was created for my STA 141B Final Project. The following application is simple, yet effective for analyzing time-series cases of multiple variables such as confirmed cases, active cases, deaths, and recovered cases. In addition, we can choose to examine a single country at a given time (I worked with 10 countries for simpler analysis). The initial variable includes "Philippines", given this was the place I was when I first heard about the spread of COVID-19. I extracted data from http://api.covid19api.com/, where we can perform real-time analysis.

In the analysis of our data for the Philippines as of June 9, 2020, we can see the number of reported cases of each type starting to increase at the end of March. The rate of increase for each case type all share a similar trend of increase. Surprisingly, as we study the data for the United States, we can observe that confirmed and recovered cases have a constant trend of increase, whereas the rate of increase for active cases and deaths become slightly lesser as we approach June. This can lead to assumptions such as seasonality, by which COVID-19 may not have as strong of an impact when temperature rises. Another assumption is that COVID-19 social distancing efforts may have led to success to slow the spread of the virus.

Another interesting discovery is the observation of Spain. Observing the data, we see that the reports of confirmed, recovered, deaths, and active cases have reached a peak, and the number of active cases have already started to decrease. Not having read any information about Spain, I can infer that Spain is doing their best in their efforts to containing the virus.

Adding to my analysis as of June 16, 2020, we can see the changes in the rate of increase for the United States confirmed cases to much much higher than when COVID-19 had first started to spread widely across the US.

Here is the link for the uploaded web application:

https://jameshizon.shinyapps.io/final_project/?_ga=2.63372518.813029001.1591649978-477903587.1591649978

As an update in August, it seems that the link above may not be working. However, when testing my code inside RStudio, I am still able to receive an RShiny web application. The following is a link to the screenshot, to see the RShiny app at work. As we can see, the API by which I extract my data is still working and up to date.

Screenshot Link:
https://github.com/JamesHizon/Covid19_Web_Application/issues/2#issue-683722968
