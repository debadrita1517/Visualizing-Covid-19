# **VISUALIZING COVID-19**

# **How it gone from Epidemic to Pandemic**
<p><img style="float: left; margin:5px 20px 5px 1px; width:40%" src="https://www.nps.gov/aboutus/news/images/CDC-coronavirus-image-23311-for-web.jpg?maxwidth=650&autorotate=false"></p>
<p>In December 2019, COVID-19 coronavirus was first identified in the Wuhan region of China. By March 11, 2020, the World Health Organization (WHO) categorized the COVID-19 outbreak as a pandemic. A lot has happened in the months in between with major outbreaks in Iran, South Korea, and Italy. </p>
<p>We know that COVID-19 spreads through respiratory droplets, such as through coughing, sneezing, or speaking. But, how quickly did the virus spread across the globe? And, can we see any effect from country-wide policies, like shutdowns and quarantines? </p>
<p>Fortunately, organizations around the world have been collecting data so that governments can monitor and learn from this pandemic. Notably, the Johns Hopkins University Center for Systems Science and Engineering created a <a href="https://github.com/RamiKrispin/coronavirus">publicly available data repository</a> to consolidate this data from sources like the WHO, the Centers for Disease Control and Prevention (CDC), and the Ministry of Health from multiple countries.</p>
<p>In this notebook, you will visualize COVID-19 data from the first several weeks of the outbreak to see at what point this virus became a global pandemic.</p>
<p><em>Please note that information and data regarding COVID-19 is frequently being updated. The data used in this project was pulled on March 17, 2020, and should not be considered to be the most up to date data available.</em></p>
"""

# Loading the requisite R packages
library(readr)
library(ggplot2)
library(dplyr)
# Reading the datasets "ConfirmedCasesWorldwide.csv"
confirmed_cases_worldwide <- read_csv("https://raw.githubusercontent.com/debadrita1517/Visualizing-Covid-19/main/confirmed_cases_worldwide.csv")
# Displaying the above dataset
confirmed_cases_worldwide

"""# **2. Confirmed Cases Throughout the World -**
<p>The table above shows the cumulative confirmed cases of COVID-19 worldwide by date. Just reading numbers in a table makes it hard to get a sense of the scale and growth of the outbreak. Let's draw a line plot to visualize the confirmed cases worldwide.</p>
"""

# Plotting the Cumulative Confirmed Cases
ggplot(confirmed_cases_worldwide, aes(date, cum_cases)) +
  geom_line() +
  ylab("Cumulative confirmed cases")

"""# **Comparing China with the Rest of the World -**
<p>The y-axis in that plot is pretty scary, with the total number of confirmed cases around the world approaching 200,000. Beyond that, some weird things are happening: there is an odd jump in mid February, then the rate of new cases slows down for a while, then speeds up again in March. We need to dig deeper to see what is happening.</p>
<p>Early on in the outbreak, the COVID-19 cases were primarily centered in China. Let's plot confirmed COVID-19 cases in China and the rest of the world separately to see if it gives us any insight.</p>
"""

# Reading the dataset "ComfirmedCasesOfChinaVersusWorld"
confirmed_cases_china_vs_world <- read_csv("https://raw.githubusercontent.com/debadrita1517/Visualizing-Covid-19/main/confirmed_cases_china_vs_world.csv")
# Seeing the result
glimpse(confirmed_cases_china_vs_world)
# Plotting it graphically
plt_cum_confirmed_cases_china_vs_world <- ggplot(confirmed_cases_china_vs_world) +
  geom_line(aes(date, cum_cases, group = is_china, color = is_china)) +
  ylab("Cumulative confirmed cases")
# Displaying the graph
plt_cum_confirmed_cases_china_vs_world

"""## **On further Annotations -**
<p>Wow! The two lines have very different shapes. In February, the majority of cases were in China. That changed in March when it really became a global outbreak: around March 14, the total number of cases outside China overtook the cases inside China. This was days after the WHO declared a pandemic.</p>
<p>There were a couple of other landmark events that happened during the outbreak. For example, the huge jump in the China line on February 13, 2020 wasn't just a bad day regarding the outbreak; China changed the way it reported figures on that day (CT scans were accepted as evidence for COVID-19, rather than only lab tests).</p>
"""

who_events <- tribble(
  ~ date, ~ event,
  "2020-01-30", "Global health\nemergency declared",
  "2020-03-11", "Pandemic\ndeclared",
  "2020-02-13", "China reporting\nchange"
) %>%
  mutate(date = as.Date(date))
plt_cum_confirmed_cases_china_vs_world +
  geom_vline(aes(xintercept = date), data = who_events, linetype = "dashed") +
  geom_text(aes(date, label = event), data = who_events, y = 1e5)

"""## **Adding a Trending Line to China -**
<p>When trying to assess how big future problems are going to be, we need a measure of how fast the number of cases is growing. A good starting point is to see if the cases are growing faster or slower than linearly.</p>
<p>There is a clear surge of cases around February 13, 2020, with the reporting change in China. However, a couple of days after, the growth of cases in China slows down. How can we describe COVID-19's growth in China after February 15, 2020?</p>
"""

# Filtering Cases in China from February 15, 2020
china_after_feb15 <- confirmed_cases_china_vs_world %>%
  filter(is_china == "China", date >= "2020-02-15")
ggplot(china_after_feb15, aes(date, cum_cases)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Cumulative confirmed cases")

"""# **Rest of the World -**
<p>From the plot above, the growth rate in China is slower than linear. That's great news because it indicates China has at least somewhat contained the virus in late February and early March.</p>
<p>How does the rest of the world compare to linear growth?</p>
"""

# Filtering ComfirmedCasesInChinaVersusWorld NOT for China
not_china <- confirmed_cases_china_vs_world %>%
  filter(is_china == "Not China")
plt_not_china_trend_lin <- ggplot(not_china, aes(date, cum_cases)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Cumulative confirmed cases")
# Viweing the Results
plt_not_china_trend_lin

"""# **Adding a Logarithmic Scale -**
<p>From the plot above, we can see a straight line does not fit well at all, and the rest of the world is growing much faster than linearly. What if we added a logarithmic scale to the y-axis?</p>
"""

# Modifying the Plotting to Use a Logarithmic Scale upon the y-axis
plt_not_china_trend_lin + 
  scale_y_log10()

"""# **Which Countries Outside China got hit the highest ?**
<p>With the logarithmic scale, we get a much closer fit to the data. From a data science point of view, a good fit is great news. Unfortunately, from a public health point of view, that means that cases of COVID-19 in the rest of the world are growing at an exponential rate, which is terrible news.</p>
<p>Not all countries are being affected by COVID-19 equally, and it would be helpful to know where in the world the problems are greatest. Let's find the countries outside of China with the most confirmed cases in our dataset.</p>
"""

# Getting the Dataset of the Confirmed Cases of Each Countries
confirmed_cases_by_country <- read_csv("https://raw.githubusercontent.com/debadrita1517/Visualizing-Covid-19/main/confirmed_cases_by_country.csv")
glimpse(confirmed_cases_by_country)
top_countries_by_total_cases <- confirmed_cases_by_country %>%
  group_by(country) %>%
  summarize(total_cases = max(cum_cases)) %>%
  top_n(7, total_cases)
# Viewing the Result -
top_countries_by_total_cases

"""# **Plotting the Hardest Hit Countries as of Mid-March 2020**
<p>Even though the outbreak was first identified in China, there is only one country from East Asia (South Korea) in the above table. Four of the listed countries (France, Germany, Italy, and Spain) are in Europe and share borders. To get more context, we can plot these countries' confirmed cases over time.</p>
<p>Finally, congratulations on getting to the last step! If you would like to continue making visualizations or find the hardest hit countries as of today.</p>
"""

# Reading the Dataset ConfirmedCasesTop7CountriesOutsideChina
confirmed_cases_top7_outside_china <- read_csv("https://raw.githubusercontent.com/debadrita1517/Visualizing-Covid-19/main/confirmed_cases_top7_outside_china.csv")
glimpse(confirmed_cases_top7_outside_china)
# Plotting them graphically -
ggplot(confirmed_cases_top7_outside_china, aes(date, cum_cases, color = country, group = country)) +
  geom_line() +
  ylab("Cumulative confirmed cases")
