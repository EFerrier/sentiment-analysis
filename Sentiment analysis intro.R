# Code source https://sites.google.com/site/miningtwitter/questions/sentiment/sentiment

library(twitteR)
library(sentiment)
library(plyr)
library(ggplot2)
library (wordcloud)
library(RColorBrewer)

library(qdap)

#some_txt is Transcript of Chair Yellen's press conference on September 21, 2016 https://www.federalreserve.gov/mediacenter/files/FOMCpresconf20160921.pdf

# some_txt <- sent_detect_nlp("At our meeting that concluded earlier today, my
# colleagues and I on the Federal Open Market Committee discussed overall economic conditions
#               and decided to keep the target range for the federal funds rate at ¼ to ½ percent. We judged that
#               the case for an increase has strengthened but decided for the time being to wait for further
#               evidence of continued progress toward our objectives. Our current policy should help move the
#               economy toward our statutory goals of maximum employment and price stability. I’ll have more
#               to say about our decision shortly, but first I will review recent economic developments and the
#               outlook.
#               Economic growth, which was subdued during the first half of the year, appears to have
#               picked up. Household spending continues to be the key source of that growth. This spending
#               has been supported by solid increases in household income as well as by relatively high levels of
#               consumer sentiment and wealth. Business investment, however, remains soft, both in the energy
#               sector and more broadly. The energy industry has been hard hit by the drop in oil prices since
#               mid-2014, and investment in that sector continued to contract through the first half of the year.
#               However, drilling is now showing signs of stabilizing. Overall, we expect that the economy will
#               expand at a moderate pace over the next few years.
#               Turning to employment, job gains averaged about 180,000 per month over the past four
#               months, about the same solid pace recorded since the beginning of the year. In the longer run,
#               that’s well above the pace that we estimate is needed to provide work for new entrants in the job
#               market. But so far this year, most measures of labor market slack have shown little change. The
#               unemployment rate in August—4.9 percent—was the same as in January. And a broader
#               measure of unemployment has also flattened out—a measure that includes people who want and 
#               September 21, 2016 Chair Yellen’s Press Conference FINAL
#               Page 2 of 24
#               are available to work but have not searched recently as well as people who are working part time
#               but would rather work full time. The fact that unemployment measures have been holding steady
#               while the number of jobs has grown solidly shows that more people, presumably in response to
#               better employment opportunities and higher wages, have started actively seeking and finding
#               jobs. This is a very welcome development, both for the individuals involved and the nation as a
#               whole. We continue to expect that labor market conditions will strengthen somewhat further
#               over time.
#               Ongoing economic growth and an improving job market are key factors supporting our
#               inflation outlook. Overall consumer price inflation—as measured by the price index for personal
#               consumption expenditures—was less than 1 percent over the 12 months ending in July, still short
#               of our 2 percent objective. Much of this shortfall continues to reflect earlier declines in energy
#               and import prices. Core inflation, which excludes energy and food prices that tend to be more
#               volatile than other prices, has been running about 1½ percent. As transitory influences holding
#               down inflation fade and as the job market strengthens further, we continue to expect inflation to
#               rise to 2 percent over the next two to three years.
#               Our inflation outlook also rests importantly on our judgment that longer-run inflation
#               expectations remain reasonably well anchored. However, we can’t take the stability of longerrun
#               inflation expectations for granted, and we will continue to carefully monitor actual and
#               expected progress toward our inflation goal. Indeed, we are fully committed to achieving our
#               2 percent inflation objective.
#               Let me turn to the economic projections—now extending through 2019—that were
#               submitted for this meeting by the Federal Open Market Committee participants. As always,
#               participants conditioned their projections on their own view of appropriate monetary policy,
#               September 21, 2016 Chair Yellen’s Press Conference FINAL
#               Page 3 of 24
#               which in turn depends on each participant’s assessment of the multitude of factors that shape the
#               outlook. The median projection for growth of inflation-adjusted gross domestic product, or
#               GDP, is 1.8 percent this year. This figure is somewhat lower than projected in June as a result of
#               the weaker-than-expected growth seen in the first half of the year. In 2017 and 2018, the median
#               growth projection is unchanged at 2 percent, somewhat higher than the median estimate of
#               longer-run normal growth. In 2019, growth edges down to 1.8 percent, in line with its estimated
#               longer-run rate, which has als—which has been revised down a bit since June. The median
#               projection for the unemployment rate stands at 4.8 percent at the end of this year, a touch higher
#               than in June. Over the next three years, the median unemployment rate runs near 4½ percent,
#               modestly below the median estimate of its longer-run normal rate. Finally, the median inflation
#               projection is 1.3 percent this year and rises to 1.9 percent next year and 2 percent in 2018
#               and 2019.
#               Returning to monetary policy, the recent pickup in economic growth and continued
#               progress in the labor market have strengthened the case for an increase in the federal funds rate.
#               Moreover, the Committee judges the risks to the outlook to be roughly balanced. So why didn’t
#               we raise the federal funds rate at today’s meeting? Our decision does not reflect a lack of
#               confidence in the economy. Conditions in the labor market are strengthening, and we expect that
#               to continue. And while inflation remains low, we expect it to rise to our 2 percent objective over
#               time. But with labor market slack being taken up at a somewhat slower pace than in previous
#               years, scope for some further improvement in the labor market remaining, and inflation
#               continuing to run below our 2 percent target, we chose to wait for further evidence of continued
#               progress toward our objectives. This cautious approach to paring back monetary policy support
#               is all the more appropriate given that short-term interest rates are still near zero, which means 
#               September 21, 2016 Chair Yellen’s Press Conference FINAL
#               Page 4 of 24
#               that we can more effectively respond to surprisingly strong inflation pressures in the future by
#               raising rates than to a weakening labor market and falling inflation by cutting rates.
#               We continue to expect that the evolution of the economy will warrant only gradual
#               increases in the federal funds rate over time to achieve and maintain our objectives. That’s based
#               on our view that the neutral nominal federal funds rate—that is, the interest rate that is neither
#               expansionary nor contractionary and keeps the economy operating on an even keel—is currently
#               quite low by historical standards. With the federal funds rate modestly below the neutral rate,
#               the current stance of monetary policy should be viewed as modestly accommodative, which is
#               appropriate to foster further progress toward our objectives. But since monetary policy is only
#               modestly accommodative, there appears little risk of falling behind the curve in the near future,
#               and gradual increases in the federal funds rate will likely be sufficient to get to a neutral policy
#               stance over the next few years.
#               This view is consistent with participants’ projections of appropriate monetary policy.
#               The median projection for the federal funds rate rises only gradually to 1.1 percent at the end of
#               next year, 1.9 percent at the end of 2018, and 2.6 percent by the end of 2019. Compared with the
#               projections made in June, the median path for the federal funds rate has been revised down ¼ to
#               ½ percentage point. Most participants also marked down their estimate of the longer-run normal
#               federal funds rate, with the median now at 2.9 percent.
#               As I have noted on previous occasions, participants’ projections for the federal funds rate,
#               including the median path, are not a fixed plan for future policy. Policy is not on a preset course.
#               These forecasts represent participants’ individual assessments of appropriate policy, given their
#               projections of economic growth, employment, inflation, and other factors at a particular point in
#               time. However, the economic outlook is inherently uncertain, and any assessment of the 
#               September 21, 2016 Chair Yellen’s Press Conference FINAL
#               Page 5 of 24
#               appropriate path for the federal funds rate will change in response to changes to the economic
#               outlook and associated risks.
#               Finally, we will continue to reinvest proceeds from maturing Treasury securities and
#               principal payments from agency debt and mortgage-backed securities. As our statement says, we
#               anticipate continuing this policy “until normalization of the level of the federal funds rate is well
#               under way.” Maintaining our sizable holdings of longer-term securities should help maintain
#               accommodative financial conditions and should reduce the risk that we might have to lower the
#               federal funds rate to zero in the event of a future large adverse shock.")

#some_txt is Transcript of Chair Yellen's press conference on December 14, 2016 https://www.federalreserve.gov/mediacenter/files/FOMCpresconf20161214.pdf

some_txt <- sent_detect_nlp("Today the Federal Open Market Committee decided
to raise the target range for the federal funds rate by ¼ percentage point, bringing it to ½ to
             ¾ percent. In doing so, my colleagues and I are recognizing the considerable progress the
             economy has made toward our dual objectives of maximum employment and price stability.
             Over the past year, 2¼ million net new jobs have been created, unemployment has fallen further,
             and inflation has moved closer to our longer-run goal of 2 percent. We expect the economy will
             continue to perform well, with the job market strengthening further and inflation rising to
             2 percent over the next couple of years. I’ll have more to say about monetary policy shortly, but
             first I’ll review recent economic developments and the outlook.
             Economic growth has picked up since the middle of the year. Household spending
             continues to rise at a moderate pace, supported by income gains and by relatively high levels of
             consumer sentiment and wealth. Business investment, however, remains soft despite some
             stabilization in the energy sector. Overall, we expect the economy will expand at a moderate
             pace over the next few years.
             Job gains averaged nearly 180,000 per month over the past three months, maintaining the
             solid pace that we’ve seen since the beginning of the year. Over the past seven years, since the
             depths of the Great Recession, more than 15 million jobs have been added to the U.S. economy.
             The unemployment rate fell to 4.6 percent in November, the lowest level since 2007, prior to the
             recession. Broader measures of labor market slack have also moved lower, and participation in
             the labor force has been little changed, on net, for about two years now, a further sign of
             improved conditions in the labor market given the underlying downward trend in participation 
             December 14, 2016 Chair Yellen’s Press Conference FINAL
             Page 2 of 20
             stemming largely from the aging of the U.S. population. Looking ahead, we expect that job
             conditions will strengthen somewhat further.
             Turning to inflation, the 12-month change in the price index for personal consumption
             expenditures was nearly 1½ percent in October, still short of our 2 percent objective but up more
             than a percentage point from a year earlier. Core inflation—which excludes energy and food
             prices that tend to be more volatile than other prices—has risen to 1¾ percent. As the transitory
             influences of earlier declines in energy prices and prices of imports continue to fade and as the
             job market strengthens further, we expect overall inflation to rise to 2 percent over the next
             couple of years.
             Our inflation outlook rests importantly on our judgment that longer-run inflation
             expectations remain reasonably well anchored. Market-based measures of inflation
             compensation have moved up considerably but are still low. Survey-based measures of longerrun
             inflation expectations are, on balance, little changed. Of course, we remain committed to our
             2 percent inflation objective and will continue to carefully monitor actual and expected progress
             toward this goal.
             Let me now turn to the economic projections that were submitted for this meeting by
             Committee participants. As always, they conditioned their projections on their own individual
             views of appropriate monetary policy, which, in turn, depend on each participant’s assessment of
             the multitude of factors that shape the outlook. The median projection for growth of inflationadjusted
             gross domestic product rises from 1.9 percent this year to 2.1 percent in 2017 and stays
             close to 2 percent in 2018 and 2019, slightly above its estimated longer-run rate. The median
             projection for the unemployment rate stands at 4.7 percent in the fourth quarter of this year.
             Over the next three years, the median unemployment rate runs at 4.5 percent, modestly below the 
             December 14, 2016 Chair Yellen’s Press Conference FINAL
             Page 3 of 20
             median estimate of its longer-run normal rate. Finally, the median inflation projection is
             1.5 percent this year and rises to 1.9 percent next year and 2 percent in 2018 and 2019. Overall,
             these economic projections are very similar to those made in September: GDP growth is a touch
             stronger; the unemployment rate is a shade lower; and inflation, beyond this year, is unchanged.
             Returning to monetary policy, the Committee judged that a modest increase in the federal
             funds rate is appropriate in light of the solid progress we have seen toward our goals of
             maximum employment and 2 percent inflation. We continue to expect that the evolution of the
             economy will warrant only gradual increases in the federal funds rate over time to achieve and
             maintain our objectives. That’s based on our view that the neutral nominal federal funds rate—
             that is, the interest rate that is neither expansionary nor contractionary and keeps the economy
             operating on an even keel—is currently quite low by historical standards. With the federal funds
             rate only modestly below the neutral rate, we continue to expect that gradual increases in the
             federal funds rate will likely be sufficient to get to a neutral policy stance over the next few
             years.
             This view is consistent with participants’ projections of appropriate monetary policy.
             The median projection for the federal funds rate rises to 1.4 percent at the end of next year,
             2.1 percent at the end of 2018, and 2.9 percent by the end of 2019. Compared with the
             projections made in September, the median path for the federal funds rate has been revised up
             just ¼ percentage point. Only a few participants altered their estimate of the longer-run normal
             federal funds rate, although the median edged up to 3 percent.
             Of course, the economic outlook is highly uncertain, and participants will adjust their
             assessments of the appropriate path for the federal funds rate in response to changes to the
             economic outlook and associated risks. As many observers have noted, changes in fiscal policy 
             December 14, 2016 Chair Yellen’s Press Conference FINAL
             Page 4 of 20
             or other economic policies could potentially affect the economic outlook. Of course, it is far too
             early to know how these policies will unfold. Moreover, changes in fiscal policy are only one of
             the many factors that can influence the outlook and the appropriate course of monetary policy.
             In making our policy decisions, we will continue—as always—to assess economic conditions
             relative to our objectives of maximum employment and 2 percent inflation. As I have noted on
             previous occasions, policy is not on a preset course.
             Finally, we will continue to reinvest proceeds from maturing Treasury securities and
             principal payments from agency debt and mortgage-backed securities. As our statement says, we
             anticipate continuing this policy “until normalization of the level of the federal funds rate is well
             under way.”")

#some_txt <- "PURPOSE The purpose of this Design Verification testing is to verify the Leakage Past Stopper  functional performance of the 5mL Eclipse Combo, aged real-time at T=5yrs. The product is sterilized by BD Sandy, UT using Ethylene Oxide (EtO) Cycle B.2, acceptable per the requirements of the product specification, SP100129. SCOPE The DV testing described herein will demonstrate that the sterilized Eclipse Needle Combo product meets the LPS functional performance specifications. Eclipse needles (without syringes) are currently produced and sterilized  BD Tuas.  Sterilization is by EtO, per  put through the cycle one time. REFERENCES, Product Specification, Syringe with Eclipse Needle,  mL, Packaged, Sterile. Test Instruction, Leak Test, Syringe Stopper.Verification Protocol - Eclipse Combo Design Verification  Eclipse Combos Sterilized by BD Sandy DV-R above ref. TEST ARTICLES All test samples are procured from BD  Nogales, MX, using normal production material and processes.Sterilization is performed by BD  Sandy, UT using EtO Cycle B.2, and put through the cycle one time . After sterilization, the samples are placed  controlled environmental chambers real-time aging. DESIGN VERIFICATION REQUIREMENTS Test Name: Stopper Seal - Pressure Method: IT21, Mode , Variable Data Type A final report covering the test results  real-time aged samples, Tyr, will be issued after completion of testing the aged samples. TRAINING All personnel involved  the execution of the study shall be trained to this protocol prior to execution with the exception of technicians/operators performing the testing, who only require training to the relevant test methods prior to testing.  Training to this protocol shall be recorded using Attachment. Training records shall be attached to the study report training is required. DEVIATION HANDLING All deviations to this protocol shall be documented using the Deviation Template."
# some_txt <- c("I liked the device, it was easy and intuitive to use.", 
# "I like the device, it was easy and intuitive to use.",
# "I like the device, it was easy and intuitive to use.",
# "I like the device, it was easy and intuitive to use.",
# "I like the device, it was easy and intuitive to use.",
# "I like the device, it was easy and intuitive to use.",
# "I like the device, it was easy and intuitive to use.",
# "I like the device, it was easy and intuitive to use.",
# "I like the device, it was easy and intuitive to use.",
# "I like the device, it was easy and intuitive to use.",
# "It is a bad device, it was not natural.",
# "It is a bad device, it was not natural.",
# "It is a bad device, it was not natural.",
# "I don't like the device, it was not natural."
# )

# Clean txt for analysis

# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)

# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
# lower case using try.error with sapply 
some_txt = sapply(some_txt, try.error)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL

# Sentiment Analysis

# classify emotion
class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(some_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]

#GENERAL STATS

# data frame with results
sent_df = data.frame(text=some_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))



# plot distribution of emotions - NOT WORKING
ggplot(data = sent_df, mapping = aes(x=emotion)) +
  geom_bar(mapping = aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  xlab("emotion categories") + ylab("number of tweets") +
  theme(title = "Sentiment Analysis")
#  plot.title = theme_text(size=12))
    
# plot distribution of polarity
ggplot(data = sent_df, mapping = aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of tweets") +
  theme(title = "Sentiment Analysis")

# WORD MAP

# separating text by emotion
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = some_txt[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)
