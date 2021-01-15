# Teaching Lab Performance Task

<p align = "center">
<b>Response to Option B</b>
</p>

<p align = "center">
[Link to Presentation](#https://rpubs.com/dungates/teachinglab_presentation)
</p>

This repository consists of parts to address each of the 3 "competencies" from part 2. The Directors of Partnerships will probably be most interested in the output of [FriendorColleague.html](FriendorColleague.html) where I visualize the "scores" of every single training session by each curriculum portfolio: EL, Guidebooks, Illustrative Mathematics (IM), or State-level work. Code is contained in the rmd folder where 5 separate R markdowns are used to obtain, clean, visualize, and edit the data. Four of these R Markdowns are knit to html files which are easy to view and what I intend as the "presentations" from part c. The PieCharter rmd can be knit to a shiny document, or in the PieCharter folder a Shiny App can be run that is also available as a shiny web app [here](https://dungates.shinyapps.io/PieCharter/).

In [DataObtainandClean.rmd](https://github.com/dungates/TeachingLab/blob/master/Rmd/DataObtainandClean.Rmd) I go over how I got the data with code that can easily be replicated by cloning from Github. This code will run on other hard drives through the use of the here library and the corresponding Data folder where I saved all my data after completing the cleaning process. [DataObtainandClean.html](DataObtainandClean.html) has the process of how I got the data and cleaned it with code output.


<p align = "center">
<b>In every html output of the r markdowns you can click images to view them in greater detail.</b>
</p>

Next in the other 3 R Markdown documents  I create separate analyses with technical details and visualizations to address part c, I could put them in a some slide-making tool but I feel that being able to scroll through them is most useful. I am addressing the ability

The 4 html files that result from knitting each of these is copied into the main folder for ease of access, and each is outlined below.

[LearningSessionSatisfaction.html](LearningSessionSatisfaction.html) looks at the column on learning session satisfaction, seeing how each instructor has fared over time, and also looking at all of them on a numeric basis, addressing the new column requirement. [SentimentAnalysis.html](SentimentAnalysis.html) has a complete textual analysis that aggregates text by column and across columns when there is textual response, and looks at several different methods/models of analyzing the data. [FriendorColleague.html](FriendorColleague.html) is almost exclusively visual and plots every single professional learning session separately by the only numeric column in the data - likeliness to recommend to a friend or colleague. Finally [PieCharter.rmd](https://dungates.shinyapps.io/PieCharter) introduces shiny functionality to create pie charts of factor columns which can facilitate such visualizations. Prettier pie charts can definitely be made with other tools but I thought the shiny functionality within an r markdown was pretty cool as I just discovered it.