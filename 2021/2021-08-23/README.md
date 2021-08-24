# 2021-08-23

Due to Covid19 NZ-wide lockdown, this meeting was held on-line using https://bigbluebutton.org/ application on servers provided by the NZ Open Source Society (NZOSS) https://nzoss.nz/

A list of public chat and shared notes from the meeting is provided at the bottom of this document.


* **Ian Stewart** delivered a presentation using BASIC to create and model OpenOffice / LibreOffice documents. He demonstrated BASIC code used in Writer, Draw and Calc documents. Note that in running any of the examples you will need "Enable Macros" for code execution to be performed. E.g. Tools--> Options--> Security --> Macro Security... --> Click on "Medium" 

* **Chris McClimans** *the Hippie Hacker* provided an update on activities of an Open Source company in the Bay of Plenty.

# Using BASIC to Create and Model OpenOffice / LibreOffice documents 

## Slide show

As an overview to this presentation Ian used the following slide show, which is provided as files in .odp and .pdf formats:

* Embedded Basic Presentation.odp
* Embedded Basic Presentation.pdf


## Writer Examples.
	
The Writer examples are:

* writer_basic_example_1.odt
* writer_basic_example_2.odt

They both demonstrate the ability of BASIC code to be able to read the embedded BASIC script and display it as text in the body of the Writer document.

Two buttons are provided. One button clears the displayed text, the other retrieves and displays the BASIC script.

In example_1 the text is displayed without making use to the "button" object that is passed to the message subroutine when the pushbutton is clicked:

```
sub message_button(button)
	doc = ThisComponent
	doc.getText().setString("Hello World! ~ Using BASIC" + _
	        chr(13) + chr(13) + Main())
end sub
```

In example_2 the text is displayed by making use of the "button" object that is passed to the message subroutine when the pushbutton is clicked. However, it is via a rather obscure path, requiring the retrieval of a method belonging to the great-grand-parent:

```
sub message_button(button)	
	button.Source.Model.Parent.Parent.Parent.getText().setString("Hello World! ~ Using BASIC" + _
	        chr(13) + chr(13) + Main())
exit sub
``` 

## Draw Example

The Draw example files is:

* draw_embedded_basic_plan.odg


This is a demonstraton of creating a floor plan for a building. On clicking the "Create" button a series of subroutines are called to draw the floor plan. So it's easier to observe the construction, a one second delay is inserted between subroutines being executed. 

Different parts of the floor plan are on different layers, so upon right-clicking a layer and clicking on "Modify Layer" the layer may have its "Visibility" turned on or off.

Once the floor plan has been drawn, then three buttons are provided to model different layouts of piles for the floor.

Note that in creating controls (E.g. push-buttons), the Draw application must be in "Design Mode". However upon completing programatically added controls, then the programatic method of turning off "Design Mode" does not function correctly. It seems to required manually toggling it a couple of times to turn it off. Thus, when clicking on "Clear" button the controls remain while everything else is removed. This avoids having to create these controls every time the "Create" button is pressed. However, the code that creates these controls is included with the BASIC script. 


### Debugging Tool

An additional subroutine called "debug" has been added at the bottom of the script. This is a utility routine to write an objects properties, methods and supported interfaces to a text file. To add the "debug" subroutine and "ShellSort" function to your Basic code, then, in your Basic code, add a line to call debug. E.g. debug(oDoc). 

The output file will be in the current directory and called "dbg_info.txt".
	
## Calc Example

The Calc example files is:

* calc_embedded_basic_amortization.ods

This is a demonstration of the amortization of a loan. Upon clicking on the "Create" pushbutton a spreadsheet labelled "Amortization" is created. Click the label to view this spreadsheet.

The creation of the spreadsheet included adding three slide controls. The top slide control models the amount of the loan from $10K to $1M. The second slide control models the percentage per annum of interest from 1% to 10%. the third control models the length of the loan in years from 1 to 40.

A table shows the mothly repayments and how much is going as interest and how much is going towards the repayment of the principle of the loan.

Also a chart is included to display the monthly amortization.


# Update on Open Source activities in the Bay of Plenty.

**Chris McClimans** *the Hippie Hacker* delivered an impromptu presentation on activities in the Open Source community in the Bay or Plenty. For more information see the web-site https://ii.nz/


# Chat and Share logs from BBB

The following are the Public Chat and Share logs from the meeting:

Public Chat from Virtual Meeting:

[19:17] Welcome to <b>WaikatoLinuxUsersGroup</b>!<br /><br />For help on using BigBlueButton see these (short) <a href="https://www.bigbluebutton.org/html5" target="_blank" target="_blank"><u>tutorial videos</u></a>.<br /><br />To join the audio bridge click the phone button.  Use a headset to avoid causing background noise for others.<br /><br />This server is running <a href="https://docs.bigbluebutton.org/" target="_blank"><u>BigBlueButton</u></a>.

[19:22] ian : ok. might log out and back in

[19:50] Peter Reutemann : sorry, only started recording now.

[20:34] Brendon : Thanks, interesting demo

[20:42] Hippie Hacker : https://github.com/ii/drones-day-out

[20:42] Hippie Hacker : https://github.com/ii/drones-day-out/blob/master/6x4-dual-mode.stl

[20:43] Hippie Hacker : Probably the best design we came up with: https://github.com/ii/drones-day-out/blob/master/hex-12.stl

[20:43] Peter Reutemann : The rendering is quite cool!

[20:44] Hippie Hacker : http://openingdesign.com publishes their architecture work using https://www.freecadweb.org to https://github.com/OpeningDesign

[20:47] Hippie Hacker : Apache Foundation Funding : https://projects.propublica.org/nonprofits/organizations/470825376

[20:47] Hippie Hacker : 2019 Total Revenue : $2,583,535USD

[20:49] Angus : Thanks!

[20:51] Peter Reutemann : https://syncthing.net/

[20:53] Hippie Hacker : Moment or two to : ii.nz
iikeyboard
pii.nz
iiphone
iisp
ii community centers

[20:53] Hippie Hacker : I thought the SHared notes was the agenda

[20:53] Hippie Hacker : Or maybe I can try for another meeting :)

[21:12] Hippie Hacker : https://careers-expo.ii.nz/monitor.html

[21:16] Hippie Hacker : https://github.com/ii/expo-prezzo

[21:22] Hippie Hacker : https://twitter.com/mauilion/status/1421261773311647745

=====

Shared

* Driving Open Office

v. cool Visual Basic Automation

* Chart / Calc

* Architectural Document

*  http://openingdesign.com

An (uberly) transparent and open source architectural studio.*

- https://github.com/OpeningDesign

- https://gitlab.com/openingdesign 

Using

- https://www.freecadweb.org

- http://openscad.org/

- https://github.com/ii/drones-day-out

- https://github.com/ii/drones-day-out/blob/master/README.md#drones-day-out


* ii.nz

iikeyboard

pii.nz

iiphone

iisp

ii community centers

