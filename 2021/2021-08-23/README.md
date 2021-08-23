# 2021-08-23

Ian Stewart delivered a presentation using BASIC to create and model OpenOffice / LibreOffice documents. He demonstrated BASIC code used in Writer, Draw and Calc documents. 

Note that in running any of the examples you will need to "Enable Macros" to be executed. E.g. Tools--> Options--> Security --> Macro Security... --> Click on "Medium" 

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
