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

## Calc Example
