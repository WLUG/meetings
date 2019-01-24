# Using Layers in the Draw application of Open Office / Libre Office to produce simple animation.

## Introduction

The Open Office / Libre Office personal productivity suite includes the application *Draw*. The Draw application support a page having many layers. By default, a page is provided five layers, but you may create additional layers. You select which objects you wish to place in each layer. Each layer maybe set to display or hide the objects that are on that layer.

You may use the GUI interface to manually change the visibility of a layer, however the *Layer Manager* API of Draw allows you to run a program that will change the *IsVisible = True / False* property of a layer.

The BASIC programming language is provided and BASIC scripts may be included in a Draw document. BASIC code can quickly turn on and off layers. If shape or image objects are places in a series of layers and these objects change slightly between layer, then a moving animation is observed.

A very simple form of animation is the *flip book*. Youtube videos [here](https://www.youtube.com/watch?v=GO9NshFE6Yc) and [here](https://www.youtube.com/watch?v=Un-BdBSOGKY) provide examples of flip book animation. Using Draw in conjunction with running a BASIC program animation similar to a flip book may be achieved. 

The seven included Draw documents demonstrate aspects of this type of animation. The Draw files are:

  * demo 1 - circles 500 - show limitation.odg
  * demo 2 - circles 250.odg
  * demo 3 - 27 circles.odg
  * demo 4 - 100 circles.odg
  * demo 5 - 200 circles spiral.odg
  * demo 6 - wheel - 30 degrees - continuous.odg
  * demo 7 - wheel - 360 degrees.odg

## Description of each Draw demonstration 

### demo 1 - circles 500 - show limitation.odg

The five provided layers for a Draw page are access via the LayerManager using the index 0 to 4. When adding layers the first layer has an index of 5. Either manually or under program control a layer may be added and onto that layer an object, for example a circle shape, is placed. Under program control it appears that thousands of layers may be added and on each layer a shape is placed. However in reality only 250 layers may be added and have shapes inserted. The 250th layer added has an index of 254. After this, attempting to insert objects into additional layers results in the object being added to the layer with the index of 255. This layer 255 appears to operate in parallel with layer 0.

This first demo program demonstrates the issues when exceeding adding of 250 layers. This program is unstable, and it is difficult to predict the results you experience. The objective is to reinforce the idea that you limit the addition of layers to a maximum of 250.


### demo 2 - circles 250.odg

This Draw page has circles randomly placed onto 250 layers. These layers are from the index value of 5 through to 254. This demo is stable and should run with predictable results.


### demo 3 - 27 circles.odg

A total of 27 layers are created with the index values ranging from 5 to 31. On each layer a circle shape object is placed. As each circle is added to a layer it is located further to the right on the page. When the layer are displayed in sequence, then the circle appears to move from left to right acros the page.

Options are provided to demonstrate the equivalent of various frame rates per second.

### demo 4 - 100 circles.odg

The animation in Demo 3 is rather jerky to the eye. In this demonstration the number of layers / circles is increased to 100. This provides a smoother animation of the circle progressing from left to right across the page.

### demo 5 - 200 circles spiral.odg

This draw document contains 200 added layers. This time the circles are placed on the layers in a spiral sequence. The animation show the circle rotating in a circles as it spirals outwards.

### demo 6 - wheel - 30 degrees - continuous.odg

The image of a wheel with spokes is displayed on each layer. The image is rotated by 3 degrees between images. The wheel has a red dot on it. The wheel, when you look at the spokes, appears to rotate in full circles clockwise, however when you look at the red dot it only moves from 0 to 30 degrees using ten images on ten layers, and then repeats itself.

### demo 7 - wheel - 360 degrees.odg

A total of 120 images of the wheel are added to 120 layers. In sequence each image has the wheel rotated 3 degrees. In displaying the 120 layers in sequence the red dot on the wheel is observed to rotate in a full circle.

In western movies you may be familiar with the spokes on the cartwheel appearing to turn backwards or stop moving when the cart is actuallly moving forwards. This is when the camera frame rate is matching cartwheel speed at which a spoke moves to the position of the next spoke in each frame of the film.

This frame-rate strobing effect may be simulated using the BASIC program. For example if the animation only shows every 10th frame, then the spokes on the wheel appear to stay still, but the red dot on the wheel is rapidly rotating. You can change the interval to display every 4th frame, and again the red dot on the wheel appears to rotate clockwise, but the spokes seem to be randomly moving in both directions.

Edit the BASIC program to vary the frames to be displayed and you will observe different effect of the spoke direction with respect to the red dot on the wheel rotating.

Note: This Draw document is 20MB in size due to the 120 images of the wheel. 

## Summary

Draw is unsuitable for high quality animation, however you may find it suitable for teaching the concepts of  animation.


Presented at Waikato Linux User Group meeting on 21 January 2019.

Presenter: Ian Stewart

