#!/usr/bin/env python3
#!
# bash-help-gui-gtk3.py
#
# Provide a GUI to display bash help.
#
# At a console you can type "& <tab> <tab>" and it will display all bash
# commands. This is due to the bash utility "compgen". This program collects
# all the bash commands by having subprocess execute the "compgen -c" command.
# 
# Upon clicking on a command the following will attempt to be displayed:
# 1. Return from "whatis", which is the man page description line.
# 2. -h Short form help  
# 3. --help Long form help
# 4. man pages
#
# Tested on: 
# ubuntu 22.04 / python 3.10.4 
# 
# Ian Stewart - CC0
#  
import sys
import os
from subprocess import check_output, Popen, PIPE, STDOUT, run
import contextlib
from io import StringIO
import gi
try: 
    gi.require_version("Gtk", "3.0")
except ValueError as e:
    print(e)
    sys.exit("Unable to run {} program. Exiting...".format(sys.argv[0]))
from gi.repository import Gtk, Gdk

# version
VERSION = "2022-08-13"
# Gtk version
GTK_VERSION = "{}.{}.{}".format(Gtk.get_major_version(), Gtk.get_minor_version(),
        Gtk.get_micro_version())
# Clicks to select items. Default is double-click ==> SINGLE_CLICK = False
SINGLE_CLICK = True
# Provide the column header with a heading.
COLUMN_HEADING = 'Bash Categories'
# Welcome message
WELCOME = """
Welcome to Bash Help Gui 
Version:{}. 
Gtk Version:{}. 
App:{}

Select a Help Category and then an item within the category.
""".format(VERSION, GTK_VERSION, sys.argv[0])


class Window(Gtk.Window):
    def __init__(self):
        Gtk.Window.__init__(self)

        # Add the title to the window
        version = sys.version.split(" ")[0]
        self.set_title("Help for Bash {}".format(version))      
        # Set default window size
        self.set_default_size(1400, 600)
        
        self.grid = Gtk.Grid()
        self.add(self.grid)

        self.create_textview()
        self.setup_treeview()
        self.set_style()
        self.textbuffer.set_text(WELCOME + MESSAGE)

    def set_style(self):
        """ Loads custom CSS for textview and treeview"""
        style_provider = Gtk.CssProvider()
        style_provider.load_from_data(b"""
        #textview { 
            font: 18px "Monospace";
            /* margin: 10px; 8 */
            }
        #treeview {
            font: 14px Sans;
            /* margin: 10px; */
            }
        """)
        Gtk.StyleContext.add_provider_for_screen(Gdk.Screen.get_default(), 
                style_provider, 
                Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)

    def create_textview(self):
        """
        Create a TextView to display the help/man information.
        TextView is inserted in a Scrolled Window.
        """
        scrolled_window = Gtk.ScrolledWindow()
        scrolled_window.set_hexpand(True)
        scrolled_window.set_vexpand(True)
        scrolled_window.set_policy(Gtk.PolicyType.AUTOMATIC, Gtk.PolicyType.AUTOMATIC)
        scrolled_window.set_border_width(10)
        self.grid.attach(scrolled_window, 0, 0, 5, 1)        

        self.textview = Gtk.TextView()
        self.textview.set_name("textview")        
        self.textbuffer = self.textview.get_buffer()
        self.textbuffer.set_text("")
        scrolled_window.add(self.textview)

    def setup_treeview(self):
        """
        Create TreeStore, add Bash commands dictionary. 
        Create TreeView insert in Scrolled windows 
        """
        # create a TreeStore with one string column to use as the model
        store = Gtk.TreeStore(str)
        
        # From help dictionary add the categories and items to the store 
        for category, items in command_dict.items():
            #print(items) # is a list of all items for that category
            category_key = store.append(None, [category]) 
            for item in items:
                store.append(category_key, [item]) 

        # create the TreeView using treestore data
        self.treeview = Gtk.TreeView()
        self.treeview.set_model(store)        
        # Set unique name so css can be applied to widget
        self.treeview.set_name("treeview")
        # Preference for self.treeview. Adjust via contants at start of program
        self.treeview.set_activate_on_single_click(SINGLE_CLICK)       
        treeview_column = Gtk.TreeViewColumn(COLUMN_HEADING)
        self.treeview.append_column(treeview_column)
        cell = Gtk.CellRendererText()
        treeview_column.pack_start(cell, True)
        treeview_column.add_attribute(cell, 'text', 0)
        # Auto highlight list as hover over.
        self.treeview.set_hover_selection(True)
        # Auto-open the category and show the list with hover_expand
        self.treeview.set_hover_expand(True)  
        # call-back on the self.treeview
        self.treeview.connect ("row-activated", self.cb_on_row_activate,)
        # Create a window that can be scrolled
        scrolled_window = Gtk.ScrolledWindow(hexpand=True, vexpand=True)
        scrolled_window.set_policy(Gtk.PolicyType.AUTOMATIC, Gtk.PolicyType.AUTOMATIC)
        scrolled_window.set_border_width(10)  
        scrolled_window.add(self.treeview)
        self.grid.attach(scrolled_window, 6, 0, 1, 1) 

    def cb_on_row_activate (self, treeview, path, column,):
        """
        Call back when click on a row in the treeview. 
        Process selected bash command
        """
        self.textbuffer.set_text("")  # Does this stop the 5 x hang?
        
        model = treeview.get_model()
        iter  = model.get_iter (path)
        #print(model[iter][0]) # Whatever item is clicked on

        # Don't want the categories only the items which have two fields
        pointer_list = path.to_string().split(":")
        if len(pointer_list) == 2:
            self.item_selected = model[iter][0]
            # Update the title to reflect selected item
            version = sys.version.split(" ")[0]
            self.set_title("Help for Bash - Selection: {}"
                    .format(self.item_selected))
                    
            # Get the whatis, -h, --help and man pages info.       
            one_line_description = self.get_whatis(self.item_selected)         
            manpage_info = self.get_manpage(self.item_selected)
            help_short = self.get_help_short(self.item_selected)
            help_long = self.get_help_long(self.item_selected)            
            
            # Build text and display 
            string = self.item_selected + ": "
            string += "\n" + one_line_description
            string += "\n" + "== -h ====" + "=" * 80
            string += "\n" + help_short
            string += "\n" + "== --help " + "=" * 80 
            string += "\n" + help_long
            string += "\n" + "== man ===" + "=" * 80                          
            string += "\n" + manpage_info  
            string += "\n" + "=" * 90            
                   
            self.textbuffer.set_text(string)

    def get_help_short(self, item):
        """
        Get the -h of an item, if there is one.
        """
        cmd = "{} -h".format(item)  
        output = run(cmd, capture_output=True, shell=True, executable='/bin/bash')
        output_byte = output.stdout
        output_str = output_byte.decode("utf-8")
        return output_str

    def get_help_long(self, item):
        """
        Get the --help of an item, if there is one.
        """
        cmd = "{} --help".format(item)
        output = run(cmd, capture_output=True, shell=True, executable='/bin/bash')
        output_byte = output.stdout               
        output_str = output_byte.decode("utf-8")
        return output_str

    def get_manpage(self, item):
        """
        Get the manpages if they exist
        """               
        cmd = "man {}".format(item)
        output = Popen([cmd], shell = True, stdin=PIPE, stdout=PIPE, stderr=STDOUT)
        output_byte = output.communicate()[0]        
        output_str = output_byte.decode("utf-8")
        return output_str
        
    def get_whatis(self, item):
        """
        From the man pages get the one-line description
        """
        cmd = "whatis -l {}".format(item)       
        output = Popen([cmd], shell = True, stdin=PIPE, stdout=PIPE, stderr=STDOUT)
        output_byte = output.communicate()[0]        
        output_str = output_byte.decode("utf-8")
        # Clear the "nothing appropriate" entries. Could change to 
        if "nothing appropriate." in output_str:
            output_str = " - "     
        # Example: cp (1)               - copy files and directories
        # All descriptions prefixed with ' - '    
        output_str = output_str.split(" - ")[1]
        #print(item, output_str)
        return output_str


# Functions for Initial Data collection on launch of bash commands
def get_compgen_list():
    """
    Get the commands from the bash utility compgen -c.
    Output is in bytes.
    Convert to a string list
    Remove duplicates.
    Sort
    """
    output = check_output('compgen -c', shell=True, executable='/bin/bash')
    command_list_byte = output.splitlines()
    # Convert from bytes list to string list
    command_list_str = []
    for item in command_list_byte:
        command_list_str.append(item.decode("utf-8"))
    # Remove duplicates in the list
    command_list = list(dict.fromkeys(command_list_str))
    # Sort the list    
    command_list.sort()
    return command_list


def build_dict(command_list):
    """
    Build a dictionary. A to Z as keywords, # as numbers, misc as miscellaneous
    >>> chr(65) 'A' >>> chr(90) 'Z'
    """            
    command_dict = {}
    # For numbers
    command_dict["#"] = []
    # Miscellaneous
    command_dict["Misc"] = []
    # Create Dictionary keywords from A to Z

    for value in range(65, 91):
        command_dict[chr(value)] = []

    for item in command_list:
        start_char = item[0:1].upper()
        if ord(start_char) >= 65 and ord(start_char) <= 90:
            command_dict[start_char].append(item)
        elif start_char.isdecimal():
            command_dict["#"].append(item)                      
        else:
            command_dict["Misc"].append(item)               
    return command_dict
        

if __name__=="__main__":

    print("""
    Collecting information and setting up Bash Help Gui application.    
    """)
    
    # Get the data and build the dictionary to pass data to TreeStore/TreeView           
    # Get list of all bash commands from compgen with duplicates removed.
    bash_command_list = get_compgen_list()
    #print(bash_command_list)
    
    MESSAGE = ("\nA total of {} bash commands have been identified."
                .format(len(bash_command_list)))
             
    # Convert into a dictionary
    command_dict = build_dict(bash_command_list)
    #print(command_dict)

    #for key, item in command_dict.items():
    #    print(key, len(item))
        
    # Start up the window
    win = Window()
    win.connect("destroy", Gtk.main_quit)
    win.show_all()
    Gtk.main()

"""
Reference: 
https://stackoverflow.com/questions/23550650/subprocess-library-wont-execute-compgen
"""
