#!/usr/bin/env python3
# bash-help-gui-qt6.py
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
# Manjaro 21.3.7 / Python 3.10.5 / Qt 6.3.1
# 
# Ian Stewart - ©CC0
#  
import sys
from subprocess import PIPE, STDOUT, run
from PyQt6.QtCore import Qt, qVersion
from PyQt6.QtWidgets import (QApplication, QMainWindow, QSplitter, 
        QTextEdit, QTreeWidgetItem,  QTreeWidget)

# version
VERSION = "2022-08-24"
PYTHON_VERSION = sys.version.split(" ")[0]
# Qt version
QT_VERSION = "{}".format(qVersion())
# Provide the column header with a heading.
HEADING = 'Bash Categories'
# Welcome message
WELCOME = """
Welcome to Bash Help Gui 
Version:{}
Python: {}
Qt Version:{}
App:{}

Select a Help Category and then an item within the category.
""".format(VERSION, PYTHON_VERSION, QT_VERSION, sys.argv[0])


class MainWindow(QMainWindow):
    def __init__(self, dictionary) -> None:
        super().__init__()
        self.setWindowTitle("Bash Help")
        self.resize(1400,600)
        # Where does QGuiApplication come from?)
        #self.resize(QGuiApplication.primaryScreen().availableGeometry().size() * 0.7)

        splitter = QSplitter()
        self.setCentralWidget(splitter)

        self.textedit = QTextEdit()
        self.textedit.setText("")  
        self.textedit.setStyleSheet("font: 14pt Monospace")     
        splitter.addWidget(self.textedit)
                
        self.tree = QTreeWidget()
        splitter.addWidget(self.tree)        
        self.tree.clear()
        self.tree.setHeaderLabel(HEADING)        
        self.tree.setColumnCount(1)
        self.tree.clicked.connect(self.treewidget_clicked)
        self.fill_tree_widget_item(self.tree.invisibleRootItem(), dictionary)        
        
        splitter.setStretchFactor(0,4)
        splitter.setStretchFactor(1,1) 
        
        self.textedit.setText(WELCOME + MESSAGE)       
              
    def fill_tree_widget_item(self, invisible_root_item, dictionary):
        for keyword, element_list in dictionary.items():
            parent = QTreeWidgetItem([str(keyword)])
            parent.setFlags(parent.flags() & ~ Qt.ItemFlag.ItemIsSelectable) 
            invisible_root_item.addChild(parent)
            #parent.setExpanded(True) # Default is False                             
            for element in element_list:
                child = QTreeWidgetItem([str(element)])
                parent.addChild(child)                    

    def treewidget_clicked(self, model_index):
        #print(model_index) # PyQt6.QtCore.QModelIndex object
        #print(model_index.flags().value) # 60 parent or 61 child        
        # Check is ItemIsSelectable based on model_index.flags()
        if not model_index.flags() & Qt.ItemFlag.ItemIsSelectable:
            #print("Item is not selectable")
            return
        self.item_selected = model_index.data()
        #print("Selected Item: {}".format(self.item_selected))

        self.setWindowTitle("Bash Help - Selection: {}"
                    .format(self.item_selected))
        string = display_bash_help(self.item_selected)
        self.textedit.setText(string)        


# Functions shared by both Window classes and initial launch code.
def display_bash_help(item):
    """
    Build the string that is displayed in the textview panel.
    """
    # Get the whatis, -h, --help and man pages info.
    one_line_description = get_info("whatis", item)
    help_short = get_info(item, "-h")
    help_long = get_info(item, "--help")
    manpage_info = get_info("man", item)

    # Clean up one_line_description - Just give the tail info
    if "nothing appropriate." in one_line_description:
        one_line_description = " - "
    # Example: cp (1)               - copy files and directories
    # All descriptions are prefixed with ' - '
    one_line_description = one_line_description.split(" - ")[1]

    # Build text and display
    string = item + ": "
    string += "\n" + one_line_description
    string += "\n" + "== -h ====" + "=" * 80
    string += "\n" + help_short
    string += "\n" + "== --help " + "=" * 80
    string += "\n" + help_long
    string += "\n" + "== man ===" + "=" * 80
    string += "\n" + manpage_info
    string += "\n" + "=" * 90
    return string


def get_info(cmd, arg):
    """
    Use subprocess.run for a bash command to get -h, --help and man pages.
    This will get the -h and --help for the command: compgen
    """
    return run("{} {}".format(cmd, arg), shell=True, executable='/bin/bash',
                    text=True, stdout=PIPE, stderr=STDOUT).stdout


# Functions for initial collection of data upon launching program.
def get_compgen_list():
    """
    Use subprocess.run to get the commands from the bash utility compgen -c.
    Output is string text using newline as the delimiter.
    Convert to a list, remove duplicates, sort, then Return the command_list
    """
    output = get_info("compgen", "-c")
    # Convert to a list
    command_list = output.splitlines()
    # Remove duplicates in the list
    command_list = list(dict.fromkeys(command_list))
    # Sort
    command_list.sort()
    return command_list


def build_dict(command_list):
    """
    Build a dictionary. A to Z as keywords, # as numbers, Misc as miscellaneous
    ord("A") = 65 and ord("Z") = 90.
    """
    command_dict = {}
    # Keyword for numbers
    command_dict["#"] = []
    # Keyword for Miscellaneous
    command_dict["Misc"] = []
    # Create Dictionary keywords from A to Z
    for value in range(ord("A"), ord("Z")+1):
        command_dict[chr(value)] = []
    for item in command_list:
        start_char = item[0:1].upper()
        if ord(start_char) >= ord("A") and ord(start_char) <= ord("Z"):
            command_dict[start_char].append(item)
        elif start_char.isdecimal():
            command_dict["#"].append(item)
        else:
            command_dict["Misc"].append(item)
    return command_dict


if __name__ == '__main__':
        
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
    
    app = QApplication([])
    w = MainWindow(command_dict)
    w.show()
    app.exec()

"""
Reference
https://stackoverflow.com/questions/21805047/qtreewidget-to-mirror-python-dictionary
"""
