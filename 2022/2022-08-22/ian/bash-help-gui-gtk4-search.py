#!/usr/bin/env python3
#
# bash-help-gui-gtk4-search.py
#
# Provide a GUI to display bash help.
#
# At a console you can type "& <tab> <tab>" and it will display all bash
# commands. This is due to the bash utility "compgen". This program collects
# all the bash commands by having subprocess execute the "compgen -c" command.
#
# Upon clicking on a command the following will attempt to be displayed:
# 1. Return from "whatis", which is the man page one-line description.
# 2. -h Short form help
# 3. --help Long form help
# 4. man pages
#
# A search sub-window is provided to lookup bash commands.
#
# Tested on:
# Manjaro 21.3.7 / Python 3.10.5 / Gtk 4.6.6
#
# Ian Stewart - ©CC0
#
import sys
from subprocess import check_output, Popen, PIPE, STDOUT, run
import gi
try:
    gi.require_version("Gtk", "4.0")
except ValueError as e:
    print(e)
    sys.exit("Unable to run {} program. Exiting...".format(sys.argv[0]))
from gi.repository import Gtk, Gdk, Gio

# version
VERSION = "2022-08-18"

PYTHON_VERSION = sys.version.split(" ")[0]
# Gtk version
GTK_VERSION = "{}.{}.{}".format(Gtk.get_major_version(), Gtk.get_minor_version(),
        Gtk.get_micro_version())
# Clicks to select items. Default is double-click ==> SINGLE_CLICK = False
SINGLE_CLICK = True
# Provide the column header with a heading.
COLUMN_HEADING = 'Bash Categories'
# Welcome message
WELCOME = """
Welcome to Bash Help Gui eith Search facility.
Version:{}
Python Version:{}
Gtk Version:{}
App:{}

Select a Help Category and then an item within the category.
""".format(VERSION, PYTHON_VERSION, GTK_VERSION, sys.argv[0])

class SearchWindow(Gtk.Window):
    """
    The SearchWindow uses a VBox to popular the window with:
    HeaderBar, SearchEntry, Scrolled ListBox, and Close Button
    """
    def __init__(self, textview_text_buffer, main_window):
        Gtk.Window.__init__(self)
        self.text_buffer = textview_text_buffer
        self.main_window = main_window
        self.set_default_size(300, 300)

        # WindowPosition NONE, CENTER, MOUSE, CENTER_ALWAYS, CENTER_ON_PARENT
        #self.set_position(Gtk.WindowPosition.CENTER)
        # set_position Not supported on Gtk4

        # Setup header bar      
        header_bar_search = Gtk.HeaderBar()       
        header_bar_search.set_show_title_buttons(False)
        label = Gtk.Label()
        label.set_text("Bash Help ~ Search")
        header_bar_search.set_title_widget(label)
        self.set_titlebar(header_bar_search)
        # Create vbox and add Search Entry, Scrolled Listbox, and Close button
        vbox = Gtk.Box()
        vbox.set_orientation(Gtk.Orientation.VERTICAL)
        self.search = Gtk.SearchEntry()
        self.search.set_name("search")
        self.search.connect('search-changed', self.search_changed)
        vbox.append(self.search)
        # Add a scrolled self.list_box
        self.list_box = Gtk.ListBox()
        self.list_box.set_name("listbox")
        self.list_box.set_selection_mode(Gtk.SelectionMode.SINGLE)
        self.list_box.connect('row-activated', self.on_row_activated)
        scrolled_window = Gtk.ScrolledWindow()
        scrolled_window.set_hexpand(True)
        scrolled_window.set_vexpand(True)
        scrolled_window.set_policy(Gtk.PolicyType.AUTOMATIC, Gtk.PolicyType.AUTOMATIC)
        scrolled_window.set_child(self.list_box)
        vbox.append(scrolled_window)
        # Add a Close Button to the search window
        button_close = Gtk.Button()
        button_close.set_name("close")
        button_close.set_label("Close")
        button_close.connect("clicked", self.close_search_window)
        hbox = Gtk.Box()
        hbox.set_orientation(Gtk.Orientation.HORIZONTAL)
        hbox.append(button_close) # was pack_end, False, False, 5)
        vbox.append(hbox)
        # Add vbox to window and display
        self.set_child(vbox)
        #self.set_style_search()

        self.present()
    
    # Methods of Search Window    
    def close_search_window(self, button):
        """
        Close the search_window.
        Doesn't seem to matter if .close() or .destroy() is used at this point.
        """
        self.destroy()

    def search_changed(self, search):
        """
        A keystroke event occurred in the SearchEntry
        Clear the contents of the list box of list rows.
        Each key stroke from the search entry widget ~ case independent.
        Filter the bash_command_list.
        Create the search_list
        """
        # Clear the self.list_box. Note: 'ListBox' has no len()
        try:
            while True:
                row_0 = self.list_box.get_row_at_index(0)
                self.list_box.remove(row_0)
        except TypeError as e:
            #print(e) # Argument 1 does not allow None as a value
            pass
        # Build the list of commands that match the search string
        if search.get_text():
            search_list = []
            for item in bash_command_list:
                if item.lower().startswith(search.get_text().lower()):
                    search_list.append(item)
        else:
            search_list = []
        # Take the data from the search list and rebuild the self.list_box.
        # List box row has a HBox, that contains a Label whose text is the command
        for item in search_list:
            row = Gtk.ListBoxRow()
            hbox = Gtk.Box()
            hbox.set_orientation(Gtk.Orientation.HORIZONTAL)            
            row.set_child(hbox)
            data = Gtk.Label()
            data.set_text(item)
            hbox.append(data)
            self.list_box.append(row)
            # Must have list_box.show() to update the list
            self.list_box.show()

    def on_row_activated(self, listbox, listboxrow):
        """
        Retrieve the data from the selected row from the list.
        List box row has a HBox, that contains a Label whose text is the command
        Call build_textview_string and then add string to TextView buffer.
        """
        row = listboxrow.get_child()
        label = row.get_first_child() 
        text = label.get_text()
        # Update the SearchEntry with selected command.
        self.search.set_text(text)
        # Using this text run it through the build routine then update TextView.
        string = build_textview_string(text)
        self.text_buffer.set_text(string)
        # Update the title to reflect selected item
        self.main_window.set_title("Help for Bash - Selection: {}".format(text))


class Window(Gtk.Window):
    """
    Main GTK Window containing TextView to display information on a bash command
    and a TreeView to select the bash command.
    A Header bar is used. It contains a button to launch the Search sub-Window
    """    
    def __init__(self, **kwargs):    
        super(Window, self).__init__(**kwargs)        
        self.connect("destroy", lambda x: Gtk.main_quit())
        # Add the title to the window
        version = sys.version.split(" ")[0]
        self.set_title("Help for Bash")
        # Set default window size
        self.set_default_size(1400, 600)
        self.grid = Gtk.Grid()
        self.set_child(self.grid)
        # Call Setup's for the Window
        self.setup_header_bar()
        self.setup_textview()
        self.setup_treeview()
        self.set_style()
        self.textbuffer.set_text(WELCOME + MESSAGE)
        # Instantiate the search sub-window, then close it.
        self.search_window = SearchWindow(self.textbuffer, self)
        #print("is_drawable", self.search_window.is_drawable()) # True
        self.search_window.destroy()
        #print("is_drawable", self.search_window.is_drawable()) # False

    # Methods of the Class Window...
    def setup_header_bar(self):
        """
        Setup the header bar and add a search button to it.
        """
        header_bar = Gtk.HeaderBar()
        header_bar.set_show_title_buttons(True)
        #header_bar.set_title_widget("Bash Help")
        #header_bar.set_subtitle("Python {} ~ Gtk {}".format(PYTHON_VERSION, GTK_VERSION))
        self.set_titlebar(header_bar)
        # Create Search button in the header bar.
        self.search_button = Gtk.Button()
        self.search_button.connect("clicked", self.open_search_window)
        icon = Gio.ThemedIcon(name ="search")
        image = Gtk.Image.new_from_gicon(icon) #, Gtk.IconSize.NORMAL) #INHERIT', 'LARGE', 'NORMAL)
        self.search_button.set_child(image)
        header_bar.pack_end(self.search_button)

    def open_search_window(self, button):
        """
        Open/close the search window using the header bar search button.
        .destroy() is necessary, not .close(), for is_drawable() to work.
        Pass self.textbuffer to the Seach Window.
        """
        if self.search_window.is_drawable():
            self.search_window.destroy()
        else:
            self.search_window = SearchWindow(self.textbuffer, self)

    def set_style(self):
        """ Loads custom CSS for textview and treeview"""
        style_provider = Gtk.CssProvider()
        style_provider.load_from_data(b"""
        #textview {
            font: 18px "Monospace";
            margin: 10px;
            }
        #treeview {
            font: 14px Sans;
            margin: 10px;
            }
        #search {
            margin: 10px;
            }
        #listbox {
            margin: 10px;
            }
        #close {
            margin: 5px;
            }              
        """)
        Gtk.StyleContext.add_provider_for_display(Gdk.Display.get_default(),
                style_provider,
                Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)

    def setup_textview(self):
        """
        Create a TextView to display the help/man information.
        TextView is inserted in a Scrolled Window.
        """
        textview = Gtk.TextView()
        textview.set_name("textview")
        self.textbuffer = textview.get_buffer()
        self.textbuffer.set_text("")
        scrolled_window = Gtk.ScrolledWindow()
        scrolled_window.set_child(textview)
        scrolled_window.set_hexpand(True)
        scrolled_window.set_vexpand(True)
        scrolled_window.set_policy(Gtk.PolicyType.AUTOMATIC, Gtk.PolicyType.AUTOMATIC)
        self.grid.attach(scrolled_window, 0, 0, 5, 1)

    def setup_treeview(self):
        """
        Create TreeStore, add Bash commands dictionary.
        Create TreeView insert in Scrolled window
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
        #scrolled_window.set_border_width(10)
        scrolled_window.set_child(self.treeview)
        self.grid.attach(scrolled_window, 6, 0, 1, 1)

    def cb_on_row_activate (self, treeview, path, column,):
        """
        Call back when click on a row in the treeview.
        Process selected bash command
        """
        self.textbuffer.set_text("")
        model = treeview.get_model()
        iterator  = model.get_iter (path)
        #print(model[iterator][0]) # Whatever item is clicked on
        # Don't want the categories only the items which have two fields
        pointer_list = path.to_string().split(":")
        if len(pointer_list) == 2:
            self.item_selected = model[iterator][0]
            # Update the title to reflect selected item. Overrides headerbar?
            self.set_title("Help for Bash - Selection: {}"
                    .format(self.item_selected))
            string = build_textview_string(self.item_selected)
            self.textbuffer.set_text(string)


#Functions shared by both Window classes and initial launch code.
def build_textview_string(item):
    """
    Build the string that is displayed in the textview panel.
    """
    # Get the whatis, -h, --help and man pages info.
    one_line_description = get_whatis(item)
    manpage_info = get_manpage(item)
    help_short = get_help_short(item)
    help_long = get_help_long(item)
    """
    one_line_description = self.get_whatis(self.item_selected)
    manpage_info = self.get_manpage(self.item_selected)
    help_short = self.get_help_short(self.item_selected)
    help_long = self.get_help_long(self.item_selected)
    """
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


def get_whatis(item):
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
    return output_str


def get_help_short(item):
    """
    Get the -h of an item, if there is one.
    """
    cmd = "{} -h".format(item)
    output = run(cmd, capture_output=True, shell=True, executable='/bin/bash')
    output_byte = output.stdout
    output_str = output_byte.decode("utf-8")
    return output_str


def get_help_long(item):
    """
    Get the --help of an item, if there is one.
    """
    cmd = "{} --help".format(item)
    output = run(cmd, capture_output=True, shell=True, executable='/bin/bash')
    output_byte = output.stdout
    output_str = output_byte.decode("utf-8")
    return output_str


def get_manpage(item):
    """
    Get the manpages if they exist
    """
    cmd = "man {}".format(item)
    output = Popen([cmd], shell = True, stdin=PIPE, stdout=PIPE, stderr=STDOUT)
    output_byte = output.communicate()[0]
    output_str = output_byte.decode("utf-8")
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
    Build a dictionary. A to Z as keywords, # as numbers, Misc as miscellaneous
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


class Application(Gtk.Application):
    ''' Main Application class '''
    def __init__(self):
        super().__init__(application_id='gtk4.python.help',
                         flags=Gio.ApplicationFlags.FLAGS_NONE)

    def do_activate(self):
        win = self.props.active_window
        if not win:
            win = Window(application=self)
        win.present()
        

if __name__ == '__main__':
    print("""
    Collecting information and setting up Bash Help Gui application...
    """)
    # Get the data and build the dictionary to pass data to TreeStore/TreeView
    # Get list of all bash commands from compgen with duplicates removed.
    bash_command_list = get_compgen_list()
    MESSAGE = ("\nA total of {} bash commands have been identified."
                .format(len(bash_command_list)))
    # Convert into a dictionary
    command_dict = build_dict(bash_command_list)
    # Run the application
    app = Application()
    app.run(sys.argv)
'''
Reference:
https://docs.gtk.org/gtk4/migrating-3to4.html
https://stackoverflow.com/questions/23550650/subprocess-library-wont-execute-compgen
'''
