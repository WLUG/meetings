#!/usr/bin/env python3
#
# sun_info.py
# Calculates: Sunrise time and angle and sunset time and angle. 
#             Max altitude and time of solar noon.
#             Altitude and azmith of the sun at the current time.
#             Calculation are based on observer location values, etc. declared
#             in the initial global parameters.
#
# The command line supports the -s or --status argument. This will show the 
# parameters of the observer.
#
# Documentation: http://rhodesmill.org/pyephem/
# 
# Ian Stewart. August 2017.
#
# import
import sys
import argparse

# Python3 check
if float(sys.version[0:3]) < 3.5:
    sys.exit("Please restart using python version 3.5 or higher.")

# ephem module may need to be installed. So perform check.
try:
    import ephem
except ImportError as e:
    print("ImportError: Ephem module is not installed")
    print("Please use the following command to install the module")
    print("sudo apt-get install python3-ephem")
    sys.exit("Exiting program...")

# arrow module may need to be installed. So perform check.
try:
    import arrow
except ImportError as e:
    print("ImportError: Arrow module is not installed")
    print("Please use the following command to install the module")
    print("sudo apt-get install python3-arrow")
    sys.exit("Exiting program...")

# Instantiate
sun = ephem.Sun()
parser = argparse.ArgumentParser(description="Sun Ephemeris")

# Variables for Observer
# Setup for Nevada Road, Hamilton, NZ.
latitude = '-37.7862648'    # Nevada Road
longitude = '175.3319996'   # Nevada Road
elevation = 40              # meters above sea level
horizon = 0                 # 0 degrees. Angle for height of the horizon?
epoch = '2000/1/1 12:00:00' # J2000
temp = 10                   # Default is 15 degrees Celsius.
pressure = 1010             # milliBar.

location_description = "Nevada Road, Hamilton, New Zealand."
status = False


def setup_observer():
    """
    Setup the observer using the global variables.
    The date and time is at the moment of instantiation.
    """
    observer = ephem.Observer()
    # observer.date = # the moment of the instantiation
    observer.lat = latitude  # -37.7862648 Nevada Road
    observer.lon = longitude  # 175.3319996 Nevada Road
    observer.elevation = elevation  # 40 meters above sea level
    observer.horizon = horizon  # 0 degrees. Angle for height of the horizon?
    observer.epoch = epoch  # '2000/1/1 12:00:00'
    observer.temp = temp  # 15
    observer.pressure = pressure  # 1010
    return observer


def status(observer):
    """Get the current observer status."""
    observer_text = ["Location", "Date UTC", "Latitude", "Longitude", 
                     "Elevation", "Horizon", "Epoch", "Temperature", 
                     "Pressure"]
    observer_list = [location_description, observer.date, observer.lat, 
                     observer.lon, observer.elevation, observer.horizon,
                     observer.epoch, observer.temp, observer.pressure]
    print("\nStatus of Observer of the Sun.")
    for index, item in enumerate(observer_list):
        print("{: >15}: {}".format(observer_text[index], item))
    print()


def degrees_to_decimal(degrees_string):
    """
    Convert degrees:minutes:seconds to degrees.decimal
    # E.g. '-37:47:10.6' returns -37.78627777777778 Originally: -37.7862648
    #      '175:19:55.2' returns 175.332 Originally:'175.3319996'  
    """
    minutes = 0
    seconds = 0
    degrees_list = degrees_string.split(":")
    degrees = float(degrees_list[0])
    if len(degrees_list) > 1:
        minutes = float(degrees_list[1]) / 60
    if len(degrees_list) > 2:
        seconds = float(degrees_list[2]) / 3600
    if degrees >= 0:
        degrees = degrees + minutes + seconds
    else:
        degrees = degrees - minutes - seconds
    # print(degrees)
    return degrees


def convert_to_utc_output_local(ephem_date_time):
    """
    Receive the ephem date/time as <class 'ephem.Date'>
    Convert to nz_time = ephem.localtime(ephem.date) <class 'datetime.datetime'
    Convert to Unix_time. unix_time = arrow.get(nz_time).format("X")
    Convert string to int and minus 12 hours. unix_utc = int(sr_unix) - 43200 
    Use Arrow to return local time with formatting.
    """
    # TODO: Check if it changes for daylight savings.
    nz_datetime_0_offset = ephem.localtime(ephem_date_time)
    nz_unix_time = arrow.get(nz_datetime_0_offset).format("X")
    utc_unix_time = int(nz_unix_time) - 43200
    nz_unix_time = (arrow.get(utc_unix_time).to('local')
                    .format("YYYY-MM-DD H:mm:ss"))
    return nz_unix_time


def sunrise_sunset_nz_time(observer):
    """
    Output the previous and next sunrise and sunsets in NZST and current day.
    Calls convert_to_utc_output_local.
    sun is a global.
    """
    sr_previous = convert_to_utc_output_local(observer.previous_rising(sun))
    ss_previous = convert_to_utc_output_local(observer.previous_setting(sun))
    print("The previous sunrise was {}".format(sr_previous))
    print("The previous sunset was  {}".format(ss_previous))


    sr_next = convert_to_utc_output_local(observer.next_rising(sun))
    ss_next = convert_to_utc_output_local(observer.next_setting(sun))
    print("The next sunrise is {}".format(sr_next))
    print("The next sunset is  {}".format(ss_next))


def nz_solar_noon_today(observer):
    """
    Provide the solar noon time and angle of altitude.    
    """
    # Compute the sun relative to the observer.
    sun.compute(observer)
    # Call converter to get the nz time for solar noon.
    solar_noon_time = convert_to_utc_output_local(sun.transit_time)
    solar_noon_time_list = solar_noon_time.split(" ")
    print("The solar noon for today at {}".format(solar_noon_time_list[1]))
    
    # Call converter to get the solar noon altitude angle.  
    solar_noon_altitude = degrees_to_decimal(str(sun.transit_alt))
    print("Today's solar noon altitude angle {:.1f} degrees."
          .format(solar_noon_altitude))

    #print(type(sun.transit_alt)) #<class 'ephem.Angle'>
    #print(sun.transit_alt)    
    #print(str(sun.transit_alt))
    #print(type(str(sun.transit_alt)))

    #print(sun.transit_alt)  # 75:38:42.5            41:28:19.7
    #print(sun.transit_time)  # 2017/12/23 00:17:37  2017/8/25 00:20:50


def nz_current_position_altitude_sun(observer):
    """
    Use the observer to report current altitude and angle of the sun.    
    """
    sun.compute(observer)

    sun_altitude = degrees_to_decimal(str(sun.alt))
    print("The sun's current altitude is {:.1f} degrees.".format(sun_altitude))

    sun_azimuth = degrees_to_decimal(str(sun.az))
    print("The sun's current azimuth is {:.1f} degrees.".format(sun_azimuth))
    

def nz_current_time():
    """
    Provides a long description of the current date and time.
    """
    nz_date_time_string = ("The date is {}, {}.\nThe time is {} hours, "
                           "{} minutes and {} seconds."
                           .format(
                                   arrow.now().format("dddd"),
                                   arrow.now().format("D MMMM YYYY"),
                                   arrow.now().format("H"),
                                   arrow.now().format("m"),
                                   arrow.now().format("s"),
                                   ))  # Do 22 = 22
    return nz_date_time_string


def main():
    """
    Control the flow of the program
    """
    # Setup the observer
    observer = setup_observer()

    # Report status
    if args.status:
        status(observer)

    # Announce current NZ date and time in long form.
    print(nz_current_time())

    # Sunrise and sunset - NZ time
    sunrise_sunset_nz_time(observer)

    # Solar noon information.
    nz_solar_noon_today(observer)

    # Sun current altitude and angle.
    nz_current_position_altitude_sun(observer)

    sys.exit()

      
if __name__ == "__main__":
    # Launch program...
    # Argparse. Provide flag to display the observer status information)
    parser.add_argument('-s', '--status',
                        action='store_true',
                        help='Provide the observer status information.')

    # Instantiate.
    args = parser.parse_args()
    
    main()

