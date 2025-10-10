// -!- rust -!- //////////////////////////////////////////////////////////////
//
//  System        : 
//  Module        : 
//  Object Name   : $RCSfile$
//  Revision      : $Revision$
//  Date          : $Date$
//  Author        : $Author$
//  Created By    : Robert Heller
//  Created       : 2025-10-03 15:59:04
//  Last Modified : <251010.0943>
//
//  Description	
//
//  Notes
//
//  History
//	
/////////////////////////////////////////////////////////////////////////////
//    Copyright (C) 2025  Robert Heller D/B/A Deepwoods Software
//			51 Locke Hill Road
//			Wendell, MA 01379-9728
//
//    This program is free software; you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation; either version 2 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program; if not, write to the Free Software
//    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
// 
//
//////////////////////////////////////////////////////////////////////////////

//! ## Model Railroad Time Table generating program
//! The Time Table  program is a port of the Time Table V2 program that is part of
//! the Model  Railroad  System.  That  program is partly in C++  (low-level  data
//! structures) and part in Tcl/Tk (GUI main program).  This is mostly just a port
//! of the C++ code. This  program  was  inspired by chapter 8 of the book _How to
//! Operate Your Model Railroad_ by Bruce A. Chubb.  I strongly  recommend reading
//! this chapter  fully before using this  program.  This program  implements  the
//! methods described in this chapter, in an automated fashion. 

pub mod station;
pub mod cab;
pub mod train;

use crate::station::*;
use crate::cab::*;
use crate::train::*;
use std::collections::HashMap;
use std::collections::LinkedList;
use std::path::PathBuf;
use std::str::FromStr;
use std::convert::Infallible;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;


/**  @brief A Vector of doubles.
  * 
  * Used as a vector of layover times.
  *
  * @author Robert Heller \<heller\@deepsoft.com\>
  *
  */
pub type DoubleVector = Vec<f64>;

/** @brief Option hash map, used for Print options.
  *
  * @author Robert Heller \<heller\@deepsoft.com\>
  *
  */

pub type OptionHashMap = HashMap<String, String>;


/** @brief List of trains.
  *
  * Simple linked list of trains, used for passing train lists
  * around. 
  *
  * @author Robert Heller \<heller\@deepsoft.com\>
  *
  */
pub type TrainList =  LinkedList<&'static Train>;

/** @brief Station times class, used by the LaTeX generator methods.
  *
  * This class holds time table information used in the code that generates 
  * the LaTeX tables.  Each StationTimes item contains one table 
  * element in the form of an arrival time and a departure time.  The flag
  * member indicates if only the arrival time, departure time, or both times
  * are valid.  An originating train has no arrival time and a terminating
  * train has no departure time.
  *
  * This class is actually used to hold the information for a single cell in
  * a formatted time table.  Each cell contains an arrivial time and a 
  * departure time.  Each row in the table contains the information for a
  * specific station and each column contains the information for a single
  * train.
  *
  * @sa TrainStationTimes TrainTimesAtStation.
  *
  * @author Robert Heller \<heller\@deepsoft.com\>
  *
  */
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct StationTimes {
    arrival: f64,
    departure: f64,
    flag: StopFlagType,
}

impl Default for StationTimes {
    fn default() -> Self {
        Self {arrival: -1.0, departure: -1.0, flag: StopFlagType::Transit}
    }
}
        

impl StationTimes {
    /** Constructor: create an entry for a time table cell.
      *   @param a The arrival time.
      *   @param d The departure time.
      *   @param f The stop flag: Origin, Terminate, or Transit.
      */
    pub fn new(a: f64, d: f64, f: StopFlagType) -> Self {
        Self {arrival: a, departure: d, flag: f }
    }
    /** Accessor for the arrival time.
      */
    pub fn Arrival(&self) -> f64 {self.arrival}
    /** Accessor for the departure time.
      */
    pub fn Departure(&self) -> f64 {self.departure}
    /** Accessor for the type of stop flag.
      */
    pub fn  Flag(&self) -> StopFlagType {self.flag}

}

/** @brief Map of station times, indexed by train number.
  *
  * These are the individual
  * rows of the time table. The train number (symbol) is the column index.
  * Each of these rows is for a single station.  This is a sparse vector, since
  * not all trains stop at (or go past) all stations.  The ommited elements
  * result in blank cells in the output table.
  *
  * @author Robert Heller \<heller\@deepsoft.com\>
  *
  */
pub type TrainStationTimes = HashMap<String,StationTimes>;

/** @brief Map of maps of station times, indexed by station index.
  *
  * This is the whole
  * time table.  The station index is the row index.  This is a sparse vector,
  * since not all trains stop at (or go past) all stations.  The ommited
  * elements result in blank cells in the output table.
  *
  * @author Robert Heller \<heller\@deepsoft.com\>
  *
  */
pub type TrainTimesAtStation = HashMap<usize, TrainStationTimes>;

/** @brief List of strings.
  *
  * This is a simple linked list of strings, used in various places.
  *
  * @author Robert Heller \<heller\@deepsoft.com\>
  *
  */
pub type StringList =  LinkedList<String>;

/** @brief Convert a list of strings to a flat string.
  *
  * The result is comma
  * separated and each string is enclosed in quote characters 
  * (@c ").  If a string contains a quote character or a
  * backslash, the character is quoted with a backslash.
  *   @param list The list of strings.
  *
  * @author Robert Heller \<heller\@deepsoft.com\>
  *
  */
pub fn StringListToString(list: StringList) -> String {
    let mut result: String = String::new();
    let mut comma: String = String::new();
    for theString in list.iter() {
        result += &comma;
        result += "\"";
        for char in theString.chars() {
            if char == '"' || char == '\\' {result += "\\";}
            result += &String::from(char);
        }
        result += "\"";
        comma = String::from(",");
    }
    result
}

/** @brief Convert a flat string to a list of strings.
  *
  * Returns false if there was a syntax error.
  *    @param strlinList The input string.
  *    @param result The output list.
  *
  * @author Robert Heller \<heller\@deepsoft.com\>
  *
  */
pub fn StringListFromString(strlinList: String) -> Option<StringList> {
    let mut result = LinkedList::new();
    let mut inString: bool = false;
    let mut expectcomma: bool = false;
    let mut expectquotes: bool = true;
    let mut escape: bool= false;
    let mut theString: String = String::new();
    for c in strlinList.chars() {
        if inString {
            if escape {
                theString += &String::from(c);
                escape = false;
            } else if c == '"' {
                result.push_back(theString.clone());
                theString.clear();
                inString = false;
                expectcomma = true;
                expectquotes = false;
            } else if c == '\\' {
                escape = true;
            } else {
                theString += &String::from(c);             
            }
        } else {
            if c == ',' && expectcomma {
                expectcomma = false;
                expectquotes = true;
            } else if c == '"' && expectquotes {
                theString.clear();
                inString = true; 
            } else {
                return None
            }
        }
    }
    if escape {
        None
    } else if result.len() == 0 && !inString && expectquotes {
        Some(result)
    } else if !inString && expectcomma {
        Some(result)
    } else {
        None
    }
}

/** @brief This is the main Time Table Class.
  *
  * It implements all of the basic data
  * and algorithms used in the Time Table program.
  *
  * This class includes code to load a set of stations and the trains that
  * run between these stations, along with code to read and write a time table
  * file and code to create a formatted time table, suitable for printing (by
  * way of LaTeX).
  *
  * @author Robert Heller \<heller\@deepsoft.com\>
  *
  */

#[derive(Debug, Clone, PartialEq)]
pub struct TimeTableSystem {
    name: String,
    filepath: PathBuf,
    timescale: u32,
    timeinterval: u32,
    stations: StationVector,
    cabs: CabNameMap,
    trains: TrainNumberMap,
    notes: Vec<String>,
    print_options: OptionHashMap,
    table_of_contents_p: bool,
    direction_name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConstructorError {
    BadFilename(Infallible),
    CouldNotOpenFile(String,String),
    PrematureEOF(String),
    FileIOError(String,String),
    MissingTimescale,
    TimescaleParseError(String,String),
    MissingTimeinterval,
    TimeintervalParseError(String,String),
    StationCountParseError(String,String),
    StationParseError(String,String),
    CabCountParseError(String,String),
    CabParseError(String,String),
    TrainCountParseError(String,String),
    TrainParseError(String,String),
    NoteCountParseError(String,String),
    OptionsCountParseError(String,String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AddTrainError {
    EmptyStopList,
    DuplicateTrain,
    RangeError(String),
    BadStationNumber(usize),
    DuplicateStorageIsAt(String,String,String),
    DuplicateStorageAt(String,String),

}

#[derive(Debug, Clone, PartialEq)] 
pub enum DeleteTrainError {
    NoSuchTrain(String),
    InternalMissingOcc(String),
    
}

impl TimeTableSystem {
    /** @brief The constructor that creates a time table system from an existing
      * file.
      *
      * The file is read in and the class is properly initialized 
      * from the data in the file.
      *   @param filename The name of the file to load.
      *   @param outmessage Pointer to a pointer to receive any error 
      *     messages for any errors that might occur.
      */
    pub fn old(filename: String) -> Result<Self, ConstructorError> {
        let filepath: PathBuf = match PathBuf::from_str(&filename) {
            Ok(f) => f,
            Err(p) => return Err(ConstructorError::BadFilename(p)),
        };
        let file = match File::open(&filepath) {
            Ok(f) => f,
            Err(e) => return Err(ConstructorError::CouldNotOpenFile(filename,
                                                            e.to_string())),
        };
        let mut buf_reader = BufReader::new(file);
        let mut name: String = String::new();
        match buf_reader.read_line(&mut name) {
            Ok(l) => if l == 0 {
                        return Err(ConstructorError::PrematureEOF(String::from("Reading name")));
                     },
            Err(e) => return Err(ConstructorError::FileIOError(String::from("Reading name"),e.to_string())),
        };
        let mut line_buffer: String = String::new();
        match buf_reader.read_line(&mut line_buffer) {
            Ok(l) => if l == 0 {
                        return Err(ConstructorError::PrematureEOF(String::from("Reading timescale and timeinterval")));
                     },
            Err(e) => return Err(ConstructorError::FileIOError(String::from("Reading timescale and timeinterval"),e.to_string())),
        };
        let mut sp = line_buffer.split(' ');
        let timescale: u32 = match sp.next() {
            None => return Err(ConstructorError::MissingTimescale),
            Some(s) => match s.parse::<u32>() {
                Ok(t) => t,
                Err(e) => return Err(ConstructorError::TimescaleParseError(s.to_string(),e.to_string())),
            }
        };
        let timeinterval: u32 = match sp.next() {
            None => return Err(ConstructorError::MissingTimeinterval),
            Some(s) => match s.parse::<u32>() {
                Ok(t) => t,
                Err(e) => return Err(ConstructorError::TimeintervalParseError(s.to_string(),e.to_string())),
            }
        };
        let mut this = Self {name: name, filepath: filepath, 
                             timescale: timescale, timeinterval: timeinterval,
                             stations: Vec::new(), cabs: HashMap::new(), 
                             trains: HashMap::new(), notes: Vec::new(), 
                             print_options: HashMap::new(),
                             table_of_contents_p: true, 
                             direction_name: String::new() };
        line_buffer.clear();
        match buf_reader.read_line(&mut line_buffer) {
            Ok(l) => if l == 0 {
                        return Err(ConstructorError::PrematureEOF(String::from("Reading Station count")));
                     },
            Err(e) => return Err(ConstructorError::FileIOError(String::from("Reading Station count"),e.to_string())),
        };
        let mut count: usize = match line_buffer.parse::<usize>() {
            Ok(c) => c,
            Err(e) => return Err(ConstructorError::StationCountParseError(line_buffer,e.to_string())),
        };
        for i in 0..count {
            line_buffer.clear();
            match buf_reader.read_line(&mut line_buffer) {
                Ok(l) => if l == 0 {
                    return Err(ConstructorError::PrematureEOF(String::from("Reading stations")));
                },
                Err(e) => return Err(ConstructorError::FileIOError(String::from("Reading stations"),e.to_string())),
            };
            let station = match Station::from_str(&line_buffer) {
                Ok(s) => s,
                Err(e) => return Err(ConstructorError::StationParseError(line_buffer,e.to_string())),
            };
            this.stations.push(station);
        }
        line_buffer.clear();
        match buf_reader.read_line(&mut line_buffer) {
            Ok(l) => if l == 0 {
                        return Err(ConstructorError::PrematureEOF(String::from("Reading Cab count")));
                     },
            Err(e) => return Err(ConstructorError::FileIOError(String::from("Reading Cab count"),e.to_string())),
        };
        count = match line_buffer.parse::<usize>() {
            Ok(c) => c,
            Err(e) => return Err(ConstructorError::CabCountParseError(line_buffer,e.to_string())),
        };
        for i in 0..count {
            line_buffer.clear();
            match buf_reader.read_line(&mut line_buffer) {
                Ok(l) => if l == 0 {
                    return Err(ConstructorError::PrematureEOF(String::from("Reading cabs")));
                },
                Err(e) => return Err(ConstructorError::FileIOError(String::from("Reading stations"),e.to_string())),
            };
            let cab = match Cab::from_str(&line_buffer) {
                Ok(s) => s,
                Err(e) => return Err(ConstructorError::CabParseError(line_buffer,e.to_string())),
            };
            this.cabs.insert(cab.Name().clone(),cab);
        }
        line_buffer.clear();
        match buf_reader.read_line(&mut line_buffer) {
            Ok(l) => if l == 0 {
                        return Err(ConstructorError::PrematureEOF(String::from("Reading Train count")));
                     },
            Err(e) => return Err(ConstructorError::FileIOError(String::from("Reading Train count"),e.to_string())),
        };
        count = match line_buffer.parse::<usize>() {
            Ok(c) => c,
            Err(e) => return Err(ConstructorError::TrainCountParseError(line_buffer,e.to_string())),
        };
        for i in 0..count {
            line_buffer.clear();
            match buf_reader.read_line(&mut line_buffer) {
                Ok(l) => if l == 0 {
                    return Err(ConstructorError::PrematureEOF(String::from("Reading cabs")));
                },
                Err(e) => return Err(ConstructorError::FileIOError(String::from("Reading stations"),e.to_string())),
            };
            let train = match Train::Read(&line_buffer,&this.cabs) {
                Ok(t) => t,
                Err(e) => return Err(ConstructorError::TrainParseError(line_buffer,e.to_string())),
            };
            this.trains.insert(train.Number(),train);
        }
        line_buffer.clear();
        match buf_reader.read_line(&mut line_buffer) {
            Ok(l) => if l == 0 {
                        return Err(ConstructorError::PrematureEOF(String::from("Reading Note count")));
                     },
            Err(e) => return Err(ConstructorError::FileIOError(String::from("Reading Note count"),e.to_string())),
        };
        count = match line_buffer.parse::<usize>() {
            Ok(c) => c,
            Err(e) => return Err(ConstructorError::NoteCountParseError(line_buffer,e.to_string())),
        };
        for i in 0..count {
            let note = match Self::ReadNote(&mut buf_reader) {
                Err(e) => return Err(ConstructorError::FileIOError(String::from("Reading Note"),e.to_string())),
                Ok(None) => return Err(ConstructorError::PrematureEOF(String::from("Reading Note"))),
                Ok(Some(n)) => n,
            };
            this.notes.push(note);
        }
        match buf_reader.read_line(&mut line_buffer) {
            Ok(l) => if l == 0 {
                        return Err(ConstructorError::PrematureEOF(String::from("Reading Options count")));
                     },
            Err(e) => return Err(ConstructorError::FileIOError(String::from("Reading Options count"),e.to_string())),
        };
        count = match line_buffer.parse::<usize>() {
            Ok(c) => c,
            Err(e) => return Err(ConstructorError::OptionsCountParseError(line_buffer,e.to_string())),
        };
        for i in 0..count {
            let optkey = match Self::ReadNote(&mut buf_reader) {
                Err(e) => return Err(ConstructorError::FileIOError(String::from("Reading Options key"),e.to_string())),
                Ok(None) => return Err(ConstructorError::PrematureEOF(String::from("Reading Options key"))),
                Ok(Some(n)) => n,
            };
            let optval = match Self::ReadNote(&mut buf_reader) {
                Err(e) => return Err(ConstructorError::FileIOError(String::from("Reading Options value"),e.to_string())),
                Ok(None) => return Err(ConstructorError::PrematureEOF(String::from("Reading Options value"))),
                Ok(Some(n)) => n,
            };
            this.print_options.insert(optkey,optval);
        }
        Ok(this)
    }
    /** @brief The constructor that creates a new, empty time table system from
      * stratch, given a set of esentual parameters.
      *
      *  @param name The name of the time table system.
      *  @param timescale Number of time units per 24 hours.  There are
      *    1440 minutes in 24 hours.
      *  @param timeinterval The tick frequency in time units.
      */
    pub fn new(name: String,timescale: u32,timeinterval: u32) -> Self {
        Self {name: name, filepath: PathBuf::new(), timescale: timescale, 
              timeinterval: timeinterval, stations: Vec::new(), 
              cabs: HashMap::new(), trains: HashMap::new(), notes: Vec::new(), 
              print_options: HashMap::new(), table_of_contents_p: true, 
              direction_name: String::new() }
    }
    /** @brief Add a new station to the system.
      *
      * Creates a new Station class
      * instance and adds it to the station vector.  Stations must be
      * added in order of their scale mile location.  If the new station
      * is out of order, -1 is returned and the station is not added!
      *
      *  @param name The name of the station.
      *  @param smile The scale mile along the route where the station is
      *    located.
      */
    pub fn AddStation(&mut self,name: String,smile: f64) -> Option<usize> {
        match self.FindStationByName(name.clone()) {
            None => match self.stations.last() {
                        None => {
                            self.stations.push(Station::new(name.clone(),smile));
                            Some(self.stations.len()-1)
                        },
                        Some(l) =>
                            if l.SMile() < smile {
                                self.stations.push(Station::new(name.clone(),smile));
                                Some(self.stations.len()-1)
                            } else {
                                None
                            },
                    },
            Some(index) => Some(index),
        }                        
    }
    /** @brief Find a station by name.
      *
      * Returns the index of the station or -1 if
      * the station cannot be found.
      * @param name The name of the station.
     */
    pub fn FindStationByName(&self,name: String) -> Option<usize> {
        for i in 0..self.stations.len() {
            if self.stations[i].Name() == name {
                return Some(i);
            }
        }
        None
    }
    /** @brief Number of stations. 
      * 
      * Returns the number of stations in the system.
      */
    pub fn NumberOfStations(&self) -> usize {self.stations.len()}
    /** @brief Return Ith station object.
      *
      * Returns the NULL pointer if the index
      * is out of range.
      *   @param i The index of the station.
      */
    pub fn IthStation(&self, i: usize) -> Option<&Station> {
        self.stations.get(i)
    }
    pub fn IthStationMut(&mut self, i: usize) -> Option<&mut Station> {
        self.stations.get_mut(i)
    }
    /** @brief Return the Ith station name. 
      *
      * Returns the NULL pointer if the index
      * is out of range.
      *    @param i The index of the station.
      */
    pub fn StationName(&self,i: usize) -> Option<String> {
        match self.stations.get(i) {
            None => None,
            Some(s) => Some(s.Name())
        }
    }
    /** @brief Return the Ith station's scale mile location.
      *
      * Returns -1.0 if
      * the index is out of range.
      *    @param i The index of the station.
      */
    pub fn SMile(&self,i: usize) -> Option<f64> {
        match self.stations.get(i) {
            None => None, 
            Some(s) => Some(s.SMile()),
        }
    }
    /** @brief The total length of the route in scale miles.  
      * 
      * This is just the
      * scale mile location of the last station along the route.
      */
    pub fn TotalLength(&self) -> f64 {
        if self.stations.len() == 0 {
            0.0
        } else {
            self.stations.last().unwrap().SMile()
        }
    }
    /** @brief The duplicate station index for a given station.  
      * 
      * Only meaningful
      * for out and back type layouts or layouts that have shared trackage.
      * This would be stations along shared trackage.  Returns -1 if
      * the index is out of range or if there is not a duplicate station for
      * the ith station.
      *    @param i The index of the station.
      */
    pub fn DuplicateStationIndex(&self,i: usize) -> Option<usize> {
        match self.stations.get(i) {
            None => None,
            Some(s) => s.DuplicateStationIndex(),
        }
    }
    /** @brief Set the duplicate station index for a given station.
      *
      * Only 
      * meaningful for out and back type layouts or layouts that have 
      * shared trackage. This would be stations along shared trackage.
      * setting the duplicate station index indicates there is no
      * duplicate station
      *    @param i The index of the station to be updated.
      *    @param dup The other station index sharing this station 
      *      location.
      */
    pub fn SetDuplicateStationIndex(&mut self,i: usize,dup: usize) {
        match self.stations.get_mut(i) {
            None => (),
            Some(s) => s.SetDuplicateStationIndex(Some(dup)),
        };
    }
    /** @brief Add a storage track to a station.  
      *
      * Sometimes stations, especially
      * major terminals, have extra tracks for storing terminating and
      * originating trains.  Returns the NULL pointer if the index is
      * out of range.  Otherwise returns the pointer to the new 
      * StorageTrack object.
      *    @param i The index of the station to be updated.
      *    @param name The name for the new storage track.
      */
    pub fn AddStorageTrack(&mut self,i: usize,name: &String) -> Option<&mut StorageTrack> {
        match self.stations.get_mut(i) {
            None => None,
            Some(s) => s.AddStorageTrack(name)
        }
    }
    /** @brief Find a storage track at a station.
      *
      * Sometimes stations, especially
      * major terminals, have extra tracks for storing terminating and
      * originating trains. Returns the NULL pointer if the index is
      * out of range or if there is no storage track with the specified
      * name.  Otherwise the StorageTrack object pointer is returned.
      *    @param i The index of the station to be updated.
      *    @param name The name of the storage track.
      */
    pub fn FindStorageTrack(&self,i: usize,name: &String) -> Option<&StorageTrack> {
        match self.stations.get(i) {
            None => None,
            Some(s) => s.FindStorageTrack(name),
        }
    }
    pub fn FindStorageTrack_mut(&mut self,i: usize,name: &String) -> Option<&mut StorageTrack> {
        match self.stations.get_mut(i) {
            None => None,
            Some(s) => s.FindStorageTrack_mut(name),
        }
    }
    /** @brief Add a new cab to the system.
      *
      * With DC systems this would be an
      * actual cab.  With DCC systems, this can be used to define a
      * logical operator for the train.  The color is used for visual
      * distintion.  A pointer to the new cab object is returned.
      *   @param name The name of the cab.
      *   @param color The color of the cab.
      */
    pub fn AddCab(&mut self,name: String,color: String) -> &Cab {
        self.cabs.entry(name.clone()).or_insert(Cab::new(name.clone(),color))
    }
    /** @brief Find a cab (by name).  
      *
      * Returns the pointer to the named cab or NULL
      * if the cab was not found.
      *   @param name The cab name to look for.
      */
    pub fn FindCab(&self,name: &String) -> Option<&Cab> {
        self.cabs.get(name)
    }
    /** @brief The nymber of cabs.
      */
    pub fn NumberOfCabs(&self) -> usize {self.cabs.len()}
    /** @brief Add a train to the system, short version.  
      *
      * Creates a new Train
      * opject and adds it to the train map.  The short version assumes
      * that the train does not layover at any of the stops.  Layover
      * times can be added later.  Returns a pointer to the new Train
      * object.
      *   @param name The name of the train.
      *   @param number The number (or symbol) of the train.
      *   @param speed The trains maximum speed.
      *   @param classnumber The class (inverse priority) of the train.
      *   @param departure The train's departure time.
      *   @param start The train's origin station index.  Defaults to the
      *    first station.
      *   @param end The train's destination station index. Defaults to
      *    the last station.
      */
    pub fn AddTrain(&mut self,name: String,number: String,speed: u32, 
                    classnumber: u32, departure: u32, start: usize, 
                    iend: isize) -> Result<&Train,AddTrainError> {
        let mut end: usize;
        if iend < 0 {
            end = self.NumberOfStations()-1;
            if start == end {end = 0;}
        } else {
            end = iend as usize;
        }
        if start == end {
            Err(AddTrainError::EmptyStopList)
        } else {
            let startsmile = self.stations[start].SMile();
            let newTrain = Train::new(name,number.clone(),speed,classnumber,departure,startsmile,start,end);
            Ok(self.trains.entry(number).or_insert(newTrain))
        }
    }
    /** @brief Add a train to the system, long version (includes storage track
      * checking).  
      *
      * This version includes layover times, cabnames, and
      * storage track assignments.  Returns a pointer to the new Train
      * object or the NULL pointer if there was an error, in which case
      * the error message will be stored in the pointer provided.
      *  @param name The name of the train.
      *  @param number The number (or symbol) of the train.
      *  @param speed The trains maximum speed.
      *  @param classnumber The class (inverse priority) of the train.
      *  @param departure The train's departure time.
      *  @param start The train's origin station index.
      *  @param end The train's destination station index.
      *  @param layoverVector The train's layover vector.
      *  @param cabnameVector The train's departure cab name vector.
      *  @param storageTrackVector The train's storage track name vector.
      *  @param outmessage Pointer to a pointer to receive any error 
      *    messages for any errors that might occur.
      */
    pub fn AddTrainLongVersion(&mut self,name: String,number: String,
                               speed: u32,classnumber: u32,departure: u32,
                               start: usize,end: usize, 
                               layoverVector: &DoubleVector,
                               cabnameVector: &StringList,
                               storageTrackVector: &StringList) 
                -> Result<&Train,AddTrainError> {
        match self.FindTrainByNumber(&number) {
            /*----------------------------------------------------------
             * Duplicate train check.
             *----------------------------------------------------------*/
            Some(t) => return Err(AddTrainError::DuplicateTrain),
            None => {
                /*----------------------------------------------------------
                 * Empty stop list  check.
                 *----------------------------------------------------------*/
                if start == end {
                    return Err(AddTrainError::EmptyStopList)
                } else {
                    /*-----------------------------------------------------------
                     * Storage track occupancy check.  Traverse the train's 
                     * travel, making sure the storage tracks it will use are
                     * available.
                     *-----------------------------------------------------------*/

                    let incr: bool;
                    let nstops: usize;
                    if start < end {
                        incr = true;
                        nstops = (end-start)+1;
                    } else {
                        incr = false;
                        nstops = (start-end)+1;
                    }
                    /*----------------------------------------------------------
                     * Range check: make sure the layover, cabname, and storage track
                     * vectors are the right size
                     *----------------------------------------------------------*/
                    if layoverVector.len() != nstops ||
                       cabnameVector.len() != nstops ||
                       storageTrackVector.len() != nstops {
                        return Err(AddTrainError::RangeError(
                            if layoverVector.len() != nstops {
                                String::from("layover vector")
                            } else if cabnameVector.len() != nstops {
                                String::from("cabname vector")
                            } else if storageTrackVector.len() != nstops {
                                String::from("storage track vector")
                            } else {
                                String::new()
                            }));
                    }
                    // for (istop = start,i=0; i < nstops; istop += inxt,i++) {
                    let mut istop: usize = start;
                    let mut layoverIter = layoverVector.iter();
                    //let mut cabnameIter = cabnameVector.iter();
                    let mut storageTrackIter = storageTrackVector.iter();
                    let mut oldDepart: f64 = -1.0;
                    let mut oldSmile:  f64 = -1.0;
                    for i in 0..nstops {
                        let layover = *layoverIter.next().unwrap();
                        let station = match self.IthStation(istop) {
                            Some(s) => s,
                            None => 
                                return Err(AddTrainError::BadStationNumber(istop)),
                        };
                        let smile = station.SMile();
                        let arrival: f64;
                        if oldDepart >= 0.0 {
                            arrival = oldDepart + 
                                ((smile - oldSmile).abs() * (speed as f64 / 60.0));
                        } else {
                            arrival = departure as f64;
                        }
                        let depart = arrival + layover;
                        oldDepart = depart;
                        oldSmile  = smile; 
                        let storageTrackName: &String = storageTrackIter.next().unwrap();
                        if storageTrackName.len()> 0 {
                            let storage = station.FindStorageTrack(storageTrackName);
                            let rStation = match station.DuplicateStationIndex() {
                                None => None,
                                Some(rStationIndex) =>
                                    self.IthStation(rStationIndex),
                            };
                            let rStorage = match rStation {
                                None => None,
                                Some(rs) => rs.FindStorageTrack(storageTrackName),
                            };
                            if istop == start {
                                match storage {
                                    None => (),
                                    Some(s) => match s.IncludesTime(departure as f64) {
                                        None => (),
                                        Some(o) => {
                                            let tn2 = o.TrainNum2();
                                            if tn2.len() > 0 {
                                                return Err(AddTrainError::DuplicateStorageIsAt(storageTrackName.to_string(),tn2,station.Name()));
                                            }
                                        }
                                    }
                                };
                                match rStorage {
                                    None => (),
                                    Some(s) => match s.IncludesTime(departure as f64) {
                                        None => (),
                                        Some(o) => {
                                            let tn2 = o.TrainNum2();
                                            if tn2.len() > 0 {
                                                return Err(AddTrainError::DuplicateStorageIsAt(storageTrackName.to_string(),tn2,rStation.unwrap().Name()));
                                            }
                                        },
                                    },
                                };
                            } else if istop == end {
                                match storage {
                                    None => (),
                                    Some(s) => match s.IncludesTime(arrival) {
                                        None => (),
                                        Some(o) => {
                                            let tn = o.TrainNum();
                                            if tn.len() > 0 {
                                                return Err(AddTrainError::DuplicateStorageIsAt(storageTrackName.to_string(),tn,station.Name()));
                                            }
                                        }
                                    }
                                };
                                match rStorage {
                                    None => (),
                                    Some(s) => match s.IncludesTime(arrival) {
                                        None => (),
                                        Some(o) => {
                                            let tn = o.TrainNum();
                                            if tn.len() > 0 {
                                                return Err(AddTrainError::DuplicateStorageIsAt(storageTrackName.to_string(),tn,rStation.unwrap().Name()));
                                            }
                                        }
                                    }
                                };
                            } else if layover > 0.0 && storage.is_some() {
                                let o1 = storage.unwrap().IncludesTime(arrival);
                                let o2 = storage.unwrap().IncludesTime(depart);
                                if o1.is_some() || o2.is_some() {
                                    return Err(AddTrainError::DuplicateStorageAt(storageTrackName.to_string(),station.Name()));
                                }
                                match rStorage {
                                    None => (),
                                    Some(rs) => {
                                        let o1 = rs.IncludesTime(arrival);
                                        let o2 = rs.IncludesTime(depart);
                                        if o1.is_some() || o2.is_some() {
                                            return Err(AddTrainError::DuplicateStorageAt(storageTrackName.to_string(),rStation.unwrap().Name()));
                                        }
                                    },
                                };
                            }
                        }
                        if incr {
                            istop += 1;
                        } else {
                            istop -= 1;
                        }
                    }                    
                    /*-------------------------------------------------------------
                     * Create and store the train.
                     *-------------------------------------------------------------*/
                    let startsmile = self.stations[start].SMile();
                    let mut newTrain = Train::new(name,number.clone(),speed,classnumber,departure,startsmile,start,end);
                    /*-------------------------------------------------------------
                     * Process the layovers, cabnames, and storage tracks.
                     *-------------------------------------------------------------*/
                    // for (istop = start,i=0; i < nstops; istop += inxt,i++) {
                    let mut istop: usize = start;
                    let mut layoverIter = layoverVector.iter();
                    let mut cabnameIter = cabnameVector.iter();
                    let mut storageTrackIter = storageTrackVector.iter();
                    let mut oldDepart: f64 = -1.0;
                    let mut oldSmile:  f64 = -1.0;
                    for i in 0..nstops {
                        let layover = *layoverIter.next().unwrap();
                        newTrain.UpdateStopLayover(istop,layover);
                        let cabName = cabnameIter.next().unwrap();
                        if cabName.len() > 0 {
                            let cab = self.FindCab(cabName).unwrap();
                            newTrain.UpdateStopCab(istop,Some(cab));
                        } else {
                            newTrain.UpdateStopCab(istop,None);
                        }
                        let storageTrackName = storageTrackIter.next().unwrap();
                        let station = self.IthStation(istop).unwrap();
                        let smile = station.SMile();
                        let arrival: f64;
                        if oldDepart >= 0.0 {
                            arrival = oldDepart + ((smile - oldSmile).abs() * (speed as f64 / 60.0));
                        } else {
                            arrival = departure as f64;
                        }
                        let depart = arrival + layover;
                        oldDepart = depart;
                        oldSmile  = smile;
                        if storageTrackName.len() > 0 {
                            Self::UpdateTrainStorageAtStop(istop,start,end,
                                                           &mut self.stations,
                                                           storageTrackName,
                                                           arrival,depart,
                                                           layover,
                                                           self.timescale as f64,
                                                           &mut newTrain);
                        }
                        if incr {
                            istop += 1;
                        } else {
                            istop -= 1;
                        }
                    }
                    Ok(self.trains.entry(number).or_insert(newTrain))
                }
            },
        }
    }
    fn UpdateTrainStorageAtStop(istop: usize, start: usize, end: usize,
                                stations: &mut StationVector,
                                storageTrackName: &String,
                                arrival: f64, depart: f64,
                                layover: f64,infi: f64,
                                newTrain: &mut Train) {
        let (station, rStation) = match stations[istop].DuplicateStationIndex() {
            None => (&mut stations[istop], None),
            Some(rsI) => stations
                .get_disjoint_mut([istop, rsI])
                .map(|[s, r]| (s, Some(r)))
                .unwrap(),
        };
        let storage  = station.FindStorageTrack_mut(storageTrackName);
        let rStorage = match rStation {
            None => None,
            Some(rs) => rs.FindStorageTrack_mut(storageTrackName),
        };
        if istop == start {
            match storage {
                None => (),
                Some(storage) => {
                    newTrain.SetOriginStorageTrack(storageTrackName.to_string());
                    let occupied = storage.IncludesTime(arrival);
                    match occupied {
                        None => {
                            storage.StoreTrain(String::new(),0.0,arrival,newTrain.Number());
                        },
                        Some(occupied) => {
                            let from = occupied.From();
                            let to   = occupied.Until();
                            storage.UpdateStoredTrain2(from,to,newTrain.Number());
                            storage.UpdateStoredTrainDeparture(from,to,arrival);
                        },
                    }
                },
            }
            match rStorage {
                None => (),
                Some(rStorage) => {
                    let occupied = rStorage.IncludesTime(arrival);
                    match occupied {
                        None => {
                            rStorage.StoreTrain(String::new(),0.0,arrival,
                                                newTrain.Number());
                        },
                        Some(occupied) => {
                            let from = occupied.From();
                            let to   = occupied.Until();
                            rStorage.UpdateStoredTrain2(from,to,newTrain.Number());
                            rStorage.UpdateStoredTrainDeparture(from,to,arrival);
                        }
                    }
                }
            }
        } else if istop == end {
            match storage {
                None => (),
                Some(storage) => {
                    newTrain.SetDestinationStorageTrack(storageTrackName.to_string());
                    let occupied = storage.IncludesTime(arrival);
                    match occupied {
                        None => {
                            storage.StoreTrain(newTrain.Number(),arrival,infi,String::new());
                        },
                        Some(occupied) => {
                            let from = occupied.From();
                            let to   = occupied.Until();
                            storage.UpdateStoredTrain(from,to,newTrain.Number());
                            storage.UpdateStoredTrainArrival(from,to,arrival);
                        },
                    };
                    match rStorage {
                        None => (),
                        Some(rStorage) => {
                            let occupied = rStorage.IncludesTime(arrival);
                            match occupied {
                                None => {
                                    rStorage.StoreTrain(newTrain.Number(),arrival,infi,String::new());
                                },
                                Some(occupied) => {
                                    let from = occupied.From();
                                    let to   = occupied.Until();
                                    rStorage.UpdateStoredTrain(from,to,newTrain.Number());
                                    rStorage.UpdateStoredTrainArrival(from,to,arrival);
                                },
                            }
                        }
                    };
                },
            };
        } else if layover > 0.0 && storage.is_some() {
            newTrain.SetTransitStorageTrack(istop,storageTrackName.to_string());
            let storage = storage.unwrap();
            storage.StoreTrain(newTrain.Number(),arrival,depart,newTrain.Number());
            match rStorage {
                None => (),
                Some(rStorage) => {
                    rStorage.StoreTrain(newTrain.Number(),arrival,depart,newTrain.Number());
                },
            };
        }
    }
    /**
      * @brief Delete a train.  
      *
      * Returns true if the train was successfully deleted
      * and false if not.  If the train was not deleted, an error message
      * will be provided in the pointer provided.
      *  @param number The train number or symbol.
      *  @param outmessage Pointer to a pointer to receive any error messages
      *      for any errors that might occur.
      */
    pub fn DeleteTrain(&mut self,number: String) -> Result<(),DeleteTrainError> {
        match self.trains.get_mut(&number) {
            None => Err(DeleteTrainError::NoSuchTrain(number.clone())),
            Some(oldTrain) => {
                /*-----------------------------------------------------------
                 * Storage track occupancy cleanup.
                 *-----------------------------------------------------------*/
                let departure = oldTrain.Departure();
                let speed     = oldTrain.Speed();
                let mut oldDepart: f64 = -1.0;
                let mut oldSmile: f64 = -1.0;
                for istop in 0..oldTrain.NumberOfStops() {
                    let stop = oldTrain.StopI(istop).unwrap();
                    let istation = stop.StationIndex();
                    let (station, mut rStation) = match self.stations[istation].DuplicateStationIndex() {
                        None => (&mut self.stations[istation], None),
                        Some(rsI) => self.stations
                            .get_disjoint_mut([istation, rsI])
                            .map(|[s, r]| (s, Some(r)))
                            .unwrap(),
                    };
                    let layover = stop.Layover();
                    let smile   = station.SMile();
                    let arrival: f64;
                    if oldDepart < 0.0 {
                        arrival = departure as f64;
                    } else {
                        arrival = oldDepart + ((smile - oldSmile) * (speed as f64 / 60.0));
                    }
                    let depart: f64 = arrival + layover;
                    oldDepart = depart;
                    oldSmile  = smile;
                    let storageTrackName = stop.StorageTrackName();
                    if storageTrackName.len() == 0 {continue;}
                    let storage = station.FindStorageTrack_mut(&storageTrackName);
                    let rStorage = match rStation {
                        None => None,
                        Some(ref mut rs) => rs.FindStorageTrack_mut(&storageTrackName),
                    };
                    match stop.Flag() {
                        StopFlagType::Origin => {
                            match storage {
                                None => {
                                  return Err(DeleteTrainError::InternalMissingOcc(station.Name()));
                                },
                                Some(storage) => { 
                                    let occupied = storage.IncludesTime(departure as f64);
                                    match occupied {
                                        None => {
                                            return Err(DeleteTrainError::InternalMissingOcc(station.Name()));
                                        },
                                        Some(occupied) => {
                                            if occupied.From() == occupied.Until() &&
                                               occupied.From() == departure as f64 &&
                                               occupied.TrainNum() == number &&
                                               occupied.TrainNum2() == number {
                                                storage.RemovedStoredTrain(occupied.From(),occupied.Until());
                                            } else {
                                                let from = occupied.From();
                                                let to   = occupied.Until();
                                                storage.UpdateStoredTrain2(from,to,occupied.TrainNum());
                                                storage.UpdateStoredTrainDeparture(from,to,from);
                                             }
                                        },
                                    }
                                }
                            };
                            match rStorage {
                                None => (),
                                Some(rStorage) => {
                                    let occupied = rStorage.IncludesTime(departure as f64);
                                    match occupied {
                                        None => {
                                            return Err(DeleteTrainError::InternalMissingOcc(rStation.unwrap().Name()));
                                        },
                                        Some(occupied) => {
                                            if occupied.From() == occupied.Until() &&
                                               occupied.From() == departure as f64 &&
                                               occupied.TrainNum() == number &&
                                               occupied.TrainNum2() == number {
                                                rStorage.RemovedStoredTrain(occupied.From(),occupied.Until());
                                            } else {
                                                let from = occupied.From();
                                                let to   = occupied.Until();
                                                rStorage.UpdateStoredTrain2(from,to,occupied.TrainNum());
                                                rStorage.UpdateStoredTrainDeparture(from,to,from);
                                             }
                                        },
                                    };
                                },
                            };
                        },
                        StopFlagType::Terminate => {
                            match storage {
                                None => {
                                    return Err(DeleteTrainError::InternalMissingOcc(station.Name()));
                                },
                                Some(storage) => {
                                    let occupied = storage.IncludesTime(arrival);
                                    match occupied {
                                        None => {
                                            return Err(DeleteTrainError::InternalMissingOcc(station.Name()));
                                        },
                                        Some(occupied) => {
                                            if occupied.From() == occupied.Until() &&
                                               occupied.From() == arrival &&
                                               occupied.TrainNum() == number &&
                                               occupied.TrainNum2() == number {
                                                storage.RemovedStoredTrain(occupied.From(),occupied.Until());
                                            } else {
                                                let from = occupied.From();
                                                let to   = occupied.Until();
                                                storage.UpdateStoredTrain(from,to,occupied.TrainNum());
                                                storage.UpdateStoredTrainArrival(from,to,to);
                                             }
                                        },
                                    };
                                },
                            };
                            match rStorage {
                                None => (),
                                Some(rStorage) => {
                                    let occupied = rStorage.IncludesTime(arrival);
                                    match occupied {
                                        None => {
                                            return Err(DeleteTrainError::InternalMissingOcc(rStation.unwrap().Name()));
                                        },
                                        Some(occupied) => {
                                            if occupied.From() == occupied.Until() &&
                                               occupied.From() == arrival &&
                                               occupied.TrainNum() == number &&
                                               occupied.TrainNum2() == number {
                                                rStorage.RemovedStoredTrain(occupied.From(),occupied.Until());
                                            } else {
                                                let from = occupied.From();
                                                let to   = occupied.Until();
                                                rStorage.UpdateStoredTrain(from,to,occupied.TrainNum());
                                                rStorage.UpdateStoredTrainArrival(from,to,to);
                                             }
                                        },
                                    };
                                },
                            };
                        },
                        StopFlagType::Transit => {
                            if layover > 0.0 && storage.is_some() {
                                let storage = storage.unwrap();
                                let o1 = storage.IncludesTime(arrival);
                                let o2 = storage.IncludesTime(depart);
                                if o1 != o2 || o1.is_none() || o2.is_none() {
                                    return Err(DeleteTrainError::InternalMissingOcc(station.Name()));
                                } else {
                                    let o1 = o1.unwrap();
                                    let o2 = o2.unwrap();
                                    storage.RemovedStoredTrain(o1.From(),o1.Until());
                                }
                            }
                            if layover > 0.0 && rStorage.is_some() {
                                let rStorage = rStorage.unwrap();
                                let o1 = rStorage.IncludesTime(arrival);
                                let o2 = rStorage.IncludesTime(depart);
                                if o1 != o2 || o1.is_none() || o2.is_none() {
                                    return Err(DeleteTrainError::InternalMissingOcc(rStation.unwrap().Name()));
                                } else {
                                    let o1 = o1.unwrap();
                                    let o2 = o2.unwrap();
                                    rStorage.RemovedStoredTrain(o1.From(),o1.Until());
                                }
                            }
                        },
                    };
                }
                /*
                 * Remove the train from the map.
                 */
                self.trains.remove(&number);
                /*
                 * Delete the train.
                 */
                /* (no pointers to free in rust...) */
                Ok(())
            }
        }
    }
    /** @brief Find a train by name.
      *
      * Returns the pointer to the named train or
      * NULL if the train was not found.
      *   @param name The train name to look for.
      */
    pub fn FindTrainByName(&self,name: &String) -> Option<&Train> {
        for train in self.trains.values() {
            if *train.Name() == *name {
                return Some(train);
            }
        }
        None
    }
    /** @brief Find a train by number (or symbol). 
      *
      * Returns the pointer to the 
      * train or NULL if the train was not found.
      *   @param number The train number (or symbol) to look for.
      */
    pub fn FindTrainByNumber(&self, number: &String) -> Option<&Train> {
        self.trains.get(number)
    }
    /** @brief Return the number of trains.
      */
    pub fn NumberOfTrains(&self) -> usize {self.trains.len()}
    /** @brief Return the number of notes.
      */
    pub fn NumberOfNotes(&self)  -> usize {self.notes.len()}
    /** @brief Return the ith note (1-based!) as a string. , 
      * Returns the NULL
      * pointer if the index is out of range.
      *   @param i The note index.  The first note is at index 1, not 0!.
      */
    pub fn Note(&self,i: usize) -> Option<String> {
        self.notes.get(i).cloned()
    }
    /** @brief Add a note to the notes vector.
      *   @param newnote The text of the new note.
      */
    pub fn AddNote(&mut self,newnote: String) -> usize {
        self.notes.push(newnote);
        self.notes.len()
    }
    /** @brief Set the ith note (1-based!).  
      *
      * Updates the text of the specificed
      * note.  Returns true if the note was updated or false if the
      * index was out of range.
      *   @param i The note index.  The first note is at index 1, not 0!.
      *   @param note The new text for the note.
      */
    pub fn  SetNote(&mut self,i: usize,note: String) -> bool {
        if i == 0 || i > self.notes.len() {
            false
        } else {
            self.notes[i-1] = note;
            true
        }
    }
    /** @brief Fetch a print option.  
      *
      * Returns the value of a specified print
      * option or the empty string if the print option was not found.
      *   @param key The name of the print option.
      */
    pub fn GetPrintOption(&self,key: String) -> Option<String>
    {
        self.print_options.get(&key).cloned()
    }
    /** @brief Set a print option.  
      *
      * Sets the value of a print option.  Creates a
      * new hash table element if the specified print option does not
      * already exist.
      *  @param key The name of the print option to be set.
      *  @param value The value to set the print option to.
      */
    pub fn SetPrintOption(&mut self,key: String,value: String) {
        self.print_options.insert(key,value);
    }
    /** @brief Write out a Time Table System to a new file.  
      *
      * The current contents
      * of the time table is written to a new time table file. Returns 
      * true if successful and false if not.
      *  @param filename The name of the file to write (if empty, use
      *    existing name, if available).
      *  @param setfilename Change the filename if true.
      *  @param outmessage Pointer to a pointer to receive any error
      *    messages for any errors that might occur.
      */
    pub fn WriteNewTimeTableFile(&mut self,filename: String, 
                                 setfilename: bool) -> std::io::Result<bool> {
        Ok(false)
    }
    /** @brief Return time scale.
      */
    pub fn TimeScale(&self) -> u32 {self.timescale}
    /** @brief Return time interval.
      */
    pub fn TimeInterval(&self) -> u32 {self.timeinterval}
    /** @brief Return the name of the system.
      */
    pub fn Name(&self) -> String {self.name.clone()}
    /** @brief Return file pathname.
      */
    pub fn Filename(&self) -> String {self.filepath.display().to_string()}
    /** @brief Create a LaTeX file for generating a (hard copy) Employee
      * Timetable. 
      *
      * This method create a LaTeX source file from
      * the information in the time table structure.  It access various
      * print options to control how the LaTeX file is generated.
      *  @param filename The name of the  LaTeX file to create.
      *  @param outmessage Pointer to a pointer to receive any error
      *    messages for any errors that might occur.
      */
    pub fn CreateLaTeXTimetable(&self,filename: String) -> std::io::Result<bool> {
        Ok(false)
    }
    /* ToDo: interators, private methods */
    fn ReadNote(inp: &mut BufReader<File>) -> std::io::Result<Option<String>> {
        let mut buffer: [u8; 1] = [0; 1];
        loop {
            let status = inp.read(&mut buffer)?;
            if status == 0 {return Ok(None);}
            let ch = buffer[0] as char;
            if ch == '"' {break;}
        }
        let mut EOF: bool = false;
        let mut result: String = String::new();
        loop {
            let mut status = inp.read(&mut buffer)?;
            if status == 0 {EOF = true;break;}
            let mut ch = buffer[0] as char;
            if ch == '"' {break;}
            if ch == '\\' {
                status = inp.read(&mut buffer)?;
                if status == 0 {EOF = true;break;}
                ch = buffer[0] as char;
            }
            result += &ch.to_string();
        }
        if EOF {
            if result.len() > 0 {
                Ok(Some(result))
            } else {
                Ok(None)
            }
        } else {
            Ok(Some(result))
        }       
    }
}








#[cfg(test)]
mod tests { 
    use super::*;

    #[test]
    fn StationTimes_new () {
        let temp = StationTimes::new(4.2, 5.1, StopFlagType::Origin);
        assert_eq!(temp,StationTimes{arrival: 4.2, departure: 5.1, flag: StopFlagType::Origin });
    }
    #[test]
    fn StationTimes_Arrival () {
        let temp = StationTimes::new(4.2, 5.1, StopFlagType::Origin);
        assert_eq!(temp.Arrival(),4.2);
    }
    #[test]
    fn StationTimes_Departure () {
        let temp = StationTimes::new(4.2, 5.1, StopFlagType::Origin);
        assert_eq!(temp.Departure(),5.1);
    }
    #[test]
    fn StationTimes_Flag () {
        let temp = StationTimes::new(4.2, 5.1, StopFlagType::Origin);
        assert_eq!(temp.Flag(),StopFlagType::Origin);
    }

    #[test]
    fn pub_StringListToString () {
        let mut temp: StringList = LinkedList::new();
        temp.push_back(String::from("Hello"));
        temp.push_back(String::from("World"));
        let output = StringListToString(temp);
        assert_eq!(output,String::from("\"Hello\",\"World\""));
    }
    #[test]
    fn pub_StringListFromString () {
        let temp = match StringListFromString(String::from("\"Hello\",\"World\"")) {
            None => panic!("Conversion failed"),
            Some(l) => l,
        };
        assert_eq!(temp.len(),2);
        assert_eq!(temp.front(),Some(&String::from("Hello")));
        assert_eq!(temp.back(),Some(&String::from("World")));
    }
}
