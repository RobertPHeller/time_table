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
//  Last Modified : <251007.2249>
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
        let mut this = Self {name: name, filepath: PathBuf::new(), 
                             timescale: timescale, timeinterval: timeinterval,
                             stations: Vec::new(), cabs: HashMap::new(), 
                             trains: HashMap::new(), notes: Vec::new(), 
                             print_options: HashMap::new(),
                             table_of_contents_p: true, 
                             direction_name: String::new() };
        this
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
