// -!- rust -!- //////////////////////////////////////////////////////////////
//
//  System        : 
//  Module        : 
//  Object Name   : $RCSfile$
//  Revision      : $Revision$
//  Date          : $Date$
//  Author        : $Author$
//  Created By    : Robert Heller
//  Created       : 2025-10-03 16:27:30
//  Last Modified : <251007.1533>
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

//!
//! The train class implements a running train and lists the stations it passes
//! (and possibly stops at).
//!
//! @author Robert Heller \<heller\@deepsoft.com\>
//!

use crate::cab::*;
use std::fmt;
use std::str::FromStr;
use std::collections::HashMap;
use std::slice::{Iter,IterMut};



/** @brief Type of stop.  
  *
  * Origin for originating trains, Terminate for
  * terminating trains, and Transit for trains passing through.
  */
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum StopFlagType {Origin, Terminate, Transit}

/** @brief This class implements a stop.
  *
  * This specifies the station the train goes
  * through, even if it does not actually stop.  A layover of 0 means the train
  * does not stop and this station is a timekeeping check point.
  *
  * @author Robert Heller \<heller\@deepsoft.com\>
  *
  */
#[derive(Debug, PartialEq, Clone)]
pub struct Stop {
    layover: f64,
    stationindex: usize,
    cab: Option<Cab>,
    notes: Vec<usize>,
    flag: StopFlagType,
    storage_track_name: String,
}

impl fmt::Display for Stop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<Stop {} {:5.3} \"{}\" {} {} {}",
            match self.flag {
                StopFlagType::Origin => "Origin",
                StopFlagType::Terminate => "Terminate",
                StopFlagType::Transit => "Transit",
            }, self.layover, self.storage_track_name, self.stationindex,
            match &self.cab {
                None => String::from("None"),
                Some(c) => format!("Some({})",c),
            }, self.notes.len())?;
        for nindx in self.notes.iter() {
            write!(f, " {}",nindx)?;
        }
        write!(f, ">")
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum StopParseError {
    StartSyntaxError,
    FlagMissing,
    LayoverMissing,
    StorageTrackNameMissing,
    StationIndexMissing,
    CabMissing,
    NoteCountMissing,
    NoteIndexMissing,
    ExtraCharacters,
}

impl fmt::Display for StopParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StopParseError::StartSyntaxError =>
                write!(f, "Missing '<Stop '"),
            StopParseError::FlagMissing =>
                write!(f, "Missing flag"),
            StopParseError::LayoverMissing =>
                write!(f, "Missing layover"),
            StopParseError::StorageTrackNameMissing =>
                write!(f, "Missing Storage Track Name"),
            StopParseError::StationIndexMissing =>
                write!(f, "Missing Station index"),
            StopParseError::CabMissing =>
                write!(f, "Missing cab"),
            StopParseError::NoteCountMissing =>
                write!(f, "Missing note count"),
            StopParseError::NoteIndexMissing =>
                write!(f, "Missing note index"),
            StopParseError::ExtraCharacters =>
                write!(f, "Extra trailing characters"),
        }
    }
}

impl FromStr for Stop {
    type Err = StopParseError;
    /// Convert from &str to Self
    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let (result,pos) = Stop::ParseStop(string)?;
        if pos == string.len() {
            Ok(result)
        } else {
            Err(StopParseError::ExtraCharacters)
        }
    }
}

impl Stop {
    /** @brief Constructor: create a new stop.
      *
      *  @param stationindex_ The index of the station.
      *  @param flag_ The type of stop (originating, terminating, or
      *    passing through).
      */
    pub fn new(stationindex_: usize,flag_: StopFlagType) -> Self {
        Self {layover: 0.0, stationindex: stationindex_, 
              cab: None, notes: Vec::new(), flag: flag_, 
              storage_track_name: String::from(""), }
    }
    /** Return layover period.
      */
    pub fn Layover(&self) -> f64 {self.layover}
    /** Update layover period.
      * ## Parameters:
      * - period New layover period.
      */
    pub fn SetLayover(&mut self,period: f64) {self.layover = period;}
    /** @brief Return departure time.  
      *
      * This is just the layover period added to
      * the arrival time.
      * ## Parameters:
      * - arrival The arrival time.
      */
    pub fn Departure(&self, arrival: f64) -> f64 {arrival+self.layover}
    /** Return the station index.
      */
    pub fn StationIndex(&self) -> usize {self.stationindex}
    /** Return the cab.
      */
    pub fn TheCab(&self) -> Option<Cab> {self.cab.clone()}
    /** Update the cab.
      * ## Parameters:
      * - newcab The new cab value.
      */
    pub fn SetCab(&mut self, newcab: Option<Cab>) {self.cab = newcab;}
    /** Return the number of notes.
      */
    pub fn NumberOfNotes(&self) -> usize {self.notes.len()}
    /** Return the ith note.  Returns -1 if the index is out of range.
      * ## Parameters:
      * - i The index of the note.
      */
    pub fn Note(&self,i: usize) -> Option<&usize> {
        self.notes.get(i)
    }
    pub fn NoteIter(&self) -> Iter<'_, usize> {
        self.notes.iter()
    }
    pub fn NoteIter_mut(&mut self) -> IterMut<'_, usize> {
        self.notes.iter_mut()
    }
    /** Add a note.
      * ## Parameters:
      * - note The note number.
      */
    pub fn AddNote(&mut self,note: usize) {
        for v in self.notes.iter() {
            if *v == note {return;}
        }
        self.notes.push(note);
    }
    /** Remove note.
      * ## Parameters:
      * - note The note number to remove.
      */
    pub fn RemoveNote(&mut self,note: usize) {
        for i in 0..self.notes.len() {
            if self.notes[i] == note {
                self.notes.remove(i);
                return;
            }
        }
    }
    /** Return the flag.
      */
    pub fn Flag(&self) -> StopFlagType {self.flag}
    /** Return storage track name.
      */
    pub fn StorageTrackName(&self) -> String {self.storage_track_name.clone()}
    /** Update storage track name.
      * ## Parameters:
      * - name The name of the storage track.
      */
    pub fn SetStorageTrackName(&mut self,name: String) {
        self.storage_track_name = name.clone();
    }
    pub fn ParseStop(string: &str) -> Result<(Self, usize), StopParseError> {
        let mut pos: usize;
        match string.match_indices("<Stop ").next() {
            None => return Err(StopParseError::StartSyntaxError),
            Some((n, m)) => pos = n + m.len(),
        };
        let flag = match string[pos..].match_indices(' ').next() {
            None => return Err(StopParseError::FlagMissing),
            Some((n, m)) => {
                let temp = &string[pos..n+pos];
                pos += n + m.len();
                match temp {
                    "Origin" => StopFlagType::Origin,
                    "Terminate" => StopFlagType::Terminate,
                    "Transit" => StopFlagType::Transit,
                    _ => return Err(StopParseError::FlagMissing)
                }
            },
        };
        let layover = match string[pos..].match_indices(' ').next() {
            None => return Err(StopParseError::LayoverMissing),
            Some((n, m)) => {
                let temp = &string[pos..n+pos];
                pos += n + m.len();
                match temp.parse::<f64>() {
                    Err(p) => return Err(StopParseError::LayoverMissing),
                    Ok(l) => l,
                }
            },
        };
        match string[pos..].match_indices('"').next() {
            None => return Err(StopParseError::StorageTrackNameMissing),
            Some((n, m)) => pos += n + m.len(),
        };
        let stname = match string[pos..].match_indices('"').next() {
            None => return Err(StopParseError::StorageTrackNameMissing),
            Some((n, m)) => {
                let temp = &string[pos..n+pos];
                pos += n + m.len();
                temp
            },
        };
        pos += 1;
        let stationindex = match string[pos..].match_indices(' ').next() {
            None => return Err(StopParseError::StationIndexMissing),
            Some((n, m)) => {
                let temp = &string[pos..n+pos]; 
                pos += n + m.len();
                match temp.parse::<usize>() {
                    Err(p) => return Err(StopParseError::StationIndexMissing),
                    Ok(i) => i,
                }
            },
        };
        let cab: Option<Cab>;
        match string[pos..].match_indices("None").next() {
            None => {
                match string[pos..].match_indices("Some(").next() {
                    None => return Err(StopParseError::CabMissing),
                    Some((n, m)) => {
                        pos += n + m.len();
                        let temp = match Cab::ParseCab(&string[pos..]) {
                                    Err(p) => return Err(StopParseError::CabMissing),
                                    Ok((c,p)) => {
                                        pos += p;
                                        if string[pos..pos+1] != *")" {
                                            return Err(StopParseError::CabMissing);
                                        } else {
                                            pos += 1;
                                        }
                                        c
                                    },
                        };
                        cab = Some(temp);
                    },
                }
            },
            Some((n, m)) => {
                pos += n + m.len();
                cab = None;
            },
        };
        pos += 1;
        let count = match string[pos..].match_indices(&[' ','>']).next() {
            None => return Err(StopParseError::NoteCountMissing),
            Some((n, m)) => {
                let temp = &string[pos..pos+n];
                pos += n + m.len();
                match temp.parse::<usize>() {
                    Err(p) => return Err(StopParseError::NoteCountMissing),
                    Ok(i) => i,
                }
            }
        };
        let mut result = Stop::new(stationindex,flag);
        result.SetLayover(layover);
        result.SetCab(cab);
        result.SetStorageTrackName(String::from(stname));
        for i in 0..count {
            let note =  match string[pos..].match_indices(&[' ','>']).next() {
                None => return Err(StopParseError::NoteIndexMissing),
                Some((n, m)) => {
                    let temp = &string[pos..pos+n];
                    pos += n + m.len();
                    match temp.parse::<usize>() {
                        Err(p) => return Err(StopParseError::NoteIndexMissing),
                        Ok(i) => i,
                    }
                }
            };
            result.AddNote(note);
        }
        Ok((result,pos))        
    }

}
/** A vector of stops.
  */
pub type StopVector = Vec<Stop>;

/** @brief This class implements a train.
  *
  * A train travels down the track passing or
  * stoping at stations along the way.
  *
  * @author Robert Heller \<heller\@deepsoft.com\>
  *
  */
#[derive(Debug, PartialEq, Clone)]
pub struct Train {
    name: String,
    number: String,
    speed: u32,
    classnumber: u32,
    notes: Vec<usize>,
    departure: u32,
    stops: StopVector,
    startsmile: f64,
}

impl fmt::Display for Train {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<Train \"{}\" \"{}\" {} {} {} {:5.3} {} ",
            self.name,self.number,self.speed,self.classnumber,self.departure,
            self.startsmile,self.notes.len())?;
        for n in self.notes.iter() {
            write!(f, "{} ",n)?;
        }
        write!(f, "{}", self.stops.len())?;
        for s in self.stops.iter() {
            write!(f, " {}", s)?;
        }
        write!(f, ">")
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TrainParseError {
    StartSyntaxError,
    MissingName,
    MissingNumber,
    MissingSpeed,
    MisingClassNumber,
    MissingDeparture,
    MissingStartSMile,
    MissingNoteCount,
    MissingNoteIndex,
    MissingStopCount,
    MissingStop,
    MissingBracket,
    ExtraCharacters,
}

impl fmt::Display for TrainParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TrainParseError::StartSyntaxError =>
                write!(f, "Missing '<Train '"),
            TrainParseError::MissingName =>
                write!(f, "Missing name"),
            TrainParseError::MissingNumber =>
                write!(f, "Missing number"),
            TrainParseError::MissingSpeed =>
                write!(f, "Missing speed"),
            TrainParseError::MisingClassNumber =>
                write!(f, "Missing class number"),
            TrainParseError::MissingDeparture =>
                write!(f, "Missing departure"),
            TrainParseError::MissingStartSMile =>
                write!(f, "Missing Start Scale Mile"),
            TrainParseError::MissingNoteCount =>
                write!(f, "Missing note count"),
            TrainParseError::MissingNoteIndex =>
                write!(f, "Missing Note index"),
            TrainParseError::MissingStopCount =>
                write!(f, "Missing stop count"),
            TrainParseError::MissingStop =>
                write!(f, "Missing stop"),
            TrainParseError::MissingBracket =>
                write!(f, "Missing close bracket"),
            TrainParseError::ExtraCharacters =>
                write!(f, "Extra characters"),
        }
    }
}


impl FromStr for Train {
    type Err = TrainParseError;
    /// Convert from &str to Self 
    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let (result,pos) = Train::ParseTrain(string)?;
        if pos == string.len() {
            Ok(result)
        } else {
            Err(TrainParseError::ExtraCharacters)
        }
    }
}

impl Train {
	/** Create and initialize a train object.
	  *  ## Parameters:
	  * - name The name of the train.
	  * - number The number (or symbol) of the train.
	  * - speed The maximum (scale) speed of the train.
	  * - classnumber The class of the train.
	  * - departure The train's departure time.
          * - startsmile The starting Scale Mile
	  * - start The originating station index.
	  * - end The terminating station index.
	  */
    pub fn new (name: String, number: String, speed: u32, classnumber: u32, 
                departure: u32, startsmile: f64, start: usize, end: usize)
             -> Self {
        let mut this = Self {name: name, number: number, speed: speed, 
                        classnumber: classnumber, notes: Vec::new(),
                        departure: departure, stops: Vec::new(), 
                         startsmile: startsmile};
        if end != start {
            this.stops.push(Stop::new(start,StopFlagType::Origin));
            if end < start {
                for istop in (end+1..start).rev() {
                    //eprintln!("*** Train::new(): (rev) istop is {}", istop);
                    this.stops.push(Stop::new(istop,StopFlagType::Transit));
                }
            } else {
                for istop in start+1..end {
                    //eprintln!("*** Train::new(): (norm) istop is {}", istop);
                    this.stops.push(Stop::new(istop,StopFlagType::Transit));
                }
            }
            this.stops.push(Stop::new(end,StopFlagType::Terminate));
            //for stop in this.stops.iter() {
            //    eprintln!("*** Train::new(): {}",stop);
            //}
        }
        this
    }
    /** Return the name of the train.
      */
    pub fn Name(&self) -> String {self.name.clone()}
    /** Return the number (or symbol) of the train.
      */
    pub fn Number(&self) -> String {self.number.clone()}
    /** Return the departure time.
      */
    pub fn Departure(&self) -> u32 {self.departure}
    /** Update departure time.
      *  @param depart The new departure time.
      */
    pub fn SetDeparture(&mut self,depart: u32) {self.departure = depart;}
    /** Return the train's speed.
      */
    pub fn Speed(&self) -> u32 {self.speed}
    /** Return the class number.
      */
    pub fn ClassNumber(&self) -> u32 {self.classnumber}
    /** Number of notes.
      */
    pub fn NumberOfNotes(&self) -> usize {self.notes.len()}
    /** @brief Return the ith note.  
      *
      * Returns -1 if the index is out of range.
      *  @param i The index of the note.
      */
    pub fn Note(&self, i: usize) -> Option<&usize> {
        self.notes.get(i)
    }
    /** Add a note.
      *   @param note The note number to add.
      */
    pub fn  AddNoteToTrain(&mut self,note: usize) {
        for n in self.notes.iter() {
            if *n == note {return;}
        }
        self.notes.push(note);
    }
    /** Remove a note.
      *   @param note The note number to remove.
      */
    pub fn RemoveNoteFromTrain(&mut self,note: usize) {
        for i in 0..self.notes.len() {
            if self.notes[i] == note {
                self.notes.remove(i);
                return;
            }
        }
    }
    pub fn NoteIter(&self) -> Iter<'_, usize> {
        self.notes.iter()
    }
    pub fn NoteIter_mut(&mut self) -> IterMut<'_, usize> {
        self.notes.iter_mut()
    }
    /** Update stop layover.
      *  @param istop The stop number to update.
      *  @param layover The new layover time.
      */
    pub fn UpdateStopLayover(&mut self,istop: usize,layover: f64) {
        match self.stops.get_mut(istop) {
            None => (),
            Some(stop) => stop.SetLayover(layover),
        };
    }
    /** Update the cab.
      *  @param istop The stop number to update.
      *  @param cab The new cab.
      */
    pub fn UpdateStopCab(&mut self,istop: usize,cab: Option<Cab>) {
        match self.stops.get_mut(istop) {
            None => (),
            Some(stop) => stop.SetCab(cab),
        };
    }
    /** Add a note to a stop.
      *  @param istop The stop number to update.
      *  @param note The note to add.
      */
    pub fn AddNoteToStop(&mut self,istop: usize,note: usize) {
        match self.stops.get_mut(istop) {
            None => (),
            Some(stop) => stop.AddNote(note),
        };
    }
    /** Remove a note from a stop.
      *  @param istop The stop number to update.
      *  @param note The note to remove.
      */
    pub fn RemoveNoteFromStop(&mut self,istop: usize,note: usize) {
        match self.stops.get_mut(istop) {
            None => (),
            Some(stop) => stop.RemoveNote(note),
        };
    }
    /** Set the origin storage track.
      *  @param trackname The originating storage track name.
      */
    pub fn SetOriginStorageTrack(&mut self,trackname: String) {
        match self.stops.get_mut(0) {
            None => (),
            Some(stop) => stop.SetStorageTrackName(trackname.clone()),
        };
    }
    /** Set the destination storage track.
      *  @param trackname The terminating storage track name.
      */
    pub fn SetDestinationStorageTrack(&mut self,trackname: String) {
        let last = self.stops.len()-1;
        match self.stops.get_mut(last) {
            None => (),
            Some(stop) => stop.SetStorageTrackName(trackname.clone()),
        };
    }
    /** Set an intermediate storage track.
      *  @param istop The stop index.
      *  @param trackname The intermediate storage track name.
      */
    pub fn SetTransitStorageTrack(&mut self,istop: usize,trackname: String) {
        match self.stops.get_mut(istop) {
            None => (),
            Some(stop) => stop.SetStorageTrackName(trackname.clone()),
        };
    }
    /** Return the number of stops.
      */
    pub fn NumberOfStops(&self) -> usize {self.stops.len()}
    /** Return the ith stop object.  Returns NULL if the index is out of
      * range.
      *  @param i The index of the stop.
      */
    pub fn StopI(&self,i: usize) -> Option<&Stop>  {
        self.stops.get(i)
    }
    pub fn StopIter(&self) -> Iter<'_, Stop> {
        self.stops.iter()
    }
    pub fn StopIter_mut(&mut self) -> IterMut<'_, Stop> {
        self.stops.iter_mut()
    }
    /** Return the start Scale Mile.
      */
    pub fn StartSMile(&self) -> f64 {self.startsmile}
    pub fn ParseTrain(string: &str) -> Result<(Self, usize), TrainParseError> {
        let mut pos: usize;
        match string.match_indices("<Train \"").next() {
            None => return Err(TrainParseError::StartSyntaxError),
            Some((n, m)) => pos = n + m.len(),
        };
        let name = match string[pos..].match_indices('"').next() {
            None => return Err(TrainParseError::MissingName),
            Some((n, m)) => {
                let temp = &string[pos..n+pos];
                pos += n + m.len();
                temp
            },
        };
        match string[pos..].match_indices('"').next() {
            None => return Err(TrainParseError::MissingNumber),
            Some((n, m)) => pos += n + m.len(),
        };
        let number = match string[pos..].match_indices('"').next() {
            None => return Err(TrainParseError::MissingNumber),
            Some((n, m)) => {
                let temp = &string[pos..n+pos];
                pos += n + m.len();
                temp
            },
        };
        pos += 1;
        let speed = match string[pos..].match_indices(&[' ','\n','\t']).next() {
            None => return Err(TrainParseError::MissingSpeed),
            Some((n, m)) => {
                let temp = &string[pos..pos+n];
                pos += n + m.len();
                match temp.parse::<u32>() {
                    Err(p) => return Err(TrainParseError::MissingSpeed),
                    Ok(s) => s,
                }
            }
        };
        let classnumber = match string[pos..].match_indices(&[' ','\n','\t']).next() {
            None => return Err(TrainParseError::MisingClassNumber),
            Some((n, m)) => {
                let temp = &string[pos..pos+n];
                pos += n + m.len();
                match temp.parse::<u32>() {
                    Err(p) => return Err(TrainParseError::MisingClassNumber),
                    Ok(s) => s,
                }
            }
        };
        let departure = match string[pos..].match_indices(&[' ','\n','\t']).next() {
            None => return Err(TrainParseError::MissingDeparture),
            Some((n, m)) => {
                let temp = &string[pos..pos+n];
                pos += n + m.len();
                match temp.parse::<u32>() {
                    Err(p) => return Err(TrainParseError::MissingDeparture),
                    Ok(s) => s,
                }
            }
        };
        let startsmile = match string[pos..].match_indices(&[' ','\n','\t']).next() {
            None => return Err(TrainParseError::MissingStartSMile),
            Some((n, m)) => {
                let temp = &string[pos..pos+n];
                pos += n + m.len();
                match temp.parse::<f64>() {
                    Err(p) => return Err(TrainParseError::MissingStartSMile),
                    Ok(s) => s,
                }
            }
        };
        let notecount = match string[pos..].match_indices(&[' ','\n','\t']).next() {
            None => return Err(TrainParseError::MissingNoteCount),
            Some((n, m)) => {
                let temp = &string[pos..pos+n];
                pos += n + m.len();
                match temp.parse::<usize>() {
                    Err(p) => return Err(TrainParseError::MissingNoteCount),
                    Ok(s) => s,
                }
            }
        };
        let mut notes: Vec<usize> = Vec::new();
        for i in 0..notecount {
            let note = match string[pos..].match_indices(&[' ','\n','\t']).next() {
                None => return Err(TrainParseError::MissingNoteCount),
                Some((n, m)) => {
                    let temp = &string[pos..pos+n];
                    pos += n + m.len();
                    match temp.parse::<usize>() {
                        Err(p) => return Err(TrainParseError::MissingNoteCount),
                        Ok(s) => s,
                    }
                }
            };
            notes.push(note);
        }
        let stopcount = match string[pos..].match_indices(&[' ','\n','\t']).next() {
            None => return Err(TrainParseError::MissingStopCount),
            Some((n, m)) => {
                let temp = &string[pos..pos+n];
                pos += n + m.len();
                match temp.parse::<usize>() {
                    Err(p) => return Err(TrainParseError::MissingStopCount),
                    Ok(s) => s,
                }
            }
        };
        let mut stops: StopVector = Vec::new();
        for istop in 0..stopcount {
            let stop = match Stop::ParseStop(&string[pos..]) {
                        Err(p) => return Err(TrainParseError::MissingStop),
                        Ok((s,p)) => {
                            pos += p;
                            s
                        },
            };
            stops.push(stop);
        }
        match string[pos..].match_indices('>').next() {
            None => return Err(TrainParseError::MissingBracket),
            Some((n,m)) => pos += n+m.len(),
        };
        Ok((Self {name: String::from(name), number: String::from(number), speed: speed, 
                  classnumber: classnumber, notes: notes,
                  departure: departure, stops: stops,
                  startsmile: startsmile}, pos))

    }
}



/** Train number map, indexed by train number (symbol).
  */
pub type TrainNumberMap = HashMap<String, Train>;












#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn Stop_new () {
        let temp = Stop::new(0,StopFlagType::Origin);
        assert_eq!(temp, Stop {layover: 0.0, stationindex: 0, cab: None, notes: Vec::new(), flag: StopFlagType::Origin, storage_track_name: String::from(""), });
    }
    #[test]
    fn Stop_Layover () {
        let temp = Stop::new(0,StopFlagType::Origin);
        assert_eq!(temp.Layover(),0.0);
    }
    #[test]
    fn Stop_SetLayover () {
        let mut temp = Stop::new(0,StopFlagType::Origin); 
        temp.SetLayover(4.2);
        assert_eq!(temp.Layover(),4.2);
    }
    #[test]
    fn Stop_Departure () {
        let temp = Stop::new(0,StopFlagType::Origin);  
        assert_eq!(temp.Departure(4.2),4.2);
    }
    #[test] 
    fn Stop_TheCab () {
        let temp = Stop::new(0,StopFlagType::Origin); 
        assert_eq!(temp.TheCab().is_none(),true);
    }
    #[test]
    fn Stop_SetCab () {
        let mut temp = Stop::new(0,StopFlagType::Origin);
        temp.SetCab(Some(Cab::new(String::from("Cab A"),String::from("red"))));
        assert_eq!(temp.TheCab(),
                    Some(Cab::new(String::from("Cab A"),String::from("red"))));
    }
    #[test] 
    fn Stop_NumberOfNotes_1 () {
        let temp = Stop::new(0,StopFlagType::Origin);
        assert_eq!(temp.NumberOfNotes(),0);
    }
    #[test]
    fn Stop_AddNote () {
        let mut temp = Stop::new(0,StopFlagType::Origin);
        temp.AddNote(5);
        assert_eq!(temp.Note(0),Some(&5));
    }
    #[test]
    fn Stop_NumberOfNotes_2 () {
        let mut temp = Stop::new(0,StopFlagType::Origin);
        temp.AddNote(5);
        assert_eq!(temp.NumberOfNotes(),1);
    }
    #[test]
    fn Stop_RemoveNote () {
        let mut temp = Stop::new(0,StopFlagType::Origin);
        temp.AddNote(5);
        temp.RemoveNote(5);
        assert_eq!(temp.NumberOfNotes(),0);
    }
    #[test]
    fn Stop_StationIndex () {
        let temp = Stop::new(0,StopFlagType::Origin);
        assert_eq!(temp.StationIndex(),0);
    }
    #[test]
    fn Stop_Flag () {
        let temp = Stop::new(0,StopFlagType::Origin);
        assert_eq!(temp.Flag(),StopFlagType::Origin);
    }
    #[test] 
    fn Stop_StorageTrackName () {
        let temp = Stop::new(0,StopFlagType::Origin);
        assert_eq!(temp.StorageTrackName(),String::from(""));
    }
    #[test] 
    fn Stop_SetStorageTrackName () {
        let mut temp = Stop::new(0,StopFlagType::Origin);
        temp.SetStorageTrackName(String::from("Track1"));
        assert_eq!(temp.StorageTrackName(),String::from("Track1"));
    }
    #[test]
    fn Stop_Display () {
        let mut temp = Stop::new(0,StopFlagType::Origin);
        temp.AddNote(5);
        temp.SetCab(Some(Cab::new(String::from("Cab A"),String::from("red"))));
        temp.SetStorageTrackName(String::from("Track1"));
        temp.SetLayover(4.0);
        let output = format!("{}",temp);
        assert_eq!(output,String::from("<Stop Origin 4.000 \"Track1\" 0 Some(<Cab \"Cab A\" \"red\">) 1 5>"));
    }
    #[test] 
    fn Stop_from_str () {
        let mut temp = Stop::new(0,StopFlagType::Origin);
        temp.AddNote(5);
        temp.SetCab(Some(Cab::new(String::from("Cab A"),String::from("red"))));
        temp.SetStorageTrackName(String::from("Track1"));
        temp.SetLayover(4.0);
        let other = Stop::from_str("<Stop Origin 4.000 \"Track1\" 0 Some(<Cab \"Cab A\" \"red\">) 1 5>")
                .expect("Parse Error");
        assert_eq!(temp,other);
    }

    //#[test]
    fn Train_new_1 () {
        let temp = Train::new(String::from("The Calif. Zepher"),
                              String::from("6"),80,1,18*60,0.0,0,10);
        assert!(true);
    }
    //#[test]
    fn Train_new_2 () {
        let temp = Train::new(String::from("The Calif. Zepher"),
                              String::from("5"),80,1,9*60,100.0,10,0);
        assert!(true);
    }
    #[test]
    fn Train_Name () {
        let temp = Train::new(String::from("The Calif. Zepher"),
                              String::from("5"),80,1,9*60,100.0,10,0);
        assert_eq!(temp.Name(),String::from("The Calif. Zepher"));
    }
    #[test]
    fn Train_Number () {
        let temp = Train::new(String::from("The Calif. Zepher"),
                              String::from("5"),80,1,9*60,100.0,10,0);
        assert_eq!(temp.Number(),String::from("5"));
    }
    #[test]
    fn Train_Departure () {
        let temp = Train::new(String::from("The Calif. Zepher"),
                              String::from("5"),80,1,9*60,100.0,10,0);
        assert_eq!(temp.Departure(),9*60);
    }
    #[test]
    fn Train_SetDeparture () {
        let mut temp = Train::new(String::from("The Calif. Zepher"),
                              String::from("5"),80,1,9*60,100.0,10,0);
        temp.SetDeparture(9*60+30);
        assert_eq!(temp.Departure(),9*60+30);
    }
    #[test]
    fn Train_Speed () {
        let temp = Train::new(String::from("The Calif. Zepher"),
                              String::from("5"),80,1,9*60,100.0,10,0);
        assert_eq!(temp.Speed(),80);
    }
    #[test]
    fn Train_ClassNumber () {
        let temp = Train::new(String::from("The Calif. Zepher"),
                              String::from("5"),80,1,9*60,100.0,10,0);
        assert_eq!(temp.ClassNumber(),1);
    }
    #[test]
    fn Train_NumberOfNotes () {
        let temp = Train::new(String::from("The Calif. Zepher"),
                              String::from("5"),80,1,9*60,100.0,10,0);
        assert_eq!(temp.NumberOfNotes(),0);
    }
    #[test]
    fn Train_AddNoteToTrain () {
        let mut temp = Train::new(String::from("The Calif. Zepher"),
                              String::from("5"),80,1,9*60,100.0,10,0);
        temp.AddNoteToTrain(4);
        assert_eq!(temp.NumberOfNotes(),1);
    }
    #[test]
    fn Train_Note () {
        let mut temp = Train::new(String::from("The Calif. Zepher"),
                              String::from("5"),80,1,9*60,100.0,10,0);
        temp.AddNoteToTrain(4);
        assert_eq!(temp.Note(0),Some(&4));
    }
    #[test]
    fn Train_RemoveNoteFromTrain () {
        let mut temp = Train::new(String::from("The Calif. Zepher"),
                              String::from("5"),80,1,9*60,100.0,10,0);
        temp.AddNoteToTrain(4);
        temp.RemoveNoteFromTrain(4);
        assert_eq!(temp.NumberOfNotes(),0);
    }
    #[test]
    fn Train_StopI () {
        let temp = Train::new(String::from("The Calif. Zepher"),
                                String::from("5"),80,1,9*60,100.0,10,0);
        let stop = temp.StopI(1).unwrap();
        assert_eq!(*stop,Stop::new(9,StopFlagType::Transit));
    }
    #[test]
    fn Train_NumberOfStops () {
        let temp = Train::new(String::from("The Calif. Zepher"),
                                String::from("5"),80,1,9*60,100.0,10,0);
        assert_eq!(temp.NumberOfStops(),11);
    }
    #[test]
    fn Train_UpdateStopLayover () {
        let mut temp = Train::new(String::from("The Calif. Zepher"),
                                String::from("5"),80,1,9*60,100.0,10,0);
        temp.UpdateStopLayover(1,10.0);
        let stop = temp.StopI(1).unwrap();
        assert_eq!(stop.Layover(),10.0);
    }
    #[test]
    fn Train_UpdateStopCab () {
        let mut temp = Train::new(String::from("The Calif. Zepher"),
                                String::from("5"),80,1,9*60,100.0,10,0);
        temp.UpdateStopCab(1,Some(Cab::new(String::from("Cab1"),String::from("orange"))));
        let stop = temp.StopI(1).unwrap();
        assert_eq!(stop.TheCab(),Some(Cab::new(String::from("Cab1"),String::from("orange"))));
    }
    #[test] 
    fn Train_AddNoteToStop () {
        let mut temp = Train::new(String::from("The Calif. Zepher"),
                                String::from("5"),80,1,9*60,100.0,10,0);
        temp.AddNoteToStop(1,3);
        let stop = temp.StopI(1).unwrap();
        assert_eq!(stop.Note(0),Some(&3));
    }
    #[test] 
    fn Train_RemoveNoteFromStop () {
        let mut temp = Train::new(String::from("The Calif. Zepher"),
                                String::from("5"),80,1,9*60,100.0,10,0);
        temp.AddNoteToStop(1,3);
        temp.RemoveNoteFromStop(1,3);
        let stop = temp.StopI(1).unwrap();
        assert_eq!(stop.NumberOfNotes(),0);
    }
    #[test] 
    fn Train_SetOriginStorageTrack () {
        let mut temp = Train::new(String::from("The Calif. Zepher"),
                                String::from("5"),80,1,9*60,100.0,10,0);
        temp.SetOriginStorageTrack(String::from("Track3"));
        let stop = temp.StopI(0).unwrap();
        assert_eq!(stop.StorageTrackName(),String::from("Track3"));
    }
    #[test] 
    fn Train_SetDestinationStorageTrack () {
        let mut temp = Train::new(String::from("The Calif. Zepher"),
                                String::from("5"),80,1,9*60,100.0,10,0);
        temp.SetDestinationStorageTrack(String::from("Track3"));
        let stop = temp.StopI(10).unwrap();
        assert_eq!(stop.StorageTrackName(),String::from("Track3"));
    }
    #[test] 
    fn Train_SetTransitStorageTrack () {
        let mut temp = Train::new(String::from("The Calif. Zepher"),
                                String::from("5"),80,1,9*60,100.0,10,0);
        temp.SetTransitStorageTrack(3,String::from("Track3"));
        let stop = temp.StopI(3).unwrap();
        assert_eq!(stop.StorageTrackName(),String::from("Track3"));
    }
    #[test]
    fn Train_StartSMile () {
        let temp = Train::new(String::from("The Calif. Zepher"),            
                                String::from("5"),80,1,9*60,100.0,10,0);
        assert_eq!(temp.StartSMile(),100.0);
    }
    #[test] 
    fn Train_Display () {
        let temp = Train::new(String::from("The Calif. Zepher"),            
                                String::from("5"),80,1,9*60,100.0,10,0);
        let output = format!("{}",temp);
        assert_eq!(output,String::from("<Train \"The Calif. Zepher\" \"5\" 80 1 540 100.000 0 11 <Stop Origin 0.000 \"\" 10 None 0> <Stop Transit 0.000 \"\" 9 None 0> <Stop Transit 0.000 \"\" 8 None 0> <Stop Transit 0.000 \"\" 7 None 0> <Stop Transit 0.000 \"\" 6 None 0> <Stop Transit 0.000 \"\" 5 None 0> <Stop Transit 0.000 \"\" 4 None 0> <Stop Transit 0.000 \"\" 3 None 0> <Stop Transit 0.000 \"\" 2 None 0> <Stop Transit 0.000 \"\" 1 None 0> <Stop Terminate 0.000 \"\" 0 None 0>>"));
    }
    #[test]
    fn Train_from_scr () {
        let temp = Train::new(String::from("The Calif. Zepher"),            
                                String::from("5"),80,1,9*60,100.0,10,0);
        let output = format!("{}",temp);
        let other = Train::from_str(&output).expect("Parse Error");
        assert_eq!(temp,other);
    }
}

