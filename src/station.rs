// -!- rust -!- //////////////////////////////////////////////////////////////
//
//  System        : 
//  Module        : 
//  Object Name   : $RCSfile$
//  Revision      : $Revision$
//  Date          : $Date$
//  Author        : $Author$
//  Created By    : Robert Heller
//  Created       : 2025-10-03 16:27:04
//  Last Modified : <251004.2324>
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

//! ##  Station and support classes.
//!
//! This class and its support classes implement information about stations
//! and station stops.  This includes where a station is along the line (its
//! scale mile), what storage tracks it has, and what trains are being stored
//! on the storage tracks during what times.  Stations are places where trains
//! stop or just important junctions or mile post locations that trains pass
//! by -- they might only be used for time keeping checks.  Note: the
//! classification tracks at a yard are not storage tracks.  Nor are RIP or 
//! service or other special purpose tracks.  Storage tracks are only for 
//! storing whole, complete trains (they might be without engines).  
//!
//! 

use std::fmt;
use std::str::FromStr;
use std::cmp::Ordering;
use std::collections::BTreeMap;

/// This class records a train sitting on a storage track during a specified 
/// time frame.  The train number (symbol) might change when the train leaves 
/// the storage track.
///
#[derive(Default, Debug, Clone, PartialEq)]
pub struct Occupied {
    trainnum: String,
    trainnum2: String,
    from: f64,
    until: f64,
}

impl Occupied {
    /// Constructor: record a train occupying a storage track.
    /// ## Parameters:
    /// - trainnum_ The arriving train number (symbol).
    /// - from_ The arrival time.
    /// - until_ The departure time.
    /// - trainnum2_ The departing train number (symbol).  If it is
    ///   the empty string, the departing train has the same number
    ///   (symbol) as the arriving train.
    ///
    /// __Returns__ a freshly initialized Occupied struct.
    pub fn new(trainnum_: String, from_: f64, until_: f64, trainnum2_: String) 
            -> Self {
        assert!(from_ < until_,"Backwars time tange!");
        Self {trainnum: trainnum_, from: from_, until: until_, trainnum2: trainnum2_ }
    }
    /// Return the train that arrives.
    pub fn TrainNum(&self) -> String {self.trainnum.clone()}
    /// Return the train that departs. 
    pub fn TrainNum2(&self) -> String {self.trainnum2.clone()}
    /// Return the start time
    pub fn From(&self) -> f64 {self.from}
    /// Return the end time.
    pub fn Until(&self) -> f64 {self.until}
}

impl fmt::Display for Occupied {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<Occupied \"{}\" {:4.2}:{:4.2} \"{}\">",
            self.trainnum,self.from,self.until,self.trainnum2)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum OccupiedParseError {
    StartSyntaxError,
    TrainnumSyntaxError,
    FromSyntaxError,
    UntilSyntaxError,
    Trainnum2SyntaxError,
}

impl fmt::Display for OccupiedParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OccupiedParseError::StartSyntaxError =>
                write!(f, "Missing '<Occupied '"),
            OccupiedParseError::TrainnumSyntaxError =>
                write!(f, "Missing trainnum"),
            OccupiedParseError::FromSyntaxError =>
                write!(f, "Missing from"),
            OccupiedParseError::UntilSyntaxError =>
                write!(f, "Missing until"),
            OccupiedParseError::Trainnum2SyntaxError =>
                write!(f, "Missing trainnum2"),
        }
    }
}

impl FromStr for Occupied {
    type Err = OccupiedParseError;
    /// Convert from &str to Self
    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let mut pos: usize;
        let trainnum: String;
        let trainnum2: String;
        let from: f64;
        let until: f64;
        match string.match_indices("<Occupied \"").next() {
            None => return Err(OccupiedParseError::StartSyntaxError),
            Some((n, m)) => pos = n + m.len(),
        };
        match string[pos..].match_indices('"').next() {
            None => return Err(OccupiedParseError::TrainnumSyntaxError),
            Some((n, m)) => {
                trainnum = String::from(&string[pos..n+pos]);
                pos += n + m.len();},
        }
        match string[pos..].match_indices(':').next() {
            None => return Err(OccupiedParseError::FromSyntaxError),
            Some((n, m)) => {
                from = match (string[pos..n+pos].trim()).parse::<f64>() {
                    Ok(f) => f,
                    Err(p) => return Err(OccupiedParseError::FromSyntaxError),
                };
                pos += n + m.len();},
        };
        match string[pos..].match_indices('"').next() {
            None => return Err(OccupiedParseError::UntilSyntaxError),
            Some((n, m)) => {
                until = match (string[pos..n+pos].trim()).parse::<f64>() {
                    Ok(u) => u,
                    Err(p) => return Err(OccupiedParseError::UntilSyntaxError),
                };
                pos += n + m.len();},
        };
        match string[pos..].match_indices("\">").next() {
            None => return Err(OccupiedParseError::Trainnum2SyntaxError),
            Some((n, m)) => trainnum2 = String::from(&string[pos..n+pos]),
        }
        Ok(Self{ trainnum: trainnum, from: from, until: until, trainnum2: trainnum2})
    }
}

/// The TimeRange class implements a range of times.
///
#[derive(Default, Debug, Clone, Copy)]
pub struct TimeRange {
    from: f64,
    to:   f64,
}

impl PartialEq for TimeRange {
    fn eq(&self, other: &Self) -> bool {
        self.to == other.to && self.from == other.from
    }
}

impl Ord for TimeRange {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.to <= other.from {
            return Ordering::Less;
        } else if self.from >= other.to {
            return Ordering::Greater;
        } else {
            return Ordering::Equal;
        }
    }
}

impl PartialOrd for TimeRange {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
    fn lt(&self, other: &Self) -> bool {
        self.to <= other.from
    }
    fn gt(&self, other: &Self) -> bool {
        self.from >= other.to
    }
}

impl Eq for TimeRange { }


impl TimeRange {
    /// Construct a time range, from a start and end time.
    /// ## Parameters:
    /// - from_ The start time.
    /// - to_ The end time.
    pub fn new(from_: f64, to_: f64) -> Self {
        assert!(from_ < to_,"Backwars time tange!");
        Self { from: from_, to: to_ }
    }
    /// Return the low end of the range.
    pub fn From (&self) -> f64 {self.from}
    /// Return the high end of the range.
    pub fn To (&self) -> f64 {self.to}
    /// Does this interval contain the specified time?
    /// ## Parameters:
    /// - time The time to check for.
    pub fn ContainsTime(&self, time: f64 ) -> bool {
        time >= self.from && time <= self.to
    }
}

impl fmt::Display for TimeRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<TimeRange {:4.2} {:4.2}>", self.from, self.to)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TimeRangeParseError {
    StartSyntaxError,
    FromSyntaxError,
    ToSyntaxError,
}

impl fmt::Display for TimeRangeParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TimeRangeParseError::StartSyntaxError =>
                write!(f, "Missing '<TimeRange '"),
            TimeRangeParseError::FromSyntaxError =>
                write!(f, "Missing from"),
            TimeRangeParseError::ToSyntaxError =>
                write!(f, "Missing to"),
        }
    }
}


impl FromStr for TimeRange {
    type Err = TimeRangeParseError;
    /// Convert from &str to Self 
    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let mut pos: usize;
        let from: f64; 
        let to: f64;
        match string.match_indices("<TimeRange").next() {
            None => return Err(TimeRangeParseError::StartSyntaxError),
            Some((n, m)) => pos = n + m.len(),
        };
        while string[pos..pos+1] == *" " {
            pos += 1;
        }
        match string[pos..].match_indices(' ').next() {
            None => return Err(TimeRangeParseError::FromSyntaxError),
            Some((n, m)) => {
                from = match (string[pos..n+pos].trim()).parse::<f64>() {
                    Ok(f) => f,
                    Err(p) => return Err(TimeRangeParseError::FromSyntaxError),
                };
                pos += n + m.len();},
        };
        while string[pos..pos+1] == *" " {
            pos += 1;
        }
        match string[pos..].match_indices('>').next() {
            None => return Err(TimeRangeParseError::ToSyntaxError),
            Some((n, m)) => 
                to = match (string[pos..n+pos].trim()).parse::<f64>() {
                    Ok(t) => t,
                    Err(p) => return Err(TimeRangeParseError::ToSyntaxError),
                },
            
        };
        Ok(Self { from: from, to: to })
    }
}
                
type OccupiedMap = BTreeMap<TimeRange, Occupied>;        

/** The StorageTrack class implements a storage track.
  *
  * Storage tracks store trains at stations.  Each storage track can only
  * store one train at a given time.  No checks are made to determing if
  * the track is actually long enough for the train.
  *
  * Each storage track has a name.
  */
#[derive(Default, Debug, Clone, PartialEq)]
pub struct StorageTrack {
    name: String,
    occupations: OccupiedMap,
}

impl StorageTrack {
    /** Construct a storage track.  The name of the track is initialized.
      * ## Parameters:
      * - name_ The name of the storage track.
      */
    pub fn new (name_: String) -> Self {
        Self {name: name_.clone(), occupations: BTreeMap::new(),}
    }
    /** Return the name of the storage track.
      */
    pub fn Name(&self) -> String {self.name.clone()}
    /** Set the storage track's name.
      * ## Parameters:
      *  - name_ The new name of the storage track.
      */
    pub fn SetName(&mut self, name_: String) {
	self.name = name_.clone();
    }
    /** Return the occupation that includes the specified time;
      * ## Parameters:
      * - time The time to check for.
      */
    pub fn IncludesTime(&self, time: f64) -> Option<&Occupied> {
        for (tr, occ) in self.occupations.iter() {
            if time > tr.To() {continue;}
            if tr.ContainsTime(time) {return Some(occ);}
            if time < tr.From() {break;}
        }
        None
    }
    /** Insert train onto storage track for a time.
      * ## Parameters:
      * - train  The arriving train.
      * - from   The arrival time.
      * - to     The departure time.
      * - train2 The departing train.
      */
    pub fn StoreTrain (&mut self, train: String, from: f64, to: f64, 
                        train2: String) -> Option<&Occupied> {
        let range = TimeRange::new(from,to);
        let newOccupied = Occupied::new(train,from,to,train2);
        self.occupations.insert(range,newOccupied);
        self.occupations.get(&range)
    }
    /** Remove stored train.
      * ## Parameters:
      * - from   The arrival time.
      * - to     The departure time.
      */
    pub fn RemovedStoredTrain (&mut self, from: f64, to: f64) -> bool {
        let range = TimeRange::new(from,to);
        if self.occupations.contains_key(&range) {
            self.occupations.remove(&range);
            true
        } else {
            false
        }
    }
    /** Return true if the time range is in use.
      * ## Parameters:
      * - from   The arrival time.
      * - to     The departure time.
      */
    pub fn UsedTimeRange(&self, from: f64, to: f64) -> bool {
        for (tr, occ) in self.occupations.iter() {
            if from > tr.To() {break;}
            if to < tr.From() {continue;}
            if tr.ContainsTime(from) ||
               tr.ContainsTime(to) {
                return true;
            }
        }
        false
    }
    /** Return occupication structure for a given time tange.
      * ## Parameters:
      * - from   The arrival time.
      * - to     The departure time.
      */
    pub fn FindOccupied(&self, from: f64, to: f64) -> Option<&Occupied>  {
        self.occupations.get(&TimeRange::new(from,to))
    }
    /** Replace a stored arrrival train.
      * ## Parameters:
      * - from   The arrival time.
      * - to     The departure time.
      * - train  The new arriving train.
      */
    pub fn UpdateStoredTrain(&mut self, from: f64, to: f64, train: String) -> Option<&Occupied>  {
        let range = TimeRange::new(from,to);
        match self.occupations.get(&range) {
            None => None,
            Some(occ) => {
                let newOccupied = Occupied::new(train,from,to,occ.TrainNum2());
                self.occupations.insert(range,newOccupied);
                self.occupations.get(&range)
            },
        }
    }
    /** Replace a stored departure train.
      * ## Parameters:
      * - from   The arrival time.
      * - to     The departure time.
      * - train  The new departing train.
      */
    pub fn UpdateStoredTrain2(&mut self, from: f64, to: f64, train: String) -> Option<&Occupied>  {
        let range = TimeRange::new(from,to);
        match self.occupations.get(&range) {
            None => None,
            Some(occ) => {
                let newOccupied = Occupied::new(occ.TrainNum(),from,to,train);
                self.occupations.insert(range,newOccupied);
                self.occupations.get(&range)
            },
        }
    }
    /** Update a train's arrival time.
      * ## Parameters:
      * - from   The arrival time.
      * - to     The departure time.
      * - newArrival The new arrival time.
      */
    pub fn UpdateStoredTrainArrival(&mut self, from: f64, to: f64, 
                                    newArrival: f64) -> Option<&Occupied> {
        let range = TimeRange::new(from,to);
        match self.occupations.get(&range) {
            None => None, 
            Some(occ) => {
                let newOccupied = Occupied::new(occ.TrainNum(),newArrival,range.To(),occ.TrainNum2());
                self.occupations.remove(&range);
                let newrange = TimeRange::new(newArrival,to);
                self.occupations.insert(newrange,newOccupied);
                self.occupations.get(&newrange)
            },
        }
    }
    /** Update a train's departure time.
      * ## Parameters:
      * - from   The arrival time.
      * - to     The departure time.
      * - newDeparture The new departure time.
      */
    pub fn UpdateStoredTrainDeparture(&mut self, from: f64, to: f64,
                                      newDeparture: f64) -> Option<&Occupied> {
        let range = TimeRange::new(from,to);
        match self.occupations.get(&range) {
            None => None, 
            Some(occ) => {
                let newOccupied = Occupied::new(occ.TrainNum(),range.From(),newDeparture,occ.TrainNum2());
                self.occupations.remove(&range);
                let newrange = TimeRange::new(from,newDeparture);
                self.occupations.insert(newrange,newOccupied);
                self.occupations.get(&newrange)
            },
        }
    }
}























#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn Occupied_new() {
        let result = Occupied::new(String::from("train1"), 4.5, 4.75,
                                   String::from(""));
        let temp = Occupied { trainnum: String::from("train1"), 
                                   from: 4.5, 
                                   until: 4.75,
                                   trainnum2: String::from("") };
        assert_eq!(result,temp);
    }
    #[test]
    fn Occupied_getter_tests() {
        let temp = Occupied::new(String::from("train1"), 4.0, 5.0, String::from(""));
        assert_eq!(temp.TrainNum(),String::from("train1"));
        assert_eq!(temp.From(),4.0);
        assert_eq!(temp.Until(),5.0);
        assert_eq!(temp.TrainNum2(),String::from(""));
    }
    #[test]
    fn Occupied_Display() {
        let temp = Occupied::new(String::from("train1"), 4.1, 5.2, String::from(""));
        let out = format!("{}",temp);
        assert_eq!(out,String::from("<Occupied \"train1\" 4.10:5.20 \"\">"));
    }
    #[test]
    fn Occupied_from_str () {
        let temp = "<Occupied \"train1\" 4.10:5.20 \"\">";
        let result = Occupied::from_str(temp);
        assert_eq!(result,Ok(Occupied::new(String::from("train1"), 4.1, 5.2, String::from(""))));
    }

    #[test]
    fn TimeRange_new () {
        let result = TimeRange::new(5.1, 6.2);
        let temp = TimeRange { from: 5.1, to: 6.2 };
        assert_eq!(result,temp);
    }
    #[test]
    fn TimeRange_getters () {
        let result = TimeRange::new(5.1, 6.2);
        assert_eq!(result.From(),5.1);
        assert_eq!(result.To(),6.2);
    }
    #[test]
    fn TimeRange_ContainsTime() {
        let result = TimeRange::new(5.1, 6.2); 
        assert_eq!(result.ContainsTime(6.0),true);
        assert_eq!(result.ContainsTime(7.0),false);
    }
    #[test]
    fn TimeRange_Eq() {
        let a = TimeRange::new(2.4, 4.2);
        let b = TimeRange::new(2.4, 4.2);
        assert_eq!(a == b,true);
        assert_eq!(b == b,true);
        assert_eq!(a != b,false);
        assert_eq!(b != a,false);
    }
    #[test]
    fn TimeRange_GtLt() {
        let a = TimeRange::new(2.4, 4.2);
        let b = TimeRange::new(4.7, 5.9);
        let c = TimeRange::new(1.2, 2.3);
        let d = TimeRange::new(2.3, 3.4);
        assert_eq!(a < b,true);
        assert_eq!(b > a,true);
        assert_eq!(d < a,false);
        assert_eq!(a > c,true);
    }
    #[test]
    fn TimeRange_Display() {
        let temp = TimeRange::new(4.1, 5.2);
        let out = format!("{}",temp);
        assert_eq!(out,String::from("<TimeRange 4.10 5.20>"));
    }
    #[test]
    fn TimeRange_from_str () {
        let temp = "<TimeRange 4.10 5.20>";
        let result = TimeRange::from_str(temp);
        assert_eq!(result,Ok(TimeRange::new(4.1, 5.2)));
    }

}
