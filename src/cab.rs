// -!- rust -!- //////////////////////////////////////////////////////////////
//
//  System        : 
//  Module        : 
//  Object Name   : $RCSfile$
//  Revision      : $Revision$
//  Date          : $Date$
//  Author        : $Author$
//  Created By    : Robert Heller
//  Created       : 2025-10-03 16:27:19
//  Last Modified : <251012.2031>
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

//! Cab class and support types.
//!
//! This only really important for pure DC systems, but it useful for DCC
//! systems as a way define crew(s).
//!

use std::fmt;
use std::str::FromStr;
use std::collections::HashMap;
use std::io::{BufReader,Read,BufWriter,Write};
use std::fs::File;
use std::io::{Error, ErrorKind};
use crate::primio::*;


/** This class maintains information about cabs.
  * A cab has a color and a name.
  *
  * @author Robert Heller \<heller\@deepsoft.com\>
  *
  */
#[derive(Default, Debug, Clone, PartialEq)]
pub struct Cab {
    name: String,
    color: String,
}

impl fmt::Display for Cab {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<Cab \"{}\" \"{}\">",self.name,self.color)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum CabParseError {
    StartSyntaxError,
    NameMissing,
    ColorMissing,
    MissingBracket,
    ExtraCharacters,
}
impl fmt::Display for CabParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CabParseError::StartSyntaxError =>
                write!(f,"Missing '<Cab '"),
            CabParseError::NameMissing =>
                write!(f,"Missing name"),
            CabParseError::ColorMissing =>
                write!(f,"Missing color"),
            CabParseError::MissingBracket =>
                write!(f,"Missing '>'"),
            CabParseError::ExtraCharacters =>
                write!(f,"Extra Characters"),
        }
    }
}
impl FromStr for Cab {
    type Err = CabParseError; 
    /// Convert from &str to Self   
    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let (result,pos) = Cab::ParseCab(string)?;
        if pos == string.len() {
            Ok(result)
        } else {
            Err(CabParseError::ExtraCharacters)
        }
    }
}

impl Cab {
    /** Construct a new cab.
      * ## Parameters:
      * - name_ The name of the new cab.
      * - color_ The color of the cab.
      */
    pub fn new(name_: String, color_: String) -> Self {
        Self { name: name_.clone(), color: color_.clone() }
    }
    /** Return the name of the cab.
      */
    pub fn Name(&self) -> String {self.name.clone()}
    /** Return the color of the cab.
      */
    pub fn Color(&self) -> String {self.color.clone()}
    pub fn ParseCab(string: &str) -> Result<(Self, usize), CabParseError> {
        let mut pos: usize;
        let name: String;
        let color: String;
        match string.match_indices("<Cab \"").next() {
            None => return Err(CabParseError::StartSyntaxError),
            Some((n, m)) => pos = n + m.len(),
        };
        match string[pos..].match_indices('"').next() {
            None => return Err(CabParseError::NameMissing),
            Some((n, m)) => {
                name = String::from(&string[pos..n+pos]);
                pos += n + m.len();
            },
        };
        match string[pos..].match_indices('"').next() {
            None => return Err(CabParseError::ColorMissing),
            Some((n, m)) => pos += n + m.len(),
        };
        match string[pos..].match_indices('"').next() {
            None => return Err(CabParseError::ColorMissing),
            Some((n, m)) => {
                color = String::from(&string[pos..n+pos]);
                pos += n + m.len();
            },
        };
        match string[pos..].match_indices('>').next() {
            None => return Err(CabParseError::MissingBracket),
            Some((n, m)) => pos += n + m.len(),
        };
        Ok((Cab::new(name,color),pos))
    }
    pub fn Write(&self,f: &mut BufWriter<File>) -> std::io::Result<()> {
        write!(f,"<Cab \"{}\" \"{}\">",self.name,self.color)
    }
    pub fn Read(inp: &mut BufReader<File>) -> std::io::Result<Option<Self>> {
        let mut ch: char;
        let mut byte: [u8; 1] = [0; 1]; 
        loop {
            let status = inp.read(&mut byte)?;
            if status == 0 {return Ok(None);}
            ch = byte[0] as char;
            match ch {
                ' '|'\t'|'\n' => (),
                _ => {break;},
            }
        }
        for c in "<Cab".chars() {
            if c != ch {return Ok(None);}
            let status = inp.read(&mut byte)?;
            if status == 0 {return Ok(None);}
            ch = byte[0] as char;
        }
        let name: String = match ReadQuotedString(inp)? {
            None => {return Ok(None);},
            Some(s) => s,
        };
        let color: String = match ReadQuotedString(inp)? {
            None => {return Ok(None);},
            Some(s) => s,
        };
        loop {
            let status = inp.read(&mut byte)?;
            if status == 0 {return Ok(None);}
            ch = byte[0] as char; 
            match ch {
                '>' => {return Ok(Some(Self::new(name,color)));},
                ' '|'\t'|'\n' => {continue;},
                _ => {return Err(Error::new(ErrorKind::Other,"Syntax error: missing '>'"));},
            };
        }
    }
}
    
/** Cab name map, cabs indexed by name.
  */
pub type CabNameMap = HashMap<String, Cab>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn Cab_new () {
        let temp = Cab::new(String::from("Cab A"),String::from("red"));
        assert_eq!(temp,Cab {name: String::from("Cab A"), color: String::from("red")});
    }
    #[test]
    fn Cab_Name () {
        let temp = Cab::new(String::from("Cab A"),String::from("red"));
        assert_eq!(temp.Name(),String::from("Cab A"));
    }
    #[test]
    fn Cab_Color () {
        let temp = Cab::new(String::from("Cab A"),String::from("red"));
        assert_eq!(temp.Color(),String::from("red"));
    }
    #[test]
    fn Cab_Display () {
        let temp = Cab::new(String::from("Cab A"),String::from("red"));
        let output = format!("{}",temp);
        assert_eq!(output,String::from("<Cab \"Cab A\" \"red\">"));
    }
    #[test]
    fn Cab_from_str () {
        let temp = Cab::new(String::from("Cab A"),String::from("red"));
        let output = format!("{}",temp);
        let other = Cab::from_str(&output).expect("Parse error");
        assert_eq!(temp,other);
    }
    
}
