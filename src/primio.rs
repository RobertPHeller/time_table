// -!- rust -!- //////////////////////////////////////////////////////////////
//
//  System        : 
//  Module        : 
//  Object Name   : $RCSfile$
//  Revision      : $Revision$
//  Date          : $Date$
//  Author        : $Author$
//  Created By    : Robert Heller
//  Created       : 2025-10-10 23:49:56
//  Last Modified : <251011.1112>
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

use std::io::{BufReader,Read};
use std::fs::File;
use std::io::{Error, ErrorKind};

pub fn ReadQuotedString(inp: &mut BufReader<File>) -> std::io::Result<Option<String>> {
    let mut buffer: [u8; 1] = [0; 1];
    loop {
        let status = inp.read(&mut buffer)?;
        if status == 0 {return Ok(None);}
        let ch = buffer[0] as char;
        match ch {
            '"' => {break;}
            ' '|'\t'|'\n' => {continue;}
            _ => {return Ok(None);}
        };
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

fn IsDigit(ch: char) -> bool {
    match ch {
        '0'..='9' => true,
        _         => false,
    }
}
fn IsAlpha(ch: char) -> bool {
    match ch {
        'a'..='z' => true,
        'A'..='Z' => true,
        _         => false,
    }
}


pub fn ReadF64(inp: &mut BufReader<File>) -> std::io::Result<Option<f64>> {
    let mut buffer: [u8; 1] = [0; 1];
    let mut ch: char;
    loop {
        let status = inp.read(&mut buffer)?;
        if status == 0 {return Ok(None);} 
        ch = buffer[0] as char; 
        match ch {
            ' '|'\t'|'\n' => {continue;}
            _ => {break;}
        };
    }
    //let mut EOF: bool = false;
    let mut temp: String = String::new();
    let mut sign: bool = false;
    let mut dot: bool = false;
    loop {
        if !sign && (ch == '-' || ch == '+') {
            sign = true;
        } else if !dot && ch == '.' {
            dot = true;
        } else if IsDigit(ch) {
            sign = true;
        } else {
            inp.seek_relative(-1)?;
            break;
        }
        temp += &ch.to_string();
        let status = inp.read(&mut buffer)?;
        if status == 0 {/*EOF = true;*/break;}
        ch = buffer[0] as char;
    }
    let result: f64 = match temp.parse() {
        Ok(f) => f,
        Err(p) => {return Err(Error::new(ErrorKind::Other,p.to_string()));}
    };
    Ok(Some(result))
}

pub fn ReadISize(inp: &mut BufReader<File>) -> std::io::Result<Option<isize>> {
    let mut buffer: [u8; 1] = [0; 1];
    let mut ch: char;
    loop {
        let status = inp.read(&mut buffer)?;
        if status == 0 {return Ok(None);} 
        ch = buffer[0] as char; 
        match ch {
            ' '|'\t'|'\n' => {continue;}
            _ => {break;}
        };
    }
    //let mut EOF: bool = false;
    let mut temp: String = String::new();
    let mut sign: bool = false;
    loop {
        if !sign && (ch == '-' || ch == '+') {
            sign = true;
        } else if IsDigit(ch) {
            sign = true;
        } else {
            inp.seek_relative(-1)?;
            break;
        }
        temp += &ch.to_string();
        let status = inp.read(&mut buffer)?;
        if status == 0 {/*EOF = true;*/break;}
        ch = buffer[0] as char;
    }
    let result: isize = match temp.parse() {
        Ok(f) => f,
        Err(p) => {return Err(Error::new(ErrorKind::Other,p.to_string()));}
    };
    Ok(Some(result))
}

pub fn ReadUSize(inp: &mut BufReader<File>) -> std::io::Result<Option<usize>> {
    let mut buffer: [u8; 1] = [0; 1];
    let mut ch: char;
    loop {
        let status = inp.read(&mut buffer)?;
        if status == 0 {return Ok(None);} 
        ch = buffer[0] as char; 
        match ch {
            ' '|'\t'|'\n' => {continue;}
            _ => {break;}
        };
    }
    //let mut EOF: bool = false;
    let mut temp: String = String::new();
    let mut sign: bool = false;
    loop {
        if !sign && ch == '+' {
            sign = true;
        } else if IsDigit(ch) {
            sign = true;
        } else {
            inp.seek_relative(-1)?;
            break;
        }
        temp += &ch.to_string();
        let status = inp.read(&mut buffer)?;
        if status == 0 {/*EOF = true;*/break;}
        ch = buffer[0] as char;
    }
    let result: usize = match temp.parse() {
        Ok(f) => f,
        Err(p) => {return Err(Error::new(ErrorKind::Other,p.to_string()));}
    };
    Ok(Some(result))
}

pub fn ReadU32(inp: &mut BufReader<File>) -> std::io::Result<Option<u32>> {
    let mut buffer: [u8; 1] = [0; 1];
    let mut ch: char;
    loop {
        let status = inp.read(&mut buffer)?;
        if status == 0 {return Ok(None);} 
        ch = buffer[0] as char; 
        match ch {
            ' '|'\t'|'\n' => {continue;}
            _ => {break;}
        };
    }
    //let mut EOF: bool = false;
    let mut temp: String = String::new();
    let mut sign: bool = false;
    loop {
        if !sign && ch == '+' {
            sign = true;
        } else if IsDigit(ch) {
            sign = true;
        } else {
            inp.seek_relative(-1)?;
            break;
        }
        temp += &ch.to_string();
        let status = inp.read(&mut buffer)?;
        if status == 0 {/*EOF = true;*/break;}
        ch = buffer[0] as char;
    }
    let result: u32 = match temp.parse() {
        Ok(f) => f,
        Err(p) => {return Err(Error::new(ErrorKind::Other,p.to_string()));}
    };
    Ok(Some(result))
}

