// -!- rust -!- //////////////////////////////////////////////////////////////
//
//  System        : 
//  Module        : 
//  Object Name   : $RCSfile$
//  Revision      : $Revision$
//  Date          : $Date$
//  Author        : $Author$
//  Created By    : Robert Heller
//  Created       : 2025-10-12 09:33:24
//  Last Modified : <251012.0944>
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
use time_table::TimeTableSystem;

fn main() {
    let mut lj_and_bs = TimeTableSystem::old("LJandBS.tt")
                            .expect("Failed to open time table file");
    lj_and_bs.CreateLaTeXTimetable("LJandBS.tex")
                            .expect("Failed to create time table LaTeX file");
    println!("LJandBS.tex created.  Now use pdflatex to process this file");
    println!("into LJandBS.pdf.  You will need to run pdflatex 2 or three");
    println!("times to get the table of contents properly created.");
}



