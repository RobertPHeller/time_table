// -!- rust -!- //////////////////////////////////////////////////////////////
//
//  System        : 
//  Module        : 
//  Object Name   : $RCSfile$
//  Revision      : $Revision$
//  Date          : $Date$
//  Author        : $Author$
//  Created By    : Robert Heller
//  Created       : 2025-10-14 16:15:25
//  Last Modified : <251014.1642>
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

fn InsertStations(time_table: &mut TimeTableSystem) {
    time_table.AddStation(String::from("Greenfield, MA"),0.0);
    time_table.AddStation(String::from("Northampton, MA"),25.0);
    time_table.AddStation(String::from("Holyoke, MA"),15.0+25.0);
    time_table.AddStation(String::from("Springfield, MA"),28.0+15.0+25.0);
}

fn InsertSouthboundTrains(time_table: &mut TimeTableSystem) {
    time_table.AddTrain(String::from("Valley Flyer"),String::from("425"),60,1,
                        6*60+5,0,3).expect("Failed to insert 425");
    time_table.AddTrain(String::from("Valley Flyer"),String::from("479"),60,1,
                        18*60+5,0,3).expect("Failed to insert 479");
}

fn InsertNorthboundTrains(time_table: &mut TimeTableSystem) {
    time_table.AddTrain(String::from("Valley Flyer"),String::from("486"),60,1,
                        15*60+15,3,0).expect("Failed to insert 486");
    time_table.AddTrain(String::from("Valley Flyer"),String::from("479"),60,1,
                        21*60+25,3,0).expect("Failed to insert 494");
}





fn main() {
    let mut valley_flyer = TimeTableSystem::new(String::from("Northern NE"),
                                                1440,15);
    InsertStations(&mut valley_flyer);
    InsertSouthboundTrains(&mut valley_flyer);
    InsertNorthboundTrains(&mut valley_flyer);
    valley_flyer.CreateLaTeXTimetable("ValleyFlyer.tex")
                    .expect("Failed to create time table LaTeX file");
}
