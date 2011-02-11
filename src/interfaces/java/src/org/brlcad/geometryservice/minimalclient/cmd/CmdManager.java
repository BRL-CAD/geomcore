/*
 * BRL-CAD
 *
 * Copyright (c) 2011 United States Government as represented by
 * the U.S. Army Research Laboratory.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this file; see the file named COPYING for more
 * information.
 */
/** @file CmdManager.java
 * 
 */
package org.brlcad.geometryservice.minimalclient.cmd;

import org.brlcad.geometryservice.minimalclient.CmdConsolePanel;

/**
 * @author david.h.loman
 *
 */
public class CmdManager {

	
	
	public static final void parseCmd(String line, CmdConsolePanel output) {
		
		/* Id-10-T check */
		if (line.length() < 1)
			return;
		
		String[] cmdStack = line.split(" ");
		
		String cmd = cmdStack[0];
		
		if (cmd.equals("Testing")) {
			output.addTextToConsole(line);
			return;
		}
		
		
		
		
		output.addTextToConsole("Unknown Command String: '" +  line + "'");
		
	}
	
}
