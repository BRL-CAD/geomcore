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

import java.util.HashMap;
import java.util.Set;

import org.brlcad.geometryservice.minimalclient.CmdConsolePanel;

/**
 * @author david.h.loman
 * 
 */
public class CmdManager {

	private static final HashMap<String, AbstractCmd> cmdMap = new HashMap<String, AbstractCmd>();

	public static final void parseCmd(String line, CmdConsolePanel console) {

		/* Id-10-T check */
		if (line.length() < 1)
			return;

		String[] cmdStack = line.split(" ");

		/* handle all strings in lowercase */
		String cmdStr = cmdStack[0].toLowerCase();

		AbstractCmd aCmd = CmdManager.getCmdByName(cmdStr);

		if (aCmd != null) {
			console.printLnToConsole(line, "red");
			aCmd.doCmd(cmdStack);
			// TODO handle boolean return val?
			console.printLnToConsole("\n", "red");
			return;
		}

		console.printToConsole("Unknown Command String: ", "blue");
		console.printLnToConsole("'" + line + "'", "blue-bold");
		
		console.printLnToConsole("Try 'help'\n", "blue");
	}

	public static final void registerCmd(AbstractCmd cmd) {
		String cmdStr = cmd.getCmd().toLowerCase();

		if (CmdManager.cmdMap.containsKey(cmdStr)) {
			// TODO error here?
			return;
		}

		CmdManager.cmdMap.put(cmdStr, cmd);
	}
	/**
	 * @return
	 * @see java.util.HashMap#keySet()
	 */
	public static final Set<String> getAllRegisteredCmds() {
		return cmdMap.keySet();
	}
	
	public static final AbstractCmd getCmdByName(String name) {
		return CmdManager.cmdMap.get(name);
	}


	public static final void registerBuiltInCmds(CmdConsolePanel console) {
		CmdManager.registerCmd(new HelpCmd(console));
		CmdManager.registerCmd(new LoginCmd(console));
	}


}
