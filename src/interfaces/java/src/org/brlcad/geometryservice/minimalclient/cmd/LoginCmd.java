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
/** @file HelpCmd.java
 * 
 */
package org.brlcad.geometryservice.minimalclient.cmd;

import org.brlcad.geometryservice.GSStatics;
import org.brlcad.geometryservice.minimalclient.CmdConsolePanel;

/**
 * @author david.h.loman
 * 
 */
public class LoginCmd extends AbstractCmd {

	/**
	 * @param cmd
	 * @param cmdConsole
	 */
	public LoginCmd(CmdConsolePanel cmdConsole) {
		super("login", cmdConsole);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.brlcad.geometryservice.minimalclient.cmd.AbstractCmd#doCmd(java.lang
	 * .String[])
	 */
	@Override
	public boolean doCmd(String[] args) {
		
		if (args.length < 4 || args.length > 5) {
			this.printUsage();
			return false;
		}

		this.cmdConsole.printToConsole("Not Implemented yet.", CmdConsolePanel.STYLE_BLUE_BOLD);

		return true;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.brlcad.geometryservice.minimalclient.cmd.AbstractCmd#getHelp()
	 */
	@Override
	public String getHelp() {
		return "Attempts to login to a Geometry Service";
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.brlcad.geometryservice.minimalclient.cmd.AbstractCmd#getUsage()
	 */
	@Override
	public String getUsage() {
		return "login uname password address [port]";
	}

}
