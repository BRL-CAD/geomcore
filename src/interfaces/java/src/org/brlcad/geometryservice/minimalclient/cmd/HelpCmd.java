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

import java.util.Set;

import org.brlcad.geometryservice.GSJavaInterface;
import org.brlcad.geometryservice.GSStatics;
import org.brlcad.geometryservice.minimalclient.CmdConsolePanel;

/**
 * @author david.h.loman
 * 
 */
public class HelpCmd extends AbstractCmd {

	/**
	 * @param cmd
	 * @param cmdConsole
	 */
	public HelpCmd(CmdConsolePanel cmdConsole) {
		super("help", cmdConsole);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.brlcad.geometryservice.minimalclient.cmd.AbstractCmd#doCmd(java.lang
	 * .String[])
	 */
	@Override
	public boolean doCmd(String[] args, GSJavaInterface gsji) {

		if (args.length <= 1) {
			/* list ALL cmds */
			this.listAllCmds();
			return true;
		}
		String out = "";
		String cmdToList = args[1];

		AbstractCmd aCmd = CmdManager.getCmdByName(cmdToList);

		if (aCmd == null) {
			out = GSStatics.tab + "Help: ";
			this.cmdConsole.printLnToConsole(out, CmdConsolePanel.STYLE_BLUE_BOLD);

			out = GSStatics.tab2x + "Unknown Command";
			this.cmdConsole.printToConsole(out, CmdConsolePanel.STYLE_BLUE);
			return true;
		}
		
		/* Get help for just ONE command.*/
		
		out = GSStatics.tab + aCmd.getCmd() + ": ";
		this.cmdConsole.printLnToConsole(out, CmdConsolePanel.STYLE_BLUE_BOLD);

		aCmd.printUsageAndHelp();
		
		return true;
	}

	private void listAllCmds() {
		Set<String> cmds = CmdManager.getAllRegisteredCmds();
		String out = GSStatics.tab + "Available Commands:";

		for (int a = 0; a < cmds.size(); ++a) {
			if (a % 2 == 0)
				out += GSStatics.nltab;

			String tCmd = (String) cmds.toArray()[a];

			out += tCmd + ", ";
		}

		this.cmdConsole.printToConsole(out, CmdConsolePanel.STYLE_BLUE);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.brlcad.geometryservice.minimalclient.cmd.AbstractCmd#getHelp()
	 */
	@Override
	public String getHelp() {
		return "Displays all registered commands if no argument is given.  If a specific command is supplied, e.g. 'help login', then the help statement for the supplied command will be printed.";
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.brlcad.geometryservice.minimalclient.cmd.AbstractCmd#getUsage()
	 */
	@Override
	public String getUsage() {
		return "help [cmd]";
	}

}
