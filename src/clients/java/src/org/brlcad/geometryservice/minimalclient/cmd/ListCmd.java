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

import java.util.ArrayList;

import org.brlcad.geometryservice.GSJavaInterface;
import org.brlcad.geometryservice.GSStatics;
import org.brlcad.geometryservice.minimalclient.CmdConsolePanel;

/**
 * @author david.h.loman
 *
 */
public class ListCmd extends AbstractCmd {

	/**
	 * @param cmd
	 * @param cmdConsole
	 */
	public ListCmd(CmdConsolePanel cmdConsole) {
		super("list", cmdConsole);
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

		if (args.length <= 2) {
			this.printUsage();
			return false;
		}

		/* validate Args */
		String path = "/";

		/* if a path is provided, use it.  If not, assume root or "/" */
		if (args.length == 2) {
			path = args[1];
			if (path.length() < 1) {
				GSStatics.stdErr.println("Path too short.");
				this.printUsage();
				return false;
			}
		}
		
		ArrayList<String> items = gsji.getList(path);
		
		this.cmdConsole.printLnToConsole(GSStatics.tab2x + "Got: " + items.size() + " items.", CmdConsolePanel.STYLE_BLUE_BOLD);
		
		return true;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.brlcad.geometryservice.minimalclient.cmd.AbstractCmd#getHelp()
	 */
	@Override
	public String getHelp() {
		return "Lists the contents of a directory";
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.brlcad.geometryservice.minimalclient.cmd.AbstractCmd#getUsage()
	 */
	@Override
	public String getUsage() {
		return "list path";
	}

}
