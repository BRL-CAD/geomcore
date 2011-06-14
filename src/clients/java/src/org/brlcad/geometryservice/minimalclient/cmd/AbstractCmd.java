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
/** @file AbstractCmd.java
 *
 */
package org.brlcad.geometryservice.minimalclient.cmd;

import org.brlcad.geometryservice.GSJavaInterface;
import org.brlcad.geometryservice.GSStatics;
import org.brlcad.geometryservice.minimalclient.CmdConsolePanel;

/**
 * @author david.h.loman
 *
 */
public abstract class AbstractCmd {

	protected final String cmd;
	protected final CmdConsolePanel cmdConsole;

	/**
	 * @param cmd
	 * @param cmdConsole
	 */
	public AbstractCmd(String cmd, CmdConsolePanel cmdConsole) {
		super();
		this.cmd = cmd;
		this.cmdConsole = cmdConsole;
	}

	public abstract boolean doCmd(String[] args, GSJavaInterface gsji);

	/**
	 * @return the cmd
	 */
	public final String getCmd() {
		return cmd;
	}

	public void printUsage() {
		String out = GSStatics.tab2x + "Usage:";
		this.cmdConsole.printToConsole(out, CmdConsolePanel.STYLE_BLUE_BOLD);

		out = GSStatics.tab + this.getUsage() ;
		this.cmdConsole.printLnToConsole(out, CmdConsolePanel.STYLE_BLUE);
	}

	public void printHelp() {
		String out = GSStatics.tab2x + "Help: " + GSStatics.tab;
		this.cmdConsole.printToConsole(out, CmdConsolePanel.STYLE_BLUE_BOLD);

		out = GSStatics.tab + this.getHelp();
		this.cmdConsole.printToConsole(out, CmdConsolePanel.STYLE_BLUE);

	}

	public void printUsageAndHelp() {
		this.printUsage();
		this.printHelp();
	}

	public abstract String getHelp();
	public abstract String getUsage();

}
