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

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.UUID;

import org.brlcad.geometryservice.GSJavaInterface;
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
	public boolean doCmd(String[] args, GSJavaInterface gsji) {

		if (args.length < 4 || args.length > 5) {
			this.printUsage();
			return false;
		}

		/* validate Args */

		// args[0] == cmd name

		String uname = args[1];
		if (uname.length() < 1) {
			GSStatics.stdErr.println("Username too short.");
			this.printUsage();
			return false;
		}

		String passwd = args[2];
		if (passwd.length() < 1) {
			GSStatics.stdErr.println("Password too short.");
			this.printUsage();
			return false;
		}

		String addyStr = args[3];
		if (addyStr.length() < 1) {
			GSStatics.stdErr.println("Address too short.");
			this.printUsage();
			return false;
		}

		InetAddress address = null;
		try {
			address = InetAddress.getByName(addyStr);

		} catch (UnknownHostException ehu) {
			GSStatics.stdErr.println("Unknown Host: " + addyStr);
			return false;

		} catch (SecurityException se) {
			GSStatics.stdErr.println("Security Exception for Host: " + addyStr);
			return false;

		} catch (Exception e) {
			GSStatics.stdErr.println("Unexpected Exception thrown for Host: " + addyStr);
			return false;
		}

		short port = 0;
		if (args.length == 4) {
			port = 5309;
		} else {
			try {
				port = Short.parseShort(args[4]);
			} catch (NumberFormatException nfe) {
				GSStatics.stdErr.println("Connect attempt failed.  Bad Port number.");
				return false;
			} catch (Exception e) {
				GSStatics.stdErr.println(e.getMessage());
				GSStatics.stdErr.println("Connect attempt failed: ");
				return false;
			}
		}

		/* Try connection */
		int retVal = gsji.connectToHost(address, port, uname, passwd);
		
		if (retVal > 0) {
			String remNodeName = gsji.getRemHostName();
			UUID sessID = gsji.getSessionID();
			String out = "";
			
			out = GSStatics.tab2x + "Successfully connected to: '" + remNodeName + "'";
			this.cmdConsole.printLnToConsole(out, CmdConsolePanel.STYLE_BLUE_BOLD);
			out = GSStatics.tab2x + "SessionID: " + sessID.toString();
			this.cmdConsole.printLnToConsole(out, CmdConsolePanel.STYLE_BLUE_BOLD);	
		}
		
		return (retVal > 0);
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
