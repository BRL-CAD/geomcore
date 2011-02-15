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
/** @file Main.java
 * 
 */
package org.brlcad.geometryservice.minimalclient;

import javax.swing.SwingUtilities;

/**
 * @author david.h.loman
 * 
 */
public class Main {

	/**
	 * @param args
	 */
	public static void main(String[] args) {

		/* Use .invokeLater() to prevent certain types of concurrency deadlocks */
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				MinimalGSClient cli = new MinimalGSClient();
				cli.run();
			}
		});

	}

}
