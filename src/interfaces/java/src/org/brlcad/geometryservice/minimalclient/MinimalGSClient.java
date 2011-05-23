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
/** @file MinimalGSClient.java
 *
 */
package org.brlcad.geometryservice.minimalclient;

import org.brlcad.geometryservice.GSJavaInterface;

/**
 * @author david.h.loman
 *
 */
public class MinimalGSClient {

	private final MinimalGSClientGUI gui;
	private final GSJavaInterface jgs;

	public MinimalGSClient() {
		this.gui = new MinimalGSClientGUI(this);
		this.jgs = new GSJavaInterface();
	}

	public final void run() {
		//This call does NOT block.
		this.gui.setVisible(true);
	}

	/**
	 * @return the jgs
	 */
	public final GSJavaInterface getGSJavaInterface() {
		return jgs;
	}


}
