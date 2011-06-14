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
/** @file GSStatics.java
 *
 */

package org.brlcad.geometryservice;

import java.io.PrintStream;

public final class GSStatics {
	/*
	 * Give use of this lib a way to hook in their custom logging system. For
	 * now, pipe all out/err text to their normal place.
	 */
	public static PrintStream stdOut = System.out;
	public static PrintStream stdErr = System.err;

	public static final String tab = "   ";
	public static final String newline = "\n";
	public static final String tabnl = tab + newline;
	public static final String nltab = newline + tab;
	public static final String tab2x = tab + tab;

}
