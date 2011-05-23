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
/** @file UUIDSerialResearch.java
 *
 */
package org.brlcad.geometryservice;

import java.util.UUID;

/**
 * @author dloman
 *
 */
public class UUIDSerialResearch {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub
		String s = "dad3c04e-5ef7-42c3-86eb-a48faae11254";
		System.out.println("Input string len: " + s.length());
		UUID u = UUID.fromString(s);
		System.out.println(u.toString());
	}

}
