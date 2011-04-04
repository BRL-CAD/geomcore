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
/** @file LoginTest.java
 * 
 */
package org.brlcad.geometryservice;

import java.net.InetAddress;
import java.net.UnknownHostException;

/**
 * @author dloman
 *
 */
public class LoginTest {

	public static void main(String[] args) throws UnknownHostException, InterruptedException {
		
		GSJavaInterface inter = new GSJavaInterface();
		
		boolean success = inter.connectToHost(InetAddress.getByName("127.0.0.1"), (short)5309, "Guest", "Guest");
		
		System.out.println("Success? " + success);
		Thread.sleep(1000);

		inter.disconnectFromHost();
		Thread.sleep(1000);
		
		System.out.println("Done.");		
	}
	
}
