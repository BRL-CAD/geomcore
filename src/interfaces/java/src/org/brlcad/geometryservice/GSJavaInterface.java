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
/** @file GSJavaInterface.java
 *
 */
package org.brlcad.geometryservice;

import java.io.File;
import java.net.BindException;
import java.net.ConnectException;
import java.net.InetAddress;
import java.net.NoRouteToHostException;
import java.net.PortUnreachableException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.brlcad.geometryservice.net.GSConnection;
import org.brlcad.geometryservice.net.GSConnection.GSConnectionManipulator;

public class GSJavaInterface implements GeometryService {
	private GSConnection conn;

	@Override
	public long estimateFootprint(String geometryName, String version) throws GeometryServiceException {
		throw new GeometryServiceException("Not Implementeded.");
	}

	@Override
	public File get(String geometryName, String version) throws GeometryServiceException {
		throw new GeometryServiceException("Not Implementeded.");
	}

	@Override
	public Map<String, String> getMetadata(String geometryName, String version) throws GeometryServiceException {
		throw new GeometryServiceException("Not Implementeded.");
	}

	@Override
	public List<CatalogEntry> query(Object query) throws GeometryServiceException {
		throw new GeometryServiceException("Not Implementeded.");
	}
	
	//TODO this should be part of the GeometryService java Interface
	public String getRemHostName()
	{
		if (this.conn != null){
			return this.conn.getRemoteNodename();
		}
		return "Not Connected.";	
	}

	//TODO this should be part of the GeometryService java Interface
	public UUID getSessionID()
	{
		if (this.conn != null){
			return this.conn.getSessionID();
		}
		return null;
	}
	
	//TODO this should be part of the GeometryService java Interface
	public ArrayList<String> getList(String path)
	{		
		return new ArrayList<String>();
	}
	
	/**
	 * 
	 * @param addy
	 * @param port
	 * @param uname
	 * @param passwd
	 * @return Error codes:
	 *  		0 = Okay
	 */
	public int connectToHost(InetAddress addy, short port, String uname, String passwd) {
		if (this.conn != null) {
			GSStatics.stdErr.println("There is already a Connection present.");
			return -1;
		}
		
		GSConnectionManipulator manip = new GSConnectionManipulator();
		GSConnection.connectToHost(addy, port, uname, passwd, manip);
		
		if (manip.getRetVal() < 1) {
			/* Something happened during the connect/handshake/auth process. */
			GSStatics.stdErr.println("Failure on connect:" + manip.getRetStr());
			return -2;
		}
		
		this.conn = manip.getConn();
		
		if (this.conn == null) {
			GSStatics.stdErr.println("Null GSConnection without throwing an error... odd.");
			return -3;
		}

		
		return 1;
	}

	public void disconnectFromHost()
	{
		if (this.conn != null){
			this.conn.disconnect();
			this.conn.stopReceiving();
			this.conn = null;
		}
	}
}
