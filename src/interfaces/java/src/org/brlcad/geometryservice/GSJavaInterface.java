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
import java.net.InetAddress;
import java.util.List;
import java.util.Map;

import org.brlcad.geometryservice.net.GSConnection;

/**
 * @author david.h.loman
 * 
 */
public class GSJavaInterface implements GeometryService {
	private GSConnection conn;

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.brlcad.geometryservice.GeometryService#estimateFootprint(java.lang
	 * .String, java.lang.String)
	 */
	@Override
	public long estimateFootprint(String geometryName, String version) throws GeometryServiceException {
		// TODO Auto-generated method stub
		return 0;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.brlcad.geometryservice.GeometryService#get(java.lang.String,
	 * java.lang.String)
	 */
	@Override
	public File get(String geometryName, String version) throws GeometryServiceException {
		// TODO Auto-generated method stub
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.brlcad.geometryservice.GeometryService#getMetadata(java.lang.String,
	 * java.lang.String)
	 */
	@Override
	public Map<String, String> getMetadata(String geometryName, String version) throws GeometryServiceException {
		// TODO Auto-generated method stub
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.brlcad.geometryservice.GeometryService#query(java.lang.Object)
	 */
	@Override
	public List<CatalogEntry> query(Object query) throws GeometryServiceException {
		// TODO Auto-generated method stub
		return null;
	}

	public boolean connectToHost(InetAddress addy, short port, String uname, String passwd) {

		if (this.conn != null) {
			GSStatics.stdErr.println("There is already a Connection present.");
			return false;
		}

		try {
			this.conn = GSConnection.connectToHost(addy, port, uname, passwd);

		} catch (Exception e) {
			GSStatics.stdErr.println(e.getMessage());
			return false;
		}

		if (this.conn == null) {
			GSStatics.stdErr.println("Null GSConnection without throwing an error... odd.");
			return false;
		}

		return true;
	}

}
