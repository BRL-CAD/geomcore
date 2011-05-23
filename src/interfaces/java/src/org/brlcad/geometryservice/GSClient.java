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
/** @file GSClient.java
 *
 */

package org.brlcad.geometryservice;

public class GSClient {


    public enum fmt { BOT, ARB, BOTARB, NURBS, OTHER };

    GSClient() {
	super();
	/* maybe this shouldn't exist? */
    }

    GSClient(String URI) {
	super();
	/* parse crud out and make a connection. Maybe use port/host, then
	 * attach the pertinent other info? */
    }

    GSClient(String host, int port) {
	super();
	/* connect to a geometry server, maybe should build a URI? */
    }

    /**
     * Get the latest revision ID of the requested object.
     */
    public String getLatestRevision() {
	return "4";
    }

    public java.util.Date getLastModified() {
	return new java.util.Date(System.currentTimeMillis());
    }

    public String getLastEditor() {
	return "Beavis";
    }

    /**
     * Get an image of the geometry (possibly raytraced)
     */
    public java.awt.image.BufferedImage getPhoto() {
	return null;
    }

    /**
     * Get the line drawing (probably rtedge)
     */
    public java.awt.image.BufferedImage getLineDrawing() {
	return null;
    }

    /**
     * Get the raw .g file data.
     */
    public java.nio.ByteBuffer getG() {
	return null;
    }

    public String getOriginator() {
	return "Somebody";
    }

    public int getFileLen() {
	return 4;
    }

    /* should this be more like an enum? number? */
    public String getLOD() {
	return "Low";
    }

    /**
     * Return the confidence in the geometry description integrity.
     */
    public double getMagicHappyFuzzyFeeling() {
	return 0.0;
    }

    public boolean getIsdirectMeasurement() {
	return true;
    }

    public String getOriginalFormat() {
	return "BRL-CAD";
    }

    /**
     * Return the name of the conversion utility used on this.
     */
    public String getConverter() {
	return "Native";
    }

    /* bot, arb, mixture of bot and arb, and ... other? nurbs? something? does
     * this even make sense? */
    public fmt getFormat() {
	return fmt.OTHER;
    }
}
