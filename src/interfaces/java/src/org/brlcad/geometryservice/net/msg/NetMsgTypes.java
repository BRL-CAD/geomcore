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
/** @file NetMsgTypes.java
 *
 */
package org.brlcad.geometryservice.net.msg;

public final class NetMsgTypes {

	public static final short Failure = 0x0050;
	public static final short Success = 0x0051;
	public static final short RemHostNameSET = 0x0100;
	public static final short DisconnectREQ = 0x0150;
	public static final short NewHostOnNet = 0x0200;
	public static final short FullHostListREQ = 0x0250;
	public static final short FullHostList = 0x0255;
	public static final short NewSessionREQ = 0x0300;
	public static final short NewSession = 0x0305;
	public static final short LogoutSession = 0x0310;
	public static final short GeometryREQ = 0x0400;
	public static final short GeometryMANIFEST = 0x0405;
	public static final short GeometryCHUNK = 0x0410;

}
