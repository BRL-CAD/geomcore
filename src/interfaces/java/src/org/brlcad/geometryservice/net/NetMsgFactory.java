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
/** @file NetMsgFactory.java
 *
 */
package org.brlcad.geometryservice.net;

import org.brlcad.geometryservice.net.msg.AbstractNetMsg;
import org.brlcad.geometryservice.net.msg.NetMsgTypes;
import org.brlcad.geometryservice.net.msg.NewSessionReqMsg;
import org.brlcad.geometryservice.net.msg.RemoteNodeNameSetMsg;
import org.brlcad.geometryservice.net.msg.SessionInfoMsg;

public final class NetMsgFactory {

	public static AbstractNetMsg makeMsg(short type, ByteBufferReader reader) {
		try {
			switch (type) {
			case NetMsgTypes.RUAlive:
				return null;
			case NetMsgTypes.IMAlive:
				return null;
			case NetMsgTypes.Failure:
				return null;
			case NetMsgTypes.Success:
				return null;
			case NetMsgTypes.Ping:
				return null;
			case NetMsgTypes.Pong:
				return null;
			case NetMsgTypes.RemNodeNameSET:
				return new RemoteNodeNameSetMsg(reader);
			case NetMsgTypes.DisconnectREQ:
				return null;
			case NetMsgTypes.NewNodeOnNet:
				return null;
			case NetMsgTypes.FullNodenameListREQ:
				return null;
			case NetMsgTypes.FullNodenameList:
				return null;
			case NetMsgTypes.NewSessionREQ:
				return new NewSessionReqMsg(reader);
			case NetMsgTypes.SessionInfo:
				return new SessionInfoMsg(reader);
			case NetMsgTypes.GeometryREQ:
				return null;
			case NetMsgTypes.GeometryMANIFEST:
				return null;
			case NetMsgTypes.GeometryCHUNK:
				return null;

			default:
				return null;
			}

		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}
}
