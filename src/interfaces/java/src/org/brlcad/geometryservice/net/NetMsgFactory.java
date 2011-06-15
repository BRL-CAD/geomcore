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

import org.brlcad.geometryservice.GSStatics;
import org.brlcad.geometryservice.net.msg.AbstractNetMsg;
import org.brlcad.geometryservice.net.msg.DirListReqMsg;
import org.brlcad.geometryservice.net.msg.DirListResMsg;
import org.brlcad.geometryservice.net.msg.FailureMsg;
import org.brlcad.geometryservice.net.msg.GeometryChunkMsg;
import org.brlcad.geometryservice.net.msg.GeometryManifestMsg;
import org.brlcad.geometryservice.net.msg.GeometryReqMsg;
import org.brlcad.geometryservice.net.msg.NetMsgTypes;
import org.brlcad.geometryservice.net.msg.NewNodeOnNetMsg;
import org.brlcad.geometryservice.net.msg.NewSessionReqMsg;
import org.brlcad.geometryservice.net.msg.PingMsg;
import org.brlcad.geometryservice.net.msg.PongMsg;
import org.brlcad.geometryservice.net.msg.RemoteNodeNameSetMsg;
import org.brlcad.geometryservice.net.msg.SessionInfoMsg;
import org.brlcad.geometryservice.net.msg.SuccessMsg;
import org.brlcad.geometryservice.net.msg.TypeOnlyMsg;

public final class NetMsgFactory {

	public static AbstractNetMsg makeMsg(short type, ByteBufferReader reader) {
		try {
			switch (type) {
			case NetMsgTypes.RUAlive:
				return new TypeOnlyMsg(type, reader);
			case NetMsgTypes.IMAlive:
				return new TypeOnlyMsg(type, reader);
			case NetMsgTypes.Failure:
				return new FailureMsg(reader);
			case NetMsgTypes.Success:
				return new SuccessMsg(reader);
			case NetMsgTypes.Ping:
				return new PingMsg(reader);
			case NetMsgTypes.Pong:
				return new PongMsg(reader);
			case NetMsgTypes.RemNodeNameSET:
				return new RemoteNodeNameSetMsg(reader);
			case NetMsgTypes.DisconnectREQ:
				return new TypeOnlyMsg(type, reader);
			case NetMsgTypes.NewNodeOnNet:
				return new NewNodeOnNetMsg(reader);
				
			case NetMsgTypes.DirListREQ:
				return new DirListReqMsg(reader);
			case NetMsgTypes.DirListRES:
				return new DirListResMsg(reader);
				
			case NetMsgTypes.NewSessionREQ:
				return new NewSessionReqMsg(reader);
			case NetMsgTypes.SessionInfo:
				return new SessionInfoMsg(reader);
			case NetMsgTypes.GeometryREQ:
				return new GeometryReqMsg(reader);
			case NetMsgTypes.GeometryMANIFEST:
				return new GeometryManifestMsg(reader);
			case NetMsgTypes.GeometryCHUNK:
				return new GeometryChunkMsg(reader);

			default:
				return null;
			}

		} catch (Exception e) {
			GSStatics.stdErr.println("NetMsgFactory::makeMsg(): " + e.getClass().getSimpleName() + ":" + e.getMessage());
			return null;
		}
	}
}
