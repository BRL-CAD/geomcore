package org.brlcad.geometryservice.net;

import org.brlcad.geometryservice.net.msg.AbstractNetMsg;
import org.brlcad.geometryservice.net.msg.NetMsgTypes;

public final class NetMsgFactory {

	public static AbstractNetMsg makeMsg(short type, ByteBufferReader reader) {

		switch (type) {
		case NetMsgTypes.Failure:
			return null;
		case NetMsgTypes.Success:
			return null;
		case NetMsgTypes.RemHostNameSET:
			return null;
		case NetMsgTypes.DisconnectREQ:
			return null;
		case NetMsgTypes.NewHostOnNet:
			return null;
		case NetMsgTypes.FullHostListREQ:
			return null;
		case NetMsgTypes.FullHostList:
			return null;
		case NetMsgTypes.NewSessionREQ:
			return null;
		case NetMsgTypes.NewSession:
			return null;
		case NetMsgTypes.LogoutSession:
			return null;
		case NetMsgTypes.GeometryREQ:
			return null;
		case NetMsgTypes.GeometryMANIFEST:
			return null;
		case NetMsgTypes.GeometryCHUNK:
			return null;

		default:
			return null;
		}
	}
}
