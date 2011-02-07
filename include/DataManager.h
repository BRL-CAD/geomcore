/*             D A T A M A N A G E R . C X X
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
/** @file DataManager.cxx
 *
 * Single point of access for all Database
 * object IO from SVN, Cache and Disk
 *
 */

#ifndef __DATAMANAGER_H__
#define __DATAMANAGER_H__

#include "INetMsgHandler.h"
#include "IDataSource.h"
#include "GeometryReqMsg.h"
#include "GeometryChunkMsg.h"
#include "Logger.h"

#include <QtCore/QString>
#include <QtCore/QUuid>
#include <QtCore/QMutex>

class DataManager :  public INetMsgHandler
{
public:
	static DataManager* getInstance();
	virtual ~DataManager();
    bool handleNetMsg(NetMsg* msg);

	QString getDbObjectByURL(QString url);
	QString getDbObjectByUUID(QUuid& uuid);

	void addDataSource(IDataSource* source);

private:
	static DataManager* pInstance;
	DataManager();

	Logger* log;
	QMutex sourceLock;
	QList<IDataSource*> datasources;

	void handleGeometryReqMsg(GeometryReqMsg* msg);
	void handleGeometryChunkMsg(GeometryChunkMsg* msg);

	/* Disable copy cstr and =operator */
	DataManager(DataManager const&){};
	DataManager& operator=(DataManager const&){};
};

#endif /* __DATAMANAGER_H__ */

/*
 * Local Variables: ***
 * mode: C++ ***
 * tab-width: 8 ***
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t ***
 * End: ***
 * ex: shiftwidth=2 tabstop=8
*/
