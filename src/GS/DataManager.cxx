/*             D A T A M A N A G E R . C X X
 * BRL-CAD
 *
 * Copyright (c) 2010 United States Government as represented by
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

#include "DataManager.h"
#include "libnet.h"

#include <QtCore/QMutexLocker>

DataManager* DataManager::pInstance = NULL;

DataManager::DataManager()
{
	this->log = Logger::getInstance();
}

DataManager::~DataManager()
{}

QString DataManager::getDbObjectByURL(QString url)
{}

QString DataManager::getDbObjectByUUID(QUuid& uuid)
{}

void
DataManager::addDataSource(IDataSource* source)
{
	QMutexLocker lock (&this->sourceLock);
	this->datasources.append(source);
}

bool
DataManager::handleNetMsg(NetMsg* msg)
{
	quint16 type = msg->getMsgType();
	switch(type) {
	case GEOMETRYREQ:
		this->handleGeometryReqMsg((GeometryReqMsg*)msg);
		return true;
	case GEOMETRYMANIFEST:
		return true;
	case GEOMETRYCHUNK:
		this->handleGeometryChunkMsg((GeometryChunkMsg*)msg);
	return true;
	}
	return false;
}

void
DataManager::handleGeometryChunkMsg(GeometryChunkMsg* msg)
{
	Portal* origin = msg->getOrigin();

	//validate incoming data
	if (origin == 0) {
		//TODO Figure out how to how to handle NULL Portal
		log->logERROR("DataManager", "handleGeometryChunkMsg(): NULL Portal!");
		return;
	}
}

void
DataManager::handleGeometryReqMsg(GeometryReqMsg* msg)
{
	quint8 reqType = msg->getRequestType();
	QString data = msg->getData();
	Portal* origin = msg->getOrigin();

	//validate incoming data
	if (origin == 0) {
		//TODO Figure out how to how to handle NULL Portal
		log->logERROR("DataManager", "handleGeometryReqMsg(): NULL Portal!");
		return;
	}
	if (data.length() == 0) {
		TypeOnlyMsg* tom = new TypeOnlyMsg(BAD_REQUEST, msg);
		origin->send(tom);
		return;
	}

	if (reqType == REQ_BY_PATH){
		//TODO remove hardcoded FileDataSource
		if (this->datasources.length() > 0) {
			IDataSource* ds = this->datasources.at(0);

			DbObject* obj = ds->getByPath(data);

			if (obj == NULL) {
				TypeOnlyMsg* tom = new TypeOnlyMsg(COULD_NOT_FIND_GEOMETRY, msg);
				origin->send(tom);
				return;
			}

			QList<QString> items;
			QByteArray* data = obj->getData();

			GeometryChunkMsg* chunk = new GeometryChunkMsg(data->data(), data->length());
			items.append(obj->getPath());

			GeometryManifestMsg* manifest = new GeometryManifestMsg(items);
			origin->send(manifest);

			origin->send(chunk);
			return;

		} else {
			TypeOnlyMsg* tom = new TypeOnlyMsg(OPERATION_NOT_AVAILABLE, msg);
			origin->send(tom);
			return;
		}

	} else if (reqType == REQ_BY_UUID) {
		//Not implemented!!!
		TypeOnlyMsg* tom = new TypeOnlyMsg(OPERATION_NOT_AVAILABLE, msg);
		origin->send(tom);
		return;
	} else {
		//Not implemented!!!
		TypeOnlyMsg* tom = new TypeOnlyMsg(OPERATION_NOT_AVAILABLE, msg);
		origin->send(tom);
		return;
	}
}

DataManager* DataManager::getInstance()
{
	if (!DataManager::pInstance)
	{
		DataManager::pInstance = new DataManager();
	}
	return DataManager::pInstance;
}

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
