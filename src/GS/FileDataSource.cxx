/*              F I L E D A T A S O U R C E . C X X
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
/** @file FileDataSource.cxx
 *
 * Brief description
 *
 */

#include "FileDataSource.h"
#include "GSThread.h"

#include <QtCore/QMutexLocker>
#include <QtCore/QFile>

FileDataSource::FileDataSource(std::string repoPath) :
	repoPath(repoPath)
{}

FileDataSource::~FileDataSource()
{}

bool
FileDataSource::lock(DbObject* obj, Account* a)
{}

bool
FileDataSource::hasLock(DbObject* obj, Account* a)
{}

bool
FileDataSource::unlock(DbObject* obj)
{}

DbObject*
FileDataSource::getByPath(std::string path)
{
	//See if there is a file lock on this path
	bool hasLock = this->hasPathLock(path);

	//TODO failsafe this loop!
	while (hasLock) {
		GSThread::msleep(567);
		hasLock = this->hasPathLock(path);
	}

	//lock it
	this->setPathLock(path);

	std::string qs(this->repoPath);
	qs += "/";
	qs += path;
	QFile f(QString(qs.c_str()));

	if (f.exists()) {
		QByteArray* data = new QByteArray(f.readAll());
		DbObject* object = new DbObject(path, data);

		f.close();

		//unlock it
		this->remPathLock(path);

		return object;
	}

	f.close();

	//unlock it
	this->remPathLock(path);

	return NULL;
}

DbObject*
FileDataSource::getByID(QUuid id)
{
	return NULL;
}

bool
FileDataSource::putObject(DbObject* obj)
{
	std::string path = obj->getPath();

	//See if there is a file lock on this path
	bool hasLock = this->hasPathLock(path);

	//TODO failsafe this loop!
	while (hasLock) {
		GSThread::msleep(567);
		hasLock = this->hasPathLock(path);
	}

	//lock it
	this->setPathLock(path);

	std::string s(this->repoPath);
	s += "/";
	s += path;
	QFile f(QString(s.c_str()));
	f.write(*obj->getData());

	f.close();

	//unlock it
	this->remPathLock(path);

	return true;
}

bool
FileDataSource::hasPathLock(std::string path)
{
	QMutexLocker(&this->lockLock);
	for(std::list<std::string>::iterator it = this->pathLocks.begin(); it!=this->pathLocks.end(); it++)
		if( *it == path )
			return true;
	return false;
}

void
FileDataSource::setPathLock(std::string path)
{
	QMutexLocker(&this->lockLock);
	if (!this->hasPathLock(path))
		this->pathLocks.push_back(path);
}

void
FileDataSource::remPathLock(std::string path) {
	QMutexLocker(&this->lockLock);
	this->pathLocks.push_back(path);
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
