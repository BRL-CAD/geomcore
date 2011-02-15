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

#include <fcntl.h>
#include <sys/stat.h>

#include <QtCore/QMutexLocker>

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
	char name[BUFSIZ];
	int f;
	DbObject *object = NULL;

	//TODO failsafe this loop!
	while (hasLock) {
		usleep(567000);
		hasLock = this->hasPathLock(path);
	}

	//lock it
	this->setPathLock(path);

	snprintf(name, BUFSIZ, "%s/%s", repoPath.c_str(), path.c_str());
	f = open(name, O_RDONLY);
	if(f != -1) {
		struct stat s;
		char *buf;

		fstat(f, &s);
		if(read(f, buf = (char *)malloc(s.st_size), s.st_size) != s.st_size) {
			fprintf(stderr, "Failed reading %s", name);
			perror("FileDataSource::getByPath");
		}
		object = new DbObject(path, new ByteArray(buf, s.st_size));
		free(buf);
		close(f);
	}

	//unlock it
	this->remPathLock(path);
	return object;
}

DbObject*
FileDataSource::getByID(GSUuid* id)
{
	return NULL;
}

bool
FileDataSource::putObject(DbObject* obj)
{
	char buf[BUFSIZ];
	int fd;

	std::string path = obj->getPath();

	//See if there is a file lock on this path
	bool hasLock = this->hasPathLock(path);

	//TODO failsafe this loop!
	while (hasLock) {
		usleep(567000);
		hasLock = this->hasPathLock(path);
	}

	//lock it
	this->setPathLock(path);

	snprintf(buf, BUFSIZ, "%s/%s", repoPath.c_str(), path.c_str());
	fd = open(buf, O_CREAT|O_WRONLY);
	write(fd, obj->getData()->data(), obj->getData()->length());
	close(fd);

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
