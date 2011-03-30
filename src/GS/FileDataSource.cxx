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
 */

#include "FileDataSource.h"
#include "brlcad/MemoryDatabase.h"


FileDataSource::FileDataSource(std::string repoPath)
:   repoPath(repoPath)
{}


FileDataSource::~FileDataSource()
{}


/* Get a single BRLCAD::Object */
BRLCAD::Object*
FileDataSource::getObj(std::string path)
{
    BRLCAD::MemoryDatabase md;

    std::string fullPath(this->repoPath + "/" + path);

    if (!md.Load(fullPath.c_str()))
	return NULL;

    BRLCAD::ConstDatabase::TopObjectIterator it = md.FirstTopObject();
    BRLCAD::Object* obj = NULL;

    if (it.Good()) {
	obj = md.Get(it.Name());
    }

    return obj;
}


/* Get a single Attribute of an object */
prop*
FileDataSource::getAttr(std::string path, std::string attrKey)
{
    return NULL;
}


/* Get a set of objects */
std::list<BRLCAD::Object*>*
FileDataSource::getObjs(std::string path)
{
    BRLCAD::MemoryDatabase md;

    std::string fullPath(this->repoPath + "/" + path);

    if (!md.Load(fullPath.c_str()))
	return NULL;

    BRLCAD::ConstDatabase::TopObjectIterator it = md.FirstTopObject();
    BRLCAD::Object* obj = NULL;

    std::list<BRLCAD::Object*>* objs = new std::list<BRLCAD::Object*>();

    while (it.Good()) {
	objs->push_back(md.Get(it.Name()));
	++it;
    }
    return objs;
}


/* Get all Attributes from object */
std::list<prop>*
FileDataSource::getAttrs(std::string path)
{
    return NULL;
}


/* Put a single BRLCAD::Object */
bool
FileDataSource::putObj(std::string path, BRLCAD::Object& obj)
{
    std::string fullPath(this->repoPath + "/" + path);

    BRLCAD::MemoryDatabase md;

    //Try to load
    md.Load(fullPath.c_str());

    if (!md.Add(obj))
	return false;

    if (!md.Save(fullPath.c_str()))
	return false;

    return true;
}


bool
FileDataSource::init()
{
    //first check to see if there is a repo at the supplied path, and if we can R/W to it.

    /* 0 == exists */
    if (bu_file_readable(this->repoPath.c_str()) != 0)
	return false;
    Logger::getInstance()->logINFO("FileDataSource", this->repoPath + " is readable.");

    if (bu_file_writable(this->repoPath.c_str()) != 0)
	return false;
    Logger::getInstance()->logINFO("FileDataSource", this->repoPath + " is writable.");

    return true;
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
