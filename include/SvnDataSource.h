/*                 S V N D A T A S O U R C E . H
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
/** @file SvnDataSource.h
 *
 * Brief description
 *
 */

#ifndef __SVNDATASOURCE_H__
#define __SVNDATASOURCE_H__

#include "IDataSource.h"

#include <string>
#include <list>

#include <GSThread.h>

class SvnDataSource : public IDataSource
{
public:
	SvnDataSource(std::string repoPath);
	virtual ~SvnDataSource();

	/* Get a single BRLCAD::Object */
	BRLCAD::Object* getObj(std::string path);

	/* Get a single Attribute of an object */
	prop* getAttr(std::string path, std::string attrKey);

	/* Get a set of objects */
	std::list<BRLCAD::Object*>* getObjs(std::string path);

	/* Get all Attributes from object */
	std::list<prop>* getAttrs(std::string path);

private:
	std::string repoPath;
};

#endif /* __SVNDATASOURCE_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
