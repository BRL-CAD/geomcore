/*                  I D A T A S O U R C E . H
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
/** @file IDataSource.h
 *
 * Interface.  Defines the required methods needed
 * to be a DataSource for the DataManager.
 *
 */

#ifndef __IDATASOURCE_H__
#define __IDATASOURCE_H__

#include <brlcad/Object.h>
#include "Account.h"
#include <map>
#include <list>

//TODO should this be put into the common.h ?
typedef std::pair<std::string, std::string>* prop;

class IDataSource {
public:
	/* Init the Datasource (If needed) */
	virtual bool init() = 0;

	/*
	 * 'GET' ers
	 */

	/* Get a single bu_external */
	virtual bu_external* getObj(std::string path) = 0;

	/* Get a set of bu_externals */
	virtual std::list<bu_external*>* getObjs(std::string path) = 0;

	/*
	 * 'PUT' ers
	 */

	/* Put a single bu_external */
	virtual bool putObj(std::string path, bu_external* ext) = 0;
};


#endif /* __IDATASOURCE_H__ */

/*
 * Local Variables: ***
 * mode: C
 * tab-width: 8
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t
 * End: ***
 * ex: shiftwidth=4 tabstop=8
*/
