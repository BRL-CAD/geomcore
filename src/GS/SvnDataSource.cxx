/*               S V N D A T A S O U R C E . C X X
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
/** @file SvnDataSource.cxx
 *
 */

#include "SvnDataSource.h"

#include <fcntl.h>
#include <sys/stat.h>

#include <GSThread.h>

SvnDataSource::SvnDataSource(std::string repoPath)
:   repoPath(repoPath)
{}

SvnDataSource::~SvnDataSource()
{}

/* get a directory listing or a child list */
std::list<std::string>*
SvnDataSource::getListing(std::string path)
{
}

/* Get a set of BRLCAD::MinimalObjects */
int
SvnDataSource::getObjs(std::string path, std::list<ExtObject*>* objs, bool recurse)
{
  return -1;
}

bool
SvnDataSource::putObj(std::string path, ExtObject* ext)
{
  return false;
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
