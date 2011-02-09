/*                A P P L A U N C H E R . H
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
/** @file AppLauncher.ch
 *
 * Launchpad for applications.
 *
 */

#ifndef __APPLAUNCHER_H__
#define __APPLAUNCHER_H__

#include "BaseApp.h"

#include <QtCore/QCoreApplication>
#include <string>
#include <QtCore/QUuid>

class AppLauncher : public QCoreApplication
{
public:
    AppLauncher(int& argc, char** argv, BaseApp* app);
    virtual ~AppLauncher();
    int exec();
    static void exit(int exitCode = 0);

private:
    BaseApp* _app;

	/* Disable copy cstr and =operator */
    AppLauncher(AppLauncher const&){};
    AppLauncher& operator=(AppLauncher const&){};
};

#endif /* __APPLAUNCHER_H__ */

/*
 * Local Variables: ***
 * mode: C++ ***
 * tab-width: 8 ***
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t ***
 * End: ***
 * ex: shiftwidth=2 tabstop=8
*/
