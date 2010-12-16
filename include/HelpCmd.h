/*                       H E L P C M D . H
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
/** @file HelpCmd.h
 * HelpCmd.h
 *
 *  Created on: Dec 16, 2010
 */

#ifndef __HELPCMD_H__
#define __HELPCMD_H__

#include "AbstractClientCmd.h"
#include <QtCore/QString>

class HelpCmd: public AbstractClientCmd {
public:
	HelpCmd();
	virtual ~HelpCmd();

	QString getUsage();
	QString getHelp();

protected:
	bool _exec(GSClient* client, QStringList args);
};

#endif /* __HELPCMD_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
