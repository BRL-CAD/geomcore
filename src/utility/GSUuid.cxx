/*                    G S U U I D . C X X
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
/** @file GSUuid.cxx
 *
 * cheezy uuid wrapper
 *
 */

#include "uuid.h"
#include "GSUuid.h"

GSUuid::GSUuid()
{
	uuid_create((uuid_t **)&this->uuid);
	uuid_make((uuid_t *)this->uuid, UUID_MAKE_V4);
}

GSUuid::GSUuid(GSUuid* src)
{
	uuid_clone((uuid_t *)src->uuid, (uuid_t **)&this->uuid);
}

GSUuid::GSUuid(std::string* str)
{
	uuid_create((uuid_t **)&this->uuid);
	uuid_import((uuid_t *)this->uuid, UUID_FMT_STR, (void *)str->c_str(), str->length());
}

GSUuid::~GSUuid()
{
	uuid_destroy((uuid_t*)this->uuid);
}

std::string*
GSUuid::toString()
{
	char *buf = NULL;
	size_t len = 0;
	uuid_export((uuid_t *)this->uuid, UUID_FMT_STR, &buf, &len);
	std::string* out = new std::string(buf,len);
	free(buf);
	return out;
}

GSUuid *
GSUuid::createUuid()
{
	return new GSUuid();
}

bool
GSUuid::equals(GSUuid* other)
{
	int eh;
	uuid_compare((uuid_t*)this->uuid, (uuid_t*)other->uuid, &eh);
	return eh == 0 ? true : false;
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
