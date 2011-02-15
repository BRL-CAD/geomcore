/*                      G S U U I D . H
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
/** @file GSUuid.h
 *
 * Brief description
 *
 */

#ifndef __GSUUID_H__
#define __GSUUID_H__

#include <string>

#ifndef _UUID_T
# include <uuid.h>
#endif

class GSUuid
{
public:
  GSUuid();
  GSUuid(GSUuid*);
  GSUuid(std::string*);
  ~GSUuid();

  static GSUuid *createUuid();

  bool equals(GSUuid*);

  std::string toString();

private:
#if 0
#if defined(uuid_s_ok)
  struct uuid uuid;
#elif defined(UUID_VARIANT_DCE)
  uuid_t uuid;
#endif
#endif
};

#endif /* __GSUUID_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
