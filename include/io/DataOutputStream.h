
/*           D A T A O U T P U T S T R E A M . H
 * BRL-CAD
 *
 * Copyright (c) 1997-2008 United States Government as represented by
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

/** @file DataOutputStream.h
 *
 *  Description -
 *      
 *
 *  Author - David Loman
 *
 */

#ifndef _DATAOUTPUTSTREAM_H_
#define _DATAOUTPUTSTREAM_H_

#ifdef __cplusplus


#include "io/DataOutput.h"
#include "io/FilterOutputStream.h"

namespace ibme {
	namespace io {
		
		class DataOutputStream : public FilterOutputStream, public DataOutput
		{

		protected:
			size_t written;

		public:
			DataOutputStream(OutputStream& out);
			virtual ~DataOutputStream();

			size_t size() const throw ();

			virtual void write(uint8_t b) throw (IOException);
			virtual void write(const uint8_t* data, size_t offset, size_t length) throw (IOException);
			virtual void write(const array<uint8_t>& b) throw (IOException);
			virtual void writeBoolean(bool v) throw (IOException);
			virtual void writeByte(uint8_t v) throw (IOException);
			virtual void writeChar(int32_t v) throw (IOException);
			virtual void writeChars(const String& s) throw (IOException);
			virtual void writeInt(int32_t v) throw (IOException);
			virtual void writeLong(int64_t v) throw (IOException);
			virtual void writeShort(int16_t v) throw (IOException);
		};
	}
}

#endif

#endif