// MD5.CC - source code for the C++/object oriented translation and 
//          modification of MD5.

// Translation and modification (c) 1995 by Mordechai T. Abzug 

// This translation/ modification is provided "as is," without express or 
// implied warranty of any kind.

// The translator/ modifier does not claim (1) that MD5 will do what you think 
// it does; (2) that this translation/ modification is accurate; or (3) that 
// this software is "merchantible."  (Language for this disclaimer partially 
// copied from the disclaimer below).

/* based on:

   MD5.H - header file for MD5C.C
   MDDRIVER.C - test driver for MD2, MD4 and MD5

   Copyright (C) 1991-2, RSA Data Security, Inc. Created 1991. All
rights reserved.

License to copy and use this software is granted provided that it
is identified as the "RSA Data Security, Inc. MD5 Message-Digest
Algorithm" in all material mentioning or referencing this software
or this function.

License is also granted to make and use derivative works provided
that such works are identified as "derived from the RSA Data
Security, Inc. MD5 Message-Digest Algorithm" in all material
mentioning or referencing the derived work.

RSA Data Security, Inc. makes no representations concerning either
the merchantability of this software or the suitability of this
software for any particular purpose. It is provided "as is"
without express or implied warranty of any kind.

These notices must be retained in any copies of any part of this
documentation and/or software.

*/

#ifndef MOCHA_MD5_H
#define MOCHA_MD5_H

#include <stdlib.h>

#include "Mocha/Platform.h"

namespace Mocha
{

//--------------------------------
class Stream;

//--------------------------------
class MD5Digest
{
public:
	Mocha::uint8 data[16];

	inline MD5Digest( )
	{
		memset( data, 0, sizeof( data ) );
	}

	inline MD5Digest( const MD5Digest& vOther )
	{
		memcpy( data, vOther.data, sizeof( data ) );
	}

	inline MD5Digest& operator = ( const MD5Digest& vOther )
	{
		memcpy( data, vOther.data, sizeof( data ) );
		return *this;
	}

	bool operator == ( const MD5Digest& vOther ) const
	{
		return memcmp( data, vOther.data, sizeof( data ) ) == 0;
	}

	bool operator != ( const MD5Digest& vOther ) const
	{
		return memcmp( data, vOther.data, sizeof( data ) ) != 0;
	}

	String getString( ) const;
};

//--------------------------------
class MOCHA_EXPORT MD5
{
private:

	// first, some types:

	typedef unsigned       int uint4; // assumes integer is 4 words long

	typedef unsigned short int uint2; // assumes short integer is 2 words long

	typedef unsigned      char uint1; // assumes char is 1 word long

	// next, the private data:

	uint4 state[4];

	uint4 count[2];     // number of *bits*, mod 2^64

	uint1 buffer[64];   // input buffer

	MD5Digest mDigest;

	uint1 finalized;

	// last, the private methods, mostly static:

	void init             ();               // called by all constructors

	void transform        (const uint1 *buffer);  // does the real update work.  Note that length is implied to be 64.

	static void encode    (uint1 *dest, const uint4 *src, uint4 length);

	static void decode    (uint4 *dest, const uint1 *src, uint4 length);

	static void memcpy    (uint1 *dest, const uint1 *src, uint4 length);

	static void memset    (uint1 *start, uint1 val, uint4 length);

	static inline uint4  rotate_left (uint4 x, uint4 n);

	static inline uint4  F           (uint4 x, uint4 y, uint4 z);

	static inline uint4  G           (uint4 x, uint4 y, uint4 z);

	static inline uint4  H           (uint4 x, uint4 y, uint4 z);

	static inline uint4  I           (uint4 x, uint4 y, uint4 z);

	static inline void   FF  (uint4& a, uint4 b, uint4 c, uint4 d, uint4 x, uint4 s, uint4 ac);

	static inline void   GG  (uint4& a, uint4 b, uint4 c, uint4 d, uint4 x, uint4 s, uint4 ac);

	static inline void   HH  (uint4& a, uint4 b, uint4 c, uint4 d, uint4 x, uint4 s, uint4 ac);

	static inline void   II  (uint4& a, uint4 b, uint4 c, uint4 d, uint4 x, uint4 s, uint4 ac);

public:
	MD5( );

	~MD5( );

	void update( const Mocha::uint8* vInput, size_t vInputLength );

	void update( const Mocha::Stream& vStream );

	void update( const Mocha::String& vText );

	void finalize( );

	const MD5Digest& getDigest( );

	Mocha::String hex_digest( );
};

}

#endif
