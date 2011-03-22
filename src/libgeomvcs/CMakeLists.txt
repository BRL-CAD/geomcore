SET(geomvcs_SRCS
	sqlite3.c
	basic.c
	schema.c
	blob.c
	printf.c
	encode.c
	diff.c
	bag.c
	pqueue.c
	descendants.c
	sha1.c
	md5.c
	file.c
	manifest.c
	content.c
	db.c
	)

add_library(geomvcs ${geomvcs_SRCS})
