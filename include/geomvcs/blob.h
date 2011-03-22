/*
** A Blob can hold a string or a binary object of arbitrary size.  The
** size changes as necessary.
*/
typedef struct Blob Blob;
struct Blob {
  unsigned int nUsed;            /* Number of bytes used in aData[] */
  unsigned int nAlloc;           /* Number of bytes allocated for aData[] */
  unsigned int iCursor;          /* Next character of input to parse */
  char *aData;                   /* Where the information is stored */
  void (*xRealloc)(struct vcs_db *db, Blob*, unsigned int); /* Function to reallocate the buffer */
};

/*
** The current size of a Blob
*/
#define blob_size(X)  ((X)->nUsed)

/*
** The buffer holding the blob data
*/
#define blob_buffer(X)  ((X)->aData)

/*
** Seek whence parameter values
*/
#define BLOB_SEEK_SET 1
#define BLOB_SEEK_CUR 2
#define BLOB_SEEK_END 3

/*
** An initializer for Blobs
*/
#define BLOB_INITIALIZER  {0,0,0,0,blobReallocMalloc}

/*
** This macro compares a blob against a string constant.  We use the sizeof()
** operator on the string constant twice, so it really does need to be a
** string literal or character array - not a character pointer.
*/
# define blob_eq(B,S) \
     ((B)->nUsed==sizeof(S)-1 && memcmp((B)->aData,S,sizeof(S)-1)==0)

int geomvcs_isspace(char c);
int geomvcs_isalnum(char c);
void blob_append(struct vcs_db *db, Blob *pBlob, const char *aData, int nData);
void blob_appendf(struct vcs_db *db, Blob *pBlob, const char *zFormat, ...);
void blob_vappendf(struct vcs_db *db, Blob *pBlob, const char *zFormat, va_list ap);
void blob_zero(Blob *pBlob);
void blob_reset(struct vcs_db *db, Blob *pBlob);
char *blob_materialize(struct vcs_db *db, Blob *pBlob);
void blobReallocMalloc(struct vcs_db *db, Blob *pBlob, unsigned int newSize);
void blob_init(Blob *pBlob, const char *zData, int size);
char *blob_str(struct vcs_db *db, Blob *p);
int blob_read_from_channel(struct vcs_db *db, Blob *pBlob, FILE *in, int nToRead);
void blob_resize(struct vcs_db *db, Blob *pBlob, unsigned int newSize);
int blob_read_from_file(struct vcs_db *db, Blob *pBlob, const char *zFilename);
int blob_compare(Blob *pA, Blob *pB);
void blob_set(Blob *pBlob, const char *zStr);
int blob_is_reset(Blob *pBlob);
int blob_uncompress(struct vcs_db *db, Blob *pIn, Blob *pOut);
void blob_copy(struct vcs_db *db, Blob *pTo, Blob *pFrom);
int blob_write_to_file(struct vcs_db *db, Blob *pBlob, const char *zFilename);
void blob_remove_cr(struct vcs_db *db, Blob *p);
int blob_line(Blob *pFrom, Blob *pTo);
