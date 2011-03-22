void file_canonical_name(struct vcs_db *db, const char *zOrigName, Blob *pOut);
i64 file_size(const char *zFilename);
int file_simplify_name(char *z, int n);
int file_mkdir(struct vcs_db *db, const char *zName, int forceFlag);
int file_is_simple_pathname(const char *z);
const char *file_tail(const char *z);
int file_isdir(struct vcs_db *db, const char *zFilename);

