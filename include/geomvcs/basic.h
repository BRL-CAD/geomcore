void geomvcs_exit(struct vcs_db *db, int rc);
void geomvcs_panic(struct vcs_db *db, const char *zFormat, ...);
void geomvcs_fatal(struct vcs_db *db, const char *zFormat, ...);
void geomvcs_fatal_recursive(struct vcs_db *db, const char *zFormat, ...);
void geomvcs_warning(struct vcs_db *db, const char *zFormat, ...);
void *geomvcs_malloc(struct vcs_db *db, size_t n);
void geomvcs_free(void *p);
void *geomvcs_realloc(struct vcs_db *db, void *p, size_t n);
int geomvcs_strcmp(const char *zA, const char *zB);
void geomvcs_sqlite_log(struct vcs_db *db, void *notUsed, int iCode, const char *zErrmsg);
static void multi_column_list(const char **azWord, int nWord);
int geomvcs_isspace(char c);
int geomvcs_isalnum(char c);

