char *htmlize(struct vcs_db *db, const char *zIn, int n);
char *httpize(struct vcs_db *db, const char *z, int n);
char *urlize(struct vcs_db *db, const char *z, int n);
char *fossilize(struct vcs_db *db, const char *zIn, int nIn);
int dehttpize(char *z);
void defossilize(char *z);
int validate16(const char *zIn, int nIn);
