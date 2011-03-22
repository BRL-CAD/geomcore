char *vmprintf(struct vcs_db *db, const char *zFormat,va_list ap);
char *mprintf(struct vcs_db *db, const char *zFormat, ...);
int vxprintf(struct vcs_db *db, Blob *pBlob, const char *fmt, va_list ap);
