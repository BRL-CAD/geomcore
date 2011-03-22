void md5sum_init(void);
void md5sum_step_text(const char *zText, int nBytes);
char *md5sum_finish(struct vcs_db *db, Blob *pOut);
int md5sum_blob(struct vcs_db *db, const Blob *pIn, Blob *pCksum);
