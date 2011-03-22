/*
** An integer can appear in the bag at most once.
** Integers must be positive.
*/
typedef struct PQueue PQueue;
struct PQueue {
  int cnt;   /* Number of entries in the queue */
  int sz;    /* Number of slots in a[] */
  struct QueueElement {
    int id;          /* ID of the element */
    void *p;         /* Content pointer */
    double value;    /* Value of element.  Kept in ascending order */
  } *a;
};

void pqueue_init(PQueue *p);
void pqueue_insert(struct vcs_db *db, PQueue *p, int e, double v, void *pData);
int pqueue_extract(PQueue *p, void **pp);
void pqueue_clear(PQueue *p);

