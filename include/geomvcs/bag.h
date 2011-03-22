/*
** An integer can appear in the bag at most once.
** Integers must be positive.
**
** On a hash collision, search continues to the next slot in the array,
** looping back to the beginning of the array when we reach the end.
** The search stops when a match is found or upon encountering a 0 entry.
**
** When an entry is deleted, its value is changed to -1.
**
** Bag.cnt is the number of live entries in the table.  Bag.used is
** the number of live entries plus the number of deleted entries.  So
** Bag.used>=Bag.cnt.  We want to keep Bag.used-Bag.cnt as small as
** possible.
**
** The length of a search increases as the hash table fills up.  So
** the table is enlarged whenever Bag.used reaches half of Bag.sz.  That
** way, the expected collision length never exceeds 2.
*/
typedef struct Bag Bag;
struct Bag {
  int cnt;   /* Number of integers in the bag */
  int sz;    /* Number of slots in a[] */
  int used;  /* Number of used slots in a[] */
  int *a;    /* Hash table of integers that are in the bag */
};

void bag_init(Bag *p);
int bag_insert(struct vcs_db *db, Bag *p, int e);
int bag_count(Bag *p);
int bag_first(Bag *p);
void bag_remove(struct vcs_db *db, Bag *p, int e);
void bag_clear(Bag *p);
int bag_find(Bag *p, int e);
