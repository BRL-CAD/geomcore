/* Definitions for Schema */
#define INTERFACE 0
#define EXPORT_INTERFACE 0
extern const char zLocalSchema[];
extern const char zConfigSchema[];
extern const char zRepositorySchema2[];
extern const char zRepositorySchema1[];
#define MAX_INT_TAG    16    /* The largest pre-assigned tag id */
#define TAG_PARENT     10    /* Change to parentage on a checkin */
#define TAG_CLOSED     9     /* Do not display this check-in as a leaf */
#define TAG_BRANCH     8     /* Value is name of the current branch */
#define TAG_CLUSTER    7     /* A cluster */
#define TAG_PRIVATE    6     /* Display but do not sync */
#define TAG_HIDDEN     5     /* Do not display or sync */
#define TAG_DATE       4     /* The date of a check-in */
#define TAG_USER       3     /* User who made a checking */
#define TAG_COMMENT    2     /* The check-in comment */
#define TAG_BGCOLOR    1     /* Set the background color for display */
#define AUX_SCHEMA      "2011-02-25 14:52"
#define CONTENT_SCHEMA  "1"
