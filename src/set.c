#include <janet.h>

static int set_gc(void *data, size_t len) {
  (void) len;
  janet_table_deinit((JanetTable *)data);
  return 0;
}

static int set_gcmark(void *data, size_t len) {
  (void) len;
  janet_mark(janet_wrap_table((JanetTable *)data));
  return 0;
}

static void set_tostring(void *data, JanetBuffer *buffer) {
  JanetTable *set = (JanetTable *)data;
  janet_buffer_push_cstring(buffer, "{");
  int first = 1;
  for (int32_t i = 0; i < set->capacity; i++) {
    JanetKV *entry = &set->data[i];
    if (janet_checktype(entry->key, JANET_NIL)) {
      continue;
    }
    if (first) {
      first = 0;
    } else {
      janet_buffer_push_cstring(buffer, " ");
    }
    janet_pretty(buffer, 0, 0, entry->key);
  }
  janet_buffer_push_cstring(buffer, "}");
}

static Janet cfun_union(int32_t argc, Janet *argv);
static Janet cfun_difference(int32_t argc, Janet *argv);
static const JanetMethod set_methods[] = {
  {"+", cfun_union},
  {"-", cfun_difference},
  {NULL, NULL}
};

static int set_get(void *data, Janet key, Janet *out) {
  if (janet_checkint(key)) {
    JanetTable *set = (JanetTable *)data;
    int32_t index = janet_unwrap_integer(key);
    if (index < 0 || index >= set->capacity) {
      janet_panicf("set key %v out of bounds (did you mutate during iteration?)", key);
    }
    Janet element = set->data[index].key;
    if (janet_checktype(element, JANET_NIL)) {
      janet_panicf("set key %v not found (did you mutate during iteration?)", key);
    }
    *out = element;
    return 1;
  } else if (janet_checktype(key, JANET_KEYWORD)) {
    return janet_getmethod(janet_unwrap_keyword(key), set_methods, out);
  } else {
    return 0;
  }
}

static Janet set_call(void *data, int32_t argc, Janet *argv) {
  janet_fixarity(argc, 1);
  JanetTable *set = (JanetTable *)data;
  Janet value = janet_table_get(set, argv[0]);
  int key_found = !janet_checktype(value, JANET_NIL);
  return janet_wrap_boolean(key_found);
}

static Janet set_next(void *data, Janet key) {
  int32_t previous_index;
  if (janet_checktype(key, JANET_NIL)) {
    previous_index = -1;
  } else if (janet_checkint(key)) {
    previous_index = janet_unwrap_integer(key);
    if (previous_index < 0) {
      janet_panicf("set key %v cannot be negative", key);
    }
  } else {
    janet_panicf("set key %v must be an integer", key);
  }

  JanetTable *set = (JanetTable *)data;
  for (int32_t i = previous_index + 1; i < set->capacity; i++) {
    if (!janet_checktype(set->data[i].key, JANET_NIL)) {
      return janet_wrap_integer(i);
    }
  }
  return janet_wrap_nil();
}

static size_t set_length(void *data, size_t len) {
  (void) len;
  JanetTable *set = (JanetTable *)data;
  return set->count;
}

static void set_marshal(void *data, JanetMarshalContext *ctx) {
  janet_marshal_abstract(ctx, data);
  JanetTable *set = (JanetTable *)data;
  janet_marshal_int(ctx, set->count);
  for (int32_t i = 0; i < set->capacity; i++) {
    Janet element = set->data[i].key;
    if (!janet_checktype(element, JANET_NIL)) {
      janet_marshal_janet(ctx, element);
    }
  }
}

static void *set_unmarshal(JanetMarshalContext *ctx) {
  JanetTable *set = (JanetTable *)janet_unmarshal_abstract(ctx, sizeof(JanetTable));
  set->gc = (JanetGCObject){0, NULL};
  janet_table_init_raw(set, 0);
  int32_t length = janet_unmarshal_int(ctx);
  for (int32_t i = 0; i < length; i++) {
    janet_table_put(set, janet_unmarshal_janet(ctx), janet_wrap_true());
  }
  return set;
}

static void set_put(void *data, Janet key, Janet value){
  JanetTable *set = (JanetTable *)data;
  janet_table_put(set, key, janet_wrap_true());
}

static int32_t set_hash(void *p, size_t len){
  return janet_struct_hash(janet_table_to_struct((JanetTable *)p));
}

static int set_compare(void *lhs, void *rhs){ // misleading
  int32_t a = ((JanetTable *)lhs)->count;
  int32_t b = ((JanetTable *)rhs)->count;
  int c = (a > b) - (a < b);
  if(c != 0){
    return c;
  }
  // TODO is hashing here enough? collision might occur here when the user does not expect them here!
  a = set_hash(lhs,0);
  b = set_hash(rhs, 0);
  return (a > b) - (a < b);
}

static const JanetAbstractType set_type = {
  .name = "set",
  .gc = set_gc,
  .gcmark = set_gcmark,
  .get = set_get,
  .put = set_put,
  .marshal = set_marshal,
  .unmarshal = set_unmarshal,
  .tostring = set_tostring,
  .compare = set_compare,
  .hash = set_hash,
  .next = set_next,
  .call = set_call,
  .length = set_length,
  .bytes = NULL,
};

static Janet cfun_add(int32_t argc, Janet *argv) {
  janet_arity(argc, 1, -1);
  JanetTable *set = (JanetTable *)janet_getabstract(argv, 0, &set_type);
  for (int32_t i = 1; i < argc; i++) {
    janet_table_put(set, argv[i], janet_wrap_true());
  }
  return janet_wrap_nil();
}

static Janet cfun_remove(int32_t argc, Janet *argv) {
  janet_arity(argc, 1, -1);
  JanetTable *set = (JanetTable *)janet_getabstract(argv, 0, &set_type);
  for (int32_t i = 1; i < argc; i++) {
    janet_table_remove(set, argv[i]);
  }
  return janet_wrap_nil();
}

static JanetTable *new_abstract_set() {
  JanetTable *set = (JanetTable *)janet_abstract(&set_type, sizeof(JanetTable));
  set->gc = (JanetGCObject){0, NULL};
  janet_table_init_raw(set, 0);
  return set;
}

static Janet cfun_new(int32_t argc, Janet *argv) {
  JanetTable *set = new_abstract_set();
  for (int32_t i = 0; i < argc; i++) {
    janet_table_put(set, argv[i], janet_wrap_true());
  }

  return janet_wrap_abstract(set);
}

static JanetTable *set_union(int32_t argc, Janet *argv){
  JanetTable *result = new_abstract_set();

  for (int32_t arg_ix = 0; arg_ix < argc; arg_ix++) {
    JanetTable *arg = (JanetTable *)janet_getabstract(argv, arg_ix, &set_type);
    for (int32_t bucket_ix = 0; bucket_ix < arg->capacity; bucket_ix++) {
      JanetKV *entry = &arg->data[bucket_ix];
      if (janet_checktype(entry->key, JANET_NIL)) {
        continue;
      }
      janet_table_put(result, entry->key, janet_wrap_true());
    }
  }
  return result;
}

static Janet cfun_union(int32_t argc, Janet *argv) {
    return janet_wrap_abstract(set_union(argc, argv));
}

static JanetTable *set_clone(JanetTable *oldSet){
  JanetTable *newSet = new_abstract_set();

  newSet->count = oldSet->count;
  newSet->capacity = oldSet->capacity;
  newSet->deleted = oldSet->deleted;
  newSet->proto = oldSet->proto;
  newSet->data = janet_malloc(newSet->capacity * sizeof(JanetKV));
  if (NULL == newSet->data) {
      JANET_OUT_OF_MEMORY;
  }
  memcpy(newSet->data, oldSet->data, (size_t) oldSet->capacity * sizeof(JanetKV));
  return newSet;
}

static Janet cfun_clone(int32_t argc, Janet *argv){
  janet_arity(argc, 1, 1);
  return janet_wrap_abstract(set_clone(janet_getabstract(argv, 0, &set_type)));
}

static JanetTable *set_intersection(int32_t argc, Janet *argv){
  JanetTable *result = new_abstract_set();

  JanetTable *arg = (JanetTable *)janet_getabstract(argv, 0, &set_type);
  for (int32_t bucket_ix = 0; bucket_ix < arg->capacity; bucket_ix++) {
    JanetKV *entry = &arg->data[bucket_ix];
    if (janet_checktype(entry->key, JANET_NIL)) {
      continue;
    }
    int useKey = 1;
    for(int32_t arg_ix = 1; arg_ix < argc; arg_ix++){
      JanetTable *t = (JanetTable *)janet_getabstract(argv, arg_ix, &set_type);
      Janet res = janet_table_get(t, entry->key);
      if(janet_checktype(res, JANET_NIL)){
        useKey = 0;
        break;
      }
    }
    if(useKey){
      janet_table_put(result, entry->key, janet_wrap_true());
    }
  }
  return result;
}

static Janet cfun_intersection(int32_t argc, Janet *argv) {
  return janet_wrap_abstract(set_intersection(argc, argv));
}

static Janet cfun_difference(int32_t argc, Janet *argv) {
  janet_arity(argc, 1, -1);
  JanetTable *result = set_clone(janet_getabstract(argv, 0, &set_type));
  JanetTable *others = set_union(argc-1, argv+1);

  for (int32_t bucket_ix = 0; bucket_ix < others->capacity; bucket_ix++) {
    JanetKV *entry = &others->data[bucket_ix];
    if (janet_checktype(entry->key, JANET_NIL)) {
      continue;
    }
    janet_table_remove(result, entry->key);
  }

  return janet_wrap_abstract(result);
}

static Janet cfun_symmetric_difference(int32_t argc, Janet *argv) {
  janet_arity(argc, 1, -1);
  JanetTable *result = set_union(argc, argv);
  JanetTable *intersection = set_intersection(argc, argv);

  for (int32_t bucket_ix = 0; bucket_ix < intersection->capacity; bucket_ix++) {
    JanetKV *entry = &intersection->data[bucket_ix];
    if (janet_checktype(entry->key, JANET_NIL)) {
      continue;
    }
    janet_table_remove(result, entry->key);
  }

  return janet_wrap_abstract(result);
}

static const JanetReg cfuns[] = {
  {"new", cfun_new, "(set/new & xs)\n\n"
    "Returns a new set containing the input elements."},
  {"clone", cfun_clone, "(set/clone set)\n\n"
    "Returns a new set containing the input elements of the input set."},
  {"add", cfun_add, "(set/add set & xs)\n\n"
    "Add input elements to a set."},
  {"remove", cfun_remove, "(set/remove set & xs)\n\n"
    "Remove input elements from a set."},
  {"union", cfun_union, "(set/union & xs)\n\n"
    "Returns the union of the input sets."},
  {"difference", cfun_difference, "(set/difference base & xs)\n\n"
    "Returns the base set without the other input sets"},
  {"symmetric-difference", cfun_symmetric_difference, "(set/symmetric-difference & xs)\n\n"
    "Returns the union of the sets without the intersection of them"},
  {"intersection", cfun_intersection, "(set/intersection & xs)\n\n"
    "Returns the intersection of the input sets."},
  {NULL, NULL, NULL}
};

JANET_MODULE_ENTRY(JanetTable *env) {
  janet_cfuns(env, "set", cfuns);
  janet_register_abstract_type(&set_type);
}
