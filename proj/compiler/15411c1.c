/* Libary 15411c1 */
/* consists of conio, string, file, and 15411 */

#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <signal.h>

typedef int c0_int; 
typedef bool c0_bool; /* in departure from L4 spec */
typedef char c0_char;
typedef char* c0_string;  /* NULL is the empty string "" */
typedef struct c0_array_header* c0_array;  /* elements follow header in memory */
typedef int c0_float;

struct c0_array_header {
  c0_int count;
  c0_int elt_size;
};

/* c0runtime.c */
void raise_msg(int signal, const char* msg) {
  fprintf(stderr, "%s\n", msg);
  fflush(stderr);
  raise(signal);
}

void c0_abort(const char *reason) {
  raise_msg(SIGABRT, reason);
}

void c0_abort_mem(const char *reason) {
  raise_msg(SIGSEGV, reason);
}

void* c0_alloc(size_t elt_size) {
  // Require non-zero allocation so that alloc acts as a gensym
  if (elt_size == 0) elt_size = 1;
  void *p = calloc(1, elt_size);
  if (p == NULL) c0_abort_mem("allocation failed");
  return (void *)p;
}

c0_array c0_array_alloc(size_t elemsize, c0_int elemcount) {
  /* test for overflow, somehow? */
  if (elemcount < 0) c0_abort_mem("array size cannot be negative");
  if (elemsize > 0 && elemcount > ((1<<30)-8)/elemsize)
    c0_abort_mem("array size too large");

  c0_array p = calloc(1, sizeof(struct c0_array_header) + elemcount*elemsize);
  if (p == NULL) c0_abort_mem("array allocation failed");
  p->count = elemcount;              /* initialize number of elements */
  p->elt_size = elemsize;            /* store element size */
  return (void*)p;          /* return pointer to struct */
}

void* c0_array_sub(c0_array A, int i, size_t elemsize) {
  if (A == NULL) c0_abort_mem("attempt to access default zero-size array");
  if (((unsigned)i) >= (unsigned)(A->count))
    c0_abort_mem("array index out of bounds");
  return (void *) ((char*)(A+1) + i*A->elt_size);
}

/* do we need to copy? */
/*
c0_string c0_string_fromcstr(const char *s) {
  char *c = c0_alloc(strlen(s) + 1);
  strcpy(c, s);
  return (c0_string)c;
}
*/
c0_string c0_string_fromcstr(const char *str) {
  return (c0_string)str;
}

const char *c0_string_tocstr(c0_string s) {
  return s ? s : "";
}

/* Library 15411 */

union float_or_int {
  int as_int;
  float as_float;
};

typedef union float_or_int float_or_int;

c0_float fadd(c0_float a, c0_float b) {
  float_or_int x; x.as_int = a;
  float_or_int y; y.as_int = b;
  float_or_int z; z.as_float = x.as_float + y.as_float;
  return z.as_int;
}

c0_float fsub(c0_float a, c0_float b) {
  float_or_int x; x.as_int = a;
  float_or_int y; y.as_int = b;
  float_or_int z; z.as_float = x.as_float - y.as_float;
  return z.as_int;
}

c0_float fmul(c0_float a, c0_float b) {
  float_or_int x; x.as_int = a;
  float_or_int y; y.as_int = b;
  float_or_int z; z.as_float = x.as_float * y.as_float;
  return z.as_int;
}

c0_float fdiv(c0_float a, c0_float b) {
  float_or_int x; x.as_int = a;
  float_or_int y; y.as_int = b;
  float_or_int z; z.as_float = x.as_float / y.as_float;
  return z.as_int;
}

c0_bool fless(c0_float a, c0_float b) {
  float_or_int x; x.as_int = a;
  float_or_int y; y.as_int = b;
  return x.as_float < y.as_float ? 1 : 0;
}

c0_float itof(int a) {
  float_or_int x; x.as_float = (float)a;
  return x.as_int;
}

int ftoi(c0_float a) {
  float_or_int x; x.as_int = a;
  return (int)x.as_float;
}

int print_fpt(c0_float a) {
  float_or_int x; x.as_int = a;
  fprintf(stderr, "%g\n", x.as_float);
  return 0;
}

int print_int(int n) {
  fprintf(stderr, "%d\n", n);
  return 0;
}

int print_hex(int n) {
  fprintf(stderr, "0x%08X\n", n);
  return 0;
}

/* util.c */
const size_t kInitReadBuffer = 64;

// Reads into a static buffer is possible, otherwise allocates on the heap
// TODO: make this gc-alloc efficiently
c0_string freadline(FILE *f) {
  char staticBuffer[kInitReadBuffer];
  char *buffer = staticBuffer;
  size_t maxBufferLength = kInitReadBuffer;
  size_t bufferSize = 0;

  int n;
  char c;
  while ((n = fgetc(f)) != '\n') {
    /* first, check for EOF or error condition */
    if (n == EOF) {
      if (feof(f)) {
        break;
      } else if (ferror(f)) {
        perror("error reading file");
        c0_abort("aborting due to file read error");
      } else {
        c0_abort("BUG: fgetc returned EOF, but !ferror and !feof... please report!");
      }
    }

    /* then, convert the int into a char and proceed as normal */
    c = n;

    if (c == '\r' || c == '\0')
      continue;

    if (bufferSize + 1 == maxBufferLength) {
      maxBufferLength *= 2;
      if (buffer == staticBuffer) {
        buffer = malloc(maxBufferLength);
        strncpy(buffer, staticBuffer, bufferSize);
      } else {
        buffer = realloc(buffer, maxBufferLength);
        if (!buffer)
          c0_abort("OOM in readline()");
      }
    }
    buffer[bufferSize++] = c;
  }

  buffer[bufferSize] = '\0';
  c0_string c0str = c0_string_fromcstr(buffer);
  if (buffer != staticBuffer)
    free(buffer);
  return c0str;
}

/* Library conio */

void print(c0_string s) {
  const char *cstr = c0_string_tocstr(s);
  printf("%s", cstr);
}

void println(c0_string s) {
  const char *cstr = c0_string_tocstr(s);
  puts(cstr);
}

void flush() {
  fflush(stdout);
}

void printint(c0_int i) {
  printf("%d", i);
}

void printbool(c0_bool b) {
  puts(b ? "true" : "false");
}

void printchar(c0_char c) {
  putchar(c);
}

c0_string readline() {
  return freadline(stdin);
}

/* Added Sep 27, 2012 -fp */
c0_bool eof() {
  return (c0_bool)(feof(stdin) ? 1 : 0);
}

/* Library string */

c0_int string_length(c0_string s) {
  char* str = (char*)s;
  return str ? strlen(str) : 0;
}

c0_int string_compare(c0_string a, c0_string b) {
  char* astr = a ? (char*)a : "";
  char* bstr = b ? (char*)b : "";
  int res = strcmp(astr,bstr);
  return (res < 0) ? -1 : ((res > 0) ? 1 : 0);
}

bool string_equal(c0_string a, c0_string b) {
  return 0 == string_compare(a,b);
}

c0_char string_charat(c0_string s, c0_int idx) {
  int len = string_length(s);
  if (!(0 <= idx && idx < len)) c0_abort("string_charat: index out of bounds");
  return s[idx];
}

c0_string string_sub(c0_string s, c0_int start, c0_int end) {
  char* str = (char*) s;
  if (0 > start) c0_abort("c0_string_sub: 0 > start");
  if (start > end) c0_abort("c0_string_sub: start > end");
  if (!str) {
    if (end > 0) c0_abort("c0_string_sub: end > length of string");
    return "";
  }
  size_t len = strlen(str);
  if (end > len) c0_abort("c0_string_sub: end > length of string");

  //@assert 0 <= start && start <= end && end <= len;
  if (start == end) return "";
  size_t sublen = end - start;
  char *sub = c0_alloc(sublen+1);
  sub[sublen] = '\0';
  strncat(sub, str+start, sublen);
  return (c0_string)sub;
}

c0_string string_join(c0_string a, c0_string b) {
  if (!a) return b;
  if (!b) return a;

  char *c = c0_alloc(strlen((char*)a) + strlen((char*)b) + 1);
  strcpy(c, a);
  strcat(c, b);
  return (c0_string)c;
}

c0_string string_frombool(c0_bool b) {
  return (c0_string)(b ? "true" : "false");
}

c0_string string_fromint(c0_int i) {
  const size_t kBufSize = 16;
  char buffer[kBufSize];
  // This should always be false due to the limits of int32
  if (kBufSize == snprintf(buffer, kBufSize, "%d", i))
    c0_abort("Very unexpected error formatting integer");
  return c0_string_fromcstr(buffer);
}

c0_string string_fromchar(c0_char c) {
    char *res = c0_alloc(2);
    res[0] = c; /* XXX assumes type c0_char = char -wjl */
    res[1] = '\0';
    return c0_string_fromcstr(res);
}

/* return a lowercased version of s */
c0_string string_tolower(c0_string s) {
    const char *str = c0_string_tocstr(s);
    char *low = c0_alloc(strlen(str)+1);
    int i;
    for (i = 0; str[i] != '\0'; i++)
        low[i] = tolower(str[i]);
    low[i] = '\0';
    return c0_string_fromcstr(low);
}

c0_bool string_terminated(c0_array A, c0_int n) {
  int i;
  for (i = 0; i < n; i++) {
    if (*(c0_char*)c0_array_sub(A, i, sizeof(c0_char)) == '\0') return true;
  }
  return false;
}

c0_array string_to_chararray(c0_string s) {
  int len = string_length(s); // does not include \0
  c0_array A = c0_array_alloc(sizeof(c0_char),len+1);
  const char *str = c0_string_tocstr(s);
  int i;
  for (i = 0; str[i] != '\0'; i++)
    *(c0_char*)c0_array_sub(A, i, sizeof(c0_char)) = str[i];
  *(c0_char*)c0_array_sub(A, i, sizeof(c0_char)) = '\0';
  return A;
}

c0_string string_from_chararray(c0_array A) {
  int len; char *cstr; int i;
  for (len = 0; *(c0_char*)c0_array_sub(A, len, sizeof(c0_char)) != '\0'; len++);
  cstr = c0_alloc(len+1);
  for (i = 0; i < len+1; i++)
    cstr[i] = *(c0_char*)c0_array_sub(A, i, sizeof(c0_char));
  return c0_string_fromcstr(cstr);
}

c0_int char_ord(c0_char c) {
  return (c0_int)c;
}

c0_char char_chr(c0_int n) {
  if (0 > n || n > 127) c0_abort("character outside ASCII range (0..127)");
  return (c0_char)n;
}

/* Library file */

struct file {
  FILE *handle;
  c0_bool isEOF;
};

typedef struct file* file_t;

c0_bool peekEOF(FILE *f) 
//@requires f != NULL;
{
  if(EOF == fgetc(f)) 
    return true;
  fseek(f, -1, SEEK_CUR);
  return false;
}

file_t file_read(c0_string path) {
  const char* filename = c0_string_tocstr(path);
  file_t f = c0_alloc(sizeof(struct file));
  f->handle = fopen(filename, "r");
  if (!f->handle) return NULL;
  f->isEOF = peekEOF(f->handle);
  return f;
}

c0_bool file_closed(file_t f) {
  assert(f != NULL);
  return f->handle == NULL;
}

void file_close(file_t f) {
  assert(f != NULL);
  assert(!file_closed(f));
  if (EOF == fclose(f->handle)) {
    c0_abort("Could not close file");
  }
  f->handle = NULL;
  return;
}

c0_bool file_eof(file_t f) {
  assert(f != NULL);
  return f->isEOF;
}

c0_string file_readline(file_t f) {
  assert(f != NULL);
  assert(!file_closed(f));
  assert(!file_eof(f));
  if (feof(f->handle)) {
    c0_abort("At end of file, but file_eof returned false (bug)");
  }
  // From util.h in the conio library
  c0_string res = freadline(f->handle);
  f->isEOF = peekEOF(f->handle);
  return res;
}
