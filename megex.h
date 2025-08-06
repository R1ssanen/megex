/**
 * @file megex.h
 * @author R1ssanen
 * @brief Micro_rEGEX - Tiny RegEx engine in pure C99.
 * @see http://www.github.com/R1ssanen/megex.git
 */

#ifndef MEGEX_H_
#define MEGEX_H_

#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

/**
 * @file darray.h
 * @author R1ssanen
 * @brief Dynamic array implementation for C99 projects. Arrays are
 * directly compatible with C standard-library functions, as they're
 * simply heap arrays with metadata headers.
 * @see http://www.github.com/R1ssanen/darray.git
 */
#include "darray.h"

#define MINLINE static inline

#define MASSERT(expr, msg)                                                     \
  do {                                                                         \
    if (!(expr)) {                                                             \
      fprintf(stderr, "megex runtime assert at %s, line %d: %s\n", __func__,   \
              __LINE__, msg);                                                  \
      abort();                                                                 \
    }                                                                          \
  } while (0)

#ifndef NDEBUG
#define MASSERT_DEBUG(expr, msg) MASSERT(expr, msg)
#else
#define MASSERT_DEBUG() ((void)0)
#endif

#define MGX_SPECIALS "^$.|*+?()[]{},-\0"
#define MGX_ESCAPES "dwsn"

typedef enum {
  MGX_TOKEN_BEGIN = '^',
  MGX_TOKEN_END = '$',
  MGX_TOKEN_ANY = '.',
  MGX_TOKEN_OR = '|',
  MGX_TOKEN_ZOM = '*', // zero or more
  MGX_TOKEN_OOM = '+', // one or more
  MGX_TOKEN_OPT = '?',

  MGX_TOKEN_LPAREN = '(',
  MGX_TOKEN_RPAREN = ')',
  MGX_TOKEN_LBRACK = '[',
  MGX_TOKEN_RBRACK = ']',
  MGX_TOKEN_LBRACE = '{',
  MGX_TOKEN_RBRACE = '}',
  MGX_TOKEN_COMMA = ',',
  MGX_TOKEN_MINUS = '-',

  MGX_TOKEN_ESCAPE_DIGIT = 'd',
  MGX_TOKEN_ESCAPE_WORD = 'w',
  MGX_TOKEN_ESCAPE_WHITESPACE = 's',
  MGX_TOKEN_ESCAPE_NEWLINE = 'n',

  MGX_TOKEN_LITERAL,
  MGX_TOKEN_EOF = '\0',
} MGX_TokenKind;

typedef struct {
  size_t position;
  MGX_TokenKind kind;
  char value;
} MGX_Token;

#define MTOKEN(k, v, p)                                                        \
  (MGX_Token) { .position = p, .kind = k, .value = v }

typedef struct {
  const char *msg;
  size_t position;
  bool ok;
} MGX_Result;

#define MOKAY                                                                  \
  (MGX_Result) { .msg = NULL, .position = 0, .ok = true }

#define MERROR(err_msg, err_position)                                          \
  (MGX_Result) { .msg = err_msg, .position = err_position, .ok = false }

MINLINE MGX_Token *mgx_tokenize_regex(const char *pattern, size_t len) {
  MASSERT_DEBUG(pattern != NULL, "null pattern string.");
  MASSERT_DEBUG(len > 0, "cannot tokenize a regex pattern of length 0.");

  MGX_Token *tokens = darray_new(MGX_Token);
  size_t pos = 1;

  for (const char *c = pattern; c != pattern + len; ++c, ++pos) {

    if (strchr(MGX_SPECIALS, *c)) {
      darray_push(tokens, MTOKEN((MGX_TokenKind)(*c), *c, pos));
    }

    else {
      if (*c == '\\') {
        MASSERT(*(++c) != '\0', "invalid escape sequence.");

        if (strchr(MGX_ESCAPES, *c)) {
          darray_push(tokens, MTOKEN((MGX_TokenKind)(*c), *c, pos));
          continue;
        }
      }

      darray_push(tokens, MTOKEN(MGX_TOKEN_LITERAL, *c, pos));
    }
  }

  darray_push(tokens, MTOKEN(MGX_TOKEN_EOF, 0, len));
  darray_shrink(tokens);
  return tokens;
}

/*
 * PARSING
 */

typedef enum {
  MGX_NODE_PATTERN,
  MGX_NODE_ALTERNATION,
  MGX_NODE_CONCATENATION,
  MGX_NODE_QUANTIFIED_ATOM,
  MGX_NODE_ATOM,
  MGX_NODE_QUANTIFIER,
  MGX_NODE_GROUP,
  MGX_NODE_CLASS,
} MGX_NodeKind;

typedef struct MGX_NodePattern MGX_NodePattern;
typedef struct MGX_NodeAlternation MGX_NodeAlternation;
typedef struct MGX_NodeConcatenation MGX_NodeConcatenation;
typedef struct MGX_NodeQuantifiedAtom MGX_NodeQuantifiedAtom;
typedef struct MGX_NodeAtom MGX_NodeAtom;
typedef struct MGX_NodeQuantifier MGX_NodeQuantifier;
typedef struct MGX_NodeGroup MGX_NodeGroup;
typedef struct MGX_NodeClass MGX_NodeClass;
typedef struct MGX_NodeRange MGX_NodeRange;

struct MGX_NodeAlternation {
  MGX_NodeConcatenation *concats; // darray
};

struct MGX_NodePattern {
  MGX_NodeAlternation root;
};

struct MGX_NodeConcatenation {
  MGX_NodeQuantifiedAtom *quantifieds; // darray
};

struct MGX_NodeQuantifiedAtom {
  MGX_NodeAtom *atom;
  MGX_NodeQuantifier *quantifier;
};

struct MGX_NodeQuantifier {
  size_t lower;
  size_t upper;
};

struct MGX_NodeAtom {
  union {
    MGX_Token literal;
    MGX_Token any;
    MGX_NodeGroup *group;
    MGX_NodeClass *class;
  } opts;

  int holds;
};

struct MGX_NodeGroup {
  MGX_NodeAlternation *pattern;
};

struct MGX_NodeRange {
  char from;
  char to;
};

struct MGX_NodeClass {
  MGX_Token *literals;   // darray
  MGX_NodeRange *ranges; // darray
};

typedef struct MGX_Parser {
  const MGX_Token *tokens;
  MGX_Token *ptr;
} MGX_Parser;

MINLINE MGX_Token mgx_peek(const MGX_Parser *parser, size_t count) {
  MASSERT_DEBUG(parser != NULL, "null parser argument.");

  MASSERT(darray_last(parser->tokens) >= (parser->ptr + count),
          "unexpected EOF.");

  return *(parser->ptr + count);
}

MINLINE bool mgx_not_eof(const MGX_Parser *parser) {
  MASSERT_DEBUG(parser != NULL, "null parser argument.");
  return parser->ptr->kind != MGX_TOKEN_EOF;
}

MINLINE void mgx_consume(MGX_Parser *parser) {
  MASSERT_DEBUG(parser != NULL, "null parser argument.");
  MASSERT(mgx_not_eof(parser), "unexpected EOF.");
  parser->ptr += 1;
}

/*
 * PARSERS
 */

MINLINE MGX_Result mgx_parse_alternation(MGX_Parser *, MGX_NodeAlternation *);
MINLINE MGX_Result mgx_parse_concatenation(MGX_Parser *,
                                           MGX_NodeConcatenation *);
MINLINE MGX_Result mgx_parse_quantified_atom(MGX_Parser *,
                                             MGX_NodeQuantifiedAtom *);
MINLINE bool mgx_parse_quantifier(MGX_Parser *, MGX_NodeQuantifier *);
MINLINE MGX_Result mgx_parse_atom(MGX_Parser *, MGX_NodeAtom *);
MINLINE MGX_Result mgx_parse_group(MGX_Parser *, MGX_NodeGroup *);
MINLINE MGX_Result mgx_parse_class(MGX_Parser *, MGX_NodeClass *);
MINLINE MGX_Result mgx_parse_range(MGX_Parser *, MGX_NodeRange *);

MINLINE MGX_Result mgx_parse_pattern(MGX_Parser *parser,
                                     MGX_NodePattern *pattern) {
  return mgx_parse_alternation(parser, &pattern->root);
}

// optional
MINLINE bool mgx_parse_quantifier(MGX_Parser *parser,
                                  MGX_NodeQuantifier *quantifier) {

  MGX_Token token = mgx_peek(parser, 0);
  switch (token.kind) {

  case MGX_TOKEN_ZOM:
    mgx_consume(parser);
    quantifier->lower = 0;
    quantifier->upper = (size_t)-1;
    return true;

  case MGX_TOKEN_OOM:
    mgx_consume(parser);
    quantifier->lower = 1;
    quantifier->upper = (size_t)-1;
    return true;

  case MGX_TOKEN_OPT:
    mgx_consume(parser);
    quantifier->lower = 0;
    quantifier->upper = 1;
    return true;

    /*case MGX_LBRACE:
      mgx_consume(parser);

      token = mgx_peek(parser, 0);
      MASSERT((token->kind == MGX_CHAR) && isdigit(token->value),
              "invalid quantifier character.");

      size_t lower = (size_t)token->value;
      mgx_consume(parser);

      token = mgx_peek(parser, 0);
      if (token->kind != MGX_COMMA) {
        quantifier->lower = lower;
        quantifier->upper = -1;
        return true;
      }
      mgx_consume(parser);

      token = mgx_peek(parser, 0);
      if (token->kind != MGX_DIGIT) {
        quantifier->lower = lower;
        quantifier->upper = -1;
        return true;
      }
      size_t upper = (size_t)token->value;
      mgx_consume(parser);

      quantifier->lower = lower;
      quantifier->upper = upper;
      return true;*/

  default:
    return false;
  }
}

MINLINE MGX_Result mgx_parse_atom(MGX_Parser *parser, MGX_NodeAtom *atom) {

  MASSERT_DEBUG(parser != NULL, "null parser argument.");
  MASSERT_DEBUG(atom != NULL, "null node argument.");

  MGX_Token token = mgx_peek(parser, 0);
  switch (token.kind) {

  case MGX_TOKEN_ANY: {
    mgx_consume(parser);
    atom->holds = MGX_TOKEN_ANY;
    atom->opts.any = token;
    return MOKAY;
  }

  case MGX_TOKEN_LITERAL: {
    mgx_consume(parser);
    atom->holds = MGX_TOKEN_LITERAL;
    atom->opts.literal = token;
    return MOKAY;
  }

  case MGX_TOKEN_LPAREN: {
    MGX_NodeGroup group;
    MGX_Result result = mgx_parse_group(parser, &group);
    if (!result.ok)
      return result;

    atom->holds = MGX_NODE_GROUP;
    atom->opts.group = malloc(sizeof(MGX_NodeGroup));
    *atom->opts.group = group;
    return MOKAY;
  }

  case MGX_TOKEN_LBRACK: {
    MGX_NodeClass class;
    MGX_Result result = mgx_parse_class(parser, &class);
    if (!result.ok)
      return result;

    atom->holds = MGX_NODE_CLASS;
    atom->opts.class = malloc(sizeof(MGX_NodeClass));
    *atom->opts.class = class;
    return MOKAY;
  }

  default:
    return MERROR("syntax error; unknown atom.", token.position);
  }
}

MINLINE MGX_Result mgx_parse_quantified_atom(
    MGX_Parser *parser, MGX_NodeQuantifiedAtom *quantified) {

  MASSERT_DEBUG(parser != NULL, "null parser argument.");
  MASSERT_DEBUG(quantified != NULL, "null node argument.");

  MGX_NodeAtom atom;
  MGX_Result result = mgx_parse_atom(parser, &atom);
  if (!result.ok)
    return result;

  quantified->atom = malloc(sizeof(MGX_NodeAtom));
  *quantified->atom = atom;

  MGX_NodeQuantifier quantifier;
  if (mgx_parse_quantifier(parser, &quantifier)) {
    quantified->quantifier = malloc(sizeof(MGX_NodeQuantifier));
    *quantified->quantifier = quantifier;
  }

  else {
    quantified->quantifier = NULL;
  }

  return MOKAY;
}

MINLINE MGX_Result mgx_parse_concatenation(MGX_Parser *parser,
                                           MGX_NodeConcatenation *concat) {

  MASSERT_DEBUG(parser != NULL, "null parser argument.");
  MASSERT_DEBUG(concat != NULL, "null node argument.");

  concat->quantifieds = darray_new(MGX_NodeQuantifiedAtom);

  while ((mgx_peek(parser, 0).kind != MGX_TOKEN_EOF) &&
         (mgx_peek(parser, 0).kind != MGX_TOKEN_OR) &&
         (mgx_peek(parser, 0).kind != MGX_TOKEN_RPAREN)) {
    MGX_NodeQuantifiedAtom quantified;
    MGX_Result result = mgx_parse_quantified_atom(parser, &quantified);
    if (!result.ok)
      return result;

    darray_push(concat->quantifieds, quantified);
  }

  if (darray_count(concat->quantifieds) == 0) {
    return MERROR("syntax error; empty concatenation.",
                  mgx_peek(parser, 0).position);
  }
  return MOKAY;
}

MINLINE MGX_Result mgx_parse_alternation(MGX_Parser *parser,
                                         MGX_NodeAlternation *alternation) {

  MASSERT_DEBUG(parser != NULL, "null parser argument.");
  MASSERT_DEBUG(alternation != NULL, "null node argument.");

  MGX_NodeConcatenation concat;
  MGX_Result result = mgx_parse_concatenation(parser, &concat);
  if (!result.ok)
    return result;

  alternation->concats = darray_new(MGX_NodeConcatenation);
  darray_push(alternation->concats, concat);

  while (mgx_not_eof(parser) && (mgx_peek(parser, 0).kind == MGX_TOKEN_OR)) {
    mgx_consume(parser);

    result = mgx_parse_concatenation(parser, &concat);
    if (!result.ok)
      return result;

    darray_push(alternation->concats, concat);
  }

  return MOKAY;
}

MINLINE MGX_Result mgx_parse_group(MGX_Parser *parser, MGX_NodeGroup *group) {

  MASSERT_DEBUG(parser != NULL, "null parser argument.");
  MASSERT_DEBUG(group != NULL, "null node argument.");

  MGX_Token token = mgx_peek(parser, 0);
  if (token.kind != MGX_TOKEN_LPAREN)
    return MERROR("syntax error; unopened group.", token.position);
  mgx_consume(parser);

  MGX_NodeAlternation alternation;
  MGX_Result result = mgx_parse_alternation(parser, &alternation);
  if (!result.ok) {
    return result;
  }

  token = mgx_peek(parser, 0);
  if (token.kind != MGX_TOKEN_RPAREN)
    return MERROR("syntax error; unclosed group.", token.position);
  mgx_consume(parser);

  group->pattern = malloc(sizeof(MGX_NodeAlternation));
  *group->pattern = alternation;

  return MOKAY;
}

MINLINE MGX_Result mgx_parse_class(MGX_Parser *parser, MGX_NodeClass *class) {

  MASSERT_DEBUG(parser != NULL, "null parser argument.");
  MASSERT_DEBUG(class != NULL, "null node argument.");

  MGX_Token token = mgx_peek(parser, 0);
  if (token.kind != MGX_TOKEN_LBRACK)
    return MERROR("syntax error; unopened class.", token.position);
  mgx_consume(parser);

  class->literals = darray_new(MGX_Token);
  class->ranges = darray_new(MGX_NodeRange);

  while (mgx_peek(parser, 0).kind != MGX_TOKEN_RBRACK) {

    if (mgx_peek(parser, 1).kind == MGX_TOKEN_MINUS) {
      MGX_NodeRange range;
      MGX_Result result = mgx_parse_range(parser, &range);
      if (!result.ok)
        return result;

      darray_push(class->ranges, range);
      continue;
    }

    MGX_Token token = mgx_peek(parser, 0);
    if (token.kind == MGX_TOKEN_LITERAL) {
      darray_push(class->literals, token);
      mgx_consume(parser);
    }

    else {
      return MERROR("syntax error; invalid class character.",
                    mgx_peek(parser, 0).position);
    }
  }
  mgx_consume(parser);

  if ((darray_count(class->literals) == 0) &&
      (darray_count(class->ranges) == 0)) {
    return MERROR("syntax error; empty character class not allowed.",
                  mgx_peek(parser, 0).position);
  }

  return MOKAY;
}

MINLINE MGX_Result mgx_parse_range(MGX_Parser *parser, MGX_NodeRange *range) {

  MASSERT_DEBUG(parser != NULL, "null parser argument.");
  MASSERT_DEBUG(range != NULL, "null node argument.");

  MGX_Token left = mgx_peek(parser, 0);
  if (left.kind != MGX_TOKEN_LITERAL)
    return MERROR("syntax error; invalid range character.", left.position);
  mgx_consume(parser);

  MGX_Token minus = mgx_peek(parser, 0);
  if (minus.kind != MGX_TOKEN_MINUS)
    return MERROR("syntax error; invalid range character.", minus.position);
  mgx_consume(parser);

  MGX_Token right = mgx_peek(parser, 0);
  if (right.kind != MGX_TOKEN_LITERAL) {
    puts("the error is here");
    return MERROR("syntax error; invalid range character.", right.position);
  }
  mgx_consume(parser);

  if (left.value > right.value) {
    return MERROR("syntax error; invalid character range, values must be in "
                  "ascending order.",
                  left.position);
  }

  range->from = left.value;
  range->to = right.value;
  return MOKAY;
}

#endif