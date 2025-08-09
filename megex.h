/**
 * @file megex.h
 * @author R1ssanen
 * @brief Micro_rEGEX - Header-only RegEx engine in pure C99.
 * @see http://www.github.com/R1ssanen/megex.git
 */

#ifndef MEGEX_H_
#define MEGEX_H_

#include <ctype.h>
#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
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

#define MGX_INLINE static inline

#define MGX_ASSERT(expr, msg)                                                  \
  do {                                                                         \
    if (!(expr)) {                                                             \
      fprintf(stderr, "megex runtime assert at %s, line %d: %s\n", __func__,   \
              __LINE__, msg);                                                  \
      abort();                                                                 \
    }                                                                          \
  } while (0)

#ifndef NDEBUG
#define MGX_ASSERT_DEBUG(expr, msg) MGX_ASSERT(expr, msg)
#else
#define MGX_ASSERT_DEBUG() ((void)0)
#endif

/*
 * LEXING
 */

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
  MGX_TokenKind kind;
  char value;
} MGX_Token;

MGX_INLINE bool mgx_is_digit(MGX_Token *token) {
  return (token->kind == MGX_TOKEN_LITERAL) && isdigit(token->value);
}

#define MGX_NEW_TOKEN(k, v)                                                    \
  (MGX_Token) { .kind = k, .value = v }

MGX_INLINE MGX_Token *mgx_tokenize_regex(const char *pattern, size_t len) {
  MGX_ASSERT_DEBUG(pattern != NULL, "null pattern string.");
  MGX_ASSERT_DEBUG(len > 0, "cannot tokenize a regex pattern of length 0.");

  MGX_Token *tokens = darray_new(MGX_Token);

  for (const char *c = pattern; c < (pattern + len); ++c) {

    if (strchr(MGX_SPECIALS, *c)) {
      darray_push(tokens, MGX_NEW_TOKEN((MGX_TokenKind)(*c), *c));
    }

    else {
      if (*c == '\\') {
        MGX_ASSERT(*(++c) != '\0', "invalid escape sequence.");

        if (strchr(MGX_ESCAPES, *c)) {
          darray_push(tokens, MGX_NEW_TOKEN((MGX_TokenKind)(*c), *c));
          continue;
        }
      }

      darray_push(tokens, MGX_NEW_TOKEN(MGX_TOKEN_LITERAL, *c));
    }
  }

  darray_push(tokens, MGX_NEW_TOKEN(MGX_TOKEN_EOF, 0));
  darray_shrink(tokens);
  return tokens;
}

/*
 * PARSING
 */

typedef struct MGX_NodeAlternation {
  struct MGX_NodeConcatenation *concats; // darray
} MGX_NodeAlternation;

typedef struct MGX_NodePattern {
  MGX_NodeAlternation root;
} MGX_NodePattern;

typedef struct MGX_NodeConcatenation {
  struct MGX_NodeQuantifiedAtom *quantifieds; // darray
} MGX_NodeConcatenation;

typedef struct MGX_NodeGroup {
  MGX_NodeAlternation pattern;
} MGX_NodeGroup;

typedef struct MGX_NodeQuantifier {
  size_t lower;
  size_t upper;
} MGX_NodeQuantifier;

typedef struct MGX_NodeRange {
  char from;
  char to;
} MGX_NodeRange;

typedef struct MGX_NodeClass {
  MGX_Token *literals;   // darray
  MGX_NodeRange *ranges; // darray
} MGX_NodeClass;

typedef struct MGX_NodeAtom {
  union {
    MGX_Token literal;
    MGX_Token any;
    MGX_NodeGroup group;
    MGX_NodeClass class;
  } opts;

  enum {
    MGX_NODEATOM_LITERAL,
    MGX_NODEATOM_ANY,
    MGX_NODEATOM_GROUP,
    MGX_NODEATOM_CLASS
  } holds;
} MGX_NodeAtom;

typedef struct MGX_NodeQuantifiedAtom {
  MGX_NodeAtom atom;
  MGX_NodeQuantifier quantifier;
  bool has_quantifier;
} MGX_NodeQuantifiedAtom;

typedef struct MGX_Parser {
  const MGX_Token *tokens;
  MGX_Token *ptr;
} MGX_Parser;

MGX_INLINE MGX_Token *mgx_peek(const MGX_Parser *parser, size_t count) {
  MGX_ASSERT_DEBUG(parser != NULL, "null parser argument.");

  MGX_ASSERT(darray_last(parser->tokens) >= (parser->ptr + count),
             "unexpected EOF.");

  return parser->ptr + count;
}

MGX_INLINE bool mgx_not_eof(const MGX_Parser *parser) {
  MGX_ASSERT_DEBUG(parser != NULL, "null parser argument.");
  return parser->ptr->kind != MGX_TOKEN_EOF;
}

MGX_INLINE MGX_Token *mgx_consume(MGX_Parser *parser) {
  MGX_ASSERT_DEBUG(parser != NULL, "null parser argument.");
  MGX_ASSERT(mgx_not_eof(parser), "unexpected EOF.");
  return parser->ptr++;
}

MGX_INLINE MGX_Token *mgx_expect(MGX_Parser *parser, MGX_TokenKind expected) {
  if (parser->ptr->kind != expected)
    return NULL;
  else
    return mgx_consume(parser);
}

MGX_INLINE MGX_Token *mgx_expect_digit(MGX_Parser *parser) {
  if (!mgx_is_digit(parser->ptr))
    return NULL;
  else
    return mgx_consume(parser);
}

MGX_INLINE size_t mgx_position(MGX_Parser *parser) {
  return (parser->ptr - parser->tokens);
}

/*
 * PARSERS
 */

typedef struct {
  const char *msg;
  size_t position;
  bool ok;
} MGX_Result;

#define MGX_OK                                                                 \
  (MGX_Result) { .msg = NULL, .position = 0, .ok = true }

#define MGX_ERROR(err_msg, err_position)                                       \
  (MGX_Result) { .msg = err_msg, .position = err_position, .ok = false }

MGX_INLINE MGX_Result mgx_parse_alternation(MGX_Parser *,
                                            MGX_NodeAlternation *);
MGX_INLINE MGX_Result mgx_parse_concatenation(MGX_Parser *,
                                              MGX_NodeConcatenation *);
MGX_INLINE MGX_Result mgx_parse_quantified_atom(MGX_Parser *,
                                                MGX_NodeQuantifiedAtom *);
MGX_INLINE MGX_Result mgx_parse_quantifier(MGX_Parser *, MGX_NodeQuantifier *,
                                           bool *);
MGX_INLINE MGX_Result mgx_parse_atom(MGX_Parser *, MGX_NodeAtom *);
MGX_INLINE MGX_Result mgx_parse_group(MGX_Parser *, MGX_NodeGroup *);
MGX_INLINE MGX_Result mgx_parse_class(MGX_Parser *, MGX_NodeClass *);
MGX_INLINE MGX_Result mgx_parse_range(MGX_Parser *, MGX_NodeRange *);

MGX_INLINE MGX_Result mgx_parse_pattern(MGX_Parser *parser,
                                        MGX_NodePattern *pattern) {

  MGX_ASSERT_DEBUG(parser != NULL, "null parser argument.");
  MGX_ASSERT_DEBUG(pattern != NULL, "null node argument.");

  return mgx_parse_alternation(parser, &pattern->root);
}

MGX_INLINE MGX_Result mgx_parse_quantifier_digits(MGX_Parser *parser,
                                                  size_t *out) {

  MGX_Token *token = mgx_expect_digit(parser);
  if (!token)
    return MGX_ERROR("syntax error; expected quantifier digit.",
                     mgx_position(parser));

  char *digit_body = darray_new(char);
  darray_push(digit_body, token->value);

  while ((token = mgx_expect_digit(parser))) {
    darray_push(digit_body, token->value);
  }

  darray_push(digit_body, '\0');
  unsigned long long num = strtoull(digit_body, NULL, 10);

  if ((errno == ERANGE) || (num > (size_t)-1)) {
    return MGX_ERROR("syntax error; quantifier value out of range.",
                     mgx_position(parser));
  }

  darray_free(digit_body);
  *out = (size_t)num;
  return MGX_OK;
}

MGX_INLINE MGX_Result mgx_parse_quantifier(MGX_Parser *parser,
                                           MGX_NodeQuantifier *quantifier,
                                           bool *no_match) {

  MGX_ASSERT_DEBUG(parser != NULL, "null parser argument.");
  MGX_ASSERT_DEBUG(quantifier != NULL, "null node argument.");

  MGX_Token *token = mgx_peek(parser, 0);
  switch (token->kind) {

  case MGX_TOKEN_ZOM:
    mgx_consume(parser);
    quantifier->lower = 0;
    quantifier->upper = (size_t)-1;
    return MGX_OK;

  case MGX_TOKEN_OOM:
    mgx_consume(parser);
    quantifier->lower = 1;
    quantifier->upper = (size_t)-1;
    return MGX_OK;

  case MGX_TOKEN_OPT:
    mgx_consume(parser);
    quantifier->lower = 0;
    quantifier->upper = 1;
    return MGX_OK;

  case MGX_TOKEN_LBRACE: {
    mgx_consume(parser);

    size_t lower;
    MGX_Result result = mgx_parse_quantifier_digits(parser, &lower);
    if (!result.ok)
      return result;

    token = mgx_consume(parser);
    if (token->kind == MGX_TOKEN_RBRACE) {
      quantifier->lower = lower;
      quantifier->upper = lower;
      return MGX_OK;
    }

    else if (token->kind != MGX_TOKEN_COMMA)
      return MGX_ERROR("syntax error; comma expected.", mgx_position(parser));

    token = mgx_consume(parser);
    if (token->kind == MGX_TOKEN_RBRACE) {
      quantifier->lower = lower;
      quantifier->upper = (size_t)-1;
      return MGX_OK;
    }

    else if (mgx_is_digit(token)) {
      quantifier->lower = lower;

      result = mgx_parse_quantifier_digits(parser, &quantifier->upper);
      if (!result.ok)
        return result;

      if (quantifier->lower > quantifier->upper)
        return MGX_ERROR(
            "syntax error; invalid quantifier range, values must be in "
            "ascending order.",
            mgx_position(parser));

    } else
      return MGX_ERROR("syntax error; invalid quantifier.",
                       mgx_position(parser));

    if (!mgx_expect(parser, MGX_TOKEN_RBRACE))
      return MGX_ERROR("syntax error; unclosed quantifier.",
                       mgx_position(parser));
    else
      return MGX_OK;
  }

  default:
    *no_match = true;
    return MGX_OK;
  }
}

MGX_INLINE MGX_Result mgx_parse_atom(MGX_Parser *parser, MGX_NodeAtom *atom) {

  MGX_ASSERT_DEBUG(parser != NULL, "null parser argument.");
  MGX_ASSERT_DEBUG(atom != NULL, "null node argument.");

  MGX_Token *token = mgx_peek(parser, 0);

  switch (token->kind) {

  case MGX_TOKEN_ANY: {
    mgx_consume(parser);
    atom->holds = MGX_NODEATOM_ANY;
    atom->opts.any = *token;
    return MGX_OK;
  }

  case MGX_TOKEN_LITERAL: {
    mgx_consume(parser);
    atom->holds = MGX_NODEATOM_LITERAL;
    atom->opts.literal = *token;
    return MGX_OK;
  }

  case MGX_TOKEN_LPAREN: {
    MGX_NodeGroup group;
    MGX_Result result = mgx_parse_group(parser, &group);
    if (!result.ok)
      return result;

    atom->holds = MGX_NODEATOM_GROUP;
    atom->opts.group = group;
    return MGX_OK;
  }

  case MGX_TOKEN_LBRACK: {
    MGX_NodeClass class;
    MGX_Result result = mgx_parse_class(parser, &class);
    if (!result.ok)
      return result;

    atom->holds = MGX_NODEATOM_CLASS;
    atom->opts.class = class;
    return MGX_OK;
  }

  default:
    return MGX_ERROR("syntax error; unknown atom.", mgx_position(parser));
  }
}

MGX_INLINE MGX_Result mgx_parse_quantified_atom(
    MGX_Parser *parser, MGX_NodeQuantifiedAtom *quantified) {

  MGX_ASSERT_DEBUG(parser != NULL, "null parser argument.");
  MGX_ASSERT_DEBUG(quantified != NULL, "null node argument.");

  MGX_NodeAtom atom;
  MGX_Result result = mgx_parse_atom(parser, &atom);
  if (!result.ok)
    return result;

  MGX_NodeQuantifier quantifier;
  bool no_match = false;
  result = mgx_parse_quantifier(parser, &quantifier, &no_match);
  if (!result.ok)
    return result;

  if (no_match)
    quantified->has_quantifier = false;

  else {
    quantified->quantifier = quantifier;
    quantified->has_quantifier = true;
  }

  quantified->atom = atom;
  return MGX_OK;
}

MGX_INLINE MGX_Result mgx_parse_concatenation(MGX_Parser *parser,
                                              MGX_NodeConcatenation *concat) {

  MGX_ASSERT_DEBUG(parser != NULL, "null parser argument.");
  MGX_ASSERT_DEBUG(concat != NULL, "null node argument.");

  concat->quantifieds = darray_new(MGX_NodeQuantifiedAtom);
  MGX_Token *token = mgx_peek(parser, 0);

  while (mgx_not_eof(parser) && (token->kind != MGX_TOKEN_OR) &&
         (token->kind != MGX_TOKEN_RPAREN)) {

    MGX_NodeQuantifiedAtom quantified;
    MGX_Result result = mgx_parse_quantified_atom(parser, &quantified);
    if (!result.ok)
      return result;

    darray_push(concat->quantifieds, quantified);
    token = mgx_peek(parser, 0);
  }

  if (darray_count(concat->quantifieds) == 0) {
    return MGX_ERROR("syntax error; empty concatenation.",
                     mgx_position(parser));
  }
  return MGX_OK;
}

MGX_INLINE MGX_Result mgx_parse_alternation(MGX_Parser *parser,
                                            MGX_NodeAlternation *alternation) {

  MGX_ASSERT_DEBUG(parser != NULL, "null parser argument.");
  MGX_ASSERT_DEBUG(alternation != NULL, "null node argument.");

  MGX_NodeConcatenation concat;
  MGX_Result result = mgx_parse_concatenation(parser, &concat);
  if (!result.ok)
    return result;

  alternation->concats = darray_new(MGX_NodeConcatenation);
  darray_push(alternation->concats, concat);

  while (mgx_expect(parser, MGX_TOKEN_OR)) {
    result = mgx_parse_concatenation(parser, &concat);
    if (!result.ok)
      return result;

    darray_push(alternation->concats, concat);
  }

  return MGX_OK;
}

MGX_INLINE MGX_Result mgx_parse_group(MGX_Parser *parser,
                                      MGX_NodeGroup *group) {

  MGX_ASSERT_DEBUG(parser != NULL, "null parser argument.");
  MGX_ASSERT_DEBUG(group != NULL, "null node argument.");

  MGX_Token *token = mgx_expect(parser, MGX_TOKEN_LPAREN);
  if (!token)
    return MGX_ERROR("syntax error; unopened group.", mgx_position(parser));

  MGX_NodeAlternation alternation;
  MGX_Result result = mgx_parse_alternation(parser, &alternation);
  if (!result.ok)
    return result;

  token = mgx_expect(parser, MGX_TOKEN_RPAREN);
  if (!token)
    return MGX_ERROR("syntax error; unclosed group.", mgx_position(parser));

  group->pattern = alternation;
  return MGX_OK;
}

MGX_INLINE MGX_Result mgx_parse_class(MGX_Parser *parser,
                                      MGX_NodeClass *class) {

  MGX_ASSERT_DEBUG(parser != NULL, "null parser argument.");
  MGX_ASSERT_DEBUG(class != NULL, "null node argument.");

  MGX_Token *token = mgx_expect(parser, MGX_TOKEN_LBRACK);
  if (!token)
    return MGX_ERROR("syntax error; unopened class.", mgx_position(parser));

  class->literals = darray_new(MGX_Token);
  class->ranges = darray_new(MGX_NodeRange);

  while (mgx_peek(parser, 0)->kind != MGX_TOKEN_RBRACK) {

    if (mgx_peek(parser, 1)->kind == MGX_TOKEN_EOF)
      return MGX_ERROR("syntax error; unclosed class.", mgx_position(parser));

    else if (mgx_peek(parser, 1)->kind == MGX_TOKEN_MINUS) {
      MGX_NodeRange range;
      MGX_Result result = mgx_parse_range(parser, &range);
      if (!result.ok)
        return result;

      darray_push(class->ranges, range);
      continue;
    }

    token = mgx_expect(parser, MGX_TOKEN_LITERAL);
    if (!token)
      return MGX_ERROR("syntax error; invalid class character.",
                       mgx_position(parser));

    darray_push(class->literals, *token);
  }

  if ((darray_count(class->literals) == 0) &&
      (darray_count(class->ranges) == 0)) {
    return MGX_ERROR("syntax error; empty character class not allowed.",
                     mgx_position(parser));
  }

  mgx_consume(parser);
  return MGX_OK;
}

MGX_INLINE MGX_Result mgx_parse_range(MGX_Parser *parser,
                                      MGX_NodeRange *range) {

  MGX_ASSERT_DEBUG(parser != NULL, "null parser argument.");
  MGX_ASSERT_DEBUG(range != NULL, "null node argument.");

  MGX_Token *left = mgx_expect(parser, MGX_TOKEN_LITERAL);
  if (!left)
    return MGX_ERROR("syntax error; invalid range character.",
                     mgx_position(parser));

  if (!mgx_expect(parser, MGX_TOKEN_MINUS))
    return MGX_ERROR("syntax error; expected '-'.", mgx_position(parser));

  MGX_Token *right = mgx_expect(parser, MGX_TOKEN_LITERAL);
  if (!right)
    return MGX_ERROR("syntax error; invalid range character.",
                     mgx_position(parser));

  if (left->value > right->value) {
    return MGX_ERROR("syntax error; invalid character range, values must be in "
                     "ascending order.",
                     mgx_position(parser));
  }

  range->from = left->value;
  range->to = right->value;
  return MGX_OK;
}

/*
 * NFA CONSTRUCTION
 */

typedef struct MGX_Transition MGX_Transition;
typedef struct MGX_State MGX_State;

typedef struct MGX_Trigger {
  bool is_range;
  char from, to; // if not range, only 'from' is checked
} MGX_Trigger;

struct MGX_State {
  MGX_Transition *transitions; // darray
  bool is_accept;
  bool visited;
};

#define MGX_NEW_STATE(is_accept_)                                              \
  (MGX_State) {                                                                \
    .transitions = darray_new(MGX_Transition), .is_accept = is_accept_,        \
    .visited = false                                                           \
  }

struct MGX_Transition {
  MGX_State target;
  bool is_epsilon;
  MGX_Trigger trigger;
};

#define MGX_NEW_EPSILON(target_)                                               \
  (MGX_Transition) { .target = target_, .is_epsilon = true, .trigger = {0} }

#define MGX_NEW_TRANSITION(target_, trigger_)                                  \
  (MGX_Transition) {                                                           \
    .target = target_, .is_epsilon = false, .trigger = trigger_                \
  }

typedef struct MGX_NFA {
  MGX_State start;
  MGX_State end;
} MGX_NFA;

MGX_INLINE MGX_NFA mgx_construct_range(const MGX_NodeRange *range) {
  MGX_ASSERT_DEBUG(range != NULL, "null node argument.");

  MGX_NFA nfa = {.start = MGX_NEW_STATE(false), .end = MGX_NEW_STATE(true)};

  MGX_Trigger trigger = {
      .is_range = true, .from = range->from, .to = range->to};

  darray_push(nfa.start.transitions, MGX_NEW_TRANSITION(nfa.end, trigger));
  return nfa;
}

MGX_INLINE MGX_NFA mgx_construct_class(const MGX_NodeClass *class) {
  MGX_ASSERT_DEBUG(class != NULL, "null node argument.");

  MGX_NFA nfa = {.start = MGX_NEW_STATE(false), .end = MGX_NEW_STATE(true)};

  darray_foreach(class->literals, MGX_Token, it) {
    MGX_Trigger trigger = {.is_range = false, .from = it->value};
    darray_push(nfa.start.transitions, MGX_NEW_TRANSITION(nfa.end, trigger));
  }

  darray_foreach(class->ranges, MGX_NodeRange, it) {
    MGX_NFA range = mgx_construct_range(it);
    darray_push(nfa.start.transitions, MGX_NEW_EPSILON(range.start));
    darray_push(range.end.transitions, MGX_NEW_EPSILON(nfa.end));
  }

  return nfa;
}

MGX_INLINE MGX_NFA mgx_construct_alternation(const MGX_NodeAlternation *);

MGX_INLINE MGX_NFA mgx_construct_group(const MGX_NodeGroup *group) {
  return mgx_construct_alternation(&group->pattern);
}

MGX_INLINE MGX_NFA mgx_construct_atom(const MGX_NodeAtom *atom) {
  MGX_ASSERT_DEBUG(atom != NULL, "null node argument.");

  MGX_NFA nfa = {.start = MGX_NEW_STATE(false), .end = MGX_NEW_STATE(true)};

  switch (atom->holds) {

  case MGX_NODEATOM_LITERAL: {
    MGX_Trigger trigger = {.is_range = false, .from = atom->opts.literal.value};
    darray_push(nfa.start.transitions, MGX_NEW_TRANSITION(nfa.end, trigger));
  } break;

  case MGX_NODEATOM_ANY: {
    MGX_Trigger trigger = {.is_range = false, .from = '.', .to = 0};
    darray_push(nfa.start.transitions, MGX_NEW_TRANSITION(nfa.end, trigger));
  } break;

  case MGX_NODEATOM_GROUP:
    nfa = mgx_construct_group(&atom->opts.group);
    break;

  case MGX_NODEATOM_CLASS:
    nfa = mgx_construct_class(&atom->opts.class);
    break;

  default:
    MGX_ASSERT(false, "unreachable.");
  }

  return nfa;
}

MGX_INLINE MGX_NFA
mgx_construct_quantified_atom(const MGX_NodeQuantifiedAtom *quantified) {
  MGX_ASSERT_DEBUG(quantified != NULL, "null node argument.");

  MGX_NFA atom = mgx_construct_atom(&quantified->atom);
  if (!quantified->has_quantifier)
    return atom;

  MGX_NodeQuantifier quantifier = quantified->quantifier;
  if (quantifier.upper == (size_t)-1) {
    MGX_NFA nfa = {.start = MGX_NEW_STATE(false), .end = MGX_NEW_STATE(true)};

    if (quantifier.lower == 0) { // *
      darray_push(nfa.start.transitions, MGX_NEW_EPSILON(nfa.end));
      darray_push(nfa.start.transitions, MGX_NEW_EPSILON(atom.start));
      darray_push(atom.end.transitions, MGX_NEW_EPSILON(atom.start));
      darray_push(atom.end.transitions, MGX_NEW_EPSILON(nfa.end));
    }

    else if (quantifier.lower == 1) { // +
      darray_push(nfa.start.transitions, MGX_NEW_EPSILON(atom.start));
      darray_push(atom.end.transitions, MGX_NEW_EPSILON(atom.start));
      darray_push(atom.end.transitions, MGX_NEW_EPSILON(nfa.end));
    }

    else {
      MGX_ASSERT(false, "unimplemented.");
    }

    return nfa;
  }

  else {
    if ((quantifier.lower == 0) && (quantifier.upper == 1)) { // ?
      darray_push(atom.start.transitions, MGX_NEW_EPSILON(atom.end));
    }

    else {
      MGX_ASSERT(false, "unimplemented.");
    }
  }

  return atom;
}

MGX_INLINE MGX_NFA
mgx_construct_concatenation(const MGX_NodeConcatenation *concat) {
  MGX_ASSERT_DEBUG(concat != NULL, "null node argument.");

  MGX_NFA nfa = {.start = MGX_NEW_STATE(false), .end = MGX_NEW_STATE(true)};
  MGX_State last = nfa.start;

  darray_foreach(concat->quantifieds, MGX_NodeQuantifiedAtom, it) {
    MGX_NFA quantified = mgx_construct_quantified_atom(it);

    darray_push(last.transitions, MGX_NEW_EPSILON(quantified.start));
    last = quantified.end;
  }

  darray_push(last.transitions, MGX_NEW_EPSILON(nfa.end));
  return nfa;
}

MGX_INLINE MGX_NFA
mgx_construct_alternation(const MGX_NodeAlternation *alternation) {
  MGX_ASSERT_DEBUG(alternation != NULL, "null node argument.");

  MGX_NFA nfa = {.start = MGX_NEW_STATE(false), .end = MGX_NEW_STATE(true)};

  darray_foreach(alternation->concats, MGX_NodeConcatenation, it) {
    MGX_NFA concat = mgx_construct_concatenation(it);

    darray_push(nfa.start.transitions, MGX_NEW_EPSILON(concat.start));
    darray_push(concat.end.transitions, MGX_NEW_EPSILON(nfa.end));
  }

  return nfa;
}

MGX_INLINE MGX_NFA mgx_construct_nfa(const MGX_NodePattern *tree) {
  MGX_ASSERT_DEBUG(tree != NULL, "null syntax-tree argument.");
  return mgx_construct_alternation(&tree->root);
}

#endif