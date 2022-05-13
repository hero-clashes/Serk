#include "TokenKinds.hpp"

static const char * const TokNames[] = {
#define TOK(ID) #ID,
#define KEYWORD(ID, FLAG) #ID,
#include "TokenKinds.inl"
 nullptr
};


const char *tok::getTokenName(TokenKind Kind) {
 return TokNames[Kind];
}

const char *tok::getPunctuatorSpelling(TokenKind Kind) {
 switch (Kind) {
#define PUNCTUATOR(ID, SP) case ID: return SP;
#include "TokenKinds.inl"
 default: break;
 }
 return nullptr;
}


const char *tok::getKeywordSpelling(TokenKind Kind) {
 switch (Kind) {
#define KEYWORD(ID, FLAG) case kw_ ## ID: return #ID;
#include "TokenKinds.inl"
 default: break;
 }
 return nullptr;
}