#include "Diagnostic.hpp"

namespace {
const char *DiagnosticText[] = {
#define DIAG(ID, Level, Msg) Msg,
#include "Diagnostic.inl"
};

SourceMgr::DiagKind DiagnosticKind[] = {
#define DIAG(ID, Level, Msg) SourceMgr::DK_##Level,
#include "Diagnostic.inl"
};
}

const char *
DiagnosticsEngine::getDiagnosticText(unsigned DiagID) {
 return DiagnosticText[DiagID];
}
SourceMgr::DiagKind
DiagnosticsEngine::getDiagnosticKind(unsigned DiagID) {
 return DiagnosticKind[DiagID];
}