#include <iostream>
#include <string>
#include <vector>
#include <regex>

#include "cling/Interpreter/Interpreter.h"
#include "cling/Interpreter/Transaction.h"

#include "clang/AST/Decl.h"
#include "clang/AST/DeclGroup.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Lex/PPCallbacks.h"
#include "clang/Lex/Preprocessor.h"
#include "llvm/Support/Casting.h"

#include <set>
#include <sstream>
#include <iomanip>
#include <fstream>

using interpreter_ptr = std::unique_ptr<cling::Interpreter>;

struct PreprocessorDirective {
    std::string type;
    int line;
    int startCol;
    int endCol;
};

class PreprocessorCallbacks : public clang::PPCallbacks {
private:
    std::vector<PreprocessorDirective>& directives_;
    clang::SourceManager& sourceManager_;

public:
    PreprocessorCallbacks(std::vector<PreprocessorDirective>& directives,
                         clang::SourceManager& sourceManager)
        : directives_(directives), sourceManager_(sourceManager) {}

    void InclusionDirective(
        clang::SourceLocation HashLoc,
        const clang::Token& IncludeTok,
        llvm::StringRef FileName,
        bool IsAngled,
        clang::CharSourceRange FilenameRange,
        clang::OptionalFileEntryRef File,
        llvm::StringRef SearchPath,
        llvm::StringRef RelativePath,
        const clang::Module* Imported,
        clang::SrcMgr::CharacteristicKind FileType) override {

        // Only capture includes from our input source
        if (HashLoc.isValid()) {
            clang::FileID fileID = sourceManager_.getFileID(HashLoc);
            const clang::FileEntry* fileEntry = sourceManager_.getFileEntryForID(fileID);

            std::string filename = "";
            if (fileEntry) {
                filename = std::string(fileEntry->getName());
            } else {
                // For memory buffers/input, use the buffer name
                if (auto bufferPtr = sourceManager_.getBufferOrNone(fileID)) {
                    filename = bufferPtr->getBufferIdentifier().str();
                }
            }

            // Filter: ONLY show includes from actual input lines
            bool isFromInput = filename.find("input_line_") != std::string::npos ||
                              filename.empty() || filename == "<stdin>";

            if (isFromInput) {
                clang::PresumedLoc presumedLoc = sourceManager_.getPresumedLoc(HashLoc);
                if (presumedLoc.isValid()) {
                    PreprocessorDirective directive;
                    directive.type = "Include";
                    directive.line = presumedLoc.getLine();
                    directive.startCol = presumedLoc.getColumn();
                    directive.endCol = directive.startCol + 8 + FileName.size(); // Rough estimate
                    directives_.push_back(directive);
                }
            }
        }
    }

    void MacroDefined(const clang::Token& MacroNameTok,
                      const clang::MacroDirective* MD) override {
        clang::SourceLocation loc = MacroNameTok.getLocation();
        if (loc.isValid()) {
            clang::FileID fileID = sourceManager_.getFileID(loc);
            const clang::FileEntry* fileEntry = sourceManager_.getFileEntryForID(fileID);

            std::string filename = "";
            if (fileEntry) {
                filename = std::string(fileEntry->getName());
            } else {
                // For memory buffers/input, use the buffer name
                if (auto bufferPtr = sourceManager_.getBufferOrNone(fileID)) {
                    filename = bufferPtr->getBufferIdentifier().str();
                }
            }

            // Filter: ONLY show defines from actual input lines
            bool isFromInput = filename.find("input_line_") != std::string::npos ||
                              filename.empty() || filename == "<stdin>";

            if (isFromInput) {
                clang::PresumedLoc presumedLoc = sourceManager_.getPresumedLoc(loc);
                if (presumedLoc.isValid()) {
                    PreprocessorDirective directive;
                    directive.type = "Define";
                    directive.line = presumedLoc.getLine();
                    directive.startCol = presumedLoc.getColumn();
                    directive.endCol = directive.startCol + MacroNameTok.getLength();
                    directives_.push_back(directive);
                }
            }
        }
    }
};

interpreter_ptr build_interpreter(int argc, char** argv)
{
    int interpreter_argc = argc + 1;
    const char** interpreter_argv = new const char*[interpreter_argc];
    interpreter_argv[0] = "cling-parser";

    // Copy all arguments in the new array excepting the process name.
    for (int i = 1; i < argc; i++)
    {
        interpreter_argv[i] = argv[i];
    }

    std::string include_dir = std::string(LLVM_DIR) + std::string("/include");
    interpreter_argv[interpreter_argc - 1] = include_dir.c_str();

    interpreter_ptr interp_ptr = std::make_unique<cling::Interpreter>(interpreter_argc, interpreter_argv, LLVM_DIR);

    delete[] interpreter_argv;
    return interp_ptr;
}

int main(int argc, char* argv[])
{
    std::string code((std::istreambuf_iterator<char>(std::cin)),
                     std::istreambuf_iterator<char>());

    auto interp = build_interpreter(argc, argv);

    // Set up preprocessor callbacks to capture directives
    std::vector<PreprocessorDirective> preprocessorDirectives;
    clang::SourceManager& SM = interp->getCI()->getSourceManager();
    clang::Preprocessor& PP = interp->getCI()->getPreprocessor();

    auto callbacks = std::make_unique<PreprocessorCallbacks>(preprocessorDirectives, SM);
    PP.addPPCallbacks(std::move(callbacks));

    // Try basic Cling parsing for declarations (this will trigger the callbacks)
    cling::Transaction* T = nullptr;
    cling::Interpreter::CompilationResult result = interp->parse(code, &T);

    std::cout << "[";
    bool first = true;

    // Output preprocessor directives first
    for (const auto& directive : preprocessorDirectives) {
        if (!first) {
            std::cout << ",";
        }
        first = false;

        std::cout << "{";
        std::cout << "\"type\":\"" << directive.type << "\"";
        std::cout << ",\"start_line\":" << directive.line;
        std::cout << ",\"start_ch\":" << directive.startCol;
        std::cout << ",\"end_line\":" << directive.line;
        std::cout << ",\"end_ch\":" << directive.endCol;
        std::cout << "}";
    }

    if (T && result == cling::Interpreter::kSuccess) {
        // Get source manager for location information
        clang::SourceManager& SM = interp->getCI()->getSourceManager();

        std::set<clang::Decl*> seenDecls; // Deduplicate declarations

        // Iterate through the declaration groups in the transaction
        for (auto it = T->decls_begin(); it != T->decls_end(); ++it) {
            const cling::Transaction::DelayCallInfo& callInfo = *it;
            clang::DeclGroupRef DGR = callInfo.m_DGR;

            // Iterate through individual declarations in this group
            for (auto declIt = DGR.begin(); declIt != DGR.end(); ++declIt) {
                clang::Decl* decl = *declIt;

                if (decl) {
                    clang::SourceLocation loc = decl->getLocation();

                    // Check if this declaration is from our input (not from system headers)
                    if (loc.isValid()) {
                        clang::FileID fileID = SM.getFileID(loc);
                        const clang::FileEntry* fileEntry = SM.getFileEntryForID(fileID);

                        // Input code shows up as special "input_line_X" files or similar
                        std::string filename = "";
                        if (fileEntry) {
                            filename = std::string(fileEntry->getName());
                        } else {
                            // For memory buffers/input, use the buffer name
                            if (auto bufferPtr = SM.getBufferOrNone(fileID)) {
                                filename = bufferPtr->getBufferIdentifier().str();
                            }
                        }

                        // Filter: ONLY show declarations from actual input lines
                        bool isFromInput = filename.find("input_line_") != std::string::npos;

                        // Skip if we've already seen this declaration (deduplicate)
                        if (isFromInput && seenDecls.find(decl) == seenDecls.end()) {
                            seenDecls.insert(decl);

                            // Get line/column information for start and end
                            clang::SourceRange sourceRange = decl->getSourceRange();
                            clang::SourceLocation startLoc = sourceRange.getBegin();
                            clang::SourceLocation endLoc = sourceRange.getEnd();

                            clang::PresumedLoc startPresumedLoc = SM.getPresumedLoc(startLoc);
                            clang::PresumedLoc endPresumedLoc = SM.getPresumedLoc(endLoc);

                            if (!first) {
                                std::cout << ",";
                            }
                            first = false;

                            std::cout << "{";
                            std::cout << "\"type\":\"" << decl->getDeclKindName() << "\"";

                            if (startPresumedLoc.isValid()) {
                                std::cout << ",\"start_line\":" << startPresumedLoc.getLine();
                                std::cout << ",\"start_ch\":" << startPresumedLoc.getColumn();
                            } else {
                                std::cout << ",\"start_line\":null";
                                std::cout << ",\"start_ch\":null";
                            }

                            if (endPresumedLoc.isValid()) {
                                std::cout << ",\"end_line\":" << endPresumedLoc.getLine();
                                std::cout << ",\"end_ch\":" << endPresumedLoc.getColumn();
                            } else {
                                std::cout << ",\"end_line\":null";
                                std::cout << ",\"end_ch\":null";
                            }

                            std::cout << "}";
                        }
                    }
                }
            }
        }
    }

    std::cout << "]" << std::endl;

    return 0;
}
