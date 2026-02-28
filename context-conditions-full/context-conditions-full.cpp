#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>
#include <set>
#include <map>
#include <sstream>
#include <cctype>
#include <Windows.h>

using namespace std;

enum DATA_TYPE {
    TYPE_UNKNOWN = 0,
    TYPE_INT = 1,
    TYPE_SHORT = 2,
    TYPE_LONG = 3,
    TYPE_BOOL = 4,
    TYPE_CHAR = 5,
    TYPE_FUNCTION = 6,
    TYPE_SCOPE = 7,
    TYPE_VOID = 8
};

enum LexemeType {
    typeInt = 10,
    typeShort = 11,
    typeLong = 12,
    typeBool = 13,
    typeChar = 14,
    typeVoid = 15,

    // Константы
    constDec = 20,
    constHex = 21,
    constChar = 22,

    // Специальные знаки
    typePoint = 30,
    typeComma = 31,
    typeSemicolon = 32,
    typeColon = 33,
    typeLeftBracket = 34,
    typeRightBracket = 35,
    typeLeftBrace = 36,
    typeRightBrace = 37,
    typeLeftSqBracket = 38,
    typeRightSqBracket = 39,

    // Операции
    typeAssign = 40,
    typeEq = 41,
    typeNeq = 42,
    typeLess = 43,
    typeLessEq = 44,
    typeGreater = 45,
    typeGreaterEq = 46,
    typeBitAnd = 47,
    typeBitXor = 48,
    typeBitOr = 49,
    typeBitNot = 50,
    typePlus = 51,
    typeMinus = 52,
    typeMul = 53,
    typeDiv = 54,
    typeMod = 55,
    typeShiftLeft = 56,
    typeShiftRight = 57,

    // Логические значения
    typeTrue = 60,
    typeFalse = 61,

    // Зарезервированные слова
    typeReservedIf = 70,
    typeReservedElse = 71,
    typeReservedWhile = 72,
    typeReservedDo = 73,
    typeReservedFor = 74,
    typeReservedSwitch = 75,
    typeReservedCase = 76,
    typeReservedBreak = 77,
    typeReservedContinue = 78,
    typeReservedReturn = 79,
    typeReservedMain = 80,

    // Дополнительно
    typeEnd = 100,
    typeError = 404,
    typeIdentifier = 200
};

struct Node {
    string id;
    DATA_TYPE DataType;
    DATA_TYPE ReturnType;
    int paramCount;
    vector<DATA_TYPE> paramTypes;
    int line;
    int col;
    bool isFunction;
    bool isReserved;
    bool isParameter;
    bool isInitialized;
    bool isUsed;
    Node* next;

    Node(string id = "", DATA_TYPE dt = TYPE_UNKNOWN, DATA_TYPE rt = TYPE_UNKNOWN,
        int line = 0, int col = 0, bool isFunc = false, bool isRes = false,
        bool isParam = false, bool initialized = false, bool used = false)
        : id(id), DataType(dt), ReturnType(rt), paramCount(0), line(line), col(col),
        isFunction(isFunc), isReserved(isRes), isParameter(isParam),
        isInitialized(initialized), isUsed(used), next(nullptr) {

        if (isFunc) {
            DataType = TYPE_FUNCTION;
        }
        else if (isParam) {
            ReturnType = TYPE_UNKNOWN;
        }
        else {
            ReturnType = TYPE_UNKNOWN;
        }
    }
};

struct Lexeme {
    string value;
    LexemeType type;
    int line;
    int col;

    Lexeme(const string& val = "", LexemeType t = typeIdentifier, int l = 0, int c = 0)
        : value(val), type(t), line(l), col(c) {
    }
};

class Tree {
private:
    set<string> reservedWords = {
        "main", "if", "else", "while", "do", "for",
        "switch", "case", "break", "continue", "return",
        "true", "false", "void", "int", "short", "long",
        "bool", "char", "struct", "class", "new", "delete"
    };

    map<DATA_TYPE, string> typeToString = {
        {TYPE_INT, "int"},
        {TYPE_SHORT, "short"},
        {TYPE_LONG, "long"},
        {TYPE_BOOL, "bool"},
        {TYPE_CHAR, "char"},
        {TYPE_FUNCTION, "function"},
        {TYPE_SCOPE, "scope"},
        {TYPE_VOID, "void"},
        {TYPE_UNKNOWN, "unknown"}
    };

    const int MAX_IDENTIFIER_LENGTH = 20;

    Tree* globalRoot;
    vector<string> lines;
    vector<Lexeme> lexemes;
    int currentLine;
    int currentCol;
    set<string> usedSystemFunctions;

    bool inGlobalScope;
    bool inFunctionBody;

    map<string, bool> functionReturnsValue;
    map<string, DATA_TYPE> functionReturnTypes;
    map<string, bool> variableInitialized;
    map<string, bool> variableUsed;
    map<string, int> variableDeclarationLine;
    map<string, DATA_TYPE> variableTypes;

    struct FunctionCallInfo {
        string name;
        int line;
        int col;
        vector<DATA_TYPE> argTypes;
    };
    vector<FunctionCallInfo> functionCalls;

public:
    Node* n;
    Tree* parent;
    Tree* left;
    Tree* right;
    static Tree* cur;

    Tree(Tree* l = nullptr, Tree* r = nullptr, Tree* u = nullptr, Node* Data = nullptr)
        : left(l), right(r), parent(u), n(Data), globalRoot(nullptr),
        currentLine(1), currentCol(1), inGlobalScope(true), inFunctionBody(false) {
        if (!globalRoot && !parent) {
            globalRoot = this;
            n = new Node("global", TYPE_SCOPE, TYPE_UNKNOWN, 0, 0, false, true);
        }
        if (!cur) {
            cur = this;
        }
    }

    ~Tree() {
        if (n) delete n;
    }

    bool LoadFile(const string& filename) {
        ifstream file(filename);
        if (!file.is_open()) {
            cerr << "Ошибка: не удалось открыть файл " << filename << endl;
            return false;
        }

        lines.clear();
        string line;
        while (getline(file, line)) {
            lines.push_back(line);
        }
        file.close();

        cout << "Файл " << filename << " загружен (" << lines.size() << " строк)" << endl;
        return true;
    }

    void LexicalAnalysis() {
        lexemes.clear();
        currentLine = 1;

        for (const auto& line : lines) {
            currentCol = 1;
            string token;
            bool inString = false;
            bool inChar = false;
            bool inComment = false;

            for (size_t i = 0; i < line.size(); i++) {
                char c = line[i];

                if (inComment) continue;

                if (c == '/' && i + 1 < line.size() && line[i + 1] == '/') {
                    inComment = true;
                    i++;
                    continue;
                }

                if (c == '"' && !inChar) {
                    if (!inString) {
                        if (!token.empty()) {
                            ProcessToken(token);
                            token.clear();
                        }
                        inString = true;
                        token += c;
                    }
                    else {
                        token += c;
                        lexemes.push_back(Lexeme(token, constChar, currentLine, currentCol - token.length()));
                        token.clear();
                        inString = false;
                    }
                    currentCol++;
                    continue;
                }

                if (c == '\'' && !inString) {
                    if (!inChar) {
                        if (!token.empty()) {
                            ProcessToken(token);
                            token.clear();
                        }
                        inChar = true;
                        token += c;
                    }
                    else {
                        token += c;
                        lexemes.push_back(Lexeme(token, constChar, currentLine, currentCol - token.length()));
                        token.clear();
                        inChar = false;
                    }
                    currentCol++;
                    continue;
                }

                if (inString || inChar) {
                    token += c;
                    currentCol++;
                    continue;
                }

                if (isspace(c)) {
                    if (!token.empty()) {
                        ProcessToken(token);
                        token.clear();
                    }
                    currentCol++;
                    continue;
                }

                if (ispunct(c)) {
                    if (!token.empty()) {
                        ProcessToken(token);
                        token.clear();
                    }

                    string twoCharOp = string(1, c);
                    if (i + 1 < line.size()) {
                        char next = line[i + 1];
                        string potentialOp = twoCharOp + next;

                        if (potentialOp == "==" || potentialOp == "!=" || potentialOp == "<=" ||
                            potentialOp == ">=" || potentialOp == "&&" || potentialOp == "||" ||
                            potentialOp == "+=" || potentialOp == "-=" || potentialOp == "*=" ||
                            potentialOp == "/=" || potentialOp == "%=" || potentialOp == "&=" ||
                            potentialOp == "|=" || potentialOp == "^=" || potentialOp == "<<=" ||
                            potentialOp == ">>=") {
                            twoCharOp = potentialOp;
                            i++;
                        }
                        else if (potentialOp == "++" || potentialOp == "--" || potentialOp == "<<" ||
                            potentialOp == ">>") {
                            twoCharOp = potentialOp;
                            i++;
                        }
                    }

                    LexemeType opType = typeError;
                    if (twoCharOp == "=") opType = typeAssign;
                    else if (twoCharOp == "==") opType = typeEq;
                    else if (twoCharOp == "!=") opType = typeNeq;
                    else if (twoCharOp == "<") opType = typeLess;
                    else if (twoCharOp == "<=") opType = typeLessEq;
                    else if (twoCharOp == ">") opType = typeGreater;
                    else if (twoCharOp == ">=") opType = typeGreaterEq;
                    else if (twoCharOp == "&") opType = typeBitAnd;
                    else if (twoCharOp == "|") opType = typeBitOr;
                    else if (twoCharOp == "^") opType = typeBitXor;
                    else if (twoCharOp == "~") opType = typeBitNot;
                    else if (twoCharOp == "+") opType = typePlus;
                    else if (twoCharOp == "-") opType = typeMinus;
                    else if (twoCharOp == "*") opType = typeMul;
                    else if (twoCharOp == "/") opType = typeDiv;
                    else if (twoCharOp == "%") opType = typeMod;
                    else if (twoCharOp == "<<") opType = typeShiftLeft;
                    else if (twoCharOp == ">>") opType = typeShiftRight;
                    else if (twoCharOp == ";") opType = typeSemicolon;
                    else if (twoCharOp == ",") opType = typeComma;
                    else if (twoCharOp == "(") opType = typeLeftBracket;
                    else if (twoCharOp == ")") opType = typeRightBracket;
                    else if (twoCharOp == "{") opType = typeLeftBrace;
                    else if (twoCharOp == "}") opType = typeRightBrace;
                    else if (twoCharOp == "[") opType = typeLeftSqBracket;
                    else if (twoCharOp == "]") opType = typeRightSqBracket;
                    else if (twoCharOp == ":") opType = typeColon;
                    else if (twoCharOp == ".") opType = typePoint;

                    if (opType == typeError) {
                        cerr << "Лексическая ошибка: неизвестная лексема '"
                            << twoCharOp << "' [строка " << currentLine
                            << ", позиция " << currentCol << "]" << endl;
                    }

                    lexemes.push_back(Lexeme(twoCharOp, opType, currentLine, currentCol));
                    currentCol += twoCharOp.length();
                    continue;
                }

                token += c;
                currentCol++;
            }

            if (!token.empty()) {
                ProcessToken(token);
            }

            if (!inString && !inChar) {
                currentLine++;
            }
        }

        cout << "Найдено " << lexemes.size() << " лексем." << endl;
    }

    void ProcessToken(string& token) {
        LexemeType type = typeIdentifier;

        if (token == "int") type = typeInt;
        else if (token == "short") type = typeShort;
        else if (token == "long") type = typeLong;
        else if (token == "bool") type = typeBool;
        else if (token == "char") type = typeChar;
        else if (token == "void") type = typeVoid;
        else if (token == "true") type = typeTrue;
        else if (token == "false") type = typeFalse;
        else if (token == "main") type = typeReservedMain;
        else if (token == "if") type = typeReservedIf;
        else if (token == "else") type = typeReservedElse;
        else if (token == "while") type = typeReservedWhile;
        else if (token == "for") type = typeReservedFor;
        else if (token == "return") type = typeReservedReturn;
        else if (token == "break") type = typeReservedBreak;
        else if (token == "continue") type = typeReservedContinue;
        else if (token == "switch") type = typeReservedSwitch;
        else if (token == "case") type = typeReservedCase;
        else if (token == "do") type = typeReservedDo;
        else if (token.size() >= 2 && token[0] == '0' && (token[1] == 'x' || token[1] == 'X')) {
            bool valid = true;
            if (token.size() == 2) {
                valid = false;
            }
            else {
                for (size_t i = 2; i < token.size(); i++) {
                    char c = token[i];
                    if (!isxdigit(c)) {
                        valid = false;
                        break;
                    }
                }
            }

            if (!valid) {
                type = typeError;
                cerr << "Лексическая ошибка: некорректная шестнадцатеричная константа '"
                    << token << "' [строка " << currentLine << ", позиция "
                    << (currentCol - (int)token.length()) << "]" << endl;
            }
            else {
                type = constHex;
            }
        }
        else if (isdigit(token[0])) {
            bool valid = true;
            for (char c : token) {
                if (!isdigit(c)) {
                    valid = false;
                    break;
                }
            }
            if (valid) {
                type = constDec;
            }
            else {
                type = typeError;
                cerr << "Лексическая ошибка: некорректная десятичная константа '"
                    << token << "' [строка " << currentLine << ", позиция "
                    << (currentCol - (int)token.length()) << "]" << endl;
            }
        }
        else if (token.size() >= 3 && token[0] == '\'' && token[token.size() - 1] == '\'') {
            if (token.size() == 3) {
                type = constChar;
            }
            else if (token.size() == 2) {
                type = typeError;
                cerr << "Лексическая ошибка: некорректная символьная константа '' [строка "
                    << currentLine << ", позиция " << (currentCol - (int)token.length())
                    << "]" << endl;
            }
            else {
                type = typeError;
                cerr << "Лексическая ошибка: некорректная символьная константа '"
                    << token << "' [строка " << currentLine << ", позиция "
                    << (currentCol - (int)token.length()) << "]" << endl;
            }
        }
        else if (token.size() >= 2 && token[0] == '"' && token[token.size() - 1] == '"') {
            type = constChar;
        }
        else {
            if (token.length() > MAX_IDENTIFIER_LENGTH) {
                type = typeError;
                cerr << "Лексическая ошибка: слишком длинный идентификатор (максимум "
                    << MAX_IDENTIFIER_LENGTH << " символов) '"
                    << token.substr(0, MAX_IDENTIFIER_LENGTH) << "...' [строка "
                    << currentLine << ", позиция " << (currentCol - (int)token.length())
                    << "]" << endl;
            }
            else {
                type = typeIdentifier;
            }
        }

        int col = max(1, currentCol - (int)token.length());
        lexemes.push_back(Lexeme(token, type, currentLine, col));
        token.clear();
    }

    void MarkVariableInitialized(const string& varName) {
        variableInitialized[varName] = true;
        Tree* varNode = FindVariableInTree(varName);
        if (varNode && varNode->n) {
            varNode->n->isInitialized = true;
        }
    }

    void MarkVariableUsed(const string& varName, int line = 0) {
        variableUsed[varName] = true;
        Tree* varNode = FindVariableInTree(varName);
        if (varNode && varNode->n) {
            varNode->n->isUsed = true;
        }
    }

    bool IsVariableInitialized(const string& varName) {
        auto it = variableInitialized.find(varName);
        if (it != variableInitialized.end()) {
            return it->second;
        }
        return false;
    }

    Tree* FindVariableInTree(const string& varName) {
        Tree* found = FindParent(cur, varName);
        if (!found) {
            Tree* global = GetGlobalScope();
            found = FindParent(global, varName);
        }
        return found;
    }

    void CheckVariableInitialization(const string& varName, int line) {
        if (IsReservedWord(varName) || varName == "true" || varName == "false") {
            return;
        }

        Tree* varNode = FindVariableInTree(varName);
        if (varNode && varNode->n && varNode->n->isFunction) {
            return;
        }

        if (!IsVariableInitialized(varName)) {
            if (varNode && varNode->n && varNode->n->isParameter) {
                return;
            }

            cerr << "Семантическая ошибка: использование неинициализированной переменной '"
                << varName << "' [строка " << line << "]" << endl;
        }
    }

    void CheckUnusedVariables() {
        bool hasUnused = false;

        for (const auto& var : variableUsed) {
            if (!var.second) {
                auto lineIt = variableDeclarationLine.find(var.first);
                if (lineIt != variableDeclarationLine.end()) {
                    hasUnused = true;
                }
            }
        }

        CheckUnusedVariablesInTree(globalRoot);
    }

    void CheckUnusedVariablesInTree(Tree* node) {
        if (!node || !node->n) return;

        if (!node->n->isFunction && !node->n->isParameter &&
            node->n->DataType != TYPE_SCOPE && !node->n->isReserved &&
            node->n->id != "global") {

            if (!node->n->isUsed) {
                cout << "Предупреждение: переменная '" << node->n->id
                    << "' типа '" << GetTypeString(node->n->DataType)
                    << "' объявлена, но не используется [строка "
                    << node->n->line << "]" << endl;
            }
        }

        Tree* child = node->left;
        while (child) {
            CheckUnusedVariablesInTree(child);
            child = child->right;
        }
    }

    void SyntaxAnalysis() {
        for (size_t i = 0; i < lexemes.size(); i++) {
            Lexeme& lex = lexemes[i];

            if (lex.type == typeError) continue;

            // Проверка объявления переменной
            if (IsTypeKeyword(lex.type)) {
                bool isFunction = false;

                if (i + 1 < lexemes.size() && (lexemes[i + 1].type == typeIdentifier ||
                    lexemes[i + 1].type == typeReservedMain)) {
                    int k = i + 2;
                    while (k < lexemes.size()) {
                        if (lexemes[k].type == typeLeftBracket) {
                            isFunction = true;
                            break;
                        }
                        if (lexemes[k].type == typeSemicolon ||
                            lexemes[k].type == typeAssign ||
                            lexemes[k].type == typeComma ||
                            lexemes[k].type == typeLeftBrace) {
                            break;
                        }
                        k++;
                    }
                }

                if (isFunction) {
                    continue;
                }

                // Проверка переменной без имени
                if (i + 1 < lexemes.size()) {
                    if (lexemes[i + 1].type != typeIdentifier && lexemes[i + 1].type != typeReservedMain) {
                        cerr << "Синтаксическая ошибка: ожидался идентификатор в списке объявлений (около '"
                            << lexemes[i + 1].value << "') [строка " << lexemes[i + 1].line
                            << ", позиция " << lexemes[i + 1].col << "]" << endl;

                        int j = i + 1;
                        while (j < lexemes.size() && lexemes[j].type != typeSemicolon) {
                            j++;
                        }
                        i = j;
                        continue;
                    }
                }

                if (i + 1 < lexemes.size() && (lexemes[i + 1].type == typeIdentifier || lexemes[i + 1].type == typeReservedMain)) {
                    int j = i + 2;
                    bool foundSemicolon = false;

                    while (j < lexemes.size()) {
                        if (lexemes[j].type == typeSemicolon) {
                            foundSemicolon = true;
                            break;
                        }
                        if (lexemes[j].type == typeLeftBrace ||
                            lexemes[j].type == typeRightBrace) {
                            break;
                        }
                        j++;
                    }

                    if (!foundSemicolon && j < lexemes.size() &&
                        lexemes[j].type != typeLeftBrace &&
                        lexemes[j].type != typeRightBrace) {

                        cerr << "Синтаксическая ошибка: ожидался ';' в конце объявления переменной [строка "
                            << lex.line << ", позиция " << lex.col << "]" << endl;
                    }
                }
            }

            // Проверка вызова функции
            if (lex.type == typeIdentifier && i + 1 < lexemes.size() &&
                lexemes[i + 1].type == typeLeftBracket) {

                bool isFunctionDeclaration = false;
                if (i > 0 && (lexemes[i - 1].type == typeVoid || IsTypeKeyword(lexemes[i - 1].type))) {
                    isFunctionDeclaration = true;
                }

                if (!isFunctionDeclaration) {
                    int bracketDepth = 1;
                    int j = i + 2;
                    bool foundRightBracket = false;

                    while (j < lexemes.size() && bracketDepth > 0) {
                        if (lexemes[j].type == typeLeftBracket) {
                            bracketDepth++;
                        }
                        else if (lexemes[j].type == typeRightBracket) {
                            bracketDepth--;
                            if (bracketDepth == 0) {
                                foundRightBracket = true;
                                break;
                            }
                        }
                        else if (lexemes[j].type == typeSemicolon && bracketDepth > 0) {
                            cerr << "Синтаксическая ошибка: ожидался ')' перед ';' [строка "
                                << lexemes[j].line << ", позиция " << lexemes[j].col << "]" << endl;
                            foundRightBracket = false;
                            break;
                        }
                        j++;
                    }

                    if (!foundRightBracket && j < lexemes.size()) {
                        cerr << "Синтаксическая ошибка: ожидался ')' после вызова функции '"
                            << lex.value << "' [строка " << lex.line << ", позиция " << lex.col << "]" << endl;
                    }

                    if (foundRightBracket) {
                        j++;
                        bool foundSemicolon = false;

                        while (j < lexemes.size()) {
                            if (lexemes[j].type == typeSemicolon) {
                                foundSemicolon = true;
                                break;
                            }
                            if (lexemes[j].type == typeRightBrace ||
                                lexemes[j].type == typeLeftBrace) {
                                break;
                            }
                            j++;
                        }

                        if (!foundSemicolon && j < lexemes.size()) {
                            cerr << "Синтаксическая ошибка: ожидался ';' после вызова функции [строка "
                                << lex.line << ", позиция " << lex.col << "]" << endl;
                        }
                    }
                }
            }

            // Проверка оператора break
            if (lex.type == typeReservedBreak) {
                int j = i + 1;
                bool foundSemicolon = false;

                while (j < lexemes.size()) {
                    if (lexemes[j].type == typeSemicolon) {
                        foundSemicolon = true;
                        break;
                    }
                    if (lexemes[j].type == typeRightBrace) {
                        break;
                    }
                    j++;
                }

                if (!foundSemicolon) {
                    cerr << "Синтаксическая ошибка: ожидался ';' после break [строка "
                        << lex.line << ", позиция " << lex.col << "]" << endl;
                }
            }

            // Проверка объявления функции
            if (i + 1 < lexemes.size() && (lexemes[i].type == typeIdentifier || lexemes[i].type == typeReservedMain)) {
                // Проверяем, что перед идентификатором есть тип (int, void, etc.)
                bool hasTypeBefore = (i > 0 && (lexemes[i - 1].type == typeVoid || IsTypeKeyword(lexemes[i - 1].type)));

                if (hasTypeBefore) {
                    // Проверяем, что это функция (после идентификатора идет '(')
                    if (i + 1 < lexemes.size() && lexemes[i + 1].type == typeLeftBracket) {
                        // Нашли объявление функции

                        // Пропускаем список параметров до закрывающей скобки
                        int bracketDepth = 1;
                        int j = i + 2;
                        bool foundRightBracket = false;

                        while (j < lexemes.size() && bracketDepth > 0) {
                            if (lexemes[j].type == typeLeftBracket) bracketDepth++;
                            else if (lexemes[j].type == typeRightBracket) {
                                bracketDepth--;
                                if (bracketDepth == 0) {
                                    foundRightBracket = true;
                                    break;
                                }
                            }
                            j++;
                        }

                        if (!foundRightBracket) {
                            cerr << "Синтаксическая ошибка: ожидался ')' после списка параметров функции '"
                                << lexemes[i].value << "' [строка " << lexemes[i].line
                                << ", позиция " << lexemes[i].col << "]" << endl;
                        }
                        else {
                            // Проверяем, что после ')' идет '{' (тело функции) или ';' (прототип)
                            j++; // Переходим к следующей лексеме после ')'

                            // Пропускаем возможные пробелы (в лексемах их нет, но проверяем следующие лексемы)
                            if (j < lexemes.size()) {
                                if (lexemes[j].type != typeLeftBrace && lexemes[j].type != typeSemicolon) {
                                    cerr << "Синтаксическая ошибка: ожидался '{' или ';' после объявления функции '"
                                        << lexemes[i].value << "' [строка " << lexemes[j].line
                                        << ", позиция " << lexemes[j].col << "]" << endl;
                                }
                            }
                        }
                    }
                }
            }

            // Проверка закрывающей фигурной скобки
            if (lex.type == typeLeftBrace) {
                int braceDepth = 1;
                int j = i + 1;
                bool foundRightBrace = false;

                while (j < lexemes.size() && braceDepth > 0) {
                    if (lexemes[j].type == typeLeftBrace) braceDepth++;
                    else if (lexemes[j].type == typeRightBrace) {
                        braceDepth--;
                        if (braceDepth == 0) {
                            foundRightBrace = true;
                            break;
                        }
                    }
                    j++;
                }

                if (!foundRightBrace && j >= lexemes.size()) {
                    cerr << "Синтаксическая ошибка: ожидался '}' для конца блока [строка "
                        << lex.line << ", позиция " << lex.col << "]" << endl;
                }
            }
        }
    }

    // Функция для получения типов аргументов функции
    vector<DATA_TYPE> GetFunctionArgumentTypes(int startPos, int line) {
        vector<DATA_TYPE> argTypes;

        if (startPos >= lexemes.size() || lexemes[startPos].type != typeLeftBracket) {
            return argTypes;
        }

        int bracketDepth = 1;
        int i = startPos + 1; // Позиция после '('
        int argStart = i;

        while (i < lexemes.size() && bracketDepth > 0) {
            LexemeType currentType = lexemes[i].type;

            if (currentType == typeLeftBracket) {
                bracketDepth++;
            }
            else if (currentType == typeRightBracket) {
                bracketDepth--;

                if (bracketDepth == 0) {
                    // Обрабатываем последний аргумент перед закрывающей скобкой
                    if (argStart < i) {
                        // Проверяем, не пустой ли аргумент (случай "()")
                        bool hasContent = false;
                        for (int k = argStart; k < i; k++) {
                            if (lexemes[k].type != typeComma) {
                                hasContent = true;
                                break;
                            }
                        }

                        if (hasContent) {
                            DATA_TYPE argType = GetExpressionType(argStart, line);
                            if (argType != TYPE_UNKNOWN) {
                                argTypes.push_back(argType);
                            }
                        }
                    }
                    break;
                }
            }
            else if (currentType == typeComma && bracketDepth == 1) {
                // Запятая на верхнем уровне - разделитель аргументов
                if (argStart < i) {
                    DATA_TYPE argType = GetExpressionType(argStart, line);
                    if (argType != TYPE_UNKNOWN) {
                        argTypes.push_back(argType);
                    }
                }
                argStart = i + 1;
            }

            i++;
        }

        return argTypes;
    }

    // Функция для проверки вызовов функций после их объявления
    void CheckFunctionCalls() {

        for (const auto& call : functionCalls) {
            Tree* func = FindParent(globalRoot, call.name);

            if (!func || !func->n || !func->n->isFunction) {
                if (!IsSystemFunction(call.name)) {
                    cerr << "Семантическая ошибка: вызов необъявленной функции '"
                        << call.name << "' [строка " << call.line << "]" << endl;
                }
                continue;
            }

            // Проверка количества параметров
            int expectedParams = func->n->paramCount;
            int actualParams = call.argTypes.size();

            if (expectedParams != actualParams) {
                cerr << "Семантическая ошибка: неверное количество аргументов при вызове функции '"
                    << call.name << "' (ожидалось " << expectedParams
                    << ", получено " << actualParams << ") [строка " << call.line << "]" << endl;
            }
            else {
                // Проверка типов параметров
                for (size_t i = 0; i < call.argTypes.size(); i++) {
                    DATA_TYPE expectedType = func->n->paramTypes[i];
                    DATA_TYPE actualType = call.argTypes[i];

                    if (!CheckTypeCompatibility(expectedType, actualType, "parameter", call.line, 0, false)) {
                        cerr << "Семантическая ошибка: несоответствие типа " << (i + 1)
                            << "-го аргумента при вызове функции '" << call.name
                            << "' (ожидался '" << GetTypeString(expectedType)
                            << "', получен '" << GetTypeString(actualType)
                            << "') [строка " << call.line << "]" << endl;
                    }
                }
            }
        }
    }

    void SemanticAnalysis() {
        InitializePredefinedFunctions();
        SetCur(globalRoot);
        inGlobalScope = true;
        inFunctionBody = false;

        variableInitialized.clear();
        variableUsed.clear();
        variableDeclarationLine.clear();
        variableTypes.clear();
        functionCalls.clear();

        bool inFunctionDeclaration = false;
        Tree* currentFunction = nullptr;
        int braceCount = 0;
        int paramCount = 0;
        vector<pair<string, DATA_TYPE>> currentParams;
        DATA_TYPE currentReturnType = TYPE_VOID;
        string currentFunctionName;

        for (size_t i = 0; i < lexemes.size(); i++) {
            Lexeme& lex = lexemes[i];

            if (lex.type == typeError) {
                continue;
            }

            if (inGlobalScope && lex.type == typeAssign && i > 0) {
                bool isInitialization = false;
                for (int j = i - 1; j >= 0; j--) {
                    if (lexemes[j].type == typeSemicolon) break;
                    if (IsTypeKeyword(lexemes[j].type)) {
                        isInitialization = true;
                        break;
                    }
                }

                if (!isInitialization) {
                    cerr << "Семантическая ошибка: оператор присваивания вне тела функции запрещен [строка "
                        << lex.line << "]" << endl;
                }
            }

            // Переменная без типа (только внутри функций)
            if (lex.type == typeIdentifier && !inFunctionDeclaration && inFunctionBody) {
                if (i + 1 < lexemes.size() && lexemes[i + 1].type == typeAssign) {
                    // Проверяем, что перед идентификатором нет типа
                    bool hasType = false;
                    if (i > 0 && IsTypeKeyword(lexemes[i - 1].type)) {
                        hasType = true;
                    }

                    if (!hasType) {
                        Tree* var = SemGetVar(lex.value, lex.line, lex.col);
                        if (!var) {
                            cerr << "Семантическая ошибка: отсутствует описание идентификатора '"
                                << lex.value << "' [строка " << lex.line << "]" << endl;
                        }
                    }
                }
            }

            if (lex.type == typeLeftBrace) {
                braceCount++;

                if (inFunctionDeclaration && currentFunction) {
                    inFunctionDeclaration = false;
                    inFunctionBody = true;
                    inGlobalScope = false;

                    for (const auto& param : currentParams) {
                        if (!DupControl(cur, param.first)) {
                            Tree* paramNode = SemInclude(param.first, param.second, lex.line, lex.col, false, TYPE_UNKNOWN, true);
                            if (paramNode && paramNode->n) {
                                paramNode->n->isParameter = true;
                                MarkVariableInitialized(param.first);
                                variableDeclarationLine[param.first] = lex.line;
                                variableTypes[param.first] = param.second;
                            }
                        }
                    }
                }

                SemEnterBlock(lex.line, lex.col);
                continue;
            }

            if (lex.type == typeRightBrace) {
                braceCount--;

                if (braceCount == 0 && inFunctionBody) {
                    inFunctionBody = false;
                    inGlobalScope = true;
                    currentFunction = nullptr;
                    currentFunctionName.clear();
                    currentParams.clear();
                }

                SemExitBlock();
                continue;
            }

            // Объявление функции
            if (lex.type == typeVoid || lex.type == typeInt ||
                lex.type == typeChar || lex.type == typeBool || lex.type == typeLong) {

                DATA_TYPE retType = GetDataTypeFromLexeme(lex.type);

                if (i + 1 < lexemes.size() &&
                    (lexemes[i + 1].type == typeIdentifier || lexemes[i + 1].type == typeReservedMain)) {
                    string funcName = lexemes[i + 1].value;

                    int j = i + 2;
                    bool isFunction = false;

                    while (j < lexemes.size()) {
                        if (lexemes[j].type == typeLeftBracket) {
                            isFunction = true;
                            break;
                        }
                        else if (lexemes[j].type == typeAssign ||
                            lexemes[j].type == typeLeftBrace ||
                            lexemes[j].type == typeSemicolon) {
                            isFunction = false;
                            break;
                        }
                        j++;
                    }

                    if (isFunction) {
                        currentFunction = SemInclude(funcName, TYPE_FUNCTION, lex.line, lex.col, true, retType);
                        inFunctionDeclaration = true;
                        currentReturnType = retType;
                        paramCount = 0;
                        currentParams.clear();
                        currentFunctionName = funcName;

                        if (currentFunction && currentFunction->n) {
                            cout << "Объявлена функция: " << funcName << " -> "
                                << GetTypeString(retType) << endl;

                            functionReturnsValue[funcName] = false;
                            functionReturnTypes[funcName] = retType;
                        }

                        j = i + 2;
                        while (j < lexemes.size() && lexemes[j].type != typeLeftBracket) {
                            j++;
                        }
                        i = j;

                        int bracketDepth = 1;
                        i++;

                        while (i < lexemes.size() && bracketDepth > 0) {
                            if (lexemes[i].type == typeRightBracket) {
                                bracketDepth--;
                                if (bracketDepth == 0) break;
                            }
                            else if (lexemes[i].type == typeLeftBracket) {
                                bracketDepth++;
                            }
                            else if (lexemes[i].type == typeInt || lexemes[i].type == typeChar ||
                                lexemes[i].type == typeBool || lexemes[i].type == typeLong ||
                                lexemes[i].type == typeVoid) {
                                DATA_TYPE paramType = GetDataTypeFromLexeme(lexemes[i].type);

                                if (i + 1 < lexemes.size() && lexemes[i + 1].type == typeIdentifier) {
                                    string paramName = lexemes[i + 1].value;
                                    currentParams.push_back({ paramName, paramType });
                                    paramCount++;
                                    i++;
                                }
                            }
                            i++;
                        }

                        if (currentFunction && currentFunction->n) {
                            currentFunction->n->paramCount = paramCount;
                            vector<DATA_TYPE> paramTypes;
                            for (const auto& param : currentParams) {
                                paramTypes.push_back(param.second);
                            }
                            currentFunction->n->paramTypes = paramTypes;
                        }
                        continue;
                    }
                }
            }

            // Объявление переменной
            if (IsTypeKeyword(lex.type)) {
                DATA_TYPE varType = GetDataTypeFromLexeme(lex.type);

                bool isFunctionDeclaration = false;
                if (i + 1 < lexemes.size() &&
                    (lexemes[i + 1].type == typeIdentifier || lexemes[i + 1].type == typeReservedMain)) {
                    string varName = lexemes[i + 1].value;

                    int j = i + 2;
                    while (j < lexemes.size()) {
                        if (lexemes[j].type == typeLeftBracket) {
                            isFunctionDeclaration = true;
                            break;
                        }
                        else if (lexemes[j].type == typeAssign ||
                            lexemes[j].type == typeSemicolon ||
                            lexemes[j].type == typeComma) {
                            isFunctionDeclaration = false;
                            break;
                        }
                        else if (lexemes[j].type == typeLeftBrace) {
                            int k = j - 1;
                            while (k > i + 1 && lexemes[k].type != typeLeftBracket &&
                                lexemes[k].type != typeSemicolon) {
                                k--;
                            }
                            if (k > i + 1 && lexemes[k].type == typeLeftBracket) {
                                isFunctionDeclaration = true;
                            }
                            else {
                                isFunctionDeclaration = false;
                            }
                            break;
                        }
                        j++;
                    }
                }

                if (!isFunctionDeclaration && i + 1 < lexemes.size() &&
                    (lexemes[i + 1].type == typeIdentifier || lexemes[i + 1].type == typeReservedMain)) {
                    string varName = lexemes[i + 1].value;

                    if (!CheckReservedWordUsage(varName, lex.line, lex.col)) {
                        Tree* var = SemInclude(varName, varType, lex.line, lex.col, false, TYPE_UNKNOWN);
                        if (var && var->n) {
                            cout << "Объявлена переменная: " << varName
                                << " : " << GetTypeString(varType)
                                << " [строка " << lex.line << "]" << endl;

                            variableInitialized[varName] = false;
                            variableUsed[varName] = false;
                            variableDeclarationLine[varName] = lex.line;
                            variableTypes[varName] = varType;

                            if (i + 2 < lexemes.size() && lexemes[i + 2].type == typeAssign) {
                                MarkVariableInitialized(varName);

                                // Проверяем инициализатор на наличие идентификаторов
                                int initPos = i + 3;
                                while (initPos < lexemes.size() && lexemes[initPos].type != typeSemicolon) {
                                    if (lexemes[initPos].type == typeIdentifier) {
                                        string initVar = lexemes[initPos].value;
                                        Tree* initVarNode = SemGetVar(initVar, lexemes[initPos].line, lexemes[initPos].col);
                                        if (!initVarNode) {
                                            /*cerr << "Семантическая ошибка: идентификатор '" << initVar
                                                << "' не объявлен в этой области видимости [строка "
                                                << lexemes[initPos].line << "]" << endl;*/
                                        }
                                        else {
                                            MarkVariableUsed(initVar, lexemes[initPos].line);
                                            CheckVariableInitialization(initVar, lexemes[initPos].line);
                                        }
                                    }
                                    initPos++;
                                }
                            }
                        }
                    }
                }
            }

            // Использование идентификатора
            if (lex.type == typeIdentifier) {
                string id = lex.value;

                if (id == "true" || id == "false") {
                    continue;
                }

                bool isFunctionCall = (i + 1 < lexemes.size() && lexemes[i + 1].type == typeLeftBracket);

                if (isFunctionCall) {
                    // Получаем типы аргументов функции - передаем позицию открывающей скобки
                    vector<DATA_TYPE> argTypes = GetFunctionArgumentTypes(i + 1, lex.line);

                    // Сохраняем информацию о вызове функции
                    FunctionCallInfo callInfo;
                    callInfo.name = id;
                    callInfo.line = lex.line;
                    callInfo.col = lex.col;
                    callInfo.argTypes = argTypes;
                    functionCalls.push_back(callInfo);

                    // Проверяем, существует ли функция
                    Tree* func = SemGetFunct(id, lex.line, lex.col);

                    if (func && func->n) {
                        // Функция найдена - проверяем параметры
                        if (func->n->paramCount != static_cast<int>(argTypes.size())) {
                            cerr << "Семантическая ошибка: неверное количество аргументов при вызове функции '"
                                << id << "' (ожидалось " << func->n->paramCount
                                << ", получено " << argTypes.size() << ") [строка " << lex.line << "]" << endl;
                        }
                        func->n->isUsed = true;
                    }

                    // Пропускаем вызов функции (включая все вложенные вызовы)
                    int bracketDepth = 1;
                    int j = i + 2; // Начинаем после '('
                    while (j < lexemes.size() && bracketDepth > 0) {
                        if (lexemes[j].type == typeLeftBracket) {
                            bracketDepth++;
                        }
                        else if (lexemes[j].type == typeRightBracket) {
                            bracketDepth--;
                            if (bracketDepth == 0) {
                                j++;
                                break;
                            }
                        }
                        j++;
                    }
                    i = j - 1;
                    continue;
                }

                if (!IsReservedWord(id) || id == "true" || id == "false") {
                    Tree* var = SemGetVar(id, lex.line, lex.col);

                    if (var) {
                        if (var->n && !var->n->isFunction) {
                            MarkVariableUsed(id, lex.line);

                            bool isLeftSideOfAssignment = false;
                            if (i + 1 < lexemes.size() && lexemes[i + 1].type == typeAssign) {
                                isLeftSideOfAssignment = true;
                            }

                            if (!isLeftSideOfAssignment) {
                                CheckVariableInitialization(id, lex.line);
                            }
                        }
                    }
                    else {
                        // Переменная не найдена
                        if (!IsReservedWord(id) && !IsSystemFunction(id)) {
                            Tree* possibleFunc = FindParent(cur, id);
                            if (possibleFunc && possibleFunc->n && possibleFunc->n->isFunction) {
                                cerr << "Семантическая ошибка: функция '" << id
                                    << "' используется без вызова [строка " << lex.line << "]" << endl;
                            }
                            else {
                                cerr << "Семантическая ошибка: идентификатор '" << id
                                    << "' не объявлен в этой области видимости [строка " << lex.line << "]" << endl;
                            }
                        }
                    }
                }
            }

            // Присваивание
            if (lex.type == typeAssign && i > 0 && i + 1 < lexemes.size()) {

                int leftPos = i - 1;

                // Смотрим, что слева не переменная а функция
                if (leftPos >= 0 && lexemes[leftPos].type == typeRightBracket) {
                    int bracketDepth = 1;
                    int j = leftPos - 1;
                    while (j >= 0 && bracketDepth > 0) {
                        if (lexemes[j].type == typeRightBracket) bracketDepth++;
                        else if (lexemes[j].type == typeLeftBracket) bracketDepth--;
                        j--;
                    }
                    if (j >= 0 && (lexemes[j].type == typeIdentifier || lexemes[j].type == typeReservedMain)) {
                        // Это вызов функции слева от присваивания
                        cerr << "Семантическая ошибка: результату функции нельзя присвоить значение'"
                            << lexemes[j].value << "' [строка " << lex.line << "]" << endl;
                        continue;
                    }
                }

                
                if (lexemes[i - 1].type == typeIdentifier) {
                    string varName = lexemes[i - 1].value;

                    // Проверяем, не функция ли это
                    Tree* leftNode = FindParent(cur, varName);
                    if (leftNode && leftNode->n && leftNode->n->isFunction) {
                        cerr << "Семантическая ошибка: функции нельзя присваивать значение '"
                            << varName << "' [строка " << lex.line << "]" << endl;
                        continue;
                    }

                    // Это обычная переменная
                    MarkVariableInitialized(varName);

                    if (i + 1 < lexemes.size()) {
                        CheckExpressionForUninitialized(i + 1, lex.line);

                        // Проверка типа при присваивании
                        DATA_TYPE leftType = variableTypes[varName];
                        DATA_TYPE rightType = GetExpressionType(i + 1, lex.line);
                        CheckTypeCompatibility(leftType, rightType, "=", lex.line, lex.col, true);
                    }
                }
            }

            // Возврат из функции
            if (lex.type == typeReservedReturn) {
                if (currentFunction && currentFunction->n) {
                    string funcName = currentFunction->n->id;
                    functionReturnsValue[funcName] = true;

                    if (i + 1 < lexemes.size()) {
                        Lexeme& returnValue = lexemes[i + 1];

                        if (returnValue.type == typeIdentifier) {
                            string returnVar = returnValue.value;
                            MarkVariableUsed(returnVar, lex.line);
                            CheckVariableInitialization(returnVar, lex.line);

                            // Проверка соответствия типа возвращаемого значения
                            DATA_TYPE returnType = variableTypes[returnVar];
                            DATA_TYPE expectedType = functionReturnTypes[funcName];
                            CheckTypeCompatibility(expectedType, returnType, "return", lex.line, lex.col, true);
                        }
                    }
                }
            }

            // Арифметические операции и сравнения
            if ((lex.type == typePlus || lex.type == typeMinus || lex.type == typeMul ||
                lex.type == typeDiv || lex.type == typeMod ||
                lex.type == typeEq || lex.type == typeNeq || lex.type == typeLess ||
                lex.type == typeLessEq || lex.type == typeGreater || lex.type == typeGreaterEq) &&
                i > 0 && i + 1 < lexemes.size()) {

                DATA_TYPE leftType = TYPE_UNKNOWN;
                DATA_TYPE rightType = TYPE_UNKNOWN;

                if (lexemes[i - 1].type == typeIdentifier) {
                    string leftVar = lexemes[i - 1].value;
                    MarkVariableUsed(leftVar, lex.line);
                    CheckVariableInitialization(leftVar, lex.line);
                    leftType = variableTypes[leftVar];
                }

                if (lexemes[i + 1].type == typeIdentifier) {
                    string rightVar = lexemes[i + 1].value;
                    MarkVariableUsed(rightVar, lex.line);
                    CheckVariableInitialization(rightVar, lex.line);
                    rightType = variableTypes[rightVar];
                }

                // Определяем операцию для проверки типов
                string op;
                switch (lex.type) {
                case typePlus: op = "+"; break;
                case typeMinus: op = "-"; break;
                case typeMul: op = "*"; break;
                case typeDiv: op = "/"; break;
                case typeMod: op = "%"; break;
                case typeEq: op = "=="; break;
                case typeNeq: op = "!="; break;
                case typeLess: op = "<"; break;
                case typeLessEq: op = "<="; break;
                case typeGreater: op = ">"; break;
                case typeGreaterEq: op = ">="; break;
                default: op = "unknown";
                }

                if (leftType != TYPE_UNKNOWN && rightType != TYPE_UNKNOWN) {
                    CheckTypeCompatibility(leftType, rightType, op, lex.line, lex.col, true);
                }
            }
        }

        // Проверяем все вызовы функций после завершения анализа
        CheckFunctionCalls();

        CheckUnusedVariables();
        CheckFunctionReturns();
        PrintResults();
    }


    DATA_TYPE GetExpressionType(int startPos, int line) {
        if (startPos >= lexemes.size()) return TYPE_UNKNOWN;

        // Пропускаем унарные операторы
        int pos = startPos;
        while (pos < lexemes.size() &&
            (lexemes[pos].type == typePlus || lexemes[pos].type == typeMinus ||
                lexemes[pos].type == typeBitNot)) {
            pos++;
        }

        if (pos >= lexemes.size()) return TYPE_UNKNOWN;

        Lexeme& lex = lexemes[pos];

        // Проверяем константы
        if (lex.type == constDec || lex.type == constHex) {
            return TYPE_INT;
        }
        else if (lex.type == constChar) {
            return TYPE_CHAR;
        }
        else if (lex.type == typeTrue || lex.type == typeFalse) {
            return TYPE_BOOL;
        }
        else if (lex.type == typeIdentifier) {
            string id = lex.value;

            // Проверяем, является ли это вызовом функции
            if (pos + 1 < lexemes.size() && lexemes[pos + 1].type == typeLeftBracket) {
                Tree* funcNode = FindParent(globalRoot, id);
                if (funcNode && funcNode->n && funcNode->n->isFunction) {
                    return funcNode->n->ReturnType;
                }
                return TYPE_UNKNOWN;
            }

            // Иначе это переменная
            Tree* varNode = FindVariableInTree(id);
            if (varNode && varNode->n) {
                return varNode->n->DataType;
            }

            return TYPE_UNKNOWN;
        }
        else if (lex.type == typeLeftBracket) {
            // Выражение в скобках
            int bracketDepth = 1;
            int j = pos + 1;
            while (j < lexemes.size() && bracketDepth > 0) {
                if (lexemes[j].type == typeLeftBracket) bracketDepth++;
                else if (lexemes[j].type == typeRightBracket) bracketDepth--;
                j++;
            }
            if (j > pos + 1) {
                return GetExpressionType(pos + 1, line);
            }
        }
        else {
            // Если начинается с оператора или чего-то ещё, пробуем определить тип по первому операнду
            return TYPE_INT; // Для арифметических операций по умолчанию int
        }

        return TYPE_UNKNOWN;
    }

    void CheckExpressionForUninitialized(int startIndex, int line) {
        for (int j = startIndex; j < lexemes.size(); j++) {
            if (lexemes[j].type == typeSemicolon) {
                break;
            }

            if (lexemes[j].type == typeIdentifier) {
                string varName = lexemes[j].value;
                CheckVariableInitialization(varName, line);
            }
        }
    }

    void CheckFunctionReturns() {

        Tree* global = GetGlobalScope();
        Tree* current = global->left;

        while (current) {
            if (current->n && current->n->isFunction && current->n->ReturnType != TYPE_VOID) {
                string funcName = current->n->id;

                if (functionReturnsValue.find(funcName) == functionReturnsValue.end() ||
                    !functionReturnsValue[funcName]) {
                    cerr << "Семантическая ошибка: функция '" << funcName
                        << "' должна возвращать значение '"
                        << GetTypeString(current->n->ReturnType)
                        << "', только void не возвращает значение" << endl;
                }
            }
            current = current->right;
        }
    }

    bool IsSystemFunction(const string& name) {
        return name == "printf" || name == "scanf" || name == "cout" || name == "cin" ||
            name == "puts" || name == "gets" || name == "putchar" || name == "getchar";
    }

    void AddSystemFunction(const string& name) {
        DATA_TYPE retType = TYPE_INT;
        int paramCount = -1;

        if (name == "cout" || name == "cin") {
            retType = TYPE_UNKNOWN;
            paramCount = 0;
        }
        else if (name == "putchar" || name == "getchar") {
            retType = TYPE_INT;
            paramCount = 1;
        }

        AddReservedFunction(name, retType, paramCount);
    }

    bool IsTypeKeyword(LexemeType type) {
        return type == typeInt || type == typeShort || type == typeLong ||
            type == typeBool || type == typeChar || type == typeVoid;
    }

    bool IsReservedWord(const string& word) {
        return reservedWords.find(word) != reservedWords.end();
    }

    Tree* FindParent(Tree* From, string id) {

        Tree* current = From;

        while (current != nullptr) {
            Tree* child = current->left;
            while (child != nullptr) {
                if (child->n && child->n->id == id) {
                    return child;
                }
                child = child->right;
            }

            // Поднимаемся выше по дереву
            current = current->parent;
        }
        return nullptr;
    }

    /*Tree* FindInChildren(Tree* node, string id) {
        if (!node) return nullptr;

        Tree* child = node->left;
        while (child != nullptr) {
            if (child->n && child->n->id == id) {
                return child;
            }

            Tree* found = FindInChildren(child, id);
            if (found) return found;

            child = child->right;
        }

        return nullptr;
    }*/

    Tree* FindParentOneLevel(Tree* From, string id) {
        if (!From) return nullptr;

        Tree* current = From->left;
        while (current != nullptr) {
            if (current->n && current->n->id == id) {
                return current;
            }
            current = current->right;
        }
        return nullptr;
    }

    bool CheckReservedWordUsage(const string& id, int line, int col) {
        if (IsReservedWord(id) && id != "true" && id != "false" && id != "main") {
            cerr << "Семантическая ошибка: использование зарезервированного слова '"
                << id << "' в качестве идентификатора [строка " << line << "]" << endl;
            return true;
        }
        return false;
    }

    Tree* SemInclude(string a, DATA_TYPE t, int line, int col, bool isFunc = false,
        DATA_TYPE retType = TYPE_UNKNOWN, bool isParam = false) {
        if (CheckReservedWordUsage(a, line, col)) {
            return nullptr;
        }

        if (DupControl(cur, a)) {
            cerr << "Семантическая ошибка: повторное описание идентификатора '"
                << a << "' [строка " << line << "]" << endl;
            return nullptr;
        }

        bool initialized = isParam;
        Node* newNode = new Node(a, t, retType, line, col, isFunc, false, isParam, initialized, false);
        Tree* newTree = new Tree(nullptr, nullptr, cur, newNode);

        if (!cur->left) {
            cur->left = newTree;
        }
        else {
            Tree* temp = cur->left;
            while (temp->right) {
                temp = temp->right;
            }
            temp->right = newTree;
        }

        return newTree;
    }


    int DupControl(Tree* Addr, string a) {
        return FindParentOneLevel(Addr, a) != nullptr;
    }


    Tree* SemGetVar(string a, int line, int col) {
        if (a == "true" || a == "false") {
            return nullptr;
        }

        Tree* found = FindParent(cur, a);

        if (!found) {
            if (IsReservedWord(a) && a != "main") {
                return nullptr;
            }

            // Если не нашли - ошибка будет выведена в другом месте
            return nullptr;
        }

        if (found->n && found->n->isFunction) {
            return nullptr;
        }

        return found;
    }

    Tree* SemGetFunct(string a, int line, int col) {
        Tree* found = FindParent(cur, a);

        if (!found) {
            Tree* global = GetGlobalScope();
            found = FindParent(global, a);
        }

        if (!found) {
            if (IsSystemFunction(a)) {
                Node* sysNode = new Node(a, TYPE_FUNCTION, TYPE_INT, line, col, true, true);
                return new Tree(nullptr, nullptr, nullptr, sysNode);
            }

            // Не выводим ошибку здесь, так как будем проверять позже в CheckFunctionCalls
            return nullptr;
        }

        if (found->n && !found->n->isFunction) {
            cerr << "Семантическая ошибка: идентификатор '"
                << a << "' не является функцией [строка " << line << "]" << endl;
            return nullptr;
        }

        return found;
    }

    Tree* SemEnterBlock(int line, int col) {
        // cout << "Вход в блок на строке " << line << endl;
        Node* scopeNode = new Node("{}", TYPE_SCOPE, TYPE_UNKNOWN, line, col, false, false);
        Tree* newScope = new Tree(nullptr, nullptr, cur, scopeNode);

        if (!cur->left) {
            cur->left = newScope;
        }
        else {
            Tree* temp = cur->left;
            while (temp->right) {
                temp = temp->right;
            }
            temp->right = newScope;
        }

        Tree* oldCur = cur;
        cur = newScope;
        cur->parent = oldCur;

        return newScope;
    }

    void SemExitBlock() {
        // cout << "Выход из блока на строке " << (cur->n ? to_string(cur->n->line) : "unknown") << endl;
        if (cur && cur->parent) {
            cur = cur->parent;
        }
    }

    DATA_TYPE GetDataTypeFromLexeme(LexemeType lexType) {
        switch (lexType) {
        case typeInt: return TYPE_INT;
        case typeShort: return TYPE_SHORT;
        case typeLong: return TYPE_LONG;
        case typeBool: return TYPE_BOOL;
        case typeChar: return TYPE_CHAR;
        case typeVoid: return TYPE_VOID;
        default: return TYPE_UNKNOWN;
        }
    }

    string GetTypeString(DATA_TYPE type) {
        auto it = typeToString.find(type);
        if (it != typeToString.end()) {
            return it->second;
        }
        return "unknown";
    }

    bool IsIntegerType(DATA_TYPE type) {
        return type == TYPE_INT || type == TYPE_SHORT || type == TYPE_LONG;
    }

    bool CheckTypeCompatibility(DATA_TYPE type1, DATA_TYPE type2, string op, int line = 0, int col = 0, bool showError = true) {
        if (type1 == TYPE_UNKNOWN || type2 == TYPE_UNKNOWN) {
            if (showError && line > 0) {
                cerr << "Семантическая ошибка: неизвестный тип в операции '"
                    << op << "' [строка " << line << "]" << endl;
            }
            return false;
        }

        if (op == "=") {
            if (type1 == type2) return true;

            if (IsIntegerType(type1) && IsIntegerType(type2)) {
                if (showError && line > 0) {
                    cout << "Предупреждение: присваивание целочисленных типов разного размера [строка " << line << "]" << endl;
                }
                return true;
            }

            /*if (showError && line > 0) {
                cerr << "Семантическая ошибка: несоответствие типов при присваивании '"
                    << GetTypeString(type1) << "' = '" << GetTypeString(type2)
                    << "' [строка " << line << "]" << endl;
            }*/
            return false;
        }

        if (op == "parameter") {
            if (type1 == type2) return true;
            if (IsIntegerType(type1) && IsIntegerType(type2)) return true;
            return false;
        }

        if (op == "+" || op == "-" || op == "*" || op == "/" || op == "%") {
            if (type1 == TYPE_VOID || type2 == TYPE_VOID) {
                if (showError && line > 0) {
                    cerr << "Семантическая ошибка: тип void не может использоваться в операции '" << op
                        << "' [строка " << line << "]" << endl;
                }
                return false;
            }

            if ((IsIntegerType(type1) || type1 == TYPE_CHAR) &&
                (IsIntegerType(type2) || type2 == TYPE_CHAR)) {
                return true;
            }

            if (showError && line > 0) {
                cerr << "Семантическая ошибка: недопустимые типы для операции '" << op
                    << "': '" << GetTypeString(type1) << "' и '" << GetTypeString(type2)
                    << "' [строка " << line << "]" << endl;
            }
            return false;
        }

        if (op == "==" || op == "!=" || op == "<" || op == ">" || op == "<=" || op == ">=") {
            if (type1 == TYPE_VOID || type2 == TYPE_VOID) {
                if (showError && line > 0) {
                    cerr << "Семантическая ошибка: тип void не может использоваться в операции сравнения '" << op
                        << "' [строка " << line << "]" << endl;
                }
                return false;
            }

            if (type1 == type2) return true;
            if (IsIntegerType(type1) && IsIntegerType(type2)) return true;
            if (type1 == TYPE_CHAR && type2 == TYPE_CHAR) return true;
            if (type1 == TYPE_BOOL && type2 == TYPE_BOOL) return true;

            if (showError && line > 0) {
                cerr << "Семантическая ошибка: недопустимые типы для операции сравнения '" << op
                    << "': '" << GetTypeString(type1) << "' и '" << GetTypeString(type2)
                    << "' [строка " << line << "]" << endl;
            }
            return false;
        }

        if (op == "return") {
            if (type1 == type2) return true;
            if (IsIntegerType(type1) && IsIntegerType(type2)) {
                if (showError && line > 0) {
                    cout << "Предупреждение: возврат целочисленного значения с преобразованием типа [строка " << line << "]" << endl;
                }
                return true;
            }

            if (showError && line > 0) {
                cerr << "Семантическая ошибка: несоответствие типа возвращаемого значения (ожидался '"
                    << GetTypeString(type1) << "', получен '" << GetTypeString(type2)
                    << "') [строка " << line << "]" << endl;
            }
            return false;
        }

        return true;
    }

    void SetCur(Tree* a) {
        cur = a;
    }

    void InitializePredefinedFunctions() {
        // Убрали предопределенную функцию main
    }

    Tree* AddReservedFunction(string name, DATA_TYPE retType, int paramCount) {
        Node* funcNode = new Node(name, TYPE_FUNCTION, retType, 0, 0, true, true);
        funcNode->paramCount = paramCount;

        Tree* global = GetGlobalScope();
        Tree* funcTree = new Tree(nullptr, nullptr, global, funcNode);

        if (!global->left) {
            global->left = funcTree;
        }
        else {
            Tree* temp = global->left;
            while (temp->right) {
                temp = temp->right;
            }
            temp->right = funcTree;
        }

        return funcTree;
    }

    Tree* GetGlobalScope() {
        Tree* current = cur;
        while (current && current->parent) {
            current = current->parent;
        }
        return current ? current : globalRoot;
    }

    void PrintResults() {
        cout << "\nРезультаты анализа" << endl;
        cout << "Строк в файле: " << lines.size() << endl;
        cout << "Лексем найдено: " << lexemes.size() << endl;

        Tree* mainFunc = FindParent(globalRoot, "main");
        if (mainFunc && mainFunc->n && mainFunc->n->isFunction) {
            cout << "Функция main найдена" << endl;
        }
        else {
            cout << "Функция main не найдена" << endl;
        }

        if (!usedSystemFunctions.empty()) {
            cout << "\nИспользованные системные функции: ";
            for (const auto& func : usedSystemFunctions) {
                cout << func << " ";
            }
            cout << endl;
        }

        cout << "\nСемантическое дерево" << endl;
        PrintTree(globalRoot, 0, true);
    }

    bool HasMainFunction() {
        Tree* mainFunc = FindParent(globalRoot, "main");
        return mainFunc && mainFunc->n && mainFunc->n->isFunction;
    }

    void PrintTree(Tree* node, int depth, bool isLast = false) {
        if (!node || !node->n) return;

        for (int i = 0; i < depth; i++) {
            cout << "    ";
        }

        if (depth > 0) {
            if (isLast) {
                cout << "--- ";
            }
            else {
                cout << "|--- ";
            }
        }

        if (node->n->id == "global") {
            cout << "глобальная область" << endl;
        }
        else if (node->n->DataType == TYPE_SCOPE) {
            cout << "{ область видимости }";
            if (node->n->line > 0) {
                cout << " [строка " << node->n->line << "]";
            }
            cout << endl;
        }
        else if (node->n->isFunction) {
            cout << node->n->id << " (функция) -> " << GetTypeString(node->n->ReturnType);
            if (node->n->paramCount > 0) {
                cout << ", параметров: " << node->n->paramCount;
                cout << " (";
                for (size_t i = 0; i < node->n->paramTypes.size(); i++) {
                    if (i > 0) cout << ", ";
                    cout << GetTypeString(node->n->paramTypes[i]);
                }
                cout << ")";
            }
            if (node->n->line > 0) {
                cout << " [строка " << node->n->line << "]";
            }
            cout << endl;
        }
        else if (node->n->isParameter) {
            cout << node->n->id << " (параметр) -> " << GetTypeString(node->n->DataType);
            if (node->n->line > 0) {
                cout << " [строка " << node->n->line << "]";
            }
            cout << endl;
        }
        else {
            cout << node->n->id << " (переменная) -> " << GetTypeString(node->n->DataType);
            if (node->n->line > 0) {
                cout << " [строка " << node->n->line << "]";
            }
            cout << " [иниц: " << (node->n->isInitialized ? "да" : "нет")
                << ", исп: " << (node->n->isUsed ? "да" : "нет") << "]" << endl;
        }

        Tree* child = node->left;
        int childCount = 0;
        while (child) {
            childCount++;
            child = child->right;
        }

        child = node->left;
        int currentChild = 0;
        while (child) {
            bool lastChild = (currentChild == childCount - 1);
            PrintTree(child, depth + 1, lastChild);
            child = child->right;
            currentChild++;
        }
    }

    void AnalyzeFile(const string& filename) {
        if (!LoadFile(filename)) {
            return;
        }

        LexicalAnalysis();
        SyntaxAnalysis();
        SemanticAnalysis();
    }
};

Tree* Tree::cur = nullptr;

int main() {
    SetConsoleCP(1251);
    SetConsoleOutputCP(1251);

    string filename = "test1.cpp";

    if (filename.empty()) {
        cout << "Вы не ввели имя файла" << endl;
        return 1;
    }

    Tree* analyzer = new Tree();
    analyzer->AnalyzeFile(filename);

    delete analyzer;
    return 0;
}