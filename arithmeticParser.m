# Parse arithmetic expression
function tree = arithmeticParser(expr)
    parsedExpr = parseExpression(expr);    
    [var parsedExpr] = prepareAssignment(parsedExpr);
    parsedExpr = prepareFirstSign(parsedExpr);
    parsedExpr = prepareFunctions(parsedExpr);
    parsedExpr = prepareMultipliers(parsedExpr);
    parsedExpr = prepareItems(parsedExpr);
    if (~isempty(var))
        tree.type = 4;
        tree.value = "=";
        tree.arg1 = var;
        tree.arg2 = optimizeTree(parsedExpr);
        #dispParsedExpr(tree);
    else
        tree = parsedExpr;
    endif
endfunction


# Parse expression to sequence of tokens
function tokenSequence = parseExpression(expr)
    i = 1;
    j = 0;      # Tokens counter
    while (i <= length(expr))
        [token.type str si ei] = getToken(expr(i:end));
        si = i + si - 1;
        ei = i + ei - 1;
        i = ei + 1;
        switch (token.type)
            case 0
                break;
            case 1
                token.value = strread(str, "%f");
            case {2, 3, 4}
                token.value = str;
            case 5
                token.value = arithmeticParser(expr(si:ei));
                i++;
        endswitch
        tokenSequence(++j) = token;
    endwhile
endfunction


# Get next token
# tokenType:
#   0 - no token
#   1 - constant
#   2 - var
#   3 - func
#   4 - operator
#   5 - other expression
# str - string representation of token
# si, ei - start and end indecies of token in expr
function [tokenType str si ei] = getToken(expr)
    tokenType = 0;
    str = "";
    si = ei = 0;
    openBracks = 0;     # Opening brackets counter
    for i = 1 : length(expr)
        c = expr(i);
        if (isspace(c))
            continue;
        endif
        if (tokenType == 0)  # What kind of token?
            si = ei = i;
            if (isNumericSym(c))
                tokenType = 1;
            elseif (isalpha(c))
                tokenType = 2;  # It's may be function too
            elseif (sum("=+-*/" == c))
                tokenType = 4;
                str = c;
                return;
            elseif (strcmp(c, "("))
                tokenType = 5;
                si = i + 1;
                openBracks++;
                continue;
            else
                return;
            endif
        else  # Where is the end of token?
            if (tokenType == 1 && ~isNumericSym(c))
                ei = i - 1;
                return;
            elseif (tokenType == 2 && strcmp(c, "("))
                tokenType = 3;
                ei = i - 1;
                return;
            elseif (tokenType == 2 && ~isalnum(c))
                ei = i - 1;
                return;
            elseif (tokenType == 5 && strcmp(c, "("))
                openBracks++;
            elseif (tokenType == 5 && strcmp(c, ")") && --openBracks == 0)
                ei = i - 1;
                return;
            endif
        endif
        str = [str, c];
    endfor
    ei = i;
endfunction


# Is it a numeric symbol (digit or ".")
function flag = isNumericSym(c)
    flag = isdigit(c) || strcmp(c, ".");
endfunction


# Identify assignment
function [var expr] = prepareAssignment(tokenSequence)
    if (length(tokenSequence) >= 3 &&...
            tokenSequence(2).type == 4 &&...
            tokenSequence(2).value == "=")
        var = tokenSequence(1);
        expr = tokenSequence(3:end);
    else
        var = [];
        expr = tokenSequence;
    endif
endfunction


# Remove "+" or "-" in token sequence
function tokenSequence = prepareFirstSign(tokenSequence)
    if (tokenSequence(1).type == 4)     # type: operator
        if (tokenSequence(1).value == "+")
            tokenSequence(1) = [];
        elseif (tokenSequence(1).value == "-")
            tokenSequence(1).value = "*";
            minusOne.type = 1;
            minusOne.value = -1;
            tokenSequence = [minusOne, tokenSequence];
        endif
    endif
endfunction


# Compose function token
function tokenSequence = prepareFunctions(tokenSequence);
    i = 1;
    while (i < length(tokenSequence))
        if (tokenSequence(i).type == 3)
            tokenSequence(i).arg1 = tokenSequence(i+1);
            tokenSequence(i+1) = [];
        endif
        i++;
    endwhile
endfunction


# Compose multipliers
function tokenSequence = prepareMultipliers(tokenSequence);
    i = 2;
    while (i < length(tokenSequence))
        t = tokenSequence(i);
        if (t.type == 4 && (t.value == "*" || t.value == "/"))
            leftArg = tokenSequence(i-1);
            righArg = tokenSequence(i+1);
            tokenSequence(i-1) = t;
            tokenSequence(i-1).arg1 = leftArg;
            tokenSequence(i-1).arg2 = righArg;
            tokenSequence(i+1) = [];
            tokenSequence(i) = [];
            continue;
        endif
        i++;
    endwhile
endfunction


# Compose items (of sums and difference)
function tokenSequence = prepareItems(tokenSequence);
    i = 2;
    while (i < length(tokenSequence))
        t = tokenSequence(i);
        if (t.type == 4 && (t.value == "+" || t.value == "-"))
            leftArg = tokenSequence(i-1);
            righArg = tokenSequence(i+1);
            tokenSequence(i-1) = t;
            tokenSequence(i-1).arg1 = leftArg;
            tokenSequence(i-1).arg2 = righArg;
            tokenSequence(i+1) = [];
            tokenSequence(i) = [];
            continue;
        endif
        i++;
    endwhile
endfunction


# Remove unnecessary expressions
function expr = optimizeTree(expr);
    #TO-DO: group same operators
    n = numfields(expr) - 2;
    for i = 1 : n
        a = ["arg", num2str(i)];
        if (isfield(expr, a) && ~isempty(expr.(a)))
            switch (expr.(a).type)
                case {1, 2}
                    continue;
                case 3
                    expr.(a) = optimizeTree(expr.(a));
                case 4
                    expr.(a) = optimizeTree(expr.(a));
                case 5
                    expr.(a) = optimizeTree(expr.(a).value);
            endswitch
        endif
    endfor
endfunction
