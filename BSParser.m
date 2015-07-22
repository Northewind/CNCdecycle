# Parse all the blocks in input code
function ocode = BSParser(icode)
    for i = 1 : length(icode)
        ocode{i} = parseBlock(icode{i});
    endfor
endfunction


# Parse the block to sequence of tokens
function parsedBlock = parseBlock(block)
    block = deblank(block);
    parsedBlock(1).tokType = "indent";
    parsedBlock(1).content = 0;
    isItIndent = true;
    i = 0;
    while (i < length(block))
        i++;
        sym = block(i);
        if (isspace(sym))
            if (isItIndent)
                parsedBlock(1).content++;
            endif
            continue;
        endif
        isItIndent = false;
        if (sym == "E")
            token.tokType = "expression";
            [token.content si ei] = extractArithmetic(block(i:end));
            i += ei - 1;
        elseif (isalpha(sym))  #G, M, X, Y, Z, S, F, ...
            token.tokType = sym;
            [token.content si ei] = readNumOrVar(block(++i:end));
            i += ei - 1;
        elseif (sym == "(")
            token.tokType = "TSC";
            [token.content si ei] = decompositeTSC(block(i:end));
            i += ei - 1;
        elseif (sym == "\"")
            token.tokType = "label";
            [si ei te token.content] = regexp(block(i:end), "(?<=\")[^\"]*(?=\")", "once");
            i += ei;
        elseif (sym == ";")
            token.tokType = "comment";
            token.content = strtrim(block(i+1:end));
            i += length(block);     # Go out
        else
            error(["unknown symbol: \"", sym, "\" in block \"", block, ...
                    "\" (position: ", num2str(i), ")"]);
        endif
        parsedBlock(end+1) = token;
    endwhile
endfunction


# Extract arithmetic expression
function [aexpr si ei] = extractArithmetic(expr)
    commentStart = index(expr, ";");
    if (commentStart == 0)
        commentStart = length(expr) + 1;
    endif
    aexpr = deblank(expr(1:commentStart-1));
    si = 1;
    ei = length(aexpr);
endfunction


# Read addressed content (number or var) in string begining
function [value si ei] = readNumOrVar(str)
    [si ei te strCont] = regexp(str, "^ *(E[0-9]+|[+-]?[0-9.]+) *", "once");
    strCont = strtrim(strCont);
    if (isempty(strCont))
        value = 0;
        si = ei = 0;
    elseif (strCont(1) == "E")
        value = strCont;
    else
        value = strread(strCont, "%f");
    endif
endfunction


# Extract three-symbol code like (OPERATION,ARG1,ARG2,...)
# Return cell array with operation and args,
#   indices of start and end TSC in expr-string
function [tsc si ei] = decompositeTSC(expr)
    si = index(expr,"(");                    # first symbol ( index of TSC
    ei = si + rindex(expr(si:end),")") - 1;  # last symbol ) index of TSC
    if (si == 0 || ei == si-1)
        error("Invalid usage of parentheses");
    endif
    firstOpenBracketOccured = false;
    openBracketsCount = 1;
    paramsCount = 1;
    tsc{paramsCount} = "";
    for i = si + 1 : ei
        if (expr(i) == ",")
            tsc{paramsCount} = strtrim(tsc{paramsCount});
            paramsCount++;
            tsc{paramsCount} = "";
            continue;
        elseif (expr(i) == "(")
            openBracketsCount++;
        elseif (expr(i) == ")")
            openBracketsCount--;
            if (openBracketsCount == 0)
                tsc{paramsCount} = strtrim(tsc{paramsCount});
                ei = i;
                return;
            endif
        endif
        tsc{paramsCount} = [tsc{paramsCount}, expr(i)];
    endfor
endfunction
