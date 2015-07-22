# Balt-System NC interpreter
function ocode = BSinterpreter(icode, iterLimit=1000)
    global iCode;
    iCode = parseInputCode(icode);
    initGlobals();
    blockNum = [1, 1];
    for i = 1 : iterLimit
        [block, blockNum] = processBlock(blockNum);
        ocode{i} = block2str(block);
        if (blockNum(1) == -1 || blockNum(1) > length(iCode))
            break;
        endif
    endfor
endfunction


global iCode;
global labels vars cycles;
global axMirrors axRot;
global axes;


# Parse all the blocks in input code
function ocode = parseInputCode(icode)
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
        elseif (isalpha(sym))
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


# Initilize global vars
function initGlobals()
    global iCode axMirrors vars axRot axes;
    # Axes mirrors
    axMirrors.X = 1;
    axMirrors.Y = 1;
    axMirrors.Z = 1;
    # E-params
    for i = 1 : 100
        vars.(["E", num2str(i)]) = 0;
    endfor
    # Axes rotation
    axRot = 0;
    # Axes position
    axes.X = 0;
    axes.Y = 0;
    axes.Z = 0;
endfunction


# Process the block
function [block blockToGo] = processBlock(blockNum)
    global iCode axMirrors axRot cycles labels axes;
    b = iCode{blockNum(1)};
    i = blockNum(2) - 1;
    if (i >= length(b))  block = [];  endif    # supress warnings
    while (i < length(b))
        i++;
        t = b(i);
        c = t.content;
        switch (t.tokType)
            case "TSC"
                switch (c{1})
                    case "BNC"
                        [blockToGo(1), blockToGo(2)] = searchLabel(c{2});
                        return;
                    case {"BGT", "BGE", "BLT", "BLE", "BEQ", "BNE"}
                        switch (c{1})
                            case "BGT"
                                cmp = @(v1,v2) v1>v2;
                            case "BGE"
                                cmp = @(v1,v2) v1>=v2;
                            case "BLT"
                                cmp = @(v1,v2) v1<v2;
                            case "BLE"
                                cmp = @(v1,v2) v1<=v2;
                            case "BEQ"
                                cmp = @(v1,v2) v1==v2;
                            case "BNE"
                                cmp = @(v1,v2) v1~=v2;
                        endswitch
                        var1 = readValue(c{2});
                        var2 = readValue(c{3});
                        if (cmp(var1,var2))
                            [blockToGo(1), blockToGo(2)] = searchLabel(c{4});
                            return;
                        endif
                        continue;
                    case "RPT"
                        cycle.remain = readValue(c{2});
                        cycle.beginBlock = [blockNum(1), i+1];
                        cycles{end+1} = cycle;
                        continue;
                    case "ERP"
                        if (--cycles{end}.remain > 0)
                            blockToGo = cycles{end}.beginBlock;
                            return;
                        endif
                        cycles(end) = [];
                        continue;
                    case "EPP"
                        startBlock = searchLabel(c{2});
                        endBlock   = searchLabel(c{3});
                        insert = iCode(startBlock : endBlock);
                        iCode = [iCode(1:blockNum(1)-1), insert, iCode(blockNum(1)+1:end)];
                        blockToGo = blockNum;
                        return;
                    case "MIR"
                        cancelMirrors();
                        for a = 2 : length(c)
                            axMirrors.(c{a}) = -1;
                            if (c{a} == "X")
                                axMirrors.I = -1;
                            elseif (c{a} == "Y")
                                axMirrors.J = -1;
                            endif
                        endfor
                        continue;
                    case "DIS"
                        for d = 2 : length(c)
                            isItVar = varInBegining(c{d});
                            if (isItVar)
                                t.content{d} = num2str(readValue(c{d}));
                            endif
                        endfor
                    case {"UAO", "DLY"}
                        t.content{2} = num2str(readValue(c{2}));
                    case "UCG"
                        # Do nothing
                    case "URT"
                        axRot = readValue(c{2});
                        t.content{2} = num2str(axRot);
                    case "SGI"
                        warning("SGI code is not supported");
                    case "SPE"
                        warning("SPE code is not supported");
                endswitch
            case {"indent", "comment"}
                # Do nothing
            case "label"
                if (~isLabelPresent(c))
                    labels.(c) = [blockNum(1), i+1];
                endif
                continue;
            case "expression"
                processArithmetic(c);
                continue;
            case "X"
                t.content = axMirrors.X * readValue(c);
            case "Y"
                t.content = axMirrors.Y * readValue(c);
            case "Z"
                t.content = axMirrors.Z * readValue(c);
            case "I"
                c = readValue(c) * axMirrors.X;
                t.content = c;
            case "J"
                c = readValue(c) * axMirrors.Y;
                t.content = c;
            case {"F", "K", "R", "b", "r"}
                t.content = readValue(c);
            case {"G"}
                t.content = mirrorG(readValue(c));
            case {"M", "N", "S", "T"}
                t.content = readValue(c);
            otherwise
                warning(["unknown token: \"", t.tokType, "\""]);
        endswitch
        block(end+1) = t;
    endwhile
    blockToGo = [blockNum(1)+1, 1];
endfunction


# Get the block number which contains the label,
#       and token number after the label
function [blockNum tokenNum] = searchLabel(label)
    global labels iCode;
    if (isLabelPresent(label))
        blockNum = labels.(label)(1);
        tokenNum = labels.(label)(2);
        return;
    endif
    blockNum = tokenNum = -1;      # Label not found
    for i = 1 : length(iCode)
        b = iCode{i};
        for j = 1 : length(b)
            if (strcmp(b(j).tokType, "label") && strcmp(b(j).content, label))
                blockNum = i;
                tokenNum = j+1;
                labels.(label) = [blockNum, tokenNum];
                return;
            endif
        endfor
    endfor
endfunction


# Is label present in labels list
function flag = isLabelPresent(label)
    global labels;
    flag = exist("labels", "var") && isfield(labels, label);
endfunction


# Read the number or var from string begining
function [val si ei] = readValue(str)
    global vars;
    if (isnumeric(str))
        val = str;
        si = ei = 0;
        return;
    endif
    si = 1;
    [isItVar varSI varEI varName] = varInBegining(str);
    if (isItVar)
        ei = varEI;
        if (isVarPresent(varName))
            val = vars.(varName);
        else
            error(["undefined var ", varName]);
        endif
    else
        [si ei te numStr] = regexp(str, "^( *[+-]?[0-9.]*)", "once");
        val = strread(numStr, "%f");
        if (isempty(val))
            val = 0;
            ei = si = 0;
        endif
    endif
endfunction


# Is variable occurs in expression begining
function [isItVar varSI varEI varName] = varInBegining(expr)
    [varSI varEI] = regexp(expr, "^( *E *)", "once");
    if (isempty(varSI))
        isItVar = false;
        varSI = varEI = 0;
        varName = "";
    else
        isItVar = true;
        [varNumSI varNumEI te varNumStr] = ...
                regexp(expr(varEI+1:end), "^( *[0-9]+)", "once");
        varEI += varNumEI;
        varName = ["E", varNumStr];
    endif
endfunction


# Cancel the (MIR,...) code
function cancelMirrors()
    global axMirrors;
    axes = fieldnames(axMirrors);
    for a = 1 : length(axes)
        axMirrors.(axes{a}) = 1;
    endfor
endfunction


# G-code in mirror processing (G2->G3, G3->G2, G42->G41, G41->G42)
function G = mirrorG(G)
    global axMirrors;
    if (axMirrors.X * axMirrors.Y == -1)
        switch (G)
            case 2
                G = 3;
            case 3
                G = 2;
            case 41
                G = 42;
            case 42
                G = 41;
            otherwise
                return;
        endswitch
    endif
endfunction


# Is var present in var list
function flag = isVarPresent(varStr)
    global vars;
    flag = exist("vars", "var") && isfield(vars, varStr);
endfunction


# Evaluate expression
function res = processArithmetic(expr)
    tree = arithmeticParser(expr);
    res = calculateExpression(tree);
endfunction


# Find value of the address (UNUSED)
function value = getValueAtAddr(token, addr)
    value = NaN;
    for i = 1 : length(token)
        if (token.tokType == addr)
            value = token.content;
            break;
        endif
    endfor
endfunction


# Convert parsed block to string
function str = block2str(block)
    # TO-DO: check token precedence in block and sort
    str = "";
    for i = 1 : length(block)
        c = block(i).content;
        switch (block(i).tokType)
            case "indent"
                s = "";
                for j = 1 : c
                    s = [" ", s];
                endfor
            case "expression"
                error("expression found while converting block to string");
            case "TSC"
                s = TSC2string(c);
            case "label"
                s = ["\"", c, "\""];
                #error("label found while converting block to string");
            case "comment"
                s = [";", c];
            otherwise
                s = [block(i).tokType, num2str(c), " "];
        endswitch
        str = [str, s];
    endfor
    str = deblank(str);
endfunction


# Convert three-symbol code to string
function str = TSC2string(tsc)
    str = ["(", strjoin(tsc, ", "), ")"];
endfunction
