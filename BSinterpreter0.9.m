# Balt-System NC interpreter
function ocode = BSinterpreter(icode, iterLimit=1000)
    global iCode;
    iCode = icode;
    initGlobals();
    currentBlock = [1 1];
    for i = 1 : iterLimit
        [ocode{i} currentBlock] = processBlock(currentBlock);
        if (currentBlock(1) == -1 || currentBlock(1) > length(iCode))
            break;
        endif
    endfor
endfunction


global iCode;
global labels vars cycles axMirrors axRot;


# Initilize global vars
function initGlobals()
    global iCode axMirrors vars axRot;
    # Axes mirrors
    axMirrors.X = 1;
    axMirrors.Y = 1;
    axMirrors.Z = 1;
    axMirrors.I = 1;
    axMirrors.J = 1;
    # E-params
    for i = 1 : 100
        vars.(["E", num2str(i)]) = 0;
    endfor
    # Axes rotation
    axRot = 0;
endfunction


# Process the block
function [ocode blockToGo] = processBlock(blockNum)
    global iCode labels cycles axMirrors axRot;
    expr = iCode{blockNum(1)}(blockNum(2):end);
    if (length(strtrim(expr)) == 0)
        ocode = "";
        blockToGo = [blockNum(1)+1, 1];
        return;
    endif
    state = "";
    i = 1;
    while (i <= length(expr) && i <= 500)
        if (isspace(expr(i)))
            i++;
            continue;
        endif
        if (strcmp(expr(i), ";"))
            break;
        endif
        state = [state, expr(i)];
        switch (state)
            case "("        # Three-symbol code starts
                [tsc si ei] = decompositeTSC(expr(i:end));
                si = i + si - 1;
                ei = i + ei - 1;
                i = ei + 1;
                state = "";
                switch (tsc{1})
                    case "BNC"
                        ocode = expr = expr(1:si-1);
                        label = ["\"" tsc{2} "\""];
                        [blockToGo(1) blockToGo(2)] = searchLabel(label);
                        return;
                    case {"BGT", "BGE", "BLT", "BLE", "BEQ", "BNE"}
                        switch (tsc{1})
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
                        var1 = readValue(tsc{2});
                        var2 = readValue(tsc{3});
                        if (cmp(var1,var2))
                            ocode = expr = expr(1:si-1);
                            label = ["\"" tsc{4} "\""];
                            [blockToGo(1) blockToGo(2)] = searchLabel(label);
                            return;
                        endif
                        expr = removeSubstring(expr, si, ei);
                        i = si;
                        continue;
                    case "RPT"
                        expr = removeSubstring(expr, si, ei);
                        i = si;
                        cycle.remain = readValue(tsc{2});
                        cycle.beginBlock = [blockNum(1), ei+1];
                        cycles{end+1} = cycle;
                        continue;
                    case "ERP"
                        if (--cycles{end}.remain > 0)
                            ocode = expr(1:si-1);
                            blockToGo = cycles{end}.beginBlock;
                            return;
                        endif
                        expr = removeSubstring(expr, si, ei);
                        i = si;
                        cycles(end) = [];
                        continue;
                    case "EPP"
                        expr = removeSubstring(expr, si, ei);
                        i = si;
                        startBlock = searchLabel(["\"" tsc{2} "\""]);
                        endBlock   = searchLabel(["\"" tsc{3} "\""]);
                        insert = iCode(startBlock : endBlock);
                        iCode = [iCode(1:blockNum(1)-1), insert, iCode(blockNum(1)+1:end)];
                        continue;
                    case "MIR"
                        expr = removeSubstring(expr, si, ei);
                        i = si;
                        cancelMirrors();
                        for a = 2 : length(tsc)
                            axMirrors.(tsc{a}) = -1;
                            if (tsc{a} == "X")
                                axMirrors.I = -1;
                            elseif (tsc{a} == "Y")
                                axMirrors.J = -1;
                            endif
                        endfor
                        continue;
                    case {"DIS"}
                        isItVar = varInBegining(tsc{2});
                        if (isItVar)
                            val = readValue(tsc{2});
                            tsc{2} = num2str(val);
                            [expr i] = substrExchange(expr, si, ei, TSC2string(tsc));
                        endif
                        continue;
                    case {"UAO", "DLY"}
                        val = readValue(tsc{2});
                        tsc{2} = num2str(val);
                        [expr i] = substrExchange(expr, si, ei, TSC2string(tsc));
                        continue;
                    case "UCG"
                        continue;
                    case "URT"
                        axRot = readValue(tsc{2});
                        tsc{2} = num2str(axRot);
                        [expr i] = substrExchange(expr, si, ei, TSC2string(tsc));
                        continue;
                    case {"SGI", "SPE"}
                        warning("SGI, SPE codes is not supported");
                endswitch
            case "\""       # Label starts
                [si ei te label] = regexp(expr, "\".*\"", "once");
                expr = removeSubstring(expr, si, ei);
                i = si;
                state = "";
                if (~isLabelPresent(label))
                    gotoPosition = [blockNum(1) ei+1];
                    labels.(label) = gotoPosition;
                endif
                continue;
            case "E"
                [arithmExpr si ei] = extractArithmetic(expr(i:end));
                processArithmetic(arithmExpr);
                expr = removeSubstring(expr, i+si-1, i+ei-1);
                i += si - 1;
                state = "";
                continue;
            case {"X", "Y", "Z", "I", "J"}
                [coord si ei] = readValue(expr(++i:end));
                coord *= axMirrors.(state);
                #TO-DO: URT
                coordStr = num2str(coord);
                [expr i] = substrExchange(expr, i, i+ei-1, coordStr);
                state = "";
                continue;
            case {"F", "K", "R", "b", "r"}
                [val si ei] = readValue(expr(++i:end));
                i += ei;
                state = "";
                continue;
            case {"G", "M", "N", "S"}
                [val si ei] = readValue(expr(++i:end));
                val = mirrorG(val);
                valStr = num2str(val);
                [expr i] = substrExchange(expr, i, i+ei-1, valStr);
                state = "";
                continue;
            case {"T"}
                [T si ei] = readValue(expr(++i:end));
                i += ei;
                state = "";
                continue;
            otherwise
                warning(["unknown token: \"", state, "\" in expression \"", expr, "\""]);
                break;
        endswitch
        i++;
    endwhile
    ocode = expr;
    blockToGo = [blockNum(1)+1, 1];
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
    switch (G)
        case 2
            inversedG = 3;
        case 3
            inversedG = 2;
        case 41
            inversedG = 42;
        case 42
            inversedG = 41;
        otherwise
            return;
    endswitch
    if (axMirrors.X * axMirrors.Y == -1)
        G = inversedG;
    endif
endfunction


# Is label present in labels list
function flag = isLabelPresent(label)
    global labels;
    flag = exist("labels", "var") && isfield(labels, label);
endfunction


# Is var present in var list
function flag = isVarPresent(varStr)
    global vars;
    flag = exist("vars", "var") && isfield(vars, varStr);
endfunction


# Get the block number which contains the label
function [blockNum colNum] = searchLabel(label)
    global labels iCode;
    if (isLabelPresent(label))
        blockNum = labels.(label)(1);
        colNum = labels.(label)(2);
    else
        blockNum = -1;      # Label not found
        for i = 1 : length(iCode)
            pos = strfind(iCode{i}, label, "overlaps", false);
            if (pos)
                blockNum = i;
                colNum = pos + length(label);
                labels.(label) = [blockNum colNum];
                break;
            endif
        endfor
    endif
endfunction


# Extract three-symbol code like (OPERATION,ARG1,ARG2,...)
# Return cell array with operation and args,
#   indices of start and end TSC in expr-string
function [tsc si ei] = decompositeTSC(expr)
    si = index(expr,"(");       # first symbol ( index of tree-symbol code
    ei = rindex(expr,")");      # last symbol ) index of tree-symbol code
    if (si == 0 || ei == 0 || ei < si)
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
        tsc{paramsCount} = [tsc{paramsCount} expr(i)];
    endfor
endfunction


# Convert three-symbol code to string
function str = TSC2string(tsc)
    str = ["(", strjoin(tsc, ", "), ")"];
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


# Read the number or var from string begining
function [val si ei] = readValue(str)
    global vars;
    si = 1;
    [isItVar varSI varEI varName] = varInBegining(str);
    if (isItVar == 1)
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


# Remove substring from position si to ei of a s
function resultString = removeSubstring(s, si, ei)
    resultString = [s(1:si-1), s(ei+1:end)];
endfunction


# Replace substring within si and ei (including) indices by sub
# Return result string, and position after the end of sub in result string
function [resultString subEndPos] = substrExchange(s, si, ei, sub)
    resultString = [s(1:si-1), sub, s(ei+1:end)];
    subEndPos = si + length(sub);
endfunction


# Extract arithmetic expression
function [aexpr si ei] = extractArithmetic(expr)
    aexpr = strtrim(expr);
    si = 1;
    ei = length(expr);
endfunction


# Evaluate expression
function res = processArithmetic(expr)
    global vars;
    tree = arithmeticParser(expr);
    res = calculateExpression(tree);
endfunction
