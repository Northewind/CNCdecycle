# Balt-System NC interpreter
function ocode = BSinterpreter(icode, iterLimit=1000)
    global iCode;
    iCode = BSlexer(icode);
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
global axes modalG;


# Initilize global vars
function initGlobals()
    global iCode axMirrors vars axRot axes modalG;
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
    # G codes
    modalG = switchModalG([0, 29, 40, 90]);
endfunction


# Process the block
function [block blockToGo] = processBlock(blockNum)
    global iCode axMirrors axRot cycles labels axes modalG;
    b = iCode{blockNum(1)};
    i = blockNum(2) - 1;
    ro = alp = NaN;     # Polar radius and angle for axes rotation
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
                        #continue;
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
			continue;
                    otherwise
                        warning([c{1}, " code is not supported"]);
                endswitch
            case {"indent", "comment"}
                # Do nothing
            case "label"
                if (~isLabelPresent(c))
                    labels.(c) = [blockNum(1), i+1];
                endif
                continue;
            case "expression"
                calculateExpression(arithmeticParser(c));
                continue;
                
            case "X"
                x = axMirrors.X * readValue(c);
                isG90 = isGCodeActual(b, 90);
                if (isG90)
                    axes.X = x;
                else
                    axes.X += x;
                endif
                if (isnan(ro))
                    y = valueAtAddr(b, "Y");
                    if (isnan(y))
                        if (isG90)
                            y = axes.Y;
                        else
                            y = 0;
                        endif
                    endif
                    if (x == 0)
                        alp = 90;
                    else
                        alp = atand(y/x);
                    endif
                    ro = norm([x y]);
                endif
                t.content = ro * cosd(alp + axRot);
            case "Y"
                y = axMirrors.Y * readValue(c);
                isG90 = isGCodeActual(b, 90);
                if (isG90)
                    axes.Y = y;
                else
                    axes.Y += y;
                endif
                if (isnan(ro))
                    x = valueAtAddr(b, "X");
                    if (isnan(x))
                        if (isG90)
                            x = axes.X;
                        else
                            x = 0;
                        endif
                    endif
                    if (x == 0)
                        alp = 90;
                    else
                        alp = atand(y/x);
                    endif
                    ro = norm([x y]);
                endif
                t.content = ro * sind(alp + axRot);
                
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
                modalG = switchModalG(t.content);
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
function isPresent = isLabelPresent(label)
    global labels;
    isPresent = exist("labels", "var") && isfield(labels, label);
endfunction


# Is var present in var list
function flag = isVarPresent(varStr)
    global vars;
    flag = exist("vars", "var") && isfield(vars, varStr);
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


# Read the number or var from begining of the string
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


# Is G codes actual
function actual = isGCodeActual(block, G)
    global modalG;
    GInBlock = [];
    for i = 1 : length(block)
        t = block(i);
        if (strcmp(t.tokType, "G"))
            GInBlock(end+1) = t.content;
        endif
    endfor
    mG = switchModalG(GInBlock);
    fnames = fieldnames(mG);
    actual = zeros(1, length(G));
    for i = 1 : length(fnames)
        fn = fnames{i};
        actual |= G == mG.(fn);
    endfor
endfunction


# Switch G codes
function mG = switchModalG(G)
    global modalG;
    mG = modalG;
    for i = 1 : length(G)
        switch G(i)
            case {0, 1, 2, 3}
                mG.G0G1G2G3 = G(i);
            case {27, 28, 29}
                mG.G27G28G29 = G(i);
            case {40, 41, 42}
                mG.G40G41G42 = G(i);
            case {90, 91}
                mG.G90G91 = G(i);
            case {94, 95}
                mG.G94G95 = G(i);
        endswitch
    endfor
endfunction


# Find value of the address
function value = valueAtAddr(block, addr)
    value = NaN;
    for i = 1 : length(block)
        if (strcmp(block(i).tokType, addr))
            value = block(i).content;
            break;
        endif
    endfor
endfunction


# Convert parsed block to string
function str = block2str(block)
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
                error("label found while converting block to string");
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

