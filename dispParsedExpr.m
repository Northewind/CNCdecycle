# Display parsed expression as syntax tree (for debugging)
function dispParsedExpr(expr)
    persistent indent = "";
    for i = 1 : length(expr)
        t = expr(i);
        if (t.type == 5)
            printf("%sexpression:\n", indent);
            indent = increaseIndent(indent);
            dispParsedExpr(t.value);
            indent = decreaseIndent(indent);
        else
            val = t.value;
            if (isnumeric(val))
                val = num2str(val);
            endif
            printf("%s%s \"%s\"\n", indent, tokenType2str(t.type), val);
            if (t.type == 3 || t.type == 4)
                if (isfield(t, "arg1"))
                    printf("%sarg1:\n", indent);
                    indent = increaseIndent(indent);
                    dispParsedExpr(t.arg1);
                    indent = decreaseIndent(indent);
                endif
                if (isfield(t, "arg2"))
                    printf("%sarg2:\n", indent);
                    indent = increaseIndent(indent);
                    dispParsedExpr(t.arg2);
                    indent = decreaseIndent(indent);
                endif
            endif
        endif
    endfor
endfunction


# Return symbolic token type by code (for debugging)
function str = tokenType2str(code)
    switch (code)
        case 0
            str = "no token";
        case 1
            str = "constant";
        case 2
            str = "variable";
        case 3
            str = "function";
        case 4
            str = "operator";
        case 5
            str = "other expression";
    endswitch
endfunction


# Increase indentation (for debugging)
function newIndent = increaseIndent(indent)
    newIndent = ["    ", indent];
endfunction


# Decrease indentation (for debugging)
function newIndent = decreaseIndent(indent)
    newIndent = indent(5:end);
endfunction