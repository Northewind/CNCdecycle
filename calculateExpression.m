# Evaluate the expression represented as syntax tree
function res = calculateExpression(syntaxTree)
    global vars;
    [isItAssign, var, syntaxTree] = checkAssignment(syntaxTree);
    res = calculate(syntaxTree);
    if (isItAssign)
        vars.(var) = res;
    endif
endfunction


# Used token types:
#   0 - no token
#   1 - constant
#   2 - var
#   3 - func
#   4 - operator
#   5 - other expression


# Is it assignment
function [isItAssign, var, expr] = checkAssignment(tree)
    if (tree.type == 4 && tree.value == "=")
        isItAssign = true;
        var = tree.arg1.value;
        expr = tree.arg2;
    else
        isItAssign = false;
        var = "";
        expr = tree;
    endif
endfunction


# Calculate the tree
function res = calculate(tree)
    global vars;
    switch (tree.type)
        case 1
            res = tree.value;
        case 2
            res = vars.(tree.value);
        case 3
            arg = calculate(tree.arg1);
            switch (tree.value)
                case "SIN"
                    res = sind(arg);
                case "COS"
                    res = cosd(arg);
                case "TAN"
                    res = tand(arg);
                case "ARS"
                    res = asind(arg);
                case "ARC"
                    res = acosd(arg);
                case "ART"
                    res = atand(arg);
                case "SQR"
                    res = arg*arg;
                case "ABS"
                    res = abs(arg);
                case "INT"
                    res = fix(arg);
                case "NEG"
                    res = -arg;
                case "MOD"
                    [n,d] = rat(arg);
                    res = mod(n,d);
                otherwise
                    error(["unknown function: ", tree.value]);
            endswitch
        case 4
            arg1 = calculate(tree.arg1);
            arg2 = calculate(tree.arg2);
            switch (tree.value)
                case "+"
                    res = arg1 + arg2;
                case "-"
                    res = arg1 - arg2;
                case "*"
                    res = arg1 * arg2;
                case "/"
                    res = arg1 / arg2;
            endswitch
        case 5
            warning("expression found");
            res = calculate(tree.value);
    endswitch
endfunction
