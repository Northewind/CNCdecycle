function CNCdecycle()
    clear;
    c = config();
    icode = readInputFile(c.inputFile);
    switch c.type
        case {"Balt-System" "NC" "nc", "balt-system"}
            interpreter = @BSinterpreter;
        otherwise
            error("CNC type is unsupported");
    endswitch
    ocode = interpreter(icode, c.maxIterations);
    writeOutputFile(c.outputFile, ocode);
endfunction


# Get settings
function c = config()
    [opt val] = textread("conf", "%s=%s");
    for i = 1 : length(opt)
        c.(opt{i}) = val{i};
    endfor
    c.maxIterations = strread(c.maxIterations, "%d");
endfunction


# Read input CNC code
function icode = readInputFile(ifile)
    # Variant 1
    #icode = textread(ifile, "%s", "delimiter", "\n");
    # Variant 2 (read keeping spaces)
    icode = [];
    fid = fopen (ifile, "r");
    i = 0;
    while (true)
        s = fgetl(fid);
        if (s == -1)    # EOF
            break;
        endif
        icode{++i} = s;
    endwhile
    fclose(fid);
endfunction


# Write output CNC code
function writeOutputFile(ofile, ocode)
    fid = fopen(ofile, "w");
    for i = 1 : length(ocode)
        if (fputs(fid, [ocode{i} "\n"]) == -1)
            fclose(fid);
            error("Some error occured while writing output code");
        endif
    endfor
    fclose(fid);
endfunction
