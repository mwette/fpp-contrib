# nyparse.py

# Copyright (C) 2025 Matthew Wette
# SPDX-License-Identifier: LGPL-3.0-or-later


DEFAULT = 1

def parser():
    state = [0]
    stack = [None]
    nval = False
    lval = False
    while True:
        if not (nval or lval):
            lval = getlex()
            continue
        lalal = nval if nval else lval
        tval = laval[0]
        sval = laval[1]
        stxl = pat_v[state[-1]]
        if tval in stxl:
            stx = stxl[tval]
        elif (tval not in skip_if_unexp) and stxl[DEFAULT]:
            stx = stxl[DEFAULT]
        else:
            error()
        #
        if stx == False:        # error
            parse_error()
        elif stx < 0:           # reduce
            gx = abs(stx)
            gl = len_v[gx]
            SS = xct[gx](stack)
        elif stx > 0:           # shift
            state.append(stx)
            stack.append(sval)
            nval = False
            lval = lval if nval != False else False
        else:
            return stack[-1]
        pass 

# --- last line ---
