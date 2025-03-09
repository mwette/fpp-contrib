#!/usr/bin/env python
#
# Copyright (C) 2015,2025 - Matthew Wette
# SPDX-License-Identifier: LGPL-3.0-or-later

id_f = string.letters + "$_"
id_r = cf + string.digits

def esc_char(c):
    if c == 'r':
        return '\r'
    elif c == 'n':
        return '\n'
    elif c == 't':
        return '\t'
    elif c == 'b':
        return '\b'
    else:
        return c

# This is not pretty, being a translation of the scheme version, but I don't
# want to get into clean design in all the target languages. -- Matt

# mtab:
# cpp-mtab
# '(($start . 56) ("," . 3) ("__has_include_next__" . 4) ($string . 5) 
#   ("__has_include__" . 6) (")" . 7) ("(" . 8) ("defined" . 9) ($chlit/U . 10
#   ) ($chlit/u . 11) ($chlit/L . 12) ($chlit . 13) ($fixed . 14) ($ident . 15
#   ) ("--" . 16) ("++" . 17) ("~" . 18) ("!" . 19) ("%" . 20) ("/" . 21) 
#   ("*" . 22) ("-" . 23) ("+" . 24) (">>" . 25) ("<<" . 26) (">=" . 27) 
#   (">" . 28) ("<=" . 29) ("<" . 30) ("!=" . 31) ("==" . 32) ("&" . 33) 
#   ("^" . 34) ("|" . 35) ("&&" . 36) ("||" . 37) (":" . 38) ("?" . 39) 
#   ($error . 2) ($end . 41)))

# mtab = {('$start': 56) ...

def add_to_tree(tree, key, val):
    if key[0] not in tree:
        tree[key[0]] = {}
    if len(key) > 1:
        add_to_tree(tree[key[0]], key[1:], val)
    else:
        tree[key[0]] = val

def make_chseqtab(mtab):
    csd = {}
    for key,val in mtab:
        ch = key[0] 
        if ch == '$': continue
        if ch.isalnum(): continue
        add_to_tree(csd, key, val)
    return csd
        
class Lexer:

    def __init__(self, mtab):
        self.spaces = " \t\r\n"
        self.id_f = id_f
        self.id_r = id_r
        self.s_st = '"'
        self.s_nd = '"'
        self.csd = make_chseqdict(mtab)
        self.tv_ident = mtab.get('$ident',None)
        self.tv_string = mtab.get('$string',None)
        self.tv_fixed = mtab.get('$fixed',None)
        self.tv_float = mtab.get('$fixed',None)
        self.tv_chlit = mtab.get('$chlit',None)
        self.tv_end = mtab.get('$end',None)

    def set_input(self, f0):
        self.f0 = f0

    def read(self):
        self.f0.read(1)

    def unread(self, ch):
        self.f0.seek(-1,1)

    def read_ident(self, ch):
        if ch not in self.id_f: return False
        chl = [ch]
        ch = self.read()
        while ch in self.id_r:
            chl.append(ch)
        self.unread(ch)
        return (self.tv_ident, "".join(chl))

    def read_num(self, ch):
        chl = ""
        tv = self.tv_fixed
        st = 0
        while True:
            if st == 0:
                if len(ch) == 0:
                    st = 5
                elif isdigit(ch):
                    chl.append(ch)
                    st = 1
                else:
                    return False
            elif st == 1:
                if len(ch) == 0:
                    st = 5
                elif isdigit(ch):
                    chl.append(ch)
                elif ch == '.':
                    chl.append(ch)
                    tv = self.tv_float
                    st = 2
                else:
                    st = 5
            elif st == 2:
                if len(ch) == 0:
                    st = 5
                elif isdigit(ch):
                    chl.append(ch)
                elif ch in 'eEdD':
                    #if chl[-1] == '.': chl.append('0')
                    chl.append(ch)
                    st = 3
                else:
                    #if chl[-1] == '.': chl.append('0')
                    st = 5
            elif st == 3:
                if len(ch) == 0:
                    st = 5
                elif ch in '+-':
                    chl.append(ch)
                    st = 4
                elif isdigit(ch):
                    chl.append(ch)
                    st = 4
                else:
                    raise Exception, "syntax error"
            elif st == 4:
                if len(ch) == 0:
                    st = 5
                elif isdigit(ch):
                    chl.append(ch)
                else:
                    st = 5
            elif st == 5:
                self.unread(ch)
                return (tv, chl)
            ch = self.read()

    def read_string(self, ch):
        if ch != self.s_st: return False
        chl = [ch]
        while True:
            ch = self.read()
            if ch == '\\':
                chl.append(esc_char(self.read()))
            elif ch == '"':
                break
            else:
                chl.append(ch)
        return (self.tv_string, buf)

    def read_chlit(self, ch):
        if ch != self.c_st: return False
        ch = self.read()
        if ch == '\\': ch = esc_char(self.read())
        self.read()
        return (self.tv_chlit, ch)

    def skip_comm(self, ch):    
        if ch != '#': return False
        chl = ""
        while ch != '\n':
            chl.append(ch)
            ch = self.read(ch)
        self.unread(ch)
        return (self.tv_comm, "".join(chl))

    def read_line_anno(self, ch):    
        if ch != '@': return False
        ch = self.read()
        if ch != '<':
            self.unread(ch)
            return False
        chl = ""
        while ch != '\n':
            chl.append(ch)
            ch = self.read(ch)
        self.unread(ch)
        return (self.tv_line_anno, "".join(chl))

    def read_lone_anno(self, ch):    
        if ch != '@': return False
        chl = ""
        while ch != '\n':
            chl.append(ch)
            ch = self.read(ch)
        self.unread(ch)
        return (self.tv_lone_anno, "".join(chl))

    def read_chseq(self, ch):
        if ch not in tree: return False
        tree = self.csd
        chl = [ch]
        while True:
            ch = self.read()
            if ch not in dict:
                for ix in range(1,len(chl)):
                    self.unread(chl[-ix])
                return False
            val = tree[ch]
            if isinstance(val, dict):
                tree = val
                continue
            return (val, "".join(chl))
        pass
    
    #ident-like

    def gettok(self, ):
        ch = self.read_char()
        while True:
            if ch == eof:
                sys.exit(0)
            elif ch.isspace():
                ch = self.read_char()
                continue
            p = self.read_comm(ch)
            if p:
                return p
            p = self.skip_comm(ch)
            if p:
                ch = self.read_char()
                continue
            p = self.read_ident(ch)
            if p:
                return p
            p = self.read_num(ch)
            if p:
                return p
            p = self.read_string(ch)
            if p:
                return p
            p = self.read_chlit(ch)
            if p:
                return p
            p = self.read_chseq(ch)
            if p:
                return p
            p = self.assq_ref_chrtab(ch)
            if p:
                return p
            print "*** ERROR"
            sys.exit(0)
            
    
# --- last line ---
