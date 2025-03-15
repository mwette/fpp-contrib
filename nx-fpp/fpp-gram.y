// Copyright 2025 Matthew Wette
// SPDX-License-Identifier: Apache-2.0
%token DO
%token ENTER
%token EXIT
%token ENTRY
%token INITIAL
%token SIGNAL
%token GUARD
%token ACTION
%token ELSE
%token IF
%token CHOICE
%token STRING
%token BOOL
%token F64
%token F32
%token U64
%token I64
%token U32
%token I32
%token U16
%token I16
%token U8
%token I8
%token '.'
%token _ident
%token _fixed
%token _float
%token '/'
%token '*'
%token '-'
%token '+'
%token WITH
%token MATCH
%token MACHINE
%token STATE
%token LOCATE
%token HOOK
%token DROP
%token BLOCK
%token ASSERT
%token REF
%token GROUP
%token PACKET
%token OMIT
%token PACKETS
%token UNMATCHED
%token HEALTH
%token CONNECTIONS
%token PRIVATE
%token IMPORT
%token TOPOLOGY
%token CPU
%token STACK
%token SIZE
%token QUEUE
%token AT
%token BASE
%token INSTANCE
%token CONTAINER
%token RECORD
%token YELLOW
%token ORANGE
%token RED
%token CHANGE
%token ON
%token ALWAYS
%token UPDATE
%token SAVE
%token WARNING
%token FATAL
%token DIAGNOSTIC
%token LOW
%token HIGH
%token ACTIVITY
%token THROTTLE
%token ID
%token SEVERITY
%token SYNC
%token GUARDED
%token ASYNC
%token TIME
%token TEXT
%token TELEMETRY
%token SEND
%token REQUEST
%token PRODUCT
%token SET
%token GET
%token PARAM
%token EVENT
%token RESP
%token REG
%token RECV
%token OPCODE
%token COMMAND
%token INTERNAL
%token PRIORITY
%token SERIAL
%token OUTPUT
%token INPUT
%token QUEUED
%token PASSIVE
%token ACTIVE
%token COMPONENT
%token ChSeq_62_45
%token ')'
%token '('
%token PORT
%token FORMAT
%token ']'
%token '['
%token STRUCT
%token DEFAULT
%token ':'
%token ENUM
%token ARRAY
%token TYPE
%token '='
%token CONSTANT
%token '}'
%token '{'
%token MODULE
%token _string
%token INCLUDE
%token ';'
%token '\n'
%token ','
%token _end
%token _lone_anno
%token _code_anno
%token _lone_comm
%token _code_comm
%define lr.default-reduction accepting
%expect 1
%start translation_unit
%%
elt_sep: ',' ;
elt_sep: '\n' ;
elt_sep: elt_sep '\n' ;
mem_sep: ';' ;
mem_sep: '\n' ;
mem_sep: mem_sep '\n' ;
include_spec: INCLUDE _string ;
translation_unit: module_mem_seq ;
module_mem_seq: %empty ;
module_mem_seq: mod_mem mem_sep module_mem_seq ;
module_mem_seq: include_spec mem_sep module_mem_seq ;
mod_mem: lone_anno ;
mod_mem: component_defn ;
mod_mem: comp_inst_defn ;
mod_mem: topology_defn ;
mod_mem: const_defn ;
mod_mem: module_defn ;
mod_mem: port_defn ;
mod_mem: struct_defn ;
mod_mem: loc_spec ;
mod_mem: abs_type_defn ;
mod_mem: array_defn ;
mod_mem: enum_defn ;
mod_mem: stmach_defn ;
module_defn: MODULE ident '{' module_mem_seq '}' ;
const_defn: CONSTANT ident '=' expr ;
abs_type_defn: TYPE ident ;
array_defn: ARRAY ident '=' index ident ;
enum_defn: enum_defn_3 ;
enum_defn_0: ENUM ident ;
enum_defn_1: enum_defn_0 ;
enum_defn_1: enum_defn_0 ':' type_name ;
enum_defn_2: enum_defn_1 '{' enum_mem_seq '}' ;
enum_defn_3: enum_defn_2 ;
enum_defn_3: enum_defn_2 DEFAULT expr ;
enum_mem_seq: %empty ;
enum_mem_seq: enum_mem elt_sep enum_mem_seq ;
enum_mem: ident ;
enum_mem: ident '=' expr ;
struct_defn: struct_defn_1 ;
struct_defn_0: STRUCT ident '{' struct_mem_seq '}' ;
struct_defn_1: struct_defn_0 ;
struct_defn_1: struct_defn_0 DEFAULT expr ;
struct_mem_seq: %empty ;
struct_mem_seq: struct_mem mem_sep struct_mem_seq ;
struct_mem: struct_mem_3 ;
struct_mem: struct_mem_3 _code_anno ;
struct_mem_0: ident ':' ;
struct_mem_1: struct_mem_0 ;
struct_mem_1: struct_mem_0 '[' expr ']' ;
struct_mem_2: struct_mem_1 type_name ;
struct_mem_3: struct_mem_2 ;
struct_mem_3: struct_mem_2 FORMAT string ;
port_defn: port_defn_2 ;
port_defn_0: PORT ident ;
port_defn_1: port_defn_0 ;
port_defn_1: port_defn_0 '(' param_list ')' ;
port_defn_2: port_defn_1 ;
port_defn_2: port_defn_1 ChSeq_62_45 type_name ;
component_defn: comp_kind COMPONENT ident '{' comp_mem_seq '}' ;
comp_kind: ACTIVE ;
comp_kind: PASSIVE ;
comp_kind: QUEUED ;
comp_mem_seq: %empty ;
comp_mem_seq: comp_mem mem_sep comp_mem_seq ;
comp_mem_seq: include_spec mem_sep comp_mem_seq ;
comp_mem: lone_anno ;
comp_mem: enum_defn ;
comp_mem: struct_defn ;
comp_mem: int_port_spec ;
comp_mem: port_inst ;
comp_mem: port_match_spec ;
comp_mem: command_spec ;
comp_mem: event_spec ;
comp_mem: param_spec ;
comp_mem: record_spec ;
comp_mem: prod_cont_spec ;
comp_mem: tlm_chan_spec ;
comp_mem: stmach_defn ;
comp_mem: stmach_inst ;
port_inst: gen_port_inst_4 ;
port_inst: spc_port_inst_3 ;
gen_port_inst_0: input_port_kind INPUT PORT ident ':' ;
gen_port_inst_0: OUTPUT PORT ident ':' ;
gen_port_inst_1: gen_port_inst_0 ;
gen_port_inst_1: gen_port_inst_0 '[' expr ']' ;
gen_port_inst_2: gen_port_inst_1 qual_ident ;
gen_port_inst_2: gen_port_inst_1 SERIAL ;
gen_port_inst_3: gen_port_inst_2 ;
gen_port_inst_3: gen_port_inst_2 PRIORITY expr ;
gen_port_inst_4: gen_port_inst_3 ;
gen_port_inst_4: gen_port_inst_3 queue_full_beh ;
spc_port_inst_0: spc_port_kind PORT ident ;
spc_port_inst_1: spc_port_inst_0 ;
spc_port_inst_1: input_port_kind spc_port_inst_0 ;
spc_port_inst_2: spc_port_inst_1 ;
spc_port_inst_2: spc_port_inst_1 PRIORITY expr ;
spc_port_inst_3: spc_port_inst_2 ;
spc_port_inst_3: spc_port_inst_2 queue_full_beh ;
int_port_spec: int_port_defn_3 ;
int_port_defn_0: INTERNAL PORT ident ;
int_port_defn_1: int_port_defn_0 ;
int_port_defn_1: int_port_defn_0 '(' param_list ')' ;
int_port_defn_2: int_port_defn_1 ;
int_port_defn_2: int_port_defn_1 PRIORITY expr ;
int_port_defn_3: int_port_defn_2 ;
int_port_defn_3: int_port_defn_2 queue_full_beh ;
command_spec: cmd_spec_4 ;
cmd_spec_0: input_port_kind COMMAND ident ;
cmd_spec_1: cmd_spec_0 ;
cmd_spec_1: cmd_spec_0 '(' param_list ')' ;
cmd_spec_2: cmd_spec_1 ;
cmd_spec_2: cmd_spec_1 OPCODE expr ;
cmd_spec_3: cmd_spec_2 ;
cmd_spec_3: cmd_spec_2 PRIORITY expr ;
cmd_spec_4: cmd_spec_3 ;
cmd_spec_4: cmd_spec_3 queue_full_beh ;
spc_port_kind: COMMAND RECV ;
spc_port_kind: COMMAND REG ;
spc_port_kind: COMMAND RESP ;
spc_port_kind: EVENT ;
spc_port_kind: PARAM GET ;
spc_port_kind: PARAM SET ;
spc_port_kind: PRODUCT GET ;
spc_port_kind: PRODUCT RECV ;
spc_port_kind: PRODUCT REQUEST ;
spc_port_kind: PRODUCT SEND ;
spc_port_kind: TELEMETRY ;
spc_port_kind: TEXT EVENT ;
spc_port_kind: TIME GET ;
input_port_kind: ASYNC ;
input_port_kind: GUARDED ;
input_port_kind: SYNC ;
event_spec: event_spec_5 ;
event_spec_0: EVENT ident ;
event_spec_1: event_spec_0 ;
event_spec_1: event_spec_0 '(' param_list ')' ;
event_spec_2: event_spec_1 SEVERITY severity ;
event_spec_3: event_spec_2 ;
event_spec_3: event_spec_2 ID expr ;
event_spec_4: event_spec_3 FORMAT string ;
event_spec_5: event_spec_4 ;
event_spec_5: event_spec_4 THROTTLE expr ;
severity: ACTIVITY HIGH ;
severity: ACTIVITY LOW ;
severity: COMMAND ;
severity: DIAGNOSTIC ;
severity: FATAL ;
severity: WARNING HIGH ;
severity: WARNING LOW ;
param_spec: param_spec_4 ;
param_spec_0: PARAM ident ':' type_name ;
param_spec_1: param_spec_0 ;
param_spec_1: param_spec_0 DEFAULT expr ;
param_spec_2: param_spec_1 ;
param_spec_2: param_spec_1 ID expr ;
param_spec_3: param_spec_2 ;
param_spec_3: param_spec_2 SET OPCODE expr ;
param_spec_4: param_spec_3 ;
param_spec_4: param_spec_3 SAVE OPCODE expr ;
tlm_chan_spec: tlm_chan_5 ;
tlm_chan_0: TELEMETRY ident ':' type_name ;
tlm_chan_1: tlm_chan_0 ;
tlm_chan_1: tlm_chan_0 ID expr ;
tlm_chan_2: tlm_chan_1 ;
tlm_chan_2: tlm_chan_1 UPDATE tlm_update ;
tlm_chan_3: tlm_chan_2 ;
tlm_chan_3: tlm_chan_2 FORMAT string ;
tlm_chan_4: tlm_chan_3 ;
tlm_chan_4: tlm_chan_3 LOW '{' tlm_lim_seq '}' ;
tlm_chan_5: tlm_chan_4 ;
tlm_chan_5: tlm_chan_4 HIGH '{' tlm_lim_seq '}' ;
tlm_update: ALWAYS ;
tlm_update: ON CHANGE ;
tlm_lim_seq: %empty ;
tlm_lim_seq: tlm_lim elt_sep tlm_lim_seq ;
tlm_lim: RED expr ;
tlm_lim: ORANGE expr ;
tlm_lim: YELLOW expr ;
record_spec: record_spec_2 ;
record_spec_0: PRODUCT RECORD ident ':' type_name ;
record_spec_1: record_spec_0 ;
record_spec_1: record_spec_0 ARRAY ;
record_spec_2: record_spec_1 ;
record_spec_2: record_spec_1 ID expr ;
prod_cont_spec: cont_spec_2 ;
cont_spec_0: PRODUCT CONTAINER ident ;
cont_spec_1: cont_spec_0 ;
cont_spec_1: cont_spec_0 ID expr ;
cont_spec_2: cont_spec_1 ;
cont_spec_2: cont_spec_1 DEFAULT PRIORITY expr ;
comp_inst_defn: comp_inst_7 ;
comp_inst_0: INSTANCE ident ':' qual_ident BASE ID expr ;
comp_inst_1: comp_inst_0 ;
comp_inst_1: comp_inst_0 TYPE string ;
comp_inst_2: comp_inst_1 ;
comp_inst_2: comp_inst_1 AT string ;
comp_inst_3: comp_inst_2 ;
comp_inst_3: comp_inst_2 QUEUE SIZE expr ;
comp_inst_4: comp_inst_3 ;
comp_inst_4: comp_inst_3 STACK SIZE expr ;
comp_inst_5: comp_inst_4 ;
comp_inst_5: comp_inst_4 PRIORITY expr ;
comp_inst_6: comp_inst_5 ;
comp_inst_6: comp_inst_5 CPU expr ;
comp_inst_7: comp_inst_6 ;
comp_inst_7: comp_inst_6 '{' string '}' ;
topology_defn: TOPOLOGY ident '{' topo_mem_seq '}' ;
topo_mem_seq: %empty ;
topo_mem_seq: topo_mem mem_sep topo_mem_seq ;
topo_mem_seq: include_spec mem_sep topo_mem_seq ;
topo_mem: lone_anno ;
topo_mem: comp_inst_spec ;
topo_mem: conn_graph_spec ;
topo_mem: tlm_pktset_spec ;
topo_mem: IMPORT qual_ident ;
comp_inst_spec: INSTANCE ident ;
comp_inst_spec: PRIVATE INSTANCE ident ;
conn_graph_spec: CONNECTIONS ident '{' conn_seq '}' ;
conn_graph_spec: pattern_kind CONNECTIONS INSTANCE qual_ident ;
conn_graph_spec: pattern_kind CONNECTIONS INSTANCE qual_ident '{' qual_ident_seq '}' ;
pattern_kind: COMMAND ;
pattern_kind: EVENT ;
pattern_kind: HEALTH ;
pattern_kind: PARAM ;
pattern_kind: TELEMETRY ;
pattern_kind: TIME ;
pattern_kind: TEXT EVENT ;
conn_seq: %empty ;
conn_seq: connection elt_sep conn_seq ;
connection: conn_from ChSeq_62_45 conn_to ;
connection: UNMATCHED conn_from ChSeq_62_45 conn_to ;
conn_from: qual_ident ;
conn_from: qual_ident '[' expr ']' ;
conn_to: qual_ident ;
conn_to: qual_ident '[' expr ']' ;
tlm_pktset_spec: TELEMETRY PACKETS ident '{' tlm_pktgrp_mem_seq '}' ;
tlm_pktset_spec: TELEMETRY PACKETS ident '{' tlm_pktgrp_mem_seq '}' OMIT '{' tlm_chan_id_seq '}' ;
tlm_pktgrp_mem_seq: %empty ;
tlm_pktgrp_mem_seq: tlm_pkt_spec elt_sep tlm_pktgrp_mem_seq ;
tlm_pktgrp_mem_seq: include_spec elt_sep tlm_pktgrp_mem_seq ;
tlm_pkt_spec: PACKET ident GROUP expr '{' tlm_pkt_mem_seq '}' ;
tlm_pkt_spec: PACKET ident GROUP expr ID expr '{' tlm_pkt_mem_seq '}' ;
tlm_pkt_mem_seq: %empty ;
tlm_pkt_mem_seq: qual_ident elt_sep tlm_pkt_mem_seq ;
tlm_pkt_mem_seq: include_spec elt_sep tlm_pkt_mem_seq ;
tlm_chan_id_seq: %empty ;
tlm_chan_id_seq: tlm_chan_id_seq elt_sep qual_ident ;
param_list: param_list_1 ;
param_list_1: %empty ;
param_list_1: formal_param elt_sep param_list_1 ;
formal_param: formal_param_1 ;
formal_param: formal_param_1 _code_anno ;
formal_param_1: ident ':' type_name ;
formal_param_1: REF ident ':' type_name ;
queue_full_beh: queue_full_beh_1 ;
queue_full_beh_1: ASSERT ;
queue_full_beh_1: BLOCK ;
queue_full_beh_1: DROP ;
queue_full_beh_1: HOOK ;
loc_spec: LOCATE INSTANCE qual_ident AT string ;
loc_spec: LOCATE COMPONENT qual_ident AT string ;
loc_spec: LOCATE CONSTANT qual_ident AT string ;
loc_spec: LOCATE PORT qual_ident AT string ;
loc_spec: LOCATE STATE MACHINE qual_ident AT string ;
loc_spec: LOCATE TOPOLOGY qual_ident AT string ;
loc_spec: LOCATE TYPE qual_ident AT string ;
port_match_spec: MATCH ident WITH ident ;
expr: add_expr ;
add_expr: mul_expr ;
add_expr: add_expr '+' mul_expr ;
add_expr: add_expr '-' mul_expr ;
mul_expr: unary_expr ;
mul_expr: mul_expr '*' unary_expr ;
mul_expr: mul_expr '/' unary_expr ;
unary_expr: prim_expr ;
unary_expr: '-' unary_expr ;
prim_expr: qual_ident ;
prim_expr: number ;
prim_expr: string ;
prim_expr: '[' expr_seq ']' ;
prim_expr: '{' struct_elt_seq '}' ;
prim_expr: '(' expr ')' ;
expr_seq: expr ;
expr_seq: expr elt_sep expr_seq ;
struct_elt_seq: %empty ;
struct_elt_seq: ident '=' expr elt_sep struct_elt_seq ;
number: _float ;
number: _fixed ;
ident: _ident ;
string: _string ;
lone_anno: _lone_anno ;
qual_ident: qual_ident_1 ;
qual_ident_1: ident ;
qual_ident_1: qual_ident_1 '.' ident ;
qual_ident_seq: %empty ;
qual_ident_seq: qual_ident elt_sep qual_ident_seq ;
index: '[' expr ']' ;
type_name: qual_ident ;
type_name: I8 ;
type_name: U8 ;
type_name: I16 ;
type_name: U16 ;
type_name: I32 ;
type_name: U32 ;
type_name: I64 ;
type_name: U64 ;
type_name: F32 ;
type_name: F64 ;
type_name: BOOL ;
type_name: STRING ;
type_name: STRING SIZE expr ;
stmach_inst: stmach_inst_2 ;
stmach_inst: stmach_inst_2 _code_anno ;
stmach_inst_0: STATE MACHINE INSTANCE ident ':' qual_ident ;
stmach_inst_1: stmach_inst_0 ;
stmach_inst_1: stmach_inst_0 PRIORITY expr ;
stmach_inst_2: stmach_inst_1 ;
stmach_inst_2: stmach_inst_1 queue_full_beh ;
stmach_defn: STATE MACHINE ident ;
stmach_defn: STATE MACHINE ident '{' stmach_mem_seq '}' ;
stmach_mem_seq: %empty ;
stmach_mem_seq: stmach_mem mem_sep stmach_mem_seq ;
stmach_mem: lone_anno ;
stmach_mem: CHOICE ident '{' IF ident trans_expr ELSE trans_expr '}' ;
stmach_mem: ACTION ident ;
stmach_mem: ACTION ident ':' type_name ;
stmach_mem: GUARD ident ;
stmach_mem: GUARD ident ':' type_name ;
stmach_mem: SIGNAL ident ;
stmach_mem: SIGNAL ident ':' type_name ;
stmach_mem: INITIAL trans_expr ;
stmach_mem: state_defn ;
state_defn: STATE ident ;
state_defn: STATE ident '{' state_defn_mem_seq '}' ;
state_defn_mem_seq: %empty ;
state_defn_mem_seq: state_defn_mem mem_sep state_defn_mem_seq ;
state_defn_mem: lone_anno ;
state_defn_mem: INITIAL trans_expr ;
state_defn_mem: CHOICE ident '{' IF ident trans_expr ELSE trans_expr '}' ;
state_defn_mem: state_defn ;
state_defn_mem: state_trans_spec ;
state_defn_mem: ENTRY do_expr ;
state_defn_mem: EXIT do_expr ;
state_trans_spec: st_tran_spec_2 ;
st_tran_spec_0: ON ident ;
st_tran_spec_1: st_tran_spec_0 ;
st_tran_spec_1: st_tran_spec_0 IF ident ;
st_tran_spec_2: st_tran_spec_1 trans_or_do ;
trans_expr: trans_expr_1 ;
trans_expr_0: ENTER qual_ident ;
trans_expr_1: trans_expr_0 ;
trans_expr_1: do_expr trans_expr_0 ;
do_expr: DO '{' action_seq '}' ;
action_seq: %empty ;
action_seq: ident elt_sep action_seq ;
trans_or_do: trans_expr ;
trans_or_do: do_expr ;

