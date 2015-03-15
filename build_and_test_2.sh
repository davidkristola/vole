#!/bin/bash

function mk_t() {
gnatmake -I.. -g test_avm;
}

function mk_v() {
gnatmake -I.. volea;
}

function mk_m() {
gnatmake -I.. volem;
}

function tst_3() {
volec ../test_3.vole && volem ../test_3.volec Driver go 1;
}

function tst_4() {
volec ../test_4.vole && volem ../test_4.volec Driver go 1;
}

cd obj
(mk_t && mk_v && mk_m && ../rebuild && tst_3 && tst_4 && cd .. && obj/test_avm && test.sh) | tee results.txt

