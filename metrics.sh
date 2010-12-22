#!/bin/sh

gnatmetric -files metrics.txt -cargs -I. -I../agpl -I../player -I../agpl/xmlada -I../agpl/trace_true -I../agpl/agpl-gdk -I/usr/local/include/gtkada -Iexpres-gtk/visor/src -Iexpres-gtk -Imain -Iplayer -Imbicp -I../agpl/concorde -I../agpl/mw -Iexpres-gtk/main
