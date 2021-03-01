# MPLAB IDE generated this makefile for use with GNU make.
# Project: ESLab_Project.mcp
# Date: Tue Dec 29 11:16:36 2020

AS = MPASMWIN.exe
CC = 
LD = mplink.exe
AR = mplib.exe
RM = rm

ESLab_Project.cof : ESLab_Project.o
	$(CC) /p16F877A "ESLab_Project.o" /u_DEBUG /z__MPLAB_BUILD=1 /z__MPLAB_DEBUG=1 /o"ESLab_Project.cof" /M"ESLab_Project.map" /W /x

ESLab_Project.o : ESLab_Project.asm ../../../../../../Program\ Files\ (x86)/Microchip/MPASM\ Suite/P16F877A.INC
	$(AS) /q /p16F877A "ESLab_Project.asm" /l"ESLab_Project.lst" /e"ESLab_Project.err" /d__DEBUG=1

clean : 
	$(CC) "ESLab_Project.o" "ESLab_Project.hex" "ESLab_Project.err" "ESLab_Project.lst" "ESLab_Project.cof"

