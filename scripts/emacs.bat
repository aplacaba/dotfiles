@echo off

@rem Just run the PowerShell script.
@rem use -noexit for debugging

PowerShell -NoProfile -ExecutionPolicy Bypass -Command "& '%~dp0emacs.ps1'"
