@echo off

@rem Just run the PowerShell script.
@rem use -noexit for debugging

PowerShell -NoProfile -ExecutionPolicy Bypass -Command "& '\\wsl.localhost\Ubuntu\home\aplacaba\dotfiles\scripts\emacs.ps1'"
